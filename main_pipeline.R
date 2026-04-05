# ============================================================
# main_pipeline.R
# User-friendly ActiGraph raw data processing pipeline
# ============================================================

options(digits.secs = 3)

# ------------------------------------------------------------
# Load default configuration
# ------------------------------------------------------------
source(file.path("config", "config.R"))

# ------------------------------------------------------------
# Source all function scripts recursively from /functions
# ------------------------------------------------------------
source_functions <- function(functions_dir = "functions") {
  if (!dir.exists(functions_dir)) {
    stop("Functions directory not found: ", functions_dir)
  }
  
  r_files <- list.files(
    path = functions_dir,
    pattern = "\\.[Rr]$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  if (length(r_files) == 0) {
    stop("No .R files found in functions directory: ", functions_dir)
  }
  
  invisible(lapply(r_files, source))
}

# ------------------------------------------------------------
# Console helper
# ------------------------------------------------------------
step_msg <- function(txt) {
  cat(sprintf("\n---- %s ----\n", txt))
  flush.console()
}

# ------------------------------------------------------------
# Optional folder chooser
# ------------------------------------------------------------
choose_directory_if_needed <- function(path = NULL, caption = "Select folder") {
  
  if (!is.null(path) && nzchar(path)) {
    return(path)
  }
  
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    
    chosen <- rstudioapi::selectDirectory(caption = caption)
    
    if (is.null(chosen) || chosen == "") {
      stop("Folder selection cancelled.")
    }
    
    return(chosen)
  }
  
  if (.Platform$OS.type == "windows") {
    chosen <- utils::choose.dir(caption = caption)
    
    if (is.na(chosen) || is.null(chosen)) {
      stop("Folder selection cancelled.")
    }
    
    return(chosen)
  }
  
  stop("No directory supplied, and interactive selection not supported in this environment.")
}

# ------------------------------------------------------------
# Ensure daily summary always has sleep columns
# ------------------------------------------------------------
ensure_sleep_summary_columns <- function(df, tz = "UTC") {
  
  required_sleep_cols <- list(
    night_id = as.integer(NA),
    spt_start = as.POSIXct(NA, origin = "1970-01-01", tz = tz),
    spt_end = as.POSIXct(NA, origin = "1970-01-01", tz = tz),
    tib_min = as.numeric(NA),
    tst_min = as.numeric(NA),
    sleep_efficiency_pct = as.numeric(NA),
    sleep_onset_time = as.POSIXct(NA, origin = "1970-01-01", tz = tz),
    sleep_latency_min = as.numeric(NA),
    waso_min = as.numeric(NA),
    total_wake_min = as.numeric(NA),
    last_sleep_time = as.POSIXct(NA, origin = "1970-01-01", tz = tz),
    midpoint_sleep = as.POSIXct(NA, origin = "1970-01-01", tz = tz),
    n_awakenings = as.integer(NA),
    sfi_awakenings_per_hr_sleep = as.numeric(NA),
    n_nonwear_epochs_in_spt = as.integer(NA),
    join_date = as.Date(NA),
    sleep_recorded = as.integer(NA)
  )
  
  for (nm in names(required_sleep_cols)) {
    if (!nm %in% names(df)) {
      df[[nm]] <- required_sleep_cols[[nm]]
    }
  }
  
  df
}

# ------------------------------------------------------------
# Helper: standardise raw ActiGraph data
# ------------------------------------------------------------
prepare_raw_data <- function(file_path, tz = "UTC") {
  ag <- suppressWarnings(read_gt3x(file_path))
  
  if (is.null(ag$header$Sample_Rate)) {
    stop("Sample rate not found in GT3X header for file: ", basename(file_path))
  }
  
  fs <- ag$header$Sample_Rate
  
  df <- as.data.frame(ag$data)
  names(df)[1] <- "timestamp"
  df$timestamp <- as.POSIXct(df$timestamp, tz = tz)
  
  required_axes <- c("x", "y", "z")
  missing_axes <- required_axes[!required_axes %in% names(df)]
  if (length(missing_axes) > 0) {
    stop("Missing expected axis column(s): ", paste(missing_axes, collapse = ", "))
  }
  
  list(
    raw = ag,
    data = df,
    sampling_rate = fs
  )
}

# ------------------------------------------------------------
# Main pipeline function
# ------------------------------------------------------------
run_pipeline <- function(
    input_dir = default_config$input_dir,
    output_dir = default_config$output_dir,
    model_location = default_config$model_location,
    run_sleep = default_config$run_sleep,
    tz = default_config$tz,
    models_dir = default_config$models_dir,
    sampling_rate = default_config$sampling_rate,
    epoch_sec = default_config$epoch_sec,
    modal_filter_span = default_config$modal_filter_span,
    modal_majority_threshold = default_config$modal_majority_threshold,
    nonwear_sd_threshold_g = default_config$nonwear_sd_threshold_g,
    nonwear_min_minutes = default_config$nonwear_min_minutes,
    wear_threshold_min = default_config$wear_threshold_min,
    waking_wear_threshold_min = default_config$waking_wear_threshold_min
) {
  
  # ----------------------------------------------------------
  # Packages
  # ----------------------------------------------------------
  required_packages <- c(
    "dplyr",
    "data.table",
    "randomForest",
    "Rcpp",
    "rstudioapi",
    "tools"
  )
  
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  
  if (length(missing_packages) > 0) {
    stop(
      "Missing required package(s): ",
      paste(missing_packages, collapse = ", "),
      "\nPlease install them before running the pipeline."
    )
  }
  
  library(dplyr)
  library(data.table)
  library(randomForest)
  library(Rcpp)
  
  # ----------------------------------------------------------
  # Source functions
  # ----------------------------------------------------------
  source_functions("functions")
  
  # ----------------------------------------------------------
  # Validate arguments / choose folders if needed
  # ----------------------------------------------------------
  input_dir <- choose_directory_if_needed(
    path = input_dir,
    caption = "Select input folder containing .gt3x files"
  )
  
  output_dir <- choose_directory_if_needed(
    path = output_dir,
    caption = "Select output folder"
  )
  
  model_location <- tolower(trimws(model_location))
  
  if (!model_location %in% c("wrist", "hip")) {
    stop("`model_location` must be either 'wrist' or 'hip'.")
  }
  
  if (!is.logical(run_sleep) || length(run_sleep) != 1 || is.na(run_sleep)) {
    stop("`run_sleep` must be TRUE or FALSE.")
  }
  
  if (!dir.exists(input_dir)) {
    stop("Input directory not found: ", input_dir)
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  message("Input directory: ", input_dir)
  flush.console()
  message("Output directory: ", output_dir)
  flush.console()
  message("Model location: ", model_location)
  flush.console()
  message("Sleep enabled: ", run_sleep)
  flush.console()
  
  # ----------------------------------------------------------
  # Load model
  # ----------------------------------------------------------
  model_file <- switch(
    model_location,
    wrist = file.path(models_dir, "RF_CP_Wrist.RData"),
    hip   = file.path(models_dir, "RF_CP_Hip.RData")
  )
  
  model_object_name <- switch(
    model_location,
    wrist = "wrist",
    hip   = "hip"
  )
  
  if (!file.exists(model_file)) {
    stop("Model file not found: ", model_file)
  }
  
  load(model_file)
  
  if (!exists(model_object_name, inherits = TRUE)) {
    stop("Expected model object '", model_object_name, "' not found in ", model_file)
  }
  
  rf_model <- get(model_object_name, inherits = TRUE)
  
  message("Loaded model from: ", model_file)
  flush.console()
  
  # ----------------------------------------------------------
  # Locate GT3X files
  # ----------------------------------------------------------
  gt3x_files <- list.files(
    path = input_dir,
    pattern = "\\.gt3x$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(gt3x_files) == 0) {
    stop("No .gt3x files found in input directory: ", input_dir)
  }
  
  file_order_num <- suppressWarnings(as.numeric(gsub("[^0-9]", "", basename(gt3x_files))))
  if (!all(is.na(file_order_num))) {
    gt3x_files <- gt3x_files[order(file_order_num, na.last = TRUE)]
  } else {
    gt3x_files <- sort(gt3x_files)
  }
  
  message("Found ", length(gt3x_files), " .gt3x file(s).")
  flush.console()
  
  # ----------------------------------------------------------
  # Daily summary output file
  # ----------------------------------------------------------
  summary_file <- file.path(
    output_dir,
    paste0("DBD_SummaryTable_", model_location, ".csv")
  )
  
  # ----------------------------------------------------------
  # Main processing loop
  # ----------------------------------------------------------
  for (file_path in gt3x_files) {
    
    file_name <- tools::file_path_sans_ext(basename(file_path))
    t_file_start <- proc.time()
    
    tryCatch({
      
      step_msg("LOAD RAW DATA")
      message("Processing file: ", file_name)
      flush.console()
      
      prep <- prepare_raw_data(file_path = file_path, tz = tz)
      df_raw <- prep$data
      fs <- prep$sampling_rate
      
      step_msg("FEATURE EXTRACTION")
      
      features <- extract_acc_features_reshape(
        data = df_raw,
        windowInSec = epoch_sec,
        time_col = "timestamp",
        sampling_rate = sampling_rate
      )
      
      if (nrow(features) == 0) {
        warning("No complete windows available for file: ", file_name, ". Skipping.")
        next
      }
      
      features$ID <- file_name
      features <- features %>% relocate(ID)
      
      step_msg(paste("APPLY", toupper(model_location), "RANDOM FOREST MODEL"))
      
      features$pred_class <- predict(rf_model, newdata = features, type = "response")
      
      step_msg("MODAL SMOOTHING")
      
      filt_features <- modal_filter_df(
        df = features,
        class_col = "pred_class",
        filter_span = modal_filter_span,
        new_col = "filt_class",
        majority_threshold = modal_majority_threshold
      )
      
      step_msg("NON-WEAR DETECTION")
      
      filt_features <- filt_features %>%
        mutate(
          non_wear = as.integer(
            detect_nonwear_from_vm_sd_15s(
              sd_vm = ACC_std_vm,
              sdThreshold = nonwear_sd_threshold_g,
              epoch_sec = epoch_sec,
              min_nonwear_min = nonwear_min_minutes
            )
          )
        )
      
      # ------------------------------------------------------
      # Optional sleep processing
      # ------------------------------------------------------
      if (run_sleep) {
        
        step_msg("SLEEP DETECTION")
        
        z_epochs <- make_epoch_zangle(
          df_raw = df_raw,
          time_col = "timestamp",
          x_col = "x",
          y_col = "y",
          z_col = "z",
          epoch_sec = epoch_sec,
          tz = tz
        )
        
        filt_features <- filt_features %>%
          left_join(
            z_epochs %>% select(timestamp, mean_angleZ, dAngleZ),
            by = "timestamp"
          )
        
        sleep_res <- add_nighttime_from_inbed_singlefile(
          df_raw = df_raw,
          filt_features = filt_features,
          sf = fs,
          ws_sec = 5,
          tz = tz,
          verbose = FALSE
        )
        
        filt_features2 <- sleep_res$filt_features
        sleep_windows <- sleep_res$sleep_windows
        
        filt_features2$sleep_score <- score_sleep_within_period(
          mean_angleZ = filt_features2$mean_angleZ,
          sleep_period = filt_features2$nighttime,
          epoch_sec = epoch_sec,
          postch_thr_deg = 5,
          min_stable_min = 5
        )
        
        filt_features2 <- apply_75pct_rule_to_sleep_blocks(
          filt_features = filt_features2,
          sleep_period_col = "nighttime",
          nonwear_col = "non_wear",
          thr = 0.75,
          use_ge = TRUE,
          valid_col = "sleep_period_valid",
          coerce_col = "sleep_block_coerced"
        )
        
      } else {
        
        step_msg("SLEEP DETECTION SKIPPED")
        
        filt_features2 <- filt_features
        sleep_windows <- data.frame()
        
        filt_features2$mean_angleZ <- NA_real_
        filt_features2$dAngleZ <- NA_real_
        filt_features2$nighttime <- 0L
        filt_features2$sleep_score <- 0L
        filt_features2$sleep_period_valid <- 0L
        filt_features2$sleep_block_coerced <- 0L
      }
      
      step_msg("FINAL ACTIVITY LABELS")
      
      filt_features2 <- add_final_pred_6class(
        filt_features = filt_features2,
        pred_class_col = "filt_class",
        nonwear_col = "non_wear",
        sleep_period_col = "nighttime",
        sleep_score_col = "sleep_score",
        sleep_block_coerced_col = "sleep_block_coerced",
        out_col = "final_pred"
      )
      
      step_msg("SAVE EPOCH-LEVEL OUTPUTS")
      
      save(
        filt_features2,
        sleep_windows,
        file = file.path(output_dir, paste0(file_name, "_", model_location, "_Scored.RData")),
        compress = "xz"
      )
      
      write.csv(
        filt_features2,
        file = file.path(output_dir, paste0(file_name, "_", model_location, "_Scored.csv")),
        quote = TRUE,
        row.names = FALSE
      )
      
      # ------------------------------------------------------
      # Sleep metrics
      # ------------------------------------------------------
      if (run_sleep) {
        
        step_msg("COMPUTE SLEEP METRICS")
        
        sleep_metrics_valid <- compute_sleep_metrics_from_sleep_windows(
          ts = filt_features2,
          sleep_windows = sleep_windows,
          tz = tz,
          activity_col = "final_pred",
          epoch_sec = epoch_sec,
          min_sleep_onset_min = 5,
          min_awake_bout_min = 5,
          require_valid_block = FALSE,
          valid_block_col = "sleep_period_valid"
        )
        
        if (nrow(sleep_metrics_valid) > 0) {
          sleep_metrics_valid$join_date <- sleep_metrics_valid$night_date
          
          sleep_metrics_valid$sleep_recorded <- as.integer(
            !is.na(sleep_metrics_valid$tst_min) & sleep_metrics_valid$tst_min > 0
          )
          
          sleep_vars_to_na <- c(
            "tst_min",
            "sleep_efficiency_pct",
            "sleep_onset_time",
            "sleep_latency_min",
            "waso_min",
            "total_wake_min",
            "last_sleep_time",
            "midpoint_sleep",
            "n_awakenings",
            "sfi_awakenings_per_hr_sleep"
          )
          
          idx_no_sleep <- sleep_metrics_valid$sleep_recorded == 0
          sleep_metrics_valid[idx_no_sleep, sleep_vars_to_na] <- NA
        }
        
      } else {
        sleep_metrics_valid <- data.frame()
      }
      
      # ------------------------------------------------------
      # Daily aggregation
      # ------------------------------------------------------
      step_msg("DAILY AGGREGATION")
      
      daily_summary <- aggregate_activity_by_day_6_class(
        ts = filt_features2,
        tz = tz,
        time_col = "timestamp",
        activity_col = "final_pred",
        file_col = "ID",
        epoch_sec = epoch_sec,
        wear_threshold_min = wear_threshold_min,
        waking_wear_threshold_min = waking_wear_threshold_min
      )
      
      if (run_sleep && nrow(sleep_metrics_valid) > 0) {
        daily_summary <- dplyr::left_join(
          daily_summary,
          sleep_metrics_valid,
          by = c("day_date" = "join_date")
        )
      }
      
      daily_summary <- ensure_sleep_summary_columns(daily_summary, tz = tz)
      
      daily_summary <- daily_summary %>%
        dplyr::relocate(file_name, day_date, day_of_week, study_day)
      
      if (!file.exists(summary_file)) {
        write.table(
          daily_summary,
          file = summary_file,
          sep = ",",
          append = FALSE,
          quote = FALSE,
          col.names = TRUE,
          row.names = FALSE,
          na = ""
        )
      } else {
        write.table(
          daily_summary,
          file = summary_file,
          sep = ",",
          append = TRUE,
          quote = FALSE,
          col.names = FALSE,
          row.names = FALSE,
          na = ""
        )
      }
      
      elapsed_sec <- as.numeric((proc.time() - t_file_start)["elapsed"])
      cat(paste0("Completed subject: ", file_name, "\n"))
      cat(
        sprintf(
          "Elapsed processing time (load -> save): %.2f seconds (%.2f minutes)\n",
          elapsed_sec,
          elapsed_sec / 60
        )
      )
      flush.console()
      
    }, error = function(e) {
      
      message("\n*** ERROR processing file: ", file_name, " ***")
      message(conditionMessage(e))
      flush.console()
      
    }, finally = {
      
      rm(
        list = intersect(
          ls(),
          c(
            "prep",
            "df_raw",
            "fs",
            "features",
            "filt_features",
            "z_epochs",
            "sleep_res",
            "filt_features2",
            "sleep_windows",
            "sleep_metrics_valid",
            "daily_summary"
          )
        )
      )
      gc(verbose = FALSE)
    })
  }
  
  step_msg("PIPELINE COMPLETE")
  message("All files processed.")
  flush.console()
  
  invisible(TRUE)
}

