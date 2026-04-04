# ============================================================
# main_pipeline.R
# ActiGraph raw data processing pipeline for CP models
# Supports wrist or hip random forest models
# Supports optional sleep processing
# ============================================================

rm(list = ls())
gc(verbose = FALSE)

options(digits.secs = 3)

# ------------------------------------------------------------
# Load configuration
# ------------------------------------------------------------
source(file.path("config", "config.R"))

# ------------------------------------------------------------
# Packages
# ------------------------------------------------------------
required_packages <- c(
  "dplyr",
  "data.table",
  "randomForest",
  "tools"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
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

# ------------------------------------------------------------
# Small helper for consistent step messages
# ------------------------------------------------------------
step_msg <- function(txt) {
  cat(sprintf("\n---- %s ----\n", txt))
}

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
  message("Sourced ", length(r_files), " function file(s) from ", functions_dir)
}

# ------------------------------------------------------------
# Ensure output directory exists
# ------------------------------------------------------------
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

# ------------------------------------------------------------
# Source functions
# ------------------------------------------------------------
source_functions("functions")

# ------------------------------------------------------------
# Validate model location
# ------------------------------------------------------------
if (!exists("model_location")) {
  stop("`model_location` is missing from config/config.R. Use 'wrist' or 'hip'.")
}

model_location <- tolower(trimws(model_location))

if (!model_location %in% c("wrist", "hip")) {
  stop("`model_location` must be either 'wrist' or 'hip'.")
}

# ------------------------------------------------------------
# Validate run_sleep
# ------------------------------------------------------------
if (!exists("run_sleep")) {
  run_sleep <- TRUE
}

if (!is.logical(run_sleep) || length(run_sleep) != 1 || is.na(run_sleep)) {
  stop("`run_sleep` must be TRUE or FALSE.")
}

if (!run_sleep) {
  message("Sleep processing is disabled.")
} else {
  message("Sleep processing is enabled.")
}

# ------------------------------------------------------------
# Model file
# ------------------------------------------------------------
if (!exists("models_dir")) {
  models_dir <- "models"
}

model_file <- switch(
  model_location,
  "wrist" = file.path(models_dir, "RF_CP_Wrist.RData"),
  "hip"   = file.path(models_dir, "RF_CP_Hip.RData")
)

if (!file.exists(model_file)) {
  stop("Model file not found: ", model_file)
}

# ------------------------------------------------------------
# Load selected model
# ------------------------------------------------------------
load(model_file)

get_model_object <- function(model_object_name = NULL) {
  if (!is.null(model_object_name) && nzchar(model_object_name)) {
    if (!exists(model_object_name, inherits = TRUE)) {
      stop("Configured model object not found after loading model file: ", model_object_name)
    }
    return(get(model_object_name, inherits = TRUE))
  }
  
  candidate_names <- c(
    paste0(model_location, "_model"),
    model_location,
    "rf_model",
    "model",
    "wrist",
    "hip"
  )
  
  existing_candidates <- candidate_names[candidate_names %in% ls()]
  
  if (length(existing_candidates) == 1) {
    return(get(existing_candidates, inherits = TRUE))
  }
  
  objs <- ls()
  rf_objs <- objs[vapply(
    objs,
    function(x) inherits(get(x, inherits = TRUE), "randomForest"),
    logical(1)
  )]
  
  if (length(rf_objs) == 1) {
    return(get(rf_objs, inherits = TRUE))
  }
  
  stop(
    "Could not uniquely identify the model object after loading ", model_file,
    ".\nSet `model_object_name` explicitly in config/config.R."
  )
}

rf_model <- get_model_object(if (exists("model_object_name")) model_object_name else NULL)

message("Using ", model_location, " model from: ", model_file)

# ------------------------------------------------------------
# Helper: read and standardise raw ActiGraph data
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
# Identify all GT3X files
# ------------------------------------------------------------
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

message("Found ", length(gt3x_files), " .gt3x file(s). Starting processing...")
message("Model location selected: ", model_location)

# ------------------------------------------------------------
# Daily summary output file
# ------------------------------------------------------------
summary_file <- file.path(
  output_dir,
  paste0("DBD_SummaryTable_", model_location, ".csv")
)

# ------------------------------------------------------------
# Main processing loop
# ------------------------------------------------------------
for (file_path in gt3x_files) {
  
  file_name <- tools::file_path_sans_ext(basename(file_path))
  t_file_start <- proc.time()
  
  tryCatch({
    
    step_msg("LOAD RAW DATA")
    message("Processing file: ", file_name)
    
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
    
    step_msg(paste("APPLY", toupper(model_location), "RANDOM FOREST MODEL"))
    
    features$pred_class <- predict(rf_model, newdata = features, type = "response")
    features$ID <- file_name
    features <- features %>% relocate(ID)
    
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
    
    # --------------------------------------------------------
    # Optional sleep processing
    # --------------------------------------------------------
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
    
    # --------------------------------------------------------
    # Sleep metrics only if enabled
    # --------------------------------------------------------
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
    
    daily_summary <- daily_summary %>%
      dplyr::relocate(file_name, day_date, day_of_week, study_day)
    
    write.table(
      daily_summary,
      file = summary_file,
      sep = ",",
      append = TRUE,
      quote = FALSE,
      col.names = !file.exists(summary_file),
      row.names = FALSE
    )
    
    elapsed_sec <- as.numeric((proc.time() - t_file_start)["elapsed"])
    cat(paste0("Completed subject: ", file_name, "\n"))
    cat(
      sprintf(
        "Elapsed processing time (load -> save): %.2f seconds (%.2f minutes)\n",
        elapsed_sec,
        elapsed_sec / 60
      )
    )
    
  }, error = function(e) {
    
    message("\n*** ERROR processing file: ", file_name, " ***")
    message(conditionMessage(e))
    
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