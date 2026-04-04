aggregate_activity_by_day_6_class <- function(ts,
                                             tz = "UTC",
                                             time_col = "timestamp",
                                             activity_col = "final_pred",
                                             file_col = "file_name",
                                             epoch_sec = 15,
                                             wear_threshold_min = 960,
                                             waking_wear_threshold_min = 600) {
  
  # checks
  req_cols <- c(time_col, activity_col, file_col)
  miss_cols <- req_cols[!req_cols %in% names(ts)]
  if (length(miss_cols) > 0) {
    stop("Column(s) not found: ", paste(miss_cols, collapse = ", "))
  }
  
  ts[[time_col]] <- as.POSIXct(ts[[time_col]], tz = tz)
  ts <- ts[order(ts[[time_col]]), , drop = FALSE]
  ts$day_date <- as.Date(ts[[time_col]], tz = tz)
  
  ep_min <- epoch_sec / 60
  
  activity_levels <- c("Sed", "Standmov", "Walk",
                       "NightSleep", "NightAwake", "NonWear")
  
  act_chr <- as.character(ts[[activity_col]])
  act_chr[is.na(act_chr)] <- "Other"
  act_chr[!act_chr %in% activity_levels] <- "Other"
  ts$act_clean <- act_chr
  
  # subject/file identifier
  file_id <- unique(ts[[file_col]])
  file_id <- file_id[!is.na(file_id)]
  
  if (length(file_id) == 0) {
    file_id <- NA_character_
  } else if (length(file_id) > 1) {
    warning("Multiple file names detected; using first value")
    file_id <- file_id[1]
  }
  
  out <- ts |>
    dplyr::group_by(day_date) |>
    dplyr::summarise(
      Sed        = sum(act_clean == "Sed", na.rm = TRUE) * ep_min,
      Standmov   = sum(act_clean == "Standmov", na.rm = TRUE) * ep_min,
      Walk       = sum(act_clean == "Walk", na.rm = TRUE) * ep_min,
      NightSleep = sum(act_clean == "NightSleep", na.rm = TRUE) * ep_min,
      NightAwake = sum(act_clean == "NightAwake", na.rm = TRUE) * ep_min,
      NonWear    = sum(act_clean == "NonWear", na.rm = TRUE) * ep_min,
      Other      = sum(act_clean == "Other", na.rm = TRUE) * ep_min,
      
      total_minutes = dplyr::n() * ep_min,
      
      # total wear (includes sleep, excludes NonWear)
      wear_minutes = sum(act_clean != "NonWear", na.rm = TRUE) * ep_min,
      
      # waking wear only (excludes sleep + nonwear)
      waking_wear_minutes = sum(!(act_clean %in% c("NonWear", "NightSleep", "NightAwake")),
                                na.rm = TRUE) * ep_min,
      
      .groups = "drop"
    )
  
  # valid day logic (dual criteria)
  out$valid_day <- as.integer(
    out$wear_minutes >= wear_threshold_min &
      out$waking_wear_minutes >= waking_wear_threshold_min
  )
  
  # identifiers / calendar info
  out$file_name   <- file_id
  out$day_of_week <- weekdays(out$day_date)
  out$study_day   <- as.integer(out$day_date - min(out$day_date, na.rm = TRUE) + 1)
  
  out <- out |>
    dplyr::select(file_name, day_date, day_of_week, study_day, dplyr::everything()) |>
    dplyr::arrange(day_date)
  
  out
}