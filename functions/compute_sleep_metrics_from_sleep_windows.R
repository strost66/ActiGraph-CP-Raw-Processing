compute_sleep_metrics_from_sleep_windows <- function(ts,
                                                     sleep_windows,
                                                     tz = "UTC",
                                                     time_col = "timestamp",
                                                     activity_col = "final_pred",
                                                     sleep_label = "NightSleep",
                                                     awake_label = "NightAwake",
                                                     nonwear_label = "NonWear",
                                                     epoch_sec = 15,
                                                     min_sleep_onset_min = 5,
                                                     min_awake_bout_min = 5,
                                                     require_valid_block = FALSE,
                                                     valid_block_col = "sleep_period_valid") {
  
  ts[[time_col]] <- as.POSIXct(ts[[time_col]], tz = tz)
  ts <- ts[order(ts[[time_col]]), , drop = FALSE]
  tt  <- ts[[time_col]]
  act <- as.character(ts[[activity_col]])
  
  sw <- sleep_windows
  sw$lightsout <- as.POSIXct(sw$lightsout, tz = tz)
  sw$lightson  <- as.POSIXct(sw$lightson,  tz = tz)
  
  min_sleep_onset_epochs <- as.integer((min_sleep_onset_min * 60) / epoch_sec)
  min_awake_epochs       <- as.integer((min_awake_bout_min * 60) / epoch_sec)
  
  rle_bouts <- function(x) {
    r <- rle(x)
    ends <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1L
    data.frame(
      value = r$values,
      start = starts,
      end = ends,
      length = r$lengths
    )
  }
  
  midpoint_time <- function(t1, t2) {
    t1n <- as.numeric(t1)
    t2n <- as.numeric(t2)
    as.POSIXct((t1n + t2n) / 2, origin = "1970-01-01", tz = tz)
  }
  
  empty_result <- function() {
    data.frame(
      night_date = as.Date(character()),
      night_id = integer(),
      spt_start = as.POSIXct(character(), tz = tz),
      spt_end = as.POSIXct(character(), tz = tz),
      tib_min = numeric(),
      tst_min = numeric(),
      sleep_efficiency_pct = numeric(),
      sleep_onset_time = as.POSIXct(character(), tz = tz),
      sleep_latency_min = numeric(),
      waso_min = numeric(),
      total_wake_min = numeric(),
      last_sleep_time = as.POSIXct(character(), tz = tz),
      midpoint_sleep = as.POSIXct(character(), tz = tz),
      n_awakenings = integer(),
      sfi_awakenings_per_hr_sleep = numeric(),
      n_nonwear_epochs_in_spt = integer(),
      stringsAsFactors = FALSE
    )
  }
  
  if (nrow(sw) == 0) {
    return(empty_result())
  }
  
  out <- vector("list", nrow(sw))
  
  for (i in seq_len(nrow(sw))) {
    
    spt_start <- sw$lightsout[i]
    spt_end   <- sw$lightson[i]
    
    idx <- which(tt >= spt_start & tt < spt_end)
    if (length(idx) == 0) next
    
    if (require_valid_block) {
      if (!valid_block_col %in% names(ts)) {
        stop("Missing ", valid_block_col)
      }
      if (sum(ts[[valid_block_col]][idx] == 1, na.rm = TRUE) == 0) next
    }
    
    act_i <- act[idx]
    tt_i  <- tt[idx]
    
    is_sleep <- act_i == sleep_label
    is_awake <- act_i == awake_label
    is_nonwear <- act_i == nonwear_label
    
    tib_min <- length(idx) * epoch_sec / 60
    tst_min <- sum(is_sleep, na.rm = TRUE) * epoch_sec / 60
    n_nonwear_epochs <- sum(is_nonwear, na.rm = TRUE)
    
    if (sum(is_sleep, na.rm = TRUE) == 0) {
      out[[i]] <- data.frame(
        night_id = i,
        spt_start = spt_start,
        spt_end = spt_end,
        tib_min = tib_min,
        tst_min = 0,
        sleep_efficiency_pct = 0,
        sleep_onset_time = as.POSIXct(NA, origin = "1970-01-01", tz = tz),
        sleep_latency_min = NA_real_,
        waso_min = NA_real_,
        total_wake_min = NA_real_,
        last_sleep_time = as.POSIXct(NA, origin = "1970-01-01", tz = tz),
        midpoint_sleep = as.POSIXct(NA, origin = "1970-01-01", tz = tz),
        n_awakenings = NA_integer_,
        sfi_awakenings_per_hr_sleep = NA_real_,
        n_nonwear_epochs_in_spt = n_nonwear_epochs,
        stringsAsFactors = FALSE
      )
      next
    }
    
    r_sleep <- rle_bouts(is_sleep)
    sleep_bouts <- r_sleep[r_sleep$value == TRUE & r_sleep$length >= min_sleep_onset_epochs, , drop = FALSE]
    onset_pos <- if (nrow(sleep_bouts) > 0) sleep_bouts$start[1] else which(is_sleep)[1]
    
    sleep_onset_time <- tt_i[onset_pos]
    sleep_latency_min <- as.numeric(difftime(sleep_onset_time, spt_start, units = "mins"))
    
    last_sleep_pos <- tail(which(is_sleep), 1)
    last_sleep_time <- tt_i[last_sleep_pos]
    
    midpoint_sleep <- midpoint_time(sleep_onset_time, last_sleep_time)
    
    awake_after_onset <- is_awake[onset_pos:length(is_awake)]
    waso_min <- sum(awake_after_onset, na.rm = TRUE) * epoch_sec / 60
    
    total_wake_min <- sleep_latency_min + waso_min
    
    r_awake <- rle_bouts(awake_after_onset)
    awake_bouts <- r_awake[r_awake$value == TRUE & r_awake$length >= min_awake_epochs, , drop = FALSE]
    n_awakenings <- nrow(awake_bouts)
    
    sleep_eff <- if (tib_min > 0) 100 * tst_min / tib_min else NA_real_
    
    tst_hr <- tst_min / 60
    sfi <- if (tst_hr > 0) n_awakenings / tst_hr else NA_real_
    
    out[[i]] <- data.frame(
      night_id = i,
      spt_start = spt_start,
      spt_end = spt_end,
      tib_min = tib_min,
      tst_min = tst_min,
      sleep_efficiency_pct = sleep_eff,
      sleep_onset_time = sleep_onset_time,
      sleep_latency_min = sleep_latency_min,
      waso_min = waso_min,
      total_wake_min = total_wake_min,
      last_sleep_time = last_sleep_time,
      midpoint_sleep = midpoint_sleep,
      n_awakenings = n_awakenings,
      sfi_awakenings_per_hr_sleep = sfi,
      n_nonwear_epochs_in_spt = n_nonwear_epochs,
      stringsAsFactors = FALSE
    )
  }
  
  out <- out[!vapply(out, is.null, logical(1))]
  
  if (length(out) == 0) {
    return(empty_result())
  }
  
  res <- do.call(rbind, out)
  res$night_date <- as.Date(res$spt_start - 12 * 3600, tz = tz)
  res <- res[, c("night_date", setdiff(names(res), "night_date")), drop = FALSE]
  
  res
}
