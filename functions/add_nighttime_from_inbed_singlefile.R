add_nighttime_from_inbed_singlefile <- function(df_raw,
                                                filt_features,
                                                sf = 30,
                                                ws_sec = 5,
                                                tz = "UTC",
                                                k = 60, perc = 0.1, inbedthreshold = 15,
                                                bedblocksize = 30, outofbedsize = 60, ws3 = 5,
                                                raw_time_col = "timestamp",
                                                raw_x_col = "x", raw_y_col = "y", raw_z_col = "z",
                                                feat_time_col = "timestamp",
                                                verbose = TRUE) {
  
  # ------------------------------------------------------------
  # slide()
  # ------------------------------------------------------------
  slide <- function(x, width, by = NULL, FUN = NULL, ...) {
    isMatrix <- FALSE
    FUN <- match.fun(FUN)
    if (is.null(by)) by <- width
    if (is.matrix(x) | is.data.frame(x)) {
      z <- x[, 3]; y <- x[, 2]; x <- x[, 1]
      isMatrix <- TRUE
    }
    lenX <- length(x)
    if (lenX < width) return(matrix(numeric(0), nrow = 0))
    QUT1 <- seq(1, lenX - width + 1, by = by)
    QUT2 <- lapply(QUT1, function(i) i:(i + width - 1))
    if (!isMatrix) QUT3 <- lapply(QUT2, function(a) FUN(x[a], ...))
    if (isMatrix)  QUT3 <- lapply(QUT2, function(a) FUN(x[a], y[a], z[a], ...))
    do.call(rbind, QUT3)
  }
  
  # ------------------------------------------------------------
  # inbed()
  # ------------------------------------------------------------
  inbed <- function(angle, k = 60, perc = 0.1, inbedthreshold = 15,
                    bedblocksize = 30, outofbedsize = 60, ws3 = 5) {
    
    medabsdi <- function(angle) stats::median(abs(diff(angle)))
    
    x <- slide(angle, width = k, FUN = medabsdi, by = 1)
    x <- as.numeric(x)
    
    nomov <- rep(0, length(x))
    inbedtime <- rep(NA, length(x))
    
    pp <- stats::quantile(x, probs = c(perc)) * inbedthreshold
    if (pp == 0) pp <- .2
    
    nomov[which(x < pp)] <- 1
    nomov <- c(0, nomov, 0)
    
    s1 <- which(diff(nomov) == 1)
    e1 <- which(diff(nomov) == -1)
    
    bedblock <- which((e1 - s1) > ((60/ws3) * bedblocksize * 1))
    
    if (length(bedblock) > 0) {
      s2 <- s1[bedblock]
      e2 <- e1[bedblock]
      
      for (j in seq_along(s2)) inbedtime[s2[j]:e2[j]] <- 1
      
      outofbed <- rep(0, length(inbedtime))
      outofbed[which(is.na(inbedtime) == TRUE)] <- 1
      outofbed <- c(0, outofbed, 0)
      
      s3 <- which(diff(outofbed) == 1)
      e3 <- which(diff(outofbed) == -1)
      
      outofbedblock <- which((e3 - s3) < ((60/ws3) * outofbedsize * 1))
      
      if (length(outofbedblock) > 0) {
        s4 <- s3[outofbedblock]
        e4 <- e3[outofbedblock]
        if (length(s4) > 0) {
          for (j in seq_along(s4)) inbedtime[s4[j]:e4[j]] <- 1
        }
      }
      
      if (length(inbedtime) == (length(x) + 1))
        inbedtime <- inbedtime[1:(length(inbedtime) - 1)]
      
      inbedtime2 <- rep(1, length(inbedtime))
      inbedtime2[which(is.na(inbedtime) == TRUE)] <- 0
      
      s5 <- which(diff(c(0, inbedtime2, 0)) == 1)
      e5 <- which(diff(c(0, inbedtime2, 0)) == -1)
      
      inbeddurations <- e5 - s5
      longestinbed <- which(inbeddurations == max(inbeddurations))
      
      lightsout <- s5[longestinbed] - 1
      lightson  <- e5[longestinbed] - 1
      
      if (length(s5) > 1) {
        naplightsout <- s5[-longestinbed] - 1
        naplightson  <- e5[-longestinbed] - 1
      } else {
        naplightsout <- NA
        naplightson <- NA
      }
    } else {
      lightson <- c()
      lightsout <- c()
      tib.threshold <- c()
      naplightsout <- NA
      naplightson <- NA
    }
    
    tib.threshold <- pp
    invisible(list(lightsout = lightsout, lightson = lightson,
                   tib.threshold = tib.threshold,
                   naplightsout = naplightsout, naplightson = naplightson))
  }
  
  # ------------------------------------------------------------
  # Coerce timestamps
  # ------------------------------------------------------------
  df_raw[[raw_time_col]] <- as.POSIXct(df_raw[[raw_time_col]], tz = tz)
  filt_features[[feat_time_col]] <- as.POSIXct(filt_features[[feat_time_col]], tz = tz)
  
  # ------------------------------------------------------------
  # Compute 5-sec mean z-angle series
  # ------------------------------------------------------------
  make_epoch_anglez <- function(df) {
    t <- df[[raw_time_col]]
    x <- as.numeric(df[[raw_x_col]])
    y <- as.numeric(df[[raw_y_col]])
    z <- as.numeric(df[[raw_z_col]])
    
    angleZ <- atan2(z, sqrt(x^2 + y^2)) * 180 / pi
    
    epoch_n <- as.integer(round(ws_sec * sf))
    n <- length(angleZ)
    n_epoch <- n %/% epoch_n
    
    if (n_epoch < 2) {
      return(data.frame(
        timestamp = as.POSIXct(character(), tz = tz),
        anglez = numeric()
      ))
    }
    
    keep_n <- n_epoch * epoch_n
    angleZ <- angleZ[seq_len(keep_n)]
    t <- t[seq_len(keep_n)]
    
    epoch_id <- (seq_len(keep_n) - 1) %/% epoch_n
    
    ok <- is.finite(angleZ)
    angle0 <- angleZ
    angle0[!ok] <- 0
    
    sumA <- rowsum(angle0, epoch_id, reorder = FALSE)
    nA   <- rowsum(as.integer(ok), epoch_id, reorder = FALSE)
    
    meanA <- as.numeric(sumA) / as.numeric(nA)
    meanA[as.numeric(nA) == 0] <- NA_real_
    
    epoch_ts <- t[seq(1, keep_n, by = epoch_n)]
    
    data.frame(timestamp = epoch_ts, anglez = meanA)
  }
  
  # ------------------------------------------------------------
  # Detect windows noon->noon
  # ------------------------------------------------------------
  detect_windows_one_file <- function(angle_df) {
    angle_df <- angle_df[order(angle_df$timestamp), ]
    if (nrow(angle_df) == 0) return(data.frame())
    
    dates <- sort(unique(as.Date(angle_df$timestamp, tz = tz)))
    if (length(dates) < 2) return(data.frame())
    
    windows <- list()
    wi <- 0L
    
    for (i in seq_len(length(dates) - 1)) {
      d0 <- dates[i]
      d1 <- dates[i + 1]
      
      start_noon <- as.POSIXct(paste(d0, "12:00:00"), tz = tz)
      end_noon   <- as.POSIXct(paste(d1, "12:00:00"), tz = tz)
      
      ga <- angle_df[angle_df$timestamp >= start_noon & angle_df$timestamp < end_noon, , drop = FALSE]
      ga <- ga[!is.na(ga$anglez), , drop = FALSE]
      if (nrow(ga) < (k + 1)) next
      
      sleepw <- tryCatch(
        inbed(angle = ga$anglez, k = k, perc = perc, inbedthreshold = inbedthreshold,
              bedblocksize = bedblocksize, outofbedsize = outofbedsize, ws3 = ws3),
        error = function(e) NULL
      )
      
      if (is.null(sleepw)) next
      if (length(sleepw$lightsout) == 0 || length(sleepw$lightson) == 0) next
      
      if (sleepw$lightsout[1] == 0) sleepw$lightsout[1] <- 1
      
      lo_idx <- sleepw$lightsout[1]
      ls_idx <- sleepw$lightson[1]
      
      if (is.na(lo_idx) || is.na(ls_idx)) next
      if (lo_idx < 1 || ls_idx < 1 || lo_idx > nrow(ga) || ls_idx > nrow(ga)) next
      
      wi <- wi + 1L
      windows[[wi]] <- data.frame(
        date_start = d0,
        date_end = d1,
        lightsout = ga$timestamp[lo_idx],
        lightson = ga$timestamp[ls_idx],
        tib_threshold = sleepw$tib.threshold
      )
    }
    
    if (length(windows) == 0) return(data.frame())
    do.call(rbind, windows)
  }
  
  # ------------------------------------------------------------
  # MAIN
  # ------------------------------------------------------------
  if (verbose) message("Processing single file")
  
  filt_features$nighttime <- 0L
  
  angle5 <- make_epoch_anglez(df_raw)
  
  if (nrow(angle5) == 0) {
    sleep_windows <- data.frame()
    return(list(filt_features = filt_features, sleep_windows = sleep_windows))
  }
  
  sleep_windows <- detect_windows_one_file(angle5)
  
  if (nrow(sleep_windows) > 0) {
    tt <- filt_features[[feat_time_col]]
    night_flag <- rep(0L, length(tt))
    
    for (j in seq_len(nrow(sleep_windows))) {
      night_flag <- night_flag | as.integer(
        tt >= sleep_windows$lightsout[j] & tt <= sleep_windows$lightson[j]
      )
    }
    
    filt_features$nighttime <- as.integer(night_flag)
  }
  
  list(filt_features = filt_features, sleep_windows = sleep_windows)
}