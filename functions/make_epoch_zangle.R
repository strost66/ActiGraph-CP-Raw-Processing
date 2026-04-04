make_epoch_zangle <- function(df_raw,
                              time_col = "timestamp",
                              x_col = "x", y_col = "y", z_col = "z",
                              epoch_sec = 15,
                              tz = "UTC") {
  
  # ensure POSIXct
  t <- df_raw[[time_col]]
  if (!inherits(t, "POSIXct")) t <- as.POSIXct(t, tz = tz)
  t <- as.POSIXct(t, tz = tz)
  
  x <- as.numeric(df_raw[[x_col]])
  y <- as.numeric(df_raw[[y_col]])
  z <- as.numeric(df_raw[[z_col]])
  
  # z-angle in degrees
  denom <- sqrt(x^2 + y^2)
  angleZ <- atan2(z, denom) * 180 / pi
  
  # epoch id anchored at first timestamp
  t0 <- t[1]
  epoch_id <- floor(as.numeric(difftime(t, t0, units = "secs")) / epoch_sec)
  
  # NA-safe mean angle per epoch via sum/count
  ok <- is.finite(angleZ)
  angleZ0 <- angleZ; angleZ0[!ok] <- 0
  
  sumA <- rowsum(angleZ0, epoch_id, reorder = FALSE)
  nA   <- rowsum(as.integer(ok), epoch_id, reorder = FALSE)
  
  meanA <- as.numeric(sumA) / as.numeric(nA)
  meanA[as.numeric(nA) == 0] <- NA_real_
  
  # epoch ids from rowsum rownames
  ids <- as.integer(rownames(sumA))
  
  out <- data.frame(
    epoch_id = ids,
    timestamp = t0 + ids * epoch_sec,
    mean_angleZ = meanA
  )
  
  out$dAngleZ <- c(NA_real_, abs(diff(out$mean_angleZ)))
  out
}