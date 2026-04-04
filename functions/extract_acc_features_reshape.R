extract_acc_features_reshape <- function(data,
                                         windowInSec,
                                         time_col = "timestamp",
                                         sampling_rate = 30) {
  
  # -----------------------------
  # Basic checks
  # -----------------------------
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  
  if (!time_col %in% names(data)) {
    stop("time_col not found in data.")
  }
  
  if (!inherits(data[[time_col]], "POSIXct")) {
    stop("timestamp column must be POSIXct.")
  }
  
  xyz_cols <- setdiff(names(data), time_col)
  if (length(xyz_cols) < 3) {
    stop("Data must contain timestamp plus x, y, z columns.")
  }
  
  data_xyz <- data[, c(time_col, xyz_cols[1:3])]
  names(data_xyz) <- c("timestamp", "x", "y", "z")
  data_xyz <- data_xyz[order(data_xyz$timestamp), ]
  data_xyz <- data_xyz[!is.na(data_xyz$timestamp), , drop = FALSE]
  
  n_total <- nrow(data_xyz)
  if (n_total < 2L) {
    stop("Not enough rows in data.")
  }
  
  # -----------------------------
  # Window size
  # -----------------------------
  window_n <- as.integer(round(windowInSec * sampling_rate))
  if (window_n < 2L) {
    stop("windowInSec * sampling_rate must be >= 2 samples.")
  }
  
  # Match your earlier Python-like behaviour:
  # select full window then drop last row
  use_n <- window_n - 1L
  if (use_n < 2L) {
    stop("Effective window size after dropping the last row is < 2.")
  }
  
  # Number of complete non-overlapping windows
  n_windows <- n_total %/% window_n
  
  if (n_windows < 1L) {
    warning("Data shorter than one full window. Returning empty data frame.")
    return(data.frame(
      timestamp = as.POSIXct(character()),
      ACC_vectorMagnitude = numeric(),
      ACC_std_vm = numeric(),
      ACC_tilt = numeric(),
      ACC_ForBack_Angle = numeric(),
      ACC_min_x = numeric(), ACC_min_y = numeric(), ACC_min_z = numeric(),
      ACC_max_x = numeric(), ACC_max_y = numeric(), ACC_max_z = numeric(),
      ACC_mean_x = numeric(), ACC_mean_y = numeric(), ACC_mean_z = numeric(),
      ACC_var_x = numeric(), ACC_var_y = numeric(), ACC_var_z = numeric(),
      ACC_std_x = numeric(), ACC_std_y = numeric(), ACC_std_z = numeric(),
      ACC_skew_x = numeric(), ACC_skew_y = numeric(), ACC_skew_z = numeric(),
      ACC_kurt_x = numeric(), ACC_kurt_y = numeric(), ACC_kurt_z = numeric(),
      ACC_median_x = numeric(), ACC_median_y = numeric(), ACC_median_z = numeric(),
      ACC_parcentile25_x = numeric(), ACC_parcentile25_y = numeric(), ACC_parcentile25_z = numeric(),
      ACC_parcentile50_x = numeric(), ACC_parcentile50_y = numeric(), ACC_parcentile50_z = numeric(),
      ACC_parcentile75_x = numeric(), ACC_parcentile75_y = numeric(), ACC_parcentile75_z = numeric(),
      ACC_corr_xy = numeric(), ACC_corr_yz = numeric(), ACC_corr_xz = numeric(),
      ACC_zerocross_x = numeric(), ACC_zerocross_y = numeric(), ACC_zerocross_z = numeric(),
      ACC_energy_x = numeric(), ACC_energy_y = numeric(), ACC_energy_z = numeric(),
      ACC_dominantMag_x = numeric(), ACC_dominantMag_y = numeric(), ACC_dominantMag_z = numeric(),
      ACC_dominantFr_x = numeric(), ACC_dominantFr_y = numeric(), ACC_dominantFr_z = numeric(),
      ACC_entropy_x = numeric(), ACC_entropy_y = numeric(), ACC_entropy_z = numeric()
    ))
  }
  
  n_use_total <- n_windows * window_n
  
  # Keep only complete windows
  ts_used  <- data_xyz$timestamp[1:n_use_total]
  xyz_used <- as.matrix(data_xyz[1:n_use_total, c("x", "y", "z")])
  storage.mode(xyz_used) <- "double"
  
  # Window timestamps = first sample of each full window
  starts <- seq.int(1L, n_use_total, by = window_n)
  out_timestamp <- ts_used[starts]
  
  # -----------------------------
  # Reshape each axis:
  # columns = windows
  # rows    = samples within window
  # Then drop the last row in each column to match previous behaviour
  # -----------------------------
  x_full <- matrix(xyz_used[, 1], nrow = window_n, ncol = n_windows)
  y_full <- matrix(xyz_used[, 2], nrow = window_n, ncol = n_windows)
  z_full <- matrix(xyz_used[, 3], nrow = window_n, ncol = n_windows)
  
  x_mat <- x_full[1:use_n, , drop = FALSE]
  y_mat <- y_full[1:use_n, , drop = FALSE]
  z_mat <- z_full[1:use_n, , drop = FALSE]
  
  # -----------------------------
  # Fast helpers
  # -----------------------------
  col_var_fast <- function(mat) {
    n <- colSums(!is.na(mat))
    mu <- colMeans(mat, na.rm = TRUE)
    ss <- colSums((sweep(mat, 2, mu, FUN = "-"))^2, na.rm = TRUE)
    out <- ss / pmax(n - 1, 1)
    out[n < 2] <- NA_real_
    out
  }
  
  col_sd_fast <- function(mat) {
    sqrt(col_var_fast(mat))
  }
  
  col_quantile_fast <- function(mat, prob) {
    apply(mat, 2, stats::quantile, probs = prob, na.rm = TRUE, names = FALSE)
  }
  
  safe_cor <- function(a, b) {
    if (all(is.na(a)) || all(is.na(b))) return(NA_real_)
    sda <- stats::sd(a, na.rm = TRUE)
    sdb <- stats::sd(b, na.rm = TRUE)
    if (!is.finite(sda) || !is.finite(sdb) || sda == 0 || sdb == 0) return(NA_real_)
    suppressWarnings(stats::cor(a, b, use = "everything"))
  }
  
  zero_cross_count_fast <- function(v) {
    v <- as.numeric(v)
    if (length(v) < 2L) return(0)
    
    s <- sign(v)
    s[s == 0] <- NA
    
    if (all(is.na(s))) return(0)
    
    if (length(s) >= 2L) {
      for (ii in 2:length(s)) {
        if (is.na(s[ii])) s[ii] <- s[ii - 1]
      }
      if (length(s) > 1L) {
        for (ii in (length(s) - 1):1) {
          if (is.na(s[ii])) s[ii] <- s[ii + 1]
          if (ii == 1L) break
        }
      }
    }
    
    sum(diff(s) != 0, na.rm = TRUE)
  }
  
  fft_features <- function(v, sampling_rate) {
    y <- as.numeric(v)
    
    if (length(y) < 2L || all(is.na(y))) {
      return(list(
        energy = NA_real_,
        dominantFr = NA_real_,
        dominantMag = NA_real_
      ))
    }
    
    mu <- mean(y, na.rm = TRUE)
    y <- y - mu
    y[!is.finite(y)] <- 0
    
    n <- length(y)
    F <- stats::fft(y)
    P <- Mod(F)^2
    
    energy <- sum(P) / n
    
    keep <- seq_len(floor(n / 2) + 1L)
    P1 <- P[keep]
    freqs <- (keep - 1L) * sampling_rate / n
    
    i_max <- which.max(P1)
    
    list(
      energy = energy,
      dominantFr = freqs[i_max],
      dominantMag = P1[i_max]
    )
  }
  
  # -----------------------------
  # Bulk time-domain features
  # -----------------------------
  ACC_min_x <- apply(x_mat, 2, min, na.rm = TRUE)
  ACC_min_y <- apply(y_mat, 2, min, na.rm = TRUE)
  ACC_min_z <- apply(z_mat, 2, min, na.rm = TRUE)
  
  ACC_max_x <- apply(x_mat, 2, max, na.rm = TRUE)
  ACC_max_y <- apply(y_mat, 2, max, na.rm = TRUE)
  ACC_max_z <- apply(z_mat, 2, max, na.rm = TRUE)
  
  ACC_mean_x <- colMeans(x_mat, na.rm = TRUE)
  ACC_mean_y <- colMeans(y_mat, na.rm = TRUE)
  ACC_mean_z <- colMeans(z_mat, na.rm = TRUE)
  
  ACC_var_x <- col_var_fast(x_mat)
  ACC_var_y <- col_var_fast(y_mat)
  ACC_var_z <- col_var_fast(z_mat)
  
  ACC_std_x <- sqrt(ACC_var_x)
  ACC_std_y <- sqrt(ACC_var_y)
  ACC_std_z <- sqrt(ACC_var_z)
  
  ACC_median_x <- apply(x_mat, 2, stats::median, na.rm = TRUE)
  ACC_median_y <- apply(y_mat, 2, stats::median, na.rm = TRUE)
  ACC_median_z <- apply(z_mat, 2, stats::median, na.rm = TRUE)
  
  ACC_parcentile25_x <- col_quantile_fast(x_mat, 0.25)
  ACC_parcentile25_y <- col_quantile_fast(y_mat, 0.25)
  ACC_parcentile25_z <- col_quantile_fast(z_mat, 0.25)
  
  ACC_parcentile50_x <- col_quantile_fast(x_mat, 0.50)
  ACC_parcentile50_y <- col_quantile_fast(y_mat, 0.50)
  ACC_parcentile50_z <- col_quantile_fast(z_mat, 0.50)
  
  ACC_parcentile75_x <- col_quantile_fast(x_mat, 0.75)
  ACC_parcentile75_y <- col_quantile_fast(y_mat, 0.75)
  ACC_parcentile75_z <- col_quantile_fast(z_mat, 0.75)
  
  # Mean and SD of vector magnitude from per-sample magnitude
  mag_mat <- sqrt(x_mat^2 + y_mat^2 + z_mat^2)
  ACC_vectorMagnitude <- colMeans(mag_mat, na.rm = TRUE)
  ACC_std_vm <- col_sd_fast(mag_mat)
  
  ACC_tilt <- acos(ACC_mean_y / (ACC_vectorMagnitude + 1e-5)) * 180 / pi
  ACC_ForBack_Angle <- -asin(ACC_mean_z / (ACC_vectorMagnitude + 1e-5)) * 180 / pi
  
  # -----------------------------
  # Preallocate loop-based features
  # -----------------------------
  ACC_skew_x <- numeric(n_windows)
  ACC_skew_y <- numeric(n_windows)
  ACC_skew_z <- numeric(n_windows)
  
  ACC_kurt_x <- numeric(n_windows)
  ACC_kurt_y <- numeric(n_windows)
  ACC_kurt_z <- numeric(n_windows)
  
  ACC_corr_xy <- numeric(n_windows)
  ACC_corr_yz <- numeric(n_windows)
  ACC_corr_xz <- numeric(n_windows)
  
  ACC_zerocross_x <- numeric(n_windows)
  ACC_zerocross_y <- numeric(n_windows)
  ACC_zerocross_z <- numeric(n_windows)
  
  ACC_energy_x <- numeric(n_windows)
  ACC_energy_y <- numeric(n_windows)
  ACC_energy_z <- numeric(n_windows)
  
  ACC_dominantMag_x <- numeric(n_windows)
  ACC_dominantMag_y <- numeric(n_windows)
  ACC_dominantMag_z <- numeric(n_windows)
  
  ACC_dominantFr_x <- numeric(n_windows)
  ACC_dominantFr_y <- numeric(n_windows)
  ACC_dominantFr_z <- numeric(n_windows)
  
  ACC_entropy_x <- numeric(n_windows)
  ACC_entropy_y <- numeric(n_windows)
  ACC_entropy_z <- numeric(n_windows)
  
  # -----------------------------
  # Loop only for harder features
  # -----------------------------
  for (i in seq_len(n_windows)) {
    x <- x_mat[, i]
    y <- y_mat[, i]
    z <- z_mat[, i]
    
    ACC_skew_x[i] <- skew_pandas_like(x)
    ACC_skew_y[i] <- skew_pandas_like(y)
    ACC_skew_z[i] <- skew_pandas_like(z)
    
    ACC_kurt_x[i] <- kurt_pandas_like(x)
    ACC_kurt_y[i] <- kurt_pandas_like(y)
    ACC_kurt_z[i] <- kurt_pandas_like(z)
    
    ACC_corr_xy[i] <- safe_cor(x, y)
    ACC_corr_yz[i] <- safe_cor(y, z)
    ACC_corr_xz[i] <- safe_cor(x, z)
    
    ACC_zerocross_x[i] <- zero_cross_count_fast(x)
    ACC_zerocross_y[i] <- zero_cross_count_fast(y)
    ACC_zerocross_z[i] <- zero_cross_count_fast(z)
    
    fx <- fft_features(x, sampling_rate)
    fy <- fft_features(y, sampling_rate)
    fz <- fft_features(z, sampling_rate)
    
    ACC_energy_x[i] <- fx$energy
    ACC_energy_y[i] <- fy$energy
    ACC_energy_z[i] <- fz$energy
    
    ACC_dominantMag_x[i] <- fx$dominantMag
    ACC_dominantMag_y[i] <- fy$dominantMag
    ACC_dominantMag_z[i] <- fz$dominantMag
    
    ACC_dominantFr_x[i] <- fx$dominantFr
    ACC_dominantFr_y[i] <- fy$dominantFr
    ACC_dominantFr_z[i] <- fz$dominantFr
    
    ACC_entropy_x[i] <- cal_entropy(x)
    ACC_entropy_y[i] <- cal_entropy(y)
    ACC_entropy_z[i] <- cal_entropy(z)
  }
  
  # -----------------------------
  # Assemble output
  # -----------------------------
  out <- data.frame(
    timestamp = out_timestamp,
    
    ACC_vectorMagnitude = ACC_vectorMagnitude,
    ACC_std_vm = ACC_std_vm,
    ACC_tilt = ACC_tilt,
    ACC_ForBack_Angle = ACC_ForBack_Angle,
    
    ACC_min_x = ACC_min_x, ACC_min_y = ACC_min_y, ACC_min_z = ACC_min_z,
    ACC_max_x = ACC_max_x, ACC_max_y = ACC_max_y, ACC_max_z = ACC_max_z,
    ACC_mean_x = ACC_mean_x, ACC_mean_y = ACC_mean_y, ACC_mean_z = ACC_mean_z,
    ACC_var_x = ACC_var_x, ACC_var_y = ACC_var_y, ACC_var_z = ACC_var_z,
    ACC_std_x = ACC_std_x, ACC_std_y = ACC_std_y, ACC_std_z = ACC_std_z,
    ACC_skew_x = ACC_skew_x, ACC_skew_y = ACC_skew_y, ACC_skew_z = ACC_skew_z,
    ACC_kurt_x = ACC_kurt_x, ACC_kurt_y = ACC_kurt_y, ACC_kurt_z = ACC_kurt_z,
    ACC_median_x = ACC_median_x, ACC_median_y = ACC_median_y, ACC_median_z = ACC_median_z,
    ACC_parcentile25_x = ACC_parcentile25_x, ACC_parcentile25_y = ACC_parcentile25_y, ACC_parcentile25_z = ACC_parcentile25_z,
    ACC_parcentile50_x = ACC_parcentile50_x, ACC_parcentile50_y = ACC_parcentile50_y, ACC_parcentile50_z = ACC_parcentile50_z,
    ACC_parcentile75_x = ACC_parcentile75_x, ACC_parcentile75_y = ACC_parcentile75_y, ACC_parcentile75_z = ACC_parcentile75_z,
    
    ACC_corr_xy = ACC_corr_xy, ACC_corr_yz = ACC_corr_yz, ACC_corr_xz = ACC_corr_xz,
    ACC_zerocross_x = ACC_zerocross_x, ACC_zerocross_y = ACC_zerocross_y, ACC_zerocross_z = ACC_zerocross_z,
    ACC_energy_x = ACC_energy_x, ACC_energy_y = ACC_energy_y, ACC_energy_z = ACC_energy_z,
    ACC_dominantMag_x = ACC_dominantMag_x, ACC_dominantMag_y = ACC_dominantMag_y, ACC_dominantMag_z = ACC_dominantMag_z,
    ACC_dominantFr_x = ACC_dominantFr_x, ACC_dominantFr_y = ACC_dominantFr_y, ACC_dominantFr_z = ACC_dominantFr_z,
    ACC_entropy_x = ACC_entropy_x, ACC_entropy_y = ACC_entropy_y, ACC_entropy_z = ACC_entropy_z,
    
    stringsAsFactors = FALSE
  )
  
  # -----------------------------
  # Replace NA / NaN with 0
  # -----------------------------
  for (j in seq_along(out)) {
    if (is.numeric(out[[j]])) {
      bad <- is.na(out[[j]]) | is.nan(out[[j]])
      out[[j]][bad] <- 0
    }
  }
  
  out
}
