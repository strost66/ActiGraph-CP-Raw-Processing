zero_cross_count <- function(x) {
  x <- as.numeric(x)
  med <- stats::median(x, na.rm = TRUE)
  seq <- x - med
  
  if (length(seq) == 0) return(0)
  
  if (!is.na(seq[1]) && seq[1] == 0) {
    nz <- seq[seq != 0]
    seq[1] <- if (length(nz) > 0) nz[1] else 0
  }
  
  if (length(seq) > 1) {
    for (i in 2:length(seq)) {
      if (!is.na(seq[i]) && seq[i] == 0) seq[i] <- seq[i - 1]
    }
  }
  
  s <- sign(seq)
  sum(diff(s) != 0, na.rm = TRUE)
}

step_msg("FEATURE EXTRACTION")

features <- extract_acc_features_reshape(
  data = df1,
  windowInSec = 10,
  time_col = "timestamp",
  sampling_rate = 30
)