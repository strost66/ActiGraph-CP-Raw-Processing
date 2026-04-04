skew_pandas_like <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  
  if (n < 3) return(NA_real_)
  
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) return(0)
  
  m <- mean(x)
  sum(((x - m) / s)^3) * n / ((n - 1) * (n - 2))
}