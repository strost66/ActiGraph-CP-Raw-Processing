kurt_pandas_like <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  
  if (n < 4) return(NA_real_)
  
  m <- mean(x)
  m2 <- mean((x - m)^2)
  if (!is.finite(m2) || m2 == 0) return(0)
  
  m4 <- mean((x - m)^4)
  g2 <- (m4 / (m2^2)) - 3
  
  ((n - 1) / ((n - 2) * (n - 3))) * ((n + 1) * g2 + 6)
}