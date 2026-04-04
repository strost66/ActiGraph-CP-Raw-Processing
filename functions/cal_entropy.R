cal_entropy <- function(labels, base = exp(1)) {
  labels <- labels[!is.na(labels)]
  n_labels <- length(labels)
  
  if (n_labels <= 1) return(0)
  
  tab <- table(labels)
  probs <- as.numeric(tab) / n_labels
  n_classes <- sum(probs > 0)
  
  if (n_classes <= 1) return(0)
  
  -sum(probs * (log(probs) / log(base)))
}