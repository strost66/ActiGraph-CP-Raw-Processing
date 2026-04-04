detect_nonwear_from_vm_sd_15s <- function(sd_vm,
                                          sdThreshold = 13/1000,   # 0.013 g = 13 mg
                                          epoch_sec = 15,
                                          min_nonwear_min = 30,
                                          swallow_wear_min = 30,
                                          swallow_prop = 0.3) {
  
  sd_vm <- as.numeric(sd_vm)
  
  # quiet windows
  quiet <- ifelse(is.finite(sd_vm) & sd_vm < sdThreshold, 1L, 0L)
  
  # require >= 30 minutes quiet
  epochs_in_30min <- as.integer((60 / epoch_sec) * min_nonwear_min)  # 120 for 15s
  r <- rle(quiet)
  runlen <- rep(r$lengths, r$lengths)
  
  nonwear <- integer(length(quiet))
  idx <- which(quiet == 1L & runlen >= epochs_in_30min)
  if (length(idx)) nonwear[idx] <- 1L
  
  # swallow short wear bouts between nonwear (actimetric rule)
  check <- nonwear
  rr <- rle(check)
  
  if (any(rr$values == 0L)) {
    vals <- rr$values
    lens <- rr$lengths
    
    for (i in seq_along(vals)) {
      if (vals[i] == 0L) {
        before  <- if (i > 1) lens[i - 1] else 0L
        after   <- if (i < length(lens)) lens[i + 1] else 0L
        current <- lens[i]
        
        nwBordering <- if ((before + after) > 0) current / (before + after) else 1
        
        if (current < epochs_in_30min && nwBordering < swallow_prop) {
          vals[i] <- 1L
        }
      }
    }
    
    check2 <- rep(vals, lens)
    nonwear <- pmin(1L, nonwear + check2)
  }
  
  nonwear
}