score_sleep_within_period <- function(mean_angleZ,
                                      sleep_period,
                                      epoch_sec = 15,
                                      postch_thr_deg = 5,
                                      min_stable_min = 5) {
  
  tilt <- as.numeric(mean_angleZ)
  sp   <- as.integer(sleep_period) == 1L
  
  sleep_score <- integer(length(tilt))
  if (!any(sp)) return(sleep_score)
  
  min_len_epochs <- as.integer(round(min_stable_min * 60 / epoch_sec))
  
  hold <- diff(c(0L, sp, 0L))
  st <- which(hold == 1L)
  en <- which(hold == -1L) - 1L
  
  for (i in seq_along(st)) {
    idx <- st[i]:en[i]
    tlt <- tilt[idx]
    
    postch <- abs(diff(tlt))
    postch[!is.finite(postch)] <- Inf
    
    stable <- as.integer(postch <= postch_thr_deg)
    
    if (length(stable) == 0L) {
      stable <- 0L
    } else {
      stable <- c(stable, tail(stable, 1))
    }
    
    r <- rle(stable)
    runlen <- rep(r$lengths, r$lengths)
    
    sleep_score[idx] <- as.integer(stable == 1L & runlen >= min_len_epochs)
  }
  
  sleep_score
}
