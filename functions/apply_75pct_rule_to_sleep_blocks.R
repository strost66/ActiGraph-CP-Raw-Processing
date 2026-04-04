apply_75pct_rule_to_sleep_blocks <- function(filt_features,
                                             sleep_period_col = "sleep_period",
                                             nonwear_col = "non_wear",
                                             thr = 0.75,
                                             use_ge = TRUE,
                                             valid_col = "sleep_period_valid",
                                             coerce_col = "sleep_block_coerced") {
  
  sp <- as.integer(filt_features[[sleep_period_col]] %in% 1L)
  nw <- as.integer(filt_features[[nonwear_col]] %in% 1L)
  
  valid <- integer(length(sp))
  coerced <- integer(length(sp))
  
  r <- rle(sp)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  
  for (i in seq_along(r$values)) {
    if (r$values[i] == 1L) {
      idx <- starts[i]:ends[i]
      prop <- mean(nw[idx] == 1L, na.rm = TRUE)
      
      kill_block <- if (use_ge) (prop >= thr) else (prop > thr)
      
      if (isTRUE(kill_block)) {
        valid[idx] <- 0L
        coerced[idx] <- 1L
      } else {
        valid[idx] <- 1L
        coerced[idx] <- 0L
      }
    }
  }
  
  filt_features[[valid_col]] <- valid
  filt_features[[coerce_col]] <- coerced
  
  filt_features
}