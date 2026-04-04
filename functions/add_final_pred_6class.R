add_final_pred_6class <- function(filt_features,
                                  pred_class_col = "pred_class",
                                  nonwear_col = "non_wear",
                                  sleep_period_col = "sleep_period",
                                  sleep_score_col = "sleep_score",
                                  sleep_block_coerced_col = "sleep_block_coerced",
                                  out_col = "final_pred") {
  
  pc <- as.integer(filt_features[[pred_class_col]])
  nw <- as.integer(filt_features[[nonwear_col]] %in% 1L)
  sp <- as.integer(filt_features[[sleep_period_col]] %in% 1L)
  ss <- as.integer(filt_features[[sleep_score_col]] %in% 1L)
  sc <- as.integer(filt_features[[sleep_block_coerced_col]] %in% 1L)
  
  act <- dplyr::case_when(
    pc == 1L ~ "Sed",
    pc == 2L ~ "Standmov",
    pc == 3L ~ "Walk",
    TRUE ~ NA_character_
  )
  
  filt_features[[out_col]] <- dplyr::case_when(
    sp == 1L & sc == 1L ~ "NonWear",
    sp == 1L & sc == 0L & ss == 1L ~ "NightSleep",
    sp == 1L & sc == 0L & ss == 0L ~ "NightAwake",
    sp == 0L & nw == 1L ~ "NonWear",
    sp == 0L & nw == 0L ~ act,
    TRUE ~ NA_character_
  )
  
  filt_features
}