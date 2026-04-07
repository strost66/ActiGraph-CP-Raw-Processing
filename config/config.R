# ============================================================
# config/config.R
# ============================================================

default_config <- list(
  tz = "Australia/Brisbane",
  input_dir = NULL,
  output_dir = NULL,
  models_dir = "models",
  model_location = "wrist",
  run_sleep = TRUE,
  sampling_rate = 30,
  epoch_sec = 10,
  modal_filter_span = 3,
  modal_majority_threshold = 0.5,
  nonwear_sd_threshold_g = 13 / 1000,
  nonwear_min_minutes = 30,
  wear_threshold_min = 600,
  waking_wear_threshold_min = 600
)
