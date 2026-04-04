# ============================================================
# config/config.R
# ============================================================

tz <- "Australia/Brisbane"

# Relative paths
input_dir  <- "data/raw"
output_dir <- "data/output"
models_dir <- "models"

# Select model location: "wrist" or "hip"
model_location <- "wrist"

# Run sleep processing?
# Recommended: TRUE for wrist, FALSE for hip
run_sleep <- TRUE

# Optional: only needed if automatic model detection fails
# model_object_name <- "wrist"

# Processing parameters
sampling_rate <- 30
epoch_sec <- 10

# Modal smoothing
modal_filter_span <- 3
modal_majority_threshold <- 0.5

# Non-wear detection
nonwear_sd_threshold_g <- 13 / 1000
nonwear_min_minutes <- 30

# Daily valid-day thresholds
wear_threshold_min <- 600
waking_wear_threshold_min <- 600