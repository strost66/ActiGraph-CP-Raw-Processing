# ActiGraph-CP-Raw-Processing

End-to-end R pipeline for processing raw ActiGraph accelerometer data to derive activity and sleep outcomes in children with cerebral palsy (CP).

---

## Overview

This repository provides a reproducible pipeline for processing raw .gt3x files from ActiGraph accelerometers. The pipeline performs:

Reading and timestamping raw acceleration data
Feature extraction from fixed-length epochs
Activity classification using random forest models (wrist or hip)
Temporal smoothing of predicted activity classes
Non-wear detection based on acceleration variability
Sleep period detection using device orientation (z-angle)
Sleep scoring and derivation of sleep metrics
Aggregation of daily activity and sleep outcomes

---

## Repository Structure
```
ActiGraph-CP-Raw-Processing/
├── main_pipeline.R
├── config/
│   └── config.R
├── functions/
├── models/
├── data/
│   ├── raw/
│   └── output/
```
## Data availability

Raw accelerometer data are not included in this repository due to ethical and governance restrictions. Researchers wishing to apply the pipeline to their own data may do so by supplying raw `.gt3x` files.

---

## Requirements
Software
R (≥ 4.0 recommended)
Required R packages
dplyr
data.table
randomForest

## Install with:

install.packages(c("dplyr", "data.table", "randomForest"))
Input Data
Raw ActiGraph .gt3x files
Place files in:
data/raw/
Configuration

## All user-defined settings are specified in:

config/config.R

## Key options include:

model_location <- "wrist"   # or "hip"
epoch_sec <- 10
sampling_rate <- 30
Models

## The pipeline supports multiple random forest models corresponding to different device placements:

Wrist model: RF_CP_Wrist.RData
Hip model: RF_CP_Hip.RData

Select the model via:

model_location <- "wrist"

## Note

If models are not included in the repository, users must supply compatible trained models with matching feature inputs.

## How to Run

From the project root directory:

source("main_pipeline.R")
Outputs

## Outputs are written to:

data/output/
Per-file outputs
*_Scored.RData — full processed dataset
*_Scored.csv — epoch-level predictions
Daily summary
DBD_SummaryTable_[wrist/hip].csv

## Includes:
time spent in activity categories
sleep metrics
wear time metrics
valid day indicators

## License

To be added.
