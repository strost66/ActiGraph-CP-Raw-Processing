# ActiGraph-CP-Raw-Processing

End-to-end R pipeline for processing raw ActiGraph accelerometer data to derive activity and (optionally) sleep outcomes.

---

## Overview

This repository provides a reproducible pipeline for processing raw `.gt3x` files from ActiGraph accelerometers. The pipeline performs:

1. Reading and timestamping raw acceleration data  
2. Feature extraction from fixed-length epochs  
3. Activity classification using random forest models (wrist or hip)  
4. Temporal smoothing of predicted activity classes  
5. Non-wear detection based on acceleration variability  
6. Optional sleep period detection using device orientation (z-angle)  
7. Optional sleep scoring and derivation of sleep metrics  
8. Aggregation of daily activity (and optional sleep) outcomes  

The pipeline is designed to support device-based measurement of movement behaviours in free-living conditions.

---

## Repository Structure

```text
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

---

## Requirements

### Software
- R (≥ 4.0 recommended)

### Required R packages

```r
dplyr
data.table
randomForest
```

Install with:

```r
install.packages(c("dplyr", "data.table", "randomForest"))
```

---

## Getting Started

### 1. Clone the repository

If you have Git installed, clone the repository:

```bash
git clone https://github.com/YOUR-USERNAME/ActiGraph-CP-Raw-Processing.git
```

Then navigate into the project folder:

```bash
cd ActiGraph-CP-Raw-Processing
```

Alternatively, download the repository as a ZIP file from GitHub and extract it.

---

### 2. Set up the project

Open the project folder in RStudio (recommended) or set your working directory:

```r
setwd("path/to/ActiGraph-CP-Raw-Processing")
```

---

### 3. Install required packages

```r
install.packages(c("dplyr", "data.table", "randomForest"))
```

---

### 4. Add your data

Place raw ActiGraph `.gt3x` files in:

```
data/raw/
```

---

### 5. Configure the pipeline

Edit:

```
config/config.R
```

Key options include:

```r
model_location <- "wrist"   # or "hip"
run_sleep <- TRUE           # TRUE = run sleep processing, FALSE = skip
epoch_sec <- 10
sampling_rate <- 30
```

---

### 6. Run the pipeline

```r
source("main_pipeline.R")
```

---

## Models

The pipeline supports multiple random forest models corresponding to different device placements:

- Wrist model: `RF_CP_Wrist.RData`  
- Hip model: `RF_CP_Hip.RData`  

Select the model via:

```r
model_location <- "wrist"
```

---

## Sleep Processing (Optional)

Sleep detection and scoring can be enabled or disabled using:

```r
run_sleep <- TRUE
```

### Recommendations

- **Wrist placement:**  
  `run_sleep <- TRUE` (recommended)

- **Hip placement:**  
  `run_sleep <- FALSE` (recommended, as hip data are not well-suited for sleep detection)

When sleep processing is disabled:
- Sleep detection steps are skipped  
- Sleep variables are set to default values  
- Daily summaries include activity metrics only  

---

## Outputs

Outputs are written to:

```
data/output/
```

### Per-file outputs
- `*_Scored.RData` — full processed dataset  
- `*_Scored.csv` — epoch-level predictions  

### Daily summary
- `DBD_SummaryTable_[wrist/hip].csv`

Includes:
- time spent in activity categories  
- wear time metrics  
- valid day indicators  
- sleep metrics (if enabled)  

---

## Notes on Data

- Raw accelerometer data are **not included** in this repository  
- Users must supply their own `.gt3x` files  
- Example data (if included) should be synthetic or de-identified  

---

## License

This project is licensed under the Apache License 2.0.
