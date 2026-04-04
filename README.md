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

## Repository Structure

```text
ActiGraph-CP-Raw-Processing/
├── LICENSE
├── README.md
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

## Quick Start

### 1. Clone or download the repository

```bash
git clone https://github.com/your-username/ActiGraph-CP-Raw-Processing.git
cd ActiGraph-CP-Raw-Processing
```

Or download as a ZIP and extract to your desired location.

---

### 2. Open the project in RStudio

- Open RStudio  
- Use **File → Open Project** (recommended), or set the working directory to the repository folder  

---

### 3. Install required packages (first time only)

```r
install.packages(c("dplyr", "data.table", "randomForest", "Rcpp", "rstudioapi"))
```

---

### 4. Prepare your data

Place your raw ActiGraph `.gt3x` files in a folder of your choice, for example:

```
D:/ActiGraph/raw/
```

---

### 5. Run the pipeline

#### Option A — Specify input and output folders (recommended)

```r
source("main_pipeline.R")

run_pipeline(
  input_dir = "D:/ActiGraph/raw",
  output_dir = "D:/ActiGraph/output",
  model_location = "wrist",   # or "hip"
  run_sleep = TRUE            # set to FALSE for hip placement
)
```

---

#### Option B — Select folders interactively (Windows)

```r
source("main_pipeline.R")

run_pipeline(
  model_location = "wrist",
  run_sleep = TRUE
)
```

You will be prompted to select:
1. Input folder (containing `.gt3x` files)  
2. Output folder  

> **Note:** Interactive folder selection is supported on Windows (via RStudio).  
> For reproducible workflows, specifying paths explicitly is recommended.

---

### 6. Outputs

Processed files will be saved to your chosen output directory, including:

- **Epoch-level data**
  - `<file>_<model>_Scored.csv`
  - `<file>_<model>_Scored.RData`

- **Daily summary file**
  - `DBD_SummaryTable_<model>.csv`

---

### 7. Model selection

- `"wrist"` → includes sleep detection and sleep metrics  
- `"hip"` → activity classification only (sleep processing typically disabled)  

---

### 8. Example

```r
source("main_pipeline.R")

run_pipeline(
  input_dir = "C:/Data/ActiGraph/raw",
  output_dir = "C:/Data/ActiGraph/output",
  model_location = "hip",
  run_sleep = FALSE
)
```

---

### Troubleshooting

- Ensure `.gt3x` files are present in the input directory  
- Ensure model files exist in the `models/` folder  
- If interactive selection fails, provide paths explicitly  

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
