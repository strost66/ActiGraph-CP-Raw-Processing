# ActiGraph-CP-Raw-Processing

End-to-end R pipeline for processing raw ActiGraph accelerometer data to derive activity and sleep outcomes in children with CP.

---

## Overview

This repository provides a reproducible pipeline for processing raw `.gt3x` files from ActiGraph accelerometers. The pipeline performs:

1. Reading and timestamping raw acceleration data  
2. Feature extraction from fixed-length epochs  
3. Activity classification using random forest models (wrist or hip)  
4. Temporal smoothing of predicted activity classes  
5. Non-wear detection based on acceleration variability  
6. Sleep period detection using device orientation (z-angle)  
7. Sleep scoring and derivation of sleep metrics  
8. Aggregation of daily activity and sleep outcomes  

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

Alternatively, you can download the repository as a ZIP file from GitHub and extract it.

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

Set key options such as:

```r
model_location <- "wrist"   # or "hip"
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

### Note
If models are not included in the repository, users must supply compatible trained models with matching feature inputs.

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
- sleep metrics  
- wear time metrics  
- valid day indicators  

---

## Notes on Data

- Raw accelerometer data are **not included** in this repository  
- Users must supply their own `.gt3x` files  
- Example data (if included) should be synthetic or de-identified  

---

## License

To be added.
