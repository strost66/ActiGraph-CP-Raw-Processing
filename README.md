# ActiGraph CP Raw Processing Pipeline

A reproducible R-based pipeline for processing raw ActiGraph `.gt3x` accelerometer data in children with cerebral palsy (CP).  

The pipeline supports:

- Raw data ingestion and timestamp alignment  
- Feature extraction from triaxial acceleration signals  
- Activity classification using random forest models (wrist or hip)  
- Modal smoothing of predicted activity classes  
- Non-wear detection  
- Optional sleep detection and sleep metrics (wrist only)  
- Daily aggregation of activity and sleep outcomes  

---

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

## Quick Start

### 1. Clone or download the repository

```bash
git clone https://github.com/your-username/ActiGraph-CP-Raw-Processing.git
cd ActiGraph-CP-Raw-Processing
```

Or download as a ZIP and extract.

---

### 2. Open in RStudio

- Open RStudio  
- Use **File → Open Project** (recommended), or set working directory to the repository folder  

---

### 3. Install required packages (first time only)

```r
install.packages(c("dplyr", "data.table", "randomForest", "Rcpp", "rstudioapi"))
```

---

### 4. Prepare your data

Place `.gt3x` files in a folder of your choice, for example:

```
D:/ActiGraph_Files/raw/
```

---

### 5. Run the pipeline

#### Option A — Specify folders (recommended)

```r
source("main_pipeline.R")

run_pipeline(
  input_dir = "D:/ActiGraph/raw",
  output_dir = "D:/ActiGraph/output",
  model_location = "wrist",   # "wrist" or "hip"
  run_sleep = TRUE
)
```

---

#### Option B — Interactive folder selection (Windows)

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

> **Note:** Interactive selection is supported on Windows via RStudio.  
> For reproducible workflows, explicitly specifying paths is recommended.

---

## Model Options

| Model | Description |
|------|------------|
| `"wrist"` | Activity classification + sleep detection and sleep metrics |
| `"hip"`   | Activity classification only (sleep typically disabled) |

---

## Outputs

All outputs are written to the specified output directory.

### Epoch-level outputs

- `<file>_<model>_Scored.csv`
- `<file>_<model>_Scored.RData`

Contain:

- Predicted activity class  
- Smoothed activity class  
- Non-wear classification  
- Sleep-related variables (if enabled)  
- Final combined activity classification  

---

### Daily summary output

- `DBD_SummaryTable_<model>.csv`

Contains:

- Daily activity summaries  
- Wear time metrics  
- Sleep metrics (if enabled)  

---

### Missing values

- Sleep-related variables are **always included** in the daily summary  
- When sleep processing is disabled (e.g., hip placement), these fields are populated with **blank values**  
- Missing values are written as empty cells (not `"NA"`) for compatibility with Excel, SAS, and other tools  

---

## Models

The pipeline expects pre-trained random forest models in the `models/` directory:

```text
models/
├── RF_CP_Wrist.RData
├── RF_CP_Hip.RData
```

Expected object names:

- `RF_CP_Wrist.RData` → object named `wrist`  
- `RF_CP_Hip.RData` → object named `hip`  

---

## Requirements

- R ≥ 4.0  
- Windows recommended for interactive folder selection  
- Java may be required depending on ActiGraph reading functions  

---

## Notes

- Raw accelerometer data are **not included** in this repository  
- Ensure `.gt3x` files are present in the input directory before running  
- The pipeline uses relative paths and is designed to be portable across systems  

---

## License

This project is licensed under the Apache License, Version 2.0.

You may obtain a copy of the License at:

http://www.apache.org/licenses/LICENSE-2.0

See the `LICENSE` file in this repository for full details.

---

## Citation

Citation for the models:

> Ahmadi MN, O'Neil ME, Baque E, Boyd RN, Trost SG. Machine Learning to Quantify Physical Activity in Children with Cerebral Palsy: Comparison of Group, Group-Personalized, and Fully-Personalized Activity Classification Models. Sensors (Basel). 2020 Jul 17;20(14):3976. doi: 10.3390/s20143976
