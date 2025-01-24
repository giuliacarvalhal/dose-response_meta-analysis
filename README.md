# dose_meta-analysis

**Overview**

This repository contains data and R scripts used in our systematic review and meta-analysis on the efficacy and safety of ensifentrine in patients with COPD. Below is a quick reference for how to reproduce each analysis using the scripts in this repository.

**Repository Structure:**

_Datasets:_

* IMMA_.xlsx  --> data for main meta-analysis
* ari.xlsx --> data for dose-response meta-analysis
* ari_one_stage.xlsx --> data for one-stage model of dose-response meta-analysis

_R Scripts:_

* meta_analysis.R --> perform standard meta-analyses
* dose_responsed_meta_analysis.R --> perform dose-response meta-analysis via dosresmeta
* leave_one_out_dose_response.R --> runs leave-one-out sensitivity analyses for the dose-response model.
* aggregated_curves.R --> perform dose-response meta-analysis for one-stage models (one dose) and aggregate groups to compare them

**How to Run the Analyses:**
1. Set up R Environment
Recommended R Version: 4.2.2 (or later)
Install necessary R packages (if not already installed):
```r
install.packages(c(
  "meta", "metafor", "dosresmeta", "tidyverse", "grid", 
  "mvmeta", "gridExtra", "rms", "mvtnorm", "DoseFinding",
  "aod", "readxl", "writexl", "lmtest", "knitr", "directlabels"
))
```

Contact:
For questions about data extraction or R scripts, contact: Giulia Carvalhal (carvalhalgiulia@gmail.com)

Thank you for your interest in our Dose-Response Meta-Analysis Project!
We welcome any feedback or questions to help improve transparency and reproducibility.
