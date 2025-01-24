# dose_meta-analysis

**Overview**

This repository contains R scripts used in our dose-response meta-analysis. 

**Repository Structure:**

_R Scripts:_

* Meta_General.R --> perform standard meta-analyses
* dose_responsed_meta_analysis.R --> perform dose-response meta-analysis via dosresmeta
* leave_one_out_dose_response.R --> runs leave-one-out sensitivity analyses for the dose-response model.
* one_stage.R --> perform dose-response meta-analysis for one-stage models (one dose) and aggregate groups to compare them

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

**Contact:**
For questions about data extraction or R scripts, contact: Giulia Carvalhal (carvalhalgiulia@gmail.com)

**References / Credits**
This project draws upon code and ideas from [alecri/dosresmeta](https://github.com/alecri/dosresmeta).

Thank you for your interest in our Dose-Response Meta-Analysis Project!
We welcome any feedback or questions to help improve transparency and reproducibility.
