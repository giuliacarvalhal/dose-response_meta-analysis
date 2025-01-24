# dose_meta-analysis

Overview
This repository contains data and R scripts used in our systematic review and meta-analysis on the efficacy and safety of ensifentrine in patients with COPD. Below is a quick reference for how to reproduce each analysis using the scripts in this repository.

Repository Structure
arduino
Copy
Edit
Ensifentrine-Meta/
├── data/
│   ├── IMMA_.xlsx 
│   ├── ari.xlsx
│   ├── ari_one_stage.xlsx
├── scripts/
│   ├── 1_meta_analysis.R
│   ├── 2_dose_responsed_meta_analysis.R
│   ├── 3_leave_one_out_dose_response.R
│   └── 4_aggregated_curves.R
└── README.md

data/

IMMA_.xlsx: Data for meta-analysis of continuous (FEV1) and binary (adverse events) outcomes.
ari.xlsx: Data for the dose-response meta-analysis.
ari_one_stage.xlsx: Data for one-stage random-effects models (subset of trials or special analyses).
scripts/

1_meta_analysis.R
Performs standard meta-analyses (continuous outcomes like FEV1, binary outcomes like adverse events/severe adverse events).
Uses packages meta and metafor for random-effects modeling, leave-one-out sensitivity analysis, and Baujat plots.
2_dose_responsed_meta_analysis.R
Implements restricted cubic splines to analyze dose-response relationships (two-stage random-effects approach) via dosresmeta.
Calculates the ED95 by bootstrapping the maximum effect.
3_leave_one_out_dose_response.R
Runs leave-one-out sensitivity analyses for the dose-response model.
Generates plots showing how each study’s exclusion influences the overall dose-response curve.
4_aggregated_curves.R
Demonstrates how different sets of studies (e.g., excluding unpublished data, requiring longer follow-up) influence the dose-response curve.
Uses one-stage random-effects modeling (proc = "1stage" in dosresmeta).
Data Preparation
Cross-Over vs. Parallel Trials

We designate each trial as “Parallel” or “Crossover” in the Excel sheets. For crossover studies, we follow the Cochrane Handbook guidelines (e.g., analyzing first-period data or applying appropriate variance adjustments).
Please see Methods section of the manuscript (link to your paper or supplement, if available) for detailed information on how the crossover data were handled.
Missing Data

When trials did not report standard deviations, we imputed them using standard methods (e.g., from standard errors, confidence intervals, or other reported measures). These imputations are integrated into the Excel files under the columns “sd.e” and “sd.c” for continuous outcomes, or “n.e” and “n.c” for binary outcomes.
Outcome Measures

PEAKFEV1DAY1: Day 01 peak FEV1 (mL)
PEAKFEV1LAST: Final assessment peak FEV1 (mL)
MORNING: Morning trough FEV1 (mL)
SAE: Severe adverse events (binary data)
ari.xlsx: Contains dose (mg/day) and corresponding mean differences for FEV1.
Other outcomes (e.g., SGRQ, TDI, E-RS) are either included in separate files or were analyzed separately.
How to Run the Analyses
1. Set up R Environment
Recommended R Version: 4.2.2 (or later)
Install necessary R packages (if not already installed):
r
Copy
Edit
install.packages(c(
  "meta", "metafor", "dosresmeta", "tidyverse", "grid", 
  "mvmeta", "gridExtra", "rms", "mvtnorm", "DoseFinding",
  "aod", "readxl", "writexl", "lmtest", "knitr", "directlabels"
))
2. Clone or Download This Repository
bash
Copy
Edit
git clone https://github.com/YourGitHubUsername/Ensifentrine-Meta.git
cd Ensifentrine-Meta
(Alternatively, download the zip file from GitHub.)

3. Run Each R Script
Open each script in R (or RStudio) and set the working directory appropriately:

r
Copy
Edit
# Example:
setwd("path/to/Ensifentrine-Meta")
Then, run the script line by line or in full:

1_meta_analysis.R
Reads IMMA_.xlsx, performs meta-analyses on FEV1 endpoints and AEs.
Generates forest plots, leave-one-out plots, and Baujat plots.
2_dose_responsed_meta_analysis.R
Reads ari.xlsx, fits restricted cubic spline models (two-stage).
Estimates ED95 via bootstrapping.
3_leave_one_out_dose_response.R
Reads ari.xlsx (or other designated file), excludes each study in turn, refits the model, and plots the results.
4_aggregated_curves.R
Performs one-stage analyses (proc = "1stage") on various subsets (e.g., excluding certain studies).
Compares how results differ by exclusion criteria or study type.
4. Outputs
Each script saves figures and tables (e.g., .tiff, .pdf, .csv) in the project directory:
Forest plots, Baujat plots, leave-one-out plots, etc.
Dose-response curves (standard approach vs. excluding certain studies).
Tables summarizing regression coefficients, ED95 and confidence intervals.
Risk of Bias and GRADE
Per the manuscript, the Risk of Bias was assessed using ROB2, and certainty of evidence was evaluated with GRADE. These steps were performed outside of these scripts (e.g., manual scoring, Excel, or an online tool). If you wish to see our final ROB2 tables and GRADE profiles, please consult the paper’s supplementary material.
Contact
Primary Corresponding Author: [Your Name] (email@address)
For questions about data extraction or R scripts, contact: [Your Name] (email@address)
License
If you wish to provide a license (e.g., MIT License or CC-BY 4.0), add a LICENSE file and a note here. For example:

This project is licensed under the terms of the MIT license.

Citation
If you use or adapt these scripts, please cite our publication:

less
Copy
Edit
[Authors]. "[Title]." *Respirology*. [Year]; [Volume(Issue)]:Pages. 
DOI: [doi link]
Additionally, if you wish, you can archive this repository on Zenodo to obtain a DOI.

Thank you for your interest in our Ensifentrine Meta-Analysis Project!
We welcome any feedback or questions to help improve transparency and reproducibility.
