<p align="center">
  <img src="man/figures/logo_pandora_seom.png" alt="Pandora SEOM" width="320"/>
</p>

# pantheia

**Pancreatic Cancer Prognostic Calculator**
*PANTHEIA-SEOM Research Group*

---

## Overview

`pantheia` is an R package that provides an interactive clinical calculator for individualized prognostic estimation in metastatic pancreatic ductal adenocarcinoma (PDAC). Developed within the **PANTHEIA-SEOM** multicenter research project, this tool illustrates the prognostic impact of the **Systemic Inflammation Response Index (SIRI)** on key therapeutic outcomes.

The calculator implements multivariable regression models (Weibull accelerated failure time for survival endpoints and logistic regression for tumor response) fitted on a multicenter Spanish cohort, enabling patient-specific predictions of:

- **Median Progression-Free Survival (PFS)**
- **Median Overall Survival (OS)**
- **Objective Response Probability (RECIST)**

All estimates are accompanied by 95% confidence intervals and predicted survival curves.

## About the PANTHEIA-SEOM Project

PANTHEIA-SEOM is a multicenter observational cohort study conducted across 22 hospitals in Spain, promoted by the Outcomes Research and Clinical Practice Section of the **Spanish Society of Medical Oncology (SEOM)**. The study utilizes the **Pandora** platform, a SEOM-proprietary web-based electronic Case Report Form (eCRF), for centralized data collection.

The project investigates the role of systemic inflammation, specifically through SIRI, as a prognostic and potentially predictive biomarker in patients with metastatic PDAC receiving first-line chemotherapy.

## Transparency, Open Code & Reproducibility

This R package is released as a commitment to **open science** and **frictionless reproducibility**. In line with best practices in biomarker research and clinical modeling, the complete source code and fitted model objects are made publicly available so that every aspect of the calculator can be independently verified.

**Why this matters:**

- **Full transparency**: Every line of code behind the prediction engine, the Shiny interface, and the model specifications is open for inspection. There are no black boxes: researchers and reviewers can trace exactly how each clinical prediction is derived, from raw model coefficients to the final survival curves displayed in the calculator.
- **Frictionless reproducibility**: With a single command (`devtools::install_github("albertocarm/pantheia")`), any researcher can install and run the exact same calculator locally, with no configuration, no external servers, and no dependencies beyond standard R packages. The models, the code, and the application are all contained within this package.
- **Audit and extension**: The fitted model objects (`.rds` files) are included in the package, allowing independent re-evaluation of coefficients, variance-covariance matrices, and spline specifications. Researchers can adapt the methodology or apply it to their own cohorts.

This approach aligns with the STROBE and REMARK reporting guidelines followed by the PANTHEIA-SEOM study, and reflects our conviction that clinical research tools should be open, verifiable, and accessible to the scientific community.

## Installation

Install directly from GitHub:

```r
# install.packages("devtools")
devtools::install_github("albertocarm/pantheia")
```

### Requirements

- R >= 4.0.0
- Dependencies (installed automatically): `shiny`, `survival`, `splines`, `ggplot2`, `scales`

## Usage

Launch the calculator locally:

```r
library(pantheia)
pantheia()
```

This opens the interactive application in your default web browser. The calculator accepts the following clinical inputs:

| Parameter | Description |
|---|---|
| **SIRI** | Systemic Inflammation Response Index: (Neutrophils x Monocytes) / Lymphocytes |
| **Tumor diameter** | Sum of all baseline target lesion diameters (<=5 cm vs >5 cm) |
| **Regimen** | First-line chemotherapy regimen |
| **ECOG PS** | Eastern Cooperative Oncology Group Performance Status |
| **CACS** | Cancer Anorexia-Cachexia Syndrome (Yes/No) |

## Online Version

An online version of the calculator is also available at:
**https://pantheia-siri.shinyapps.io/calc/**

## How It Works

The calculator implements three fitted multivariable models:

- **PFS and OS**: Weibull Accelerated Failure Time (AFT) models with natural spline transformation of log(SIRI) and interaction terms between chemotherapy regimen and inflammatory status
- **Tumor response**: Logistic regression model for objective response probability (RECIST 1.1)

All models adjust for tumor burden, ECOG performance status, cancer anorexia-cachexia syndrome, and chemotherapy regimen. SIRI is modeled as a continuous variable using natural splines to capture nonlinear associations.

## Reproducible Functions

This package provides a complete set of functions to reproduce every table and figure from the associated manuscript. After installation, all outputs can be regenerated with a single function call:

```r
library(pantheia)
```

### Available Functions

| Function | Description |
|---|---|
| `pantheia()` | Launch the interactive prognostic calculator (Shiny app) |
| `table1()` | Baseline patient characteristics |
| `fig_1()` | Prognostic validation of SIRI after discretization: Kaplan-Meier survival curves (OS, PFS) and objective response rates by SIRI group |
| `fig_2()` | Nonlinear multivariable association between continuous log(SIRI) and clinical outcomes (PFS, OS, response) |
| `fig_s1()` | Comparative performance of log-transformed inflammatory indices |
| `fig_s2()` | Association between SIRI levels and cancer anorexia-cachexia syndrome and symptom burden |
| `fig_s3()` | Association between SIRI levels and hepatic involvement and tumor burden |
| `fig_s4()` | Association between SIRI levels and demographic and lifestyle factors |

### Accessing the Dataset

The complete anonymized study dataset is included in the package and can be loaded directly into R for independent analysis:

```r
data("pantheia_data", package = "pantheia")
```

## Citation

The methodological details and clinical validation of the PANTHEIA-SEOM study are described in a manuscript currently under review. Citation details will be provided upon publication.

## License

This project is licensed under the GNU General Public License v3.0. See [LICENSE](LICENSE) for details.

## Acknowledgments

This work was conducted within the framework of the **PANTHEIA-SEOM** research project, promoted by the **Spanish Society of Medical Oncology (SEOM)** through its Outcomes Research and Clinical Practice Section, and supported by the **Pandora** data collection platform.
