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

## Reproducible Functions & Model Objects

This package contains all the functions and fitted model objects required to reproduce the analytical outputs presented in the associated manuscript. The complete source code is available at [`inst/app/app.R`](inst/app/app.R).

### Prediction Functions

| Function | Description |
|---|---|
| `predecir_final()` | Weibull AFT prediction engine for PFS and OS. Constructs the design matrix from clinical inputs, maps coefficients (including spline and interaction terms), computes the linear predictor, and derives median survival times with 95% confidence intervals from the variance-covariance matrix. |
| `predecir_respuesta()` | Logistic regression prediction engine for objective response (RECIST). Manually constructs the natural spline basis for log(SIRI) using the original interior knots and boundary knots, applies all covariate effects and regimen-by-SIRI interaction terms, and returns the predicted response probability. |

### Survival Curve Generation

The application generates individualized Weibull survival curves for each patient profile:

- **PFS curve**: Predicted survival function over 24 months with 95% confidence band, derived from the AFT linear predictor and scale parameter
- **OS curve**: Predicted survival function over 36 months with 95% confidence band, using the same Weibull parameterization

Both curves are rendered using `ggplot2` and display the median survival estimate with its confidence interval.

### Fitted Model Objects

The following serialized R model objects are included in the package under `inst/app/` and can be loaded independently for inspection or reuse:

| File | Model | Contents |
|---|---|---|
| `FINAL_PFS_RUBIN.rds` | Weibull AFT for PFS | Pooled coefficients, variance-covariance matrix, spline knots, scale parameter, factor levels (from multiple imputation via Rubin's rules) |
| `FINAL_OS_RUBIN.rds` | Weibull AFT for OS | Same structure as PFS model |
| `FINAL_RESP_RUBIN_NS13.rds` | Logistic regression for ORR | Pooled coefficients, interior knots for natural splines, factor levels |

These objects contain the complete model specification needed to reproduce any prediction: coefficients, variance-covariance matrices, spline knot locations, and the original factor levels used during fitting. Researchers can load them directly:

```r
mod_pfs  <- readRDS(system.file("app", "FINAL_PFS_RUBIN.rds", package = "pantheia"))
mod_os   <- readRDS(system.file("app", "FINAL_OS_RUBIN.rds", package = "pantheia"))
mod_resp <- readRDS(system.file("app", "FINAL_RESP_RUBIN_NS13.rds", package = "pantheia"))
```

## Citation

The methodological details and clinical validation of the PANTHEIA-SEOM study are described in a manuscript currently under review. Citation details will be provided upon publication.

## License

This project is licensed under the GNU General Public License v3.0. See [LICENSE](LICENSE) for details.

## Acknowledgments

This work was conducted within the framework of the **PANTHEIA-SEOM** research project, promoted by the **Spanish Society of Medical Oncology (SEOM)** through its Outcomes Research and Clinical Practice Section, and supported by the **Pandora** data collection platform.
