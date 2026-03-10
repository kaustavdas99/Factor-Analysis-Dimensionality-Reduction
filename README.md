# Immunisation Index using Factor Analysis

This repository provides an R-based statistical workflow to construct a composite **Childhood Immunisation Performance Index** using **Factor Analysis**. The approach combines multiple birth-dose vaccination indicators into a single latent index representing district-level immunisation performance.

Public health datasets often contain several related indicators that reflect different aspects of healthcare delivery. Analysing these indicators separately may obscure broader patterns. This workflow applies dimensionality reduction techniques to summarise correlated immunisation indicators into a single interpretable index that allows systematic comparison across districts and helps identify regions where immunisation coverage may require policy attention.

## Objective
To measure district-level immunisation performance using key birth-dose vaccines:

- BCG  
- OPV 0 (Birth Dose)  
- Hepatitis B0 (Birth Dose)  

These indicators represent early childhood immunisation coverage and are widely used as proxies for health system accessibility and responsiveness immediately after birth.

The method extracts a latent immunisation performance factor and generates a composite index that can be used to classify districts based on relative immunisation risk.

## Data Source
The analysis uses administrative health data obtained from the **Health Management Information System (HMIS)**. The dataset contains district-level childhood immunisation coverage along with contextual classifications such as rural and urban areas and the type of healthcare facility delivering immunisation services.

## Statistical Methods Used

### Factor Suitability Testing
Before constructing the composite index, the dataset is evaluated to ensure that the indicators share sufficient common variation.

- **Kaiser–Meyer–Olkin (KMO) Test** is used to measure sampling adequacy and assess the degree of shared variance among the immunisation indicators.
- **Bartlett’s Test of Sphericity** evaluates whether the correlation matrix significantly differs from an identity matrix, indicating that the variables are sufficiently correlated for factor analysis.

### Factor Analysis
A **single-factor model** is estimated using either the **Minimum Residual (MINRES)** or **Maximum Likelihood (ML)** extraction method. The model identifies the common latent dimension underlying the immunisation indicators and estimates factor loadings representing the contribution of each indicator to the latent immunisation performance construct.

### Factor Score Estimation
Regression-based factor scores are calculated for each observation. These scores represent the **Immunisation Performance Index**, where higher values indicate stronger immunisation coverage and lower values indicate weaker performance.

### Risk Classification
Districts are classified into performance categories using **quantile-based thresholds**:

- High Risk – districts with relatively low immunisation index values  
- Moderate Risk – districts with intermediate performance  
- Low Risk – districts with relatively strong immunisation coverage  

This classification helps identify districts requiring additional programmatic attention.

### Statistical Validation
Several statistical procedures are used to evaluate the properties of the immunisation index:

- **Shapiro–Wilk Test** to assess the distribution of the district-level index
- **Wilcoxon Signed-Rank Test** to compare paired immunisation indices between Rural vs Urban and Public vs Private facilities
- **Kruskal–Wallis Test** to evaluate differences in immunisation performance across district risk groups

## Outputs

The workflow produces several analytical outputs:

- District Immunisation Index  
- Month-wise Immunisation Index  
- Rural vs Urban comparison  
- Public vs Private facility comparison  
- Risk classification of districts  
- Publication-ready visualisations  
- Exportable CSV and Excel summary tables  

## Required R Packages

psych  
dplyr  
tidyr  
ggplot2  
readxl  
lubridate  
stringr  
writexl  
openxlsx  
