# Heart Disease Risk Prediction

Analyzed 1,025 patient records from the UCI Heart Disease dataset in R. Built a logistic regression model achieving 84.8% accuracy and 0.93 AUC. Key predictors: chest pain type, major vessels, and thalassemia results.

## Overview

This project performs end-to-end exploratory data analysis and predictive modeling on the [UCI Cleveland Heart Disease Dataset](https://archive.ics.uci.edu/ml/datasets/heart+disease). The goal is to identify key clinical risk factors and build a model capable of predicting heart disease presence.

## Tools & Technologies

- **Language:** R
- **Libraries:** tidyverse, ggplot2, caret, pROC, corrplot, gridExtra
- **Modeling:** Logistic Regression
- **Report:** R Markdown

## Dataset

- **Source:** UCI Machine Learning Repository
- **Records:** 1,025 patients
- **Features:** 14 clinical variables including age, sex, chest pain type, cholesterol, resting blood pressure, max heart rate, and more
- **Target:** Binary classification — heart disease present or absent

## Key Findings

- **Age & Gender:** Patients with heart disease were significantly older on average, and males showed substantially higher disease prevalence than females
- **Chest Pain Type:** Asymptomatic chest pain was the strongest single indicator of heart disease
- **Max Heart Rate:** Lower maximum heart rate achieved during exercise was associated with disease presence
- **ST Depression:** Higher oldpeak values correlated with increased disease likelihood

## Model Performance

| Metric | Score |
|--------|-------|
| Accuracy | 84.8% |
| AUC | 0.93 |
| Sensitivity | 89.5% |
| Specificity | 79.8% |
| F1 Score | 85.8% |

## Top Predictors

1. Chest pain type (`cp`)
2. Number of major vessels colored by fluoroscopy (`ca`)
3. Sex (`sex`)
4. Thalassemia results (`thal`)
5. ST depression (`oldpeak`)

## Project Structure

```
├── heart.csv                   # Dataset
├── heart_disease_analysis.R    # Full analysis script
├── heart_disease_report.Rmd    # R Markdown report source
├── heart_disease_report.pdf    # Rendered report
└── README.md
```

## How to Run

1. Clone the repository
2. Open `heart_disease_analysis.R` or `heart_disease_report.Rmd` in RStudio
3. Ensure required packages are installed:
```r
install.packages(c("tidyverse", "caret", "pROC", "corrplot", "gridExtra", "RColorBrewer", "scales"))
```
4. Run the script or knit the R Markdown file

## Limitations

- Dataset originates from a single medical center in the 1980s
- Some risk factors (family history, lifestyle) are not captured
- Model should be validated on external data before any clinical use
