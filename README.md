# 🏎️ Formula 1 Performance & Marketing Analytics (2010–2020)

![R](https://img.shields.io/badge/Language-R-276DC3?style=flat&logo=r&logoColor=white)
![Statistics](https://img.shields.io/badge/Techniques-Regression%20%7C%20Classification%20%7C%20EDA-orange)
![Dataset](https://img.shields.io/badge/Dataset-Kaggle%20F1%201950--2020-brightgreen)
![Status](https://img.shields.io/badge/Status-Completed-success)
![Academic](https://img.shields.io/badge/Academic-MSc%20Business%20Analytics%20%7C%20University%20of%20Birmingham-blue)

---

## Table of Contents

- [Project Overview](#project-overview)
- [Business Problem](#business-problem)
- [Key Results at a Glance](#key-results-at-a-glance)
- [Tech Stack](#tech-stack)
- [Repository Structure](#repository-structure)
- [Methodology](#methodology)
  - [1. Dataset](#1-dataset)
  - [2. Data Preparation](#2-data-preparation)
  - [3. Feature Engineering](#3-feature-engineering)
- [Exploratory Data Analysis — Key Findings](#exploratory-data-analysis--key-findings)
- [Regression Modelling](#regression-modelling)
  - [Model 1: Grid Position + Constructor Prestige → Points](#model-1-grid-position--constructor-prestige--points)
  - [Model 2: Grid Position × Circuit Type Interaction → Points](#model-2-grid-position--circuit-type-interaction--points)
- [Classification Modelling — Podium Prediction](#classification-modelling--podium-prediction)
- [Business Implications](#business-implications)
- [How to Run](#how-to-run)
- [Limitations & Future Work](#limitations--future-work)



##  Project Overview

This project analyses **Formula 1 race data from 2010 to 2020** to quantify the relationship between on-track performance metrics and commercial/marketing value. Using statistical modelling and data visualisation in R, it demonstrates how performance analytics can help **sponsors, teams, and marketers** make data-driven decisions in one of the world's most commercially complex sports.

> F1 sponsorship is projected to generate **$677 million in 2025**, with team title rights worth a combined **$433 million** — making performance prediction directly tied to real financial outcomes.

---

##  Business Problem

F1 stakeholders, sponsors, teams, broadcasters; need to quantify the relationship between on-track results and commercial returns such as sponsor ROI, fan engagement, and brand equity. This project answers three core questions:

1. **Do grid position and constructor prestige reliably predict race points earned?**
2. **Does the type of circuit moderate the effect of starting position on points?**
3. **Can we predict podium finishes accurately enough to inform sponsor activation strategies?**

---

##  Key Results at a Glance

| Metric | Value |
|--------|-------|
| Variance in points explained (Regression Model 1) | **~47%** (Adj. R² = 0.4698) |
| Overall model accuracy (Podium Classifier) | **91.31%** |
| AUC – ROC (Podium Classification) | **0.917** |
| Specificity (conservative podium prediction) | **95.79%** |
| Sensitivity | **63.73%** |
| Training / Test Split | **70 / 30** |
| Dataset Size | **>4,600 race entries** (2010–2020) |
| Race completion rate in dataset | **80.5%** |

---

##  Tech Stack

| Tool / Library | Purpose |
|----------------|---------|
| **R (v4.2.1)** | Core programming language |
| **tidyverse / dplyr** | Data wrangling and transformation |
| **ggplot2** | Data visualisation |
| **caret** | Model training, cross-validation |
| **pROC** | ROC curve and AUC analysis |
| **naniar** | Missing value detection and imputation |
| **effects / interactions / sjPlot** | Model visualisation and interaction analysis |
| **gridExtra / scales** | Multi-panel plots and formatting |
| **pastecs** | Detailed descriptive statistics |

---

##  Repository Structure
```
f1-marketing-performance-analytics/
│
├── data/
│ ├── raw/ # Original Kaggle CSVs (races, results, drivers, etc.)
│ │ ├── races.csv
│ │ ├── results.csv
│ │ ├── drivers.csv
│ │ ├── constructors.csv
│ │ ├── circuits.csv
│ │ ├── qualifying.csv
│ │ └── status.csv
│ ├── processed/
│ │ ├── final_data1.csv # Post-join, pre-cleaning dataset
│ │ └── f1_data_processed.csv # Final cleaned & feature-engineered dataset
│ └── data_dictionary.md # Full variable descriptions and marketing relevance
│
├── src/
│ ├── 01_data_preparation.R # Table joins, filtering (2010–2020), prestige tiers
│ ├── 02_data_cleaning.R # Missing value imputation, feature engineering
│ ├── 03_eda_analysis.R # Descriptive stats, visualisations
│ ├── 04_regression_model1.R # Model 1: Points ~ Grid + Constructor Prestige
│ ├── 05_regression_model2.R # Model 2: Interaction (Grid * Circuit Type) + ANOVA
│ └── 06_classification_model.R # Logistic regression for podium prediction
│
├── visualisations/
│ ├── points_by_constructor.png # Boxplot: Points distribution by top constructors
│ ├── qualifying_vs_points.png # Scatterplot: Qualifying position vs. points
│ ├── performance_trends.png # Line chart: Constructor trends 2010–2020
│ ├── position_improvement.png # Bar chart: Improvement rate by circuit type
│ ├── model1_predictions.png # Actual vs. Predicted points scatter
│ ├── roc_curve.png # ROC curve for podium classification
│ └── grid_podium_probability.png # Grid position vs. podium probability
│
├── reports/
│ └── F1_Technical_Report.pdf # Full academic report
│
├── .gitignore
├── LICENSE
└── README.md
```

---

##  Methodology

### 1. Dataset
The analysis uses the **Formula 1 World Championship dataset (1950–2020)** from [Kaggle](https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020), filtered to **2010–2020** to ensure a consistent points scoring system (25-18-15-12-10-8-6-4-2-1 introduced in 2010).

### 2. Data Preparation
Seven relational tables were joined using inner joins on their respective ID fields:
```
races ──► results ──► drivers
──► constructors
──► circuits
──► status
──► qualifying (left join)
```

A **constructor prestige tier** variable was engineered to reflect historical standing:

| Tier | Constructors |
|------|-------------|
| 1 | Mercedes |
| 2 | Red Bull, Ferrari |
| 3 | McLaren |
| 4 | Renault, Williams |
| 5 | Force India |
| 6 | All other backmarker teams |

### 3. Feature Engineering
The following derived variables were created to enhance analytical depth:

- `position_change` — grid position minus finishing position (positive = gained places)
- `race_completed` — binary flag based on status field
- `dnf` — Did Not Finish indicator
- `performance_index` — weighted composite: `(points × 0.5) + (position_change × 0.3) + ((24 - qualifying_pos) × 0.2)`
- `podium_finish` — binary: position ≤ 3 (for classification)
- `marketing_exposure` — tiered: High (top 3) / Medium (top 10) / Low (outside top 10)
- Qualifying times converted from `MM:SS` format to seconds; missing values imputed using **median by circuit and year** via `naniar`

---

##  Exploratory Data Analysis — Key Findings

### Constructor Performance Hierarchy
Mercedes led the decade with a mean **18.73 points per race**, followed by Red Bull (15.46) and Ferrari (11.92). Despite lower average points, **Ferrari showed the narrowest distribution (SD: 7.84)**, suggesting more reliable sponsor exposure value.

### Qualifying Position vs. Points
A strong **negative correlation (r = -0.78)** was found between qualifying position and points scored. The relationship is non-linear - **positions 1–3 yield disproportionately higher points**, confirming the commercial premium of front-row grid slots.

### Circuit Type Analysis
Street circuits showed the **highest position improvement rate at 52.3%**, making them optimal for underdog narrative marketing content. High-speed circuits favour starting position retention.

### Seasonal Dominance Patterns
Performance trends (2010–2020) show **Red Bull dominating 2010–2013**, **Mercedes commanding 2014–2020**, and Ferrari showing intermittent competitiveness; a pattern with direct implications for long-term sponsorship valuation.

---

##  Regression Modelling

### Model 1: Grid Position + Constructor Prestige → Points

```
model1 <- lm(points ~ grid + constructor_prestige, data = f1_data)
```
| Metric                    | Value             |
| ------------------------- | ----------------- |
| F-statistic               | F(2, 4601) = 2041 |
| p-value                   | < 2.2e-16         |
| Adjusted R²               | 0.4698            |
| Grid Position (β₁)        | -0.52 (p < 0.001) |
| Constructor Prestige (β₂) | -1.13 (p < 0.001) |

Interpretation: Every position further back on the grid reduces predicted points by ~0.52. Each step down in constructor prestige tier reduces predicted points by ~1.13. Both variables independently and significantly predict race points. H₁ supported.

### Model 2: Grid Position × Circuit Type Interaction → Points

```
model2 <- lm(points ~ grid + circuit_type + grid*circuit_type, data = f1_data)
```

| Metric                                  | Value                      |
| --------------------------------------- | -------------------------- |
| F-statistic                             | F(5, 4598) = 664.7         |
| p-value                                 | < 2.2e-16                  |
| Adjusted R²                             | 0.4189                     |
| Interaction terms (grid × circuit_type) | Not significant (p > 0.05) |

Interpretation: Circuit type does NOT significantly moderate the relationship between grid position and points. Model 1 is the more parsimonious model. H₁ not supported for interaction hypothesis. Marketing strategies focused on grid position do not need substantial adaptation based on circuit type alone.

---

##  Classification Modelling — Podium Prediction

**Problem:**
Predict whether a driver will finish in the top 3 (podium), a binary outcome with direct high-value marketing implications. Podiums account for only 13.98% of all race outcomes (class imbalance).
Model
```
podium_model <- glm(podium_finish ~ grid + constructor_prestige + circuit_type,
                    data = train_data, family = "binomial")
```

| Metric            | Value                        |
| ----------------- | ---------------------------- |
| Accuracy          | 91.31% (95% CI: 0.897–0.927) |
| AUC               | 0.917                        |
| Balanced Accuracy | 79.76%                       |
| Specificity       | 95.79%                       |
| Sensitivity       | 63.73%                       |
| True Positives    | 123                          |
| True Negatives    | 1,138                        |
| False Positives   | 50                           |
| False Negatives   | 70                           |

**Error Analysis** - 

False Negatives (70 cases): Unexpected podiums from prestigious constructors (Ferrari, Red Bull, Mercedes) starting mid-grid (mean grid: 7) - missed sponsor activation opportunities.

False Positives (50 cases): Predicted podiums for top teams from front rows (mean grid: 2.34) that failed to materialise (mean finish: 13.1) - risk of premature marketing resource commitment.

---

##  Business Implications

For Sponsors: Use regression coefficients to structure performance-based incentive deals, quantifying expected points from a given grid position and team tier removes uncertainty from contract negotiations

For Teams: Prioritise investment in qualifying performance and constructor development over circuit-specific strategies, as circuit type showed no significant moderating effect

For Broadcasters/Marketers: The classifier's high specificity (95.79%) makes it a conservative but reliable tool for pre-race content planning and sponsor activation scheduling

For F1 Marketing Strategy: The 47% variance explained leaves significant room for incorporating additional variables (pit stop strategy, weather, driver-specific form) in future models

---

##  How to Run
Prerequisites
```
install.packages(c(
  "tidyverse", "ggplot2", "dplyr", "caret", "pROC",
  "naniar", "effects", "interactions", "sjPlot",
  "gridExtra", "scales", "pastecs", "lubridate"
))
```

Steps
### 1. Clone the repository
```
 git clone https://github.com/YourUsername/msc-f1-marketing-performance-analytics.git
```
### 2. Place raw Kaggle CSVs in data/raw/

### 3. Run scripts in order:
```
source("src/01_data_preparation.R")
source("src/02_data_cleaning.R")
source("src/03_eda_analysis.R")
source("src/04_regression_model1.R")
source("src/05_regression_model2.R")
source("src/06_classification_model.R")
```

> Note: Update file paths in each script to use relative paths (e.g., "data/raw/races.csv") before running.

# Data Source

Download the dataset from Kaggle – Formula 1 World Championship (1950–2020) and place the CSV files in data/raw/.

---

##  Limitations & Future Work
Models explain ~47% of variance; significant unexplained factors remain (weather, pit strategy, safety cars, driver psychology)

Analysis is retrospective (2010–2020) and does not reflect post-2021 regulatory changes (budget caps, ground-effect cars)

Moderate sensitivity (63.7%) means the podium classifier misses genuine upsets

Future enhancements: Incorporate real-time telemetry, driver-specific form variables, NLP-based fan sentiment analysis, and pit stop strategy data for richer predictive models.


