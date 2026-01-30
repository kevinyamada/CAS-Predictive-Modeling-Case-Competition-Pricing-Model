# CAS Predictive Modeling Case Competition — Pricing Model

## Overview
This project implements an insurance pricing model developed for the CAS Predictive Modeling Case Competition. The objective is to estimate fair annual insurance premiums for students based on observed risk characteristics and historical claims data.

The model produces renewal and new business premiums by coverage and aggregates them to the student level.

## Modeling Approach
- Separate models were built for each coverage:
  - Additional Living Expense
  - Personal Property
  - Guest Medical
  - Liability
- A two part model was used:
  - Frequency model: probability of having a claim (logistic regression)
  - Severity model: claim size given a claim (Gamma regression)
- Predictions were combined as:  
  **Expected Loss = Frequency × Severity**

## Risk Factors Used
- Risk Tier
- Housing (On/Off Campus)
- Sprinklered Building
- Greek Affiliation
- Distance to Campus (banded)
- GPA (banded)

These factors were chosen for interpretability, actuarial relevance, and stability on holdout data.

## Model Stabilization Techniques
- Severity capping to reduce outlier influence
- Prediction capping at high percentiles by coverage
- Credibility weighting for small segments
- Special blending for Liability due to low claim volume
- Relativity shrinkage and caps to control premium dispersion

## Validation
Model performance was evaluated using a holdout dataset not seen during training.

Holdout ratios (Predicted / Actual):
- Additional Living Expense: 0.99
- Personal Property: 1.07
- Guest Medical: 1.10
- Liability: 0.75

Ratios close to 1 indicate good calibration.

## Output Files
- `model1.R`: Full modeling and pricing pipeline
- `student_totals.csv`: Final annual premiums by student and risk tier

## Notes
The original dataset is provided by the CAS Predictive Modeling Case Competition and is not publicly shareable.
