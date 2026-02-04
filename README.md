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

## Approach (Model 2)
Models are fit separately for each coverage:
- Additional Living Expense
- Personal Property
- Guest Medical
- Liability

Each coverage uses a two part frequency severity structure:
- Frequency: Poisson GLM on claim count (`claim_n`)
- Severity: Gamma GLM (log link) on average paid per claim (`avg_paid`) for claimants

Expected loss is computed as:
- Expected Loss = E[Claim Count] × E[Avg Severity]

The model then applies calibration and stabilization before converting losses to premiums.

## Inputs / Risk Factors
- Risk tier
- On/off campus
- Sprinklered building
- Greek affiliation
- Distance to campus (banded)

GPA and gender are not used in Model 2.

## Stabilization + Pricing Controls
- Severity capping by coverage (high percentile caps)
- Partial uncapping adjustment to restore some tail risk
- Out of fold (OOF) coverage-level calibration with credibility shrinkage
- Predicted loss caps by coverage
- Liability blending toward tier mean due to low volume
- Tier base loss credibility smoothing
- Within-tier relativity shrinkage and caps to limit premium dispersion

## Validation (Holdout)
Performance is checked on the holdout split using Predicted / Actual mean loss by coverage:

- Additional Living Expense: ~0.99
- Guest Medical: ~1.11
- Liability: ~0.74
- Personal Property: ~1.07

## Files
- `model2.R` — full modeling + pricing pipeline (final)
- `student_totals_model2.csv` — final output: annual premiums per student (`total_renew`, `total_new`)
- `model1.R` — earlier version (logistic frequency + Gamma severity)
