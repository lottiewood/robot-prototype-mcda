# Multi-Criteria Decision Analysis of Autonomous Delivery Robot Prototypes Using TOPSIS

**Lottie Wood** · MSc Data Science and Analytics, University of Leeds

## Overview

A fictional Leeds-based start-up, AutonomousShipment, has developed seven autonomous
delivery robot prototypes and needs to select just two for a resource-constrained trial.
This project uses the **TOPSIS** (Technique for Order Preference by Similarity to Ideal
Solution) method of Multi-Criteria Decision Analysis (MCDA) to recommend the best
prototype under two different business plans, one prioritising delivery capacity, the
other prioritising robot technology sales, based on seven weighted criteria (carrying
capacity, battery size, speed, mobility, aesthetic, cost, and reliability).

## Method

- Performance and weighting data for all 7 prototypes were structured into the inputs
  required by R's `TOPSIS()` function: a performance table, criteria weights, and
  min/max preference direction per criterion.
- TOPSIS was chosen over alternatives like the Weighted Sum Method (too simplistic) and
  AHP (too reliant on extensive pairwise comparisons), as it balances closeness to the
  ideal solution against distance from the worst, without extra subjectivity.
- A sensitivity analysis was run to test how robust the top-ranked prototype was to
  changes in criteria weighting — including local weight perturbations, alternative
  weighting schemes, an averaged-weights scenario, and an equal-weights baseline.

## Results

- **Gamma** was the top-scoring prototype under both business plans (0.621 for
  delivery-focused, 0.704 for sales-focused), and remained the top choice across nearly
  all sensitivity tests — recommended for trial under the technology-sales plan.
- **Bravo** was the strongest runner-up, scoring highest overall when mobility was
  prioritised, and consistently ranking in the top 2–3 across scenarios — recommended
  for trial under the delivery-focused plan, since two distinct prototypes were required.

## Repository contents

- `report.pdf` — full write-up: methodology, data tables, TOPSIS scores, and sensitivity
  analysis
- `topsis_analysis.R` — R script for data preparation, TOPSIS scoring, and sensitivity analysis
- `/data` — robot prototype performance and management priorities data

## Tools

R, `topsis` package, `ggplot2`
