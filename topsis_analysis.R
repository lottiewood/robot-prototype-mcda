# ==============================================================================
# Robot Prototype Selection via TOPSIS
# Multi-Criteria Decision Analysis for AutonomousShipment delivery robots
# ==============================================================================

# --- Setup --------------------------------------------------------------
# Run once if packages aren't installed:
# install.packages(c("MCDA", "xtable", "readxl", "dplyr"))

library(readxl)
library(xtable)
library(MCDA)
library(dplyr)

# --- Load data ----------------------------------------------------------

# Robot performance data:
# THe data is transposed so it is in the correct format for
# TOPSIS() (alternatives in rows, criteria in columns) 
robot <- read.csv("data/Robot_Info.csv", row.names = 1)
robot <- as.data.frame(t(robot))

# Management's priority data:
management_priority <- read_xlsx("data/Management_Priority.xlsx")

# Export tables to LaTeX for the report
xtable(robot)
xtable(management_priority)

# --- Criteria direction ---------------------------------------------------
# Whether each criterion should be maximised or minimised
criteria_min_max <- c(
  "Carrying.Capacity" = "max",
  "Battery.Size"      = "max",
  "Speed"             = "max",
  "Mobility"          = "max",
  "Aesthetic"         = "max",
  "Cost.Per.Unit"     = "min",
  "Reliability"       = "max"
)
names(criteria_min_max) <- colnames(robot)

# --- Plotting helpers ------------------------------------------------------

robot_colours <- c("#f94144", "#f8961e", "#f9c74f", "#90be6d",
                   "#43aa8b", "#577590", "#775c7f")

#' Bar plot of a single criterion across all robot prototypes
plot_criterion <- function(values, ylab) {
  barplot(values, ylab = ylab, col = robot_colours, las = 2)
}

#' Bar plot of TOPSIS scores across all robot prototypes
plot_topsis_scores <- function(scores, main = "") {
  barplot(scores, xlab = "Robot Prototype", ylab = "TOPSIS score",
          col = robot_colours, main = main)
}

# --- Data visualisation: one panel per criterion ----------------------------

layout(matrix(1:8, nrow = 2, byrow = TRUE))
par(mar = c(4, 4, 2, 2))

for (criterion in colnames(robot)) {
  plot_criterion(setNames(robot[[criterion]], rownames(robot)), ylab = criterion)
}

# --- TOPSIS: Scenario 1 - maximising large shipments ------------------------

ship_weights <- setNames(
  c(0.25, 0.10, 0.15, 0.15, 0.07, 0.20, 0.08),
  colnames(robot)
)

ship_overall <- TOPSIS(robot, ship_weights, criteria_min_max)
ship_overall
plot_topsis_scores(ship_overall, main = "Maximising large shipments")

# --- TOPSIS: Scenario 2 - maximising robot technology sales -----------------

sell_weights <- setNames(
  c(0.10, 0.20, 0.12, 0.12, 0.06, 0.20, 0.20),
  colnames(robot)
)

sell_overall <- TOPSIS(robot, sell_weights, criteria_min_max)
sell_overall
plot_topsis_scores(sell_overall, main = "Maximising technology sales")

# ==============================================================================
# Sensitivity analysis
# ==============================================================================
#
# Each scenario below adjusts the base weights and re-runs TOPSIS, to check
# how robust the top-ranked prototype is to changes in criteria weighting.

run_sensitivity <- function(weight_scenarios, performance_table, min_max) {
  results <- lapply(names(weight_scenarios), function(scenario_name) {
    scores <- TOPSIS(performance_table, weight_scenarios[[scenario_name]], min_max)
    data.frame(
      scenario = scenario_name,
      top_prototype = names(which.max(scores)),
      top_score = max(scores)
    )
  })
  do.call(rbind, results)
}

# --- Scenario 1 sensitivity: local weight perturbations (+/- 0.06) ----------

ship_local_scenarios <- list(
  "Carrying Capacity -0.06" = c(0.19, 0.11, 0.16, 0.16, 0.08, 0.21, 0.09),
  "Carrying Capacity +0.06" = c(0.31, 0.09, 0.14, 0.14, 0.07, 0.19, 0.07),
  "Battery Size -0.06"      = c(0.26, 0.04, 0.16, 0.16, 0.08, 0.21, 0.09),
  "Battery Size +0.06"      = c(0.24, 0.16, 0.14, 0.14, 0.06, 0.19, 0.07),
  "Speed +0.06"             = c(0.24, 0.09, 0.21, 0.14, 0.07, 0.19, 0.07),
  "Speed -0.06"             = c(0.26, 0.11, 0.09, 0.16, 0.08, 0.21, 0.09),
  "Mobility +0.06"          = c(0.24, 0.09, 0.14, 0.21, 0.06, 0.19, 0.07),
  "Mobility -0.06"          = c(0.26, 0.11, 0.16, 0.09, 0.08, 0.21, 0.09),
  "Aesthetic +0.06"         = c(0.24, 0.09, 0.14, 0.14, 0.13, 0.19, 0.07),
  "Aesthetic -0.06"         = c(0.26, 0.11, 0.16, 0.16, 0.01, 0.21, 0.09),
  "Cost Per Unit +0.06"     = c(0.24, 0.09, 0.14, 0.14, 0.06, 0.26, 0.07),
  "Cost Per Unit -0.06"     = c(0.26, 0.11, 0.16, 0.16, 0.08, 0.14, 0.09),
  "Reliability +0.06"       = c(0.24, 0.09, 0.14, 0.14, 0.06, 0.19, 0.14),
  "Reliability -0.06"       = c(0.26, 0.11, 0.16, 0.16, 0.08, 0.21, 0.02)
)
ship_local_scenarios <- lapply(ship_local_scenarios, setNames, colnames(robot))

ship_local_results <- run_sensitivity(ship_local_scenarios, robot, criteria_min_max)
ship_local_results

# --- Scenario 1 sensitivity: alternative weighting schemes ------------------

ship_scheme_scenarios <- list(
  "Top 2 +0.1 each"                   = c(0.35, 0.06, 0.11, 0.11, 0.03, 0.30, 0.04),
  "Top 2 +0.125 each"                 = c(0.375, 0.05, 0.10, 0.10, 0.02, 0.325, 0.03),
  "Top 2 +0.15 each"                  = c(0.40, 0.04, 0.09, 0.09, 0.01, 0.35, 0.02),
  "CC -0.05, Cost -0.025"             = c(0.20, 0.115, 0.165, 0.165, 0.085, 0.175, 0.095),
  "Top 4 +0.03 each"                  = c(0.28, 0.06, 0.18, 0.18, 0.03, 0.23, 0.04),
  "Top 4 +0.015 each"                 = c(0.265, 0.08, 0.165, 0.165, 0.05, 0.215, 0.06),
  "Top 4 +0.02 each (critical thr.)"  = c(0.27, 0.1 - 0.08 / 3, 0.17, 0.17, 0.07 - 0.08 / 3, 0.22, 0.08 - 0.08 / 3),
  "Top 4 +0.025 each (critical thr.)" = c(0.275, 0.1 - 0.1 / 3, 0.175, 0.175, 0.07 - 0.1 / 3, 0.225, 0.08 - 0.1 / 3),
  "Carrying Capacity +0.12"           = c(0.37, 0.08, 0.13, 0.13, 0.05, 0.18, 0.06)
)
ship_scheme_scenarios <- lapply(ship_scheme_scenarios, setNames, colnames(robot))

ship_scheme_results <- run_sensitivity(ship_scheme_scenarios, robot, criteria_min_max)
ship_scheme_results

# --- Scenario 2 sensitivity: local weight perturbations (+/- 0.06) ----------

sell_local_scenarios <- list(
  "Carrying Capacity +0.06" = c(0.16, 0.19, 0.11, 0.11, 0.05, 0.19, 0.19),
  "Carrying Capacity -0.06" = c(0.04, 0.21, 0.13, 0.13, 0.07, 0.21, 0.21),
  "Battery Size +0.06"      = c(0.09, 0.26, 0.11, 0.11, 0.05, 0.19, 0.19),
  "Battery Size -0.06"      = c(0.11, 0.14, 0.13, 0.13, 0.07, 0.21, 0.21),
  "Speed +0.06"             = c(0.09, 0.19, 0.18, 0.11, 0.05, 0.19, 0.19),
  "Speed -0.06"             = c(0.11, 0.21, 0.06, 0.13, 0.07, 0.21, 0.21),
  "Mobility +0.06"          = c(0.09, 0.19, 0.11, 0.18, 0.05, 0.19, 0.19),
  "Mobility -0.06"          = c(0.11, 0.21, 0.13, 0.06, 0.07, 0.21, 0.21),
  "Aesthetic +0.06"         = c(0.09, 0.19, 0.11, 0.11, 0.12, 0.19, 0.19),
  "Aesthetic -0.06"         = c(0.11, 0.21, 0.13, 0.13, 0.00, 0.21, 0.21),
  "Cost Per Unit +0.06"     = c(0.09, 0.19, 0.11, 0.11, 0.05, 0.26, 0.19),
  "Cost Per Unit -0.06"     = c(0.11, 0.21, 0.13, 0.13, 0.07, 0.14, 0.21),
  "Reliability +0.06"       = c(0.09, 0.19, 0.11, 0.11, 0.05, 0.19, 0.26),
  "Reliability -0.06"       = c(0.11, 0.21, 0.13, 0.13, 0.07, 0.21, 0.14)
)
sell_local_scenarios <- lapply(sell_local_scenarios, setNames, colnames(robot))

sell_local_results <- run_sensitivity(sell_local_scenarios, robot, criteria_min_max)
sell_local_results

# --- Scenario 2 sensitivity: alternative weighting schemes ------------------

sell_scheme_scenarios <- list(
  "Top 3 +0.05 each"           = c(0.0625, 0.25, 0.0825, 0.0825, 0.0225, 0.25, 0.25),
  "Top 3 -0.03 each"           = c(0.1225, 0.17, 0.1425, 0.1425, 0.0825, 0.17, 0.17),
  "Top 3 +0.075, rest reduced" = c(0.04, 0.275, 0.06, 0.06, 0.015, 0.275, 0.275),
  "Battery Size +0.12"         = c(0.08, 0.32, 0.10, 0.10, 0.04, 0.18, 0.18),
  "Cost +0.12"                 = c(0.08, 0.18, 0.10, 0.10, 0.04, 0.32, 0.18),
  "Reliability +0.12"          = c(0.08, 0.18, 0.10, 0.10, 0.04, 0.18, 0.32),
  "Top 3 at 0.3, rest 0.025"   = c(0.025, 0.30, 0.025, 0.025, 0.025, 0.30, 0.30)
)
sell_scheme_scenarios <- lapply(sell_scheme_scenarios, setNames, colnames(robot))

sell_scheme_results <- run_sensitivity(sell_scheme_scenarios, robot, criteria_min_max)
sell_scheme_results

# --- Combined view: averaged weights and equal weights ----------------------

average_weights <- setNames((ship_weights + sell_weights) / 2, colnames(robot))
average_overall <- TOPSIS(robot, average_weights, criteria_min_max)
average_overall
plot_topsis_scores(average_overall, main = "Averaged weights (both scenarios)")

equal_weights <- setNames(rep(1 / 7, 7), colnames(robot))
equal_overall <- TOPSIS(robot, equal_weights, criteria_min_max)
equal_overall
plot_topsis_scores(equal_overall, main = "Equal weights")