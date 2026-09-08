# ==============================================================================
# Project Setup
# Author       : Johannes Julius Mohn
# Contact      : johannes.j.mohn@maxplanckschools.de
# Date Created : 2026-05-17
# R Version    : R version 4.4.1 (2024-06-14)
# ==============================================================================

# --- display options ---
options(
  scipen = 6,
  digits = 5
)

# --- packages ---
library(tidyverse)
library(here)
library(charite)

# --- directories ---
path2data               <- here("data")
path2data_demographics  <- here("data", "demographics") 
path2data_maltreatment  <- here("data", "maltreatment")
path2data_mri           <- here("data", "mri")
path2data_braincharts   <- file.path(path2data_mri, "braincharts", "braincharts-global_bethlehem", "01_model_output")
path2data_clinical      <- here("data", "clinical")
path2data_cognition     <- here("data", "cognition")
path2outputs            <- here("output")
path2results            <- here("output", "results")
path2figures            <- here("output", "figures")
path2tables             <- here("output", "tables")

dir.create(path2outputs,  recursive = TRUE, showWarnings = FALSE)
dir.create(path2results,  recursive = TRUE, showWarnings = FALSE)
dir.create(path2figures,  recursive = TRUE, showWarnings = FALSE)
dir.create(path2tables,   recursive = TRUE, showWarnings = FALSE)

# --- functions ---
source(here("R", "visualization_parameters.R"))
