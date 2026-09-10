# ==============================================================================
# Project      : Kids2Health, Charite - Universitatsmedizin Berlin
# Author       : Johannes Julius Mohn
# Contact      : johannes.j.mohn@maxplanckschools.de
# Date Created : 2026-02-18
# R Version    : R version 4.4.1 (2024-06-14)
# ==============================================================================
# History      : 2025-02-18: Script created, mohnjj
#                2026-09-10: Clean-up for public repository
#
# Description:
# Testing associations of clinically verified childhood maltreatment with brain 
# centiles, intellectual ability, and externalizing/internalizing behaviors.
#
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

# --- packages ---
library(lme4)
library(lmerTest)
library(broom.mixed)

# --- directories ---
path2results_associations <- file.path(path2results, "global_results")
dir.create(path2results_associations, recursive = TRUE)

path2tables_analyses <- file.path(path2tables, "analyses")
dir.create(path2tables_analyses, recursive = TRUE)

path2results_analyses_raw <- file.path(path2results, "analyses")
dir.create(path2results_analyses_raw, recursive = TRUE)

# --- data files ---
file_demographics     <- "k2h_demographics_sp-1_t-0_2026-05-19.csv"
file_maltreatment     <- "k2h_maltreatment_sp-1_t-0_2026-05-19.csv"
file_braincharts_gmv  <- "k2h_braincharts-global_cgm_bethlehem_2025-02-18.csv"
file_braincharts_wmv  <- "k2h_braincharts-global_wm_bethlehem_2025-02-18.csv"
file_braincharts_sgmv <- "k2h_braincharts-global_sgm_bethlehem_2025-02-18.csv"
file_iq               <- "k2h_iq_scores_sp-1_t-0_2026-05-19.csv"
file_sdq              <- "k2h_sdq_scores_sp-1_t-0_2026-05-19.csv"


# Functions --------------------------------------------------------------------

source(here("R/fit_lmm.R"))
source(here("R/extract_lmm_results.R"))
source(here("R/format_lmm_table.R"))
source(here("R/convert_lmm_table_to_word.R"))


# Data -------------------------------------------------------------------------

# --- demographic information ---
demos <- read.csv(file.path(path2data_demographics, file_demographics)) %>%
  select(id, family_id, age_at_mri, sex, sex_factor, ses_composite) %>%
  rename(age = age_at_mri) %>%
  mutate(
    age_centred = age - mean(age, na.rm = TRUE),
    ses_z = scale(ses_composite)[,1]
  )

# --- maltreatment information ---
cm <- read.csv(file.path(path2data_maltreatment, file_maltreatment)) %>%
  mutate(cm_group = factor(cm_group, levels = c("CM-", "CM+"), labels = c("CM-", "CM+")))

# --- brain chart centile scores ---
# cortical gray matter
bc_cgm  <- read.csv(file.path(path2data_braincharts, file_braincharts_gmv)) %>%
  rename(cgm_c = centile) %>% select(id, cgm_c)

# total white matter
bc_wm   <- read.csv(file.path(path2data_braincharts, file_braincharts_wmv)) %>%
  rename(wm_c = centile) %>% select(id, wm_c)

# subcortical gray matter
bc_sgm  <- read.csv(file.path(path2data_braincharts, file_braincharts_sgmv)) %>%
  rename(sgm_c = centile) %>% select(id, sgm_c)

# --- intelligence scores ---
iq <- read.csv(file.path(path2data_cognition, file_iq))

# --- SDQ scores ---
sdq <- read.csv(file.path(path2data_clinical, file_sdq)) %>%
  select(-sdq_total)

# --- combined data frame ---
df <- reduce(list(demos, cm, iq, sdq, bc_cgm, bc_wm, bc_sgm), inner_join, by = "id")


# Model parameters -------------------------------------------------------------

# --- analysis parameters ---
ADJUSTMENT_METHOD <- "fdr"

# --- model parameters ---
GROUP_VAR     <- "cm_group"
SES_VAR       <- "ses_z"
RANDOM_EFFECT <- "family_id"

COVARIATES <- c(
  "age_centred",
  "sex_factor"
)


# Associations of brain centiles with maximum maltreatment severity ------------

# --- model specification ---
outcomes <- c(cGM = "cgm_c", WM  = "wm_c", sGM = "sgm_c")

predictor <- "micm_total_max_severity"

analysis_df <- df %>%
  filter(
    cm_group == "CM+",
    micm_total_max_severity >= 2
  )


# --- run analyses ---
models <- map(
  outcomes,
  ~ fit_lmm(
    outcome = .x,
    predictor = predictor,
    covariates = COVARIATES,
    random_effect = RANDOM_EFFECT,
    data = analysis_df
  )
)


# --- extract model results ---

raw_results <- extract_lmm_results(
  models = models,
  effect = "micm_total_max_severity",
  p_adjust = ADJUSTMENT_METHOD
)

table_results <- format_lmm_table(
  results = results,
  significance_stars = TRUE,
  include_sample_size = FALSE
)


# --- export the raw results table ---
readr::write_csv(
  raw_results,
  file.path(path2results_analyses_raw, "results_severity_and_brain_centiles.csv")
)


# --- export the formatted results ---

out_path <- file.path(path2tables_analyses, paste0("tableSx_results_severity_and_centiles_", Sys.Date(), ".docx"))

convert_lmm_table_to_word(
  table_results = table_results,
  out_path = out_path,
  table_number = "Sx",
  orientation = "landscape",
  table_title = "Association between childhood maltreatment severity and brain centiles.",
  note = paste0(
    "Linear mixed-effects models included age and sex as covariates ",
    "and a random intercept for family. ",
    "*p<.05, **p<.01, ***p<.001."
  )
)
