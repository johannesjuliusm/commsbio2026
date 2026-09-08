# ==============================================================================
# Project      : Kids2Health, Charite - Universitatsmedizin Berlin
# Author       : Johannes Julius Mohn
# Contact      : johannes.j.mohn@maxplanckschools.de
# Date Created : 2026-07-15
# R Version    : R version 4.4.1 (2024-06-14)
# ==============================================================================
# History      : 2026-07-15: Script created, mohnjj
#                2026-09-08: Clean-up for public repository
#
# Description:
# Correlation matrix of key variables.
#
# Notes:
# Note that calculate_ppcorr_matrix() is a cutstom function that calculates
# partial correlations over a provided data frame, but can be set to canonical
# bivariate Pearson correlations by setting covariates = NULL.
#
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

# --- packages ---
library(corrplot)

# --- directories ---
path2figures_out  <- file.path(path2figures, "correlation_matrix")
dir.create(path2figures_out, recursive = TRUE, showWarnings = FALSE)

# --- data files ---
file_demographics     <- "k2h_demographics_sp-1_t-0_2026-05-19.csv"
file_iq               <- "k2h_iq_scores_sp-1_t-0_2026-05-19.csv"
file_sdq              <- "k2h_sdq_scores_sp-1_t-0_2026-05-19.csv"
file_braincharts_gmv  <- "k2h_braincharts-global_cgm_bethlehem_2025-02-18.csv"
file_braincharts_wmv  <- "k2h_braincharts-global_wm_bethlehem_2025-02-18.csv"
file_braincharts_sgmv <- "k2h_braincharts-global_sgm_bethlehem_2025-02-18.csv"


# Functions --------------------------------------------------------------------

source(here("R", "calculate_ppcorr_matrix.R"))
source(here("R", "plot_correlation_matrix.R"))


# Data -------------------------------------------------------------------------

# --- intelligence data ---
iq_scores <- read.csv(file.path(path2data_cognition, file_iq))

# --- clinical data ---
sdq_scores <- read.csv(file.path(path2data_clinical, file_sdq)) %>%
  select(id, sdq_extern, sdq_intern)

# --- brain chart centile scores ---
# cortical gray matter
bc_cgm  <- read.csv(file.path(path2data_braincharts, file_braincharts_gmv)) %>%
  rename(cGM = centile) %>% select(id, cGM)

# total white matter
bc_wm   <- read.csv(file.path(path2data_braincharts, file_braincharts_wmv)) %>%
  rename(WM = centile) %>% select(id, WM)

# subcortical gray matter
bc_sgm  <- read.csv(file.path(path2data_braincharts, file_braincharts_sgmv)) %>%
  rename(sGM = centile) %>% select(id, sGM)

# --- combined data frame ---
df <- reduce(list(iq_scores, sdq_scores, bc_cgm, bc_wm, bc_sgm), full_join, by = "id") %>%
  filter(id %in% iq_scores$id) %>%
  select(-id)


# Correlation matrix -----------------------------------------------------------

pal <- make_charite_palette(c(
  charite_colors$ROT, charite_colors$KORALL, "#f7f7f7",
  charite_colors$SECOND_LBLUE, charite_colors$SECOND_DBLUE))(10)

names(df) <- c("Nonverbal IQ", "Verbal IQ", "Externalizing", "Internalizing", "cGM Centile", "WM Centile", "sGM Centile")

corr_results <- calculate_ppcorr_matrix(
  df = df,
  covariates = NULL,
  pcor.method = "pearson",
  plot = TRUE
)


# Export -----------------------------------------------------------------------

setwd(path2figures_out)

# PDF vector format
pdf(
  file = "correlation_matrix.pdf",
  width = 4.5,
  height = 4.5
)

plot_correlation_matrix(
  coeffs  = corr_results$correlations,
  pvals   = corr_results$pvalues,
  pch.cex = 2,
  pch.col = "white",
  insig   = "n"
)

dev.off()


# PNG high resolution
png(
  filename = "correlation_matrix.png",
  width = 4,
  height = 4,
  units = "in",
  res = 600
)

plot_correlation_matrix(
  coeffs  = corr_results$correlations,
  pvals   = corr_results$pvalues,
  pch.cex = 2,
  pch.col = "white",
  insig   = "n"
)

dev.off()
