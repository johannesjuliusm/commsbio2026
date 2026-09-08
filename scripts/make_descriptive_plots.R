# ==============================================================================
# Project      : Kids2Health, Charite - Universitatsmedizin Berlin
# Author       : Johannes Julius Mohn
# Contact      : johannes.j.mohn@maxplanckschools.de
# Date Created : 2026-06-07
# R Version    : R version 4.4.1 (2024-06-14)
# ==============================================================================
# History      : 2026-06-07: Script created from previous .Rmd file, mohnjj
#                2026-09-08: Clean-up for public repository
#
# Description:
# Descriptive plots of variables and maltreatment group comparisons.
#
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

# --- packages ---
library(ggplot2)
library(dplyr)
library(scales)

# --- directories ---
path2figures_covariates  <- file.path(path2outputs, "figures", "covariates")
dir.create(path2figures_covariates, recursive = TRUE)

# --- data files ---
file_demographics     <- "k2h_demographics_sp-1_t-0_2026-05-19.csv"
file_developmental    <- "k2h_developmental_sp-1_t-0_2026-05-19.csv"
file_maltreatment     <- "k2h_maltreatment_sp-1_t-0_2026-05-19.csv"
file_iq               <- "k2h_iq_scores_sp-1_t-0_2026-05-19.csv"
file_sdq              <- "k2h_sdq_scores_sp-1_t-0_2026-05-19.csv"


# Parameters -------------------------------------------------------------------

# list for storing descriptive plots
descriptive_plots = list()
descriptive_plots_shorter = list()

# colour palette
colour_palette <- c(
  "CM-" = charite::charite_colors$PRIME_DGREY,
  "CM+" = charite::charite_colors$KORALL
)

# plot size
custom_size = c(2.4, 2.4)


# Functions --------------------------------------------------------------------

source(here("R/plot_density_jitter_box.R"))


# Data -------------------------------------------------------------------------

# --- demographic information ---
demos <- read.csv(file.path(path2data_demographics, file_demographics)) %>%
  rename(age = age_at_mri)

dev <- read.csv(file.path(path2data_demographics, file_developmental))

# --- maltreatment information ---
cm <- read.csv(file.path(path2data_maltreatment, file_maltreatment)) %>%
  mutate(cm_group = factor(cm_group, levels = c("CM-", "CM+"), labels = c("CM-", "CM+")))

# --- scores ---
iq <- read.csv(file.path(path2data_cognition, file_iq))
sdq <- read.csv(file.path(path2data_clinical, file_sdq))

# --- combined data frame ---
df <- reduce(list(demos, cm, dev, iq, sdq), inner_join, by = "id")


# Figures ----------------------------------------------------------------------

descriptive_plots[["age"]] <- plot_density_jitter_box(
  df, x = "age", group = "cm_group",
  title = "Age",
  xlab = "Age (years)",
  palette = colour_palette
)

descriptive_plots[["ses_composite"]] <- plot_density_jitter_box(
  df, x = "ses_composite", group = "cm_group",
  title = "SES",
  xlab = "SES",
  palette = colour_palette
)

descriptive_plots[["height"]] <- plot_density_jitter_box(
  df, x = "height", group = "cm_group",
  title = "Height",
  xlab = "Height (cm)",
  palette = colour_palette
)

descriptive_plots[["height_percentile"]] <- plot_density_jitter_box(
  df, x = "height_percentile", group = "cm_group",
  title = "Height Percentile",
  xlab = "Height (percentile)",
  palette = colour_palette
)

descriptive_plots[["bmi"]] <- plot_density_jitter_box(
  df, x = "bmi", group = "cm_group",
  title = "BMI",
  xlab = "BMI",
  palette = colour_palette
)

descriptive_plots[["bmi_percentile"]] <- plot_density_jitter_box(
  df, x = "bmi_percentile", group = "cm_group",
  title = "BMI (Percentile)",
  xlab = "BMI (percentile)",
  palette = colour_palette
)

descriptive_plots[["headcirc"]] <- plot_density_jitter_box(
  df, x = "headcirc", group = "cm_group",
  title = "Head Circumference",
  xlab = "Head circumference (cm)",
  palette = colour_palette
)

descriptive_plots[["headcirc_percentile"]] <- plot_density_jitter_box(
  df, x = "headcirc_percentile", group = "cm_group",
  title = "Head Circumference",
  xlab = "Head circumference (percentile)",
  palette = colour_palette
)

descriptive_plots[["sonr"]] <- plot_density_jitter_box(
  df, x = "sonr", group = "cm_group",
  title = "Non-verbal IQ",
  xlab = "SON-R IQ score",
  palette = colour_palette
)

descriptive_plots[["wisc"]] <- plot_density_jitter_box(
  df, x = "wisc", group = "cm_group",
  title = "Verbal IQ",
  xlab = "WISC verbal score",
  palette = colour_palette
)

descriptive_plots[["sdq_intern"]] <- plot_density_jitter_box(
  df, x = "sdq_intern", group = "cm_group",
  title = "Internalizing",
  xlab = "SDQ internalizing score",
  palette = colour_palette
)

descriptive_plots[["sdq_extern"]] <- plot_density_jitter_box(
  df, x = "sdq_extern", group = "cm_group",
  title = "Externalizing",
  xlab = "SDQ externalizing score",
  palette = colour_palette
)


# Export -----------------------------------------------------------------------

nice_save("descriptive_age.pdf", descriptive_plots[["age"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_ses.pdf", descriptive_plots[["ses_composite"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_height.pdf", descriptive_plots[["height"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_height_percentile.pdf", descriptive_plots[["height_percentile"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_bmi.pdf", descriptive_plots[["bmi"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_bmi_percentile.pdf", descriptive_plots[["bmi_percentile"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_headcirc.pdf", descriptive_plots[["headcirc"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_headcirc_percentile.pdf", descriptive_plots[["headcirc_percentile"]], custom = custom_size, path = path2figures_covariates)

nice_save("descriptive_age.png", descriptive_plots[["age"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_ses.png", descriptive_plots[["ses_composite"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_height.png", descriptive_plots[["height"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_height_percentile.png", descriptive_plots[["height_percentile"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_bmi.png", descriptive_plots[["bmi"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_bmi_percentile.png", descriptive_plots[["bmi_percentile"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_headcirc.png", descriptive_plots[["headcirc"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_headcirc_percentile.png", descriptive_plots[["headcirc_percentile"]], custom = custom_size, path = path2figures_covariates)

nice_save("descriptive_sonr.pdf", descriptive_plots[["sonr"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_wisc.pdf", descriptive_plots[["wisc"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_sdq_intern.pdf", descriptive_plots[["sdq_intern"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_sdq_extern.pdf", descriptive_plots[["sdq_extern"]], custom = custom_size, path = path2figures_covariates)

nice_save("descriptive_sonr.png", descriptive_plots[["sonr"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_wisc.png", descriptive_plots[["wisc"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_sdq_intern.png", descriptive_plots[["sdq_intern"]], custom = custom_size, path = path2figures_covariates)
nice_save("descriptive_sdq_extern.png", descriptive_plots[["sdq_extern"]], custom = custom_size, path = path2figures_covariates)
