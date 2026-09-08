# ==============================================================================
# Project      : Kids2Health, Charite - Universitatsmedizin Berlin
# Author       : Johannes Julius Mohn
# Contact      : johannes.j.mohn@maxplanckschools.de
# Date Created : 2025-02-18
# R Version    : R version 4.4.1 (2024-06-14)
# ==============================================================================
# History      : 2025-02-18: Script created, mohnjj
#                2026-07-05: Clean-up for repository sharing
#                2026-09-08: Histograms of raw volumes with export
#
# Description:
# This script prepares freesurfer output for upload to the BrainChart tool.
#
# Notes:
# (1) Brainchart tool: https://brainchart.shinyapps.io/brainchart/
# (2) Total cortical and subcortical gray matter volumes are available directly
#     in the freesurfer output, total white matter volume must be calculated
#     from other variables.
# (3) For freesrufer variables documentation, see:
#     https://surfer.nmr.mgh.harvard.edu/fswiki/MorphometryStats
# (4) "total cortical gray matter volume" is freesurfer "cortexvol"
#     "total subcortical gray matter volume" is freesurfer "subcortgrayvol"
# (5) "total cerebral white matter volume" calculation:
#         "supratentorialvolnotvent" (i.e. everything except for brainstem,
#             cerebellum gray + white matter, and all ventricles)
#       MINUS
#         "totalgrayvol" (i.e. gray volumes of left and right cortex
#             + subcortical gray matter volume + cerebellum gray matter volume)
#       PLUS
#         "left_cerebellum_cortex" + "right_cerebellum_cortex"
#     Because cerebellar GM is not part of supratentorial, but subtracted with
#     totalgray, need to add it back or else too much is subtracted
#
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

# --- packages ---
library(psych)
library(ggplot2)

# --- directories ---
path2data_mri           <- file.path(path2data, "mri", "segmentations")
path2data_out           <- file.path(path2data, "mri", "braincharts", "braincharts-global_bethlehem", "00_model_input")
path2figures_out        <- file.path(path2figures, "mri")

dir.create(path2data_out,     recursive = TRUE, showWarnings = FALSE)
dir.create(path2figures_out,  recursive = TRUE, showWarnings = FALSE)

# --- data files ---
file_demographics <- "k2h_demographics_sp-1_t-0_2026-05-19.csv"
file_maltreatment <- "k2h_maltreatment_sp-1_t-0_2026-05-19.csv"
file_mri          <- "k2h_abcd-hcp_fs53_destrieux_and_aseg_sp-1_t-0_2026-05-19.csv"


# Data -------------------------------------------------------------------------

# freesurfer output
fs_output <- read.csv(file.path(path2data_mri, file_mri))

# subset and format
names(fs_output) <- tolower(names(fs_output))

# --- demographic information ---
demos <- read.csv(file.path(path2data_demographics, file_demographics)) %>%
  rename(age = age_at_mri) %>%
  select(id, sex_factor, age)

# --- maltreatment information ---
cm <- read.csv(file.path(path2data_maltreatment, file_maltreatment)) %>%
  rename(dx = cm_group) %>%
  select(id, dx)

demos <- merge(demos, cm, by = "id")


# White matter calculation -----------------------------------------------------

fs <- fs_output %>%
  mutate(
    # see notes above for explanation of the formula
    whitevol = supratentorialvolnotvent - totalgrayvol + left_cerebellum_cortex + right_cerebellum_cortex,
    ventricles = left_lateral_ventricle + right_lateral_ventricle + left_inf_lat_vent + right_inf_lat_vent + x3rd_ventricle + x4th_ventricle + x5th_ventricle
  ) %>%
  select(
    id,
    cortexvol,
    whitevol,
    subcortgrayvol
  )


# Generate data file for braincharts.io ----------------------------------------

brainchartio <- merge(demos, fs, by = "id", all = TRUE)

brainchartio <- brainchartio %>%
  rename(
    participant = id,
    age_years = age,
    sex = sex_factor,
    GMV = cortexvol,
    WMV = whitevol,
    sGMV = subcortgrayvol
  ) %>%
  # create additional required variables
  mutate(
    age_days = (age_years * 365.245) + 280,
    study = "K2H",
    fs_version = "FS53",
    country = "Germany",
    run = 1,
    session = 1
  ) %>%
  select(
    participant,
    study,
    country,
    session,
    run,
    fs_version,
    age_years,
    age_days,
    sex,
    dx,
    GMV,
    WMV,
    sGMV
  ) %>%
  mutate(
    dx = ifelse(dx == "CM-", "CN", "Index")
  )


# Investigate extreme values ---------------------------------------------------

# histograms of the raw brain volumes
histogram_cgmv <- ggplot(brainchartio, aes(x = GMV)) +
  geom_histogram(fill = "black", color = "white", linewidth = 0.25) +
  scale_x_continuous(labels = function(x) x / 1000, expand = expansion(mult = c(0.05, 0.075))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Cortical Grey Matter Volume", x = expression("Volume (cm"^3*")"), y = "Count") +
  charite::theme_sci(font = "Helvetica", font_size = 8)

histogram_wmv <- ggplot(brainchartio, aes(x = WMV)) +
  geom_histogram(fill = "black", color = "white", linewidth = 0.25) +
  scale_x_continuous(labels = function(x) x / 1000, expand = expansion(mult = c(0.05, 0.075))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "White Matter Volume", x = expression("Volume (cm"^3*")"), y = "Count") +
  charite::theme_sci(font = "Helvetica", font_size = 8)

histogram_sgmv <- ggplot(brainchartio, aes(x = sGMV)) +
  geom_histogram(fill = "black", color = "white", linewidth = 0.25) +
  scale_x_continuous(labels = function(x) x / 1000, expand = expansion(mult = c(0.05, 0.075))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Subcortical Grey Matter Volume", x = expression("Volume (cm"^3*")"), y = "Count") +
  charite::theme_sci(font = "Helvetica", font_size = 8)

# save out the histograms
charite::nice_save(file.path(path2figures_out, "histogram_raw_cgm_volume.png"), histogram_cgmv, layout = "full col", bg = "white")
charite::nice_save(file.path(path2figures_out, "histogram_raw_wm_volume.png"), histogram_wmv, layout = "full col", bg = "white")
charite::nice_save(file.path(path2figures_out, "histogram_raw_sgm_volume.png"), histogram_sgmv, layout = "full col", bg = "white")

# get the extreme cases for each global brain metric
large_gmv <- brainchartio %>%
  filter(GMV > 750000) %>%
  pull(participant)
small_gmv <- brainchartio %>%
  filter(GMV < 450000) %>%
  pull(participant)

large_wmv <- brainchartio %>%
  filter(WMV > 520000) %>%
  pull(participant)
small_wmv <- brainchartio %>%
  filter(WMV < 250000) %>%
  pull(participant)

large_sgmv <- brainchartio %>%
  filter(sGMV > 80000) %>%
  pull(participant)
small_sgmv <- brainchartio %>%
  filter(sGMV < 50000) %>%
  pull(participant)

# ids with extreme brains
extreme_deviants <- c(large_gmv, small_gmv, large_wmv, small_wmv, large_sgmv, small_sgmv)
extreme_deviants <- sort(unique(extreme_deviants))

# extract relevant covariates from the data
check <- brainchartio %>%
  filter(participant %in% extreme_deviants)

# export the extreme outliers for inspection
file_name_to_write <- paste0("extreme_brains_", Sys.Date(), ".csv")
write.table(check, file.path(path2data_out, file_name_to_write),
            sep = ",", dec = ".", row.names = FALSE, col.names = TRUE)


# Export -----------------------------------------------------------------------

file_name_to_write <- paste0("k2h_aseg_to_brainchartio_sp-1_t-0_", Sys.Date(), ".csv")
write.table(brainchartio, file.path(path2data_out, file_name_to_write),
            sep = ",", dec = ".", row.names = FALSE, col.names = TRUE)
