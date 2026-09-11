# ==============================================================================
# Project      : Kids2Health, Charite - Universitatsmedizin Berlin
# Author       : Johannes Julius Mohn
# Contact      : johannes.j.mohn@maxplanckschools.de
# Date Created : 2026-02-18
# R Version    : R version 4.4.1 (2024-06-14)
# ==============================================================================
# History      : 2025-02-18: Script created, mohnjj
#                2026-07-08: Flexibility for switching between lm() and lmer()
#                2026-09-08: Clean-up for public repository
#
# Description:
# Group comparisons of differences in brain centiles between children
# with and without documented and clinically verified maltreatment exposure and
# with population norms.
#
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

# --- packages ---
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)
library(gghalves)
library(patchwork)
library(rlang)
library(performance)
library(Hmisc)

# --- directories ---
path2figures  <- file.path(path2outputs, "figures", "global_centiles")
dir.create(path2figures, recursive = TRUE)

path2results_global   <- file.path(path2results, "global_results")
dir.create(path2results_global, recursive = TRUE)

# --- data files ---
file_demographics     <- "k2h_demographics_sp-1_t-0_2026-05-19.csv"
file_maltreatment     <- "k2h_maltreatment_sp-1_t-0_2026-05-19.csv"
file_braincharts_gmv  <- "k2h_braincharts-global_cgm_bethlehem_2025-02-18.csv"
file_braincharts_wmv  <- "k2h_braincharts-global_wm_bethlehem_2025-02-18.csv"
file_braincharts_sgmv <- "k2h_braincharts-global_sgm_bethlehem_2025-02-18.csv"
file_brainvolumes     <- "k2h_aseg_to_brainchartio_sp-1_t-0_2025-02-18.csv"

  
# Parameters -------------------------------------------------------------------

# analysis parameters
ADJUSTMENT_METHOD <- "fdr"
ADJUST_FOR_SES <- FALSE
POP_REF <- 50

# plot parameters
HIDE_NS <- FALSE
SHOW_VIOLIN <- TRUE
SHOW_BOXPLOT <- TRUE
SHOW_MARGINAL_MEANS <- TRUE
YSTACK <- c(108, 118, 128, 138, 148, 158)

# model parameters
MODEL_TYPE  <- "lmer" # "lm" or "lmer"
COVARIATES  <- c("age", "sex_factor")
GROUP_VAR   <- "cm_group"
SES_VAR     <- "ses_z"
RANDOM_INTERCEPT <- "family_id"


# Functions --------------------------------------------------------------------

source(here("R/run_popmean_tests.R"))
source(here("R/run_global_centile_model.R"))
source(here("R/get_pairwise.R"))
source(here("R/geom_flat_violin.R"))


# Data -------------------------------------------------------------------------

# --- demographic information ---
demos <- read.csv(file.path(path2data_demographics, file_demographics)) %>%
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

# --- raw brain volumes ---
raw <- read.csv(file.path(path2data_mri, "braincharts", "braincharts-global_bethlehem", "00_model_input", file_brainvolumes)) %>%
  rename(id = participant, cGMV = GMV) %>%
  select(id, cGMV, WMV, sGMV) %>%
  mutate(across(where(is.numeric), ~ .x / 10000))

# --- combined data frame ---
df <- reduce(list(demos, cm, bc_cgm, bc_wm, bc_sgm, raw), inner_join, by = "id")

# data in long format
df_long <- df %>%
  pivot_longer(
    cols = c(cgm_c, wm_c, sgm_c),
    names_to = "tissue",
    values_to = "centile"
  ) %>%
  mutate(
    tissue = factor(
      tissue,
      levels = tissues_order,
      labels = tissues_labels
    ),
    centile = centile * 100
  )

# determine the number of groups
N_GROUPS <- length(unique(df$cm_group))


# Maltreatment status comparisons against population mean ----------------------

# --- comparisons against population mean ---
# tests against population mean without adjustment for covariates
popmean_tests_unadjusted <- purrr::map_dfr(
  tissues_labels,
  ~ run_popmean_tests(
    data = df_long,
    tissue_name = .x,
    group_var = GROUP_VAR,
    covariates = NULL,
    pop_mean = POP_REF,
    adjust_method = ADJUSTMENT_METHOD
  )
)

# test against population mean with same adjustment as in group comparisons
popmean_tests_adjusted <- purrr::map_dfr(
  tissues_labels,
  ~ run_popmean_tests(
    data = df_long,
    tissue_name = .x,
    group_var = GROUP_VAR,
    covariates = COVARIATES,
    pop_mean = POP_REF,
    adjust_method = ADJUSTMENT_METHOD
  )
)

# run global adjustment for multiple comparisons across all p-values, else it is
# only performed within group x tissue
if (ADJUSTMENT_METHOD != "none") {
  popmean_tests_adjusted <- popmean_tests_adjusted %>%
    mutate(
      p.adj = p.adjust(p.value, method = ADJUSTMENT_METHOD),
      p.adj.signif = case_when(
        p.adj < 0.001 ~ "***",
        p.adj < 0.01  ~ "**",
        p.adj < 0.05  ~ "*",
        TRUE ~ "ns"
      )
    )
}

# combined test results against population means
popmean_tests_all <- bind_rows(
  popmean_tests_unadjusted,
  popmean_tests_adjusted
) %>%
  mutate(
    across(
      where(is.numeric) & !c(p.value, p.adj),
      ~ round(.x, 2)
    ),
    across(
      c(p.value, p.adj),
      ~ round(.x, 3)
    )
  )

# --- export results tables ---
write_csv(popmean_tests_all, file.path(path2results_global, "results_cm_group_vs_population_mean.csv"))


# Maltreatment status group comparisons ----------------------------------------

# --- group comparisons ---
# run linear mixed effects models for each tissue to test maltreatment status
# group differences
cm_status_results <- set_names(tissues_labels) %>%
  map(
    ~ run_global_centile_model(
      data = df_long,
      tissue_name = .x,
      group_var = cm_group,
      covariates = COVARIATES,
      ses_covariate = SES_VAR,
      random_effect = RANDOM_INTERCEPT,
      model_type = MODEL_TYPE,
      adjust_for_ses = ADJUST_FOR_SES,
      adjustment_method = ADJUSTMENT_METHOD
    )
  )

# estimated marginal means
cm_status_emm <- map_dfr(cm_status_results, "emm") %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# omnibus tests for parameters
cm_status_parameters <- map_dfr(
  cm_status_results,
  ~ as.data.frame(.x$joint_tests),
  .id = "tissue"
) %>%
  rename(
    "model.term" = "model term"
  )

adj <- cm_status_parameters %>%
  filter(model.term == "cm_group") %>%
  mutate(
    p.adj = p.adjust(p.value, method = ADJUSTMENT_METHOD)
  ) %>%
  select(tissue, model.term, p.adj)

cm_status_parameters <- cm_status_parameters %>%
  left_join(adj, by = c("tissue", "model.term")) %>%
  mutate(
    p.adj.signif = case_when(
      is.na(p.adj)  ~ NA_character_,
      p.adj < 0.001 ~ "***",
      p.adj < 0.01  ~ "**",
      p.adj < 0.05  ~ "*",
      TRUE          ~ "ns"
    )
  ) %>%
  mutate(
    across(
      where(is.numeric) & !c(p.value, p.adj),
      ~ round(.x, 2)
    ),
    across(
      c(p.value, p.adj),
      ~ round(.x, 3)
    )
  )

# all pairwise comparisons
cm_status_pairwise <- purrr::map_dfr(
  names(cm_status_results),
  ~ get_pairwise(
    model = cm_status_results[[.x]]$model,
    tissue_label = .x,
    group_var = "cm_group",
    adjust = ADJUSTMENT_METHOD,
    ystack = YSTACK,
    n_groups = N_GROUPS
  )
)


# --- export results tables ---
write_csv(cm_status_emm, file.path(path2results_global, "results_cm_group_estimated_marginal_means.csv"))
write_csv(cm_status_parameters, file.path(path2results_global, "results_cm_group_joint_omnibus_tests.csv"))
write_csv(cm_status_pairwise, file.path(path2results_global, "results_cm_group_pairwise_comparisons.csv"))


# Maltreatment status results figure -------------------------------------------

fig1_list <- map(tissues_labels, \(tt) {
  
  df_tt <- df_long %>% filter(tissue == tt)
  
  ann_tt <- cm_status_pairwise %>%
    filter(tissue == tt)
  
  if (HIDE_NS) {
    ann_tt <- ann_tt %>%
      filter(p.adj.signif != "ns")
  }

  ann_tt <- ann_tt %>%
    arrange(group1, group2) %>%
    mutate(
      y.position = rep(YSTACK, length.out = n())
    )
  
  pop_tt <- popmean_tests_adjusted %>%
    filter(tissue == tt) %>% 
    mutate(
      y.position = YSTACK[N_GROUPS] - 6,
      label = p.adj.signif
    )
  
  if (HIDE_NS) {
    pop_tt <- pop_tt %>%
      filter(p.adj.signif != "ns")
  }
  
  ymax <- YSTACK[N_GROUPS] - 4
  
  if (SHOW_VIOLIN) {
    p <- ggplot(df_tt, aes(x = cm_group, y = centile, color = cm_group, fill = cm_group)) +
      geom_flat_violin(
        aes(group = cm_group, color = cm_group, fill = cm_group),
        position = position_nudge(x = 0, y = 0),
        adjust = 1,
        color = "white",
        alpha = 0.7,
        width = 0.65
      ) +
      ggbeeswarm::geom_beeswarm(
        aes(x = as.numeric(cm_group) - 0.05),
        side = -1,
        cex = 1.5,
        size = 2,
        alpha = 0.7,
        stroke = NA,
        shape = 19
      )
  } else {
    p <- ggplot(df_tt, aes(x = cm_group, y = centile, color = cm_group, fill = cm_group)) +
      ggbeeswarm::geom_quasirandom(
        width = 0.2,
        size = 2,
        alpha = 0.7,
        stroke = NA
      )
  }
  
  if (SHOW_BOXPLOT) {
    p <- p +
      geom_boxplot(
        outlier.shape = NA,
        fill = "white",
        color = "black",
        alpha = 0,
        lwd = 1.5,
        width = 0.2
      )
  }
  
  if (SHOW_MARGINAL_MEANS == TRUE & SHOW_VIOLIN == FALSE) {
    p <- p +
      geom_point(
        data = cm_status_emm %>% filter(tissue == tt), aes(y = emmean),
        pch = 18,
        size = 3.5,
        color = "black"
      ) +
      geom_errorbar(
        data = cm_status_emm %>% filter(tissue == tt),
        aes(x = cm_group, ymin = lower.CL, ymax = upper.CL),
        width = 0.12,
        size = 0.75,
        color = "black",
        inherit.aes = FALSE
      )
  }
  
  if (SHOW_MARGINAL_MEANS == TRUE & SHOW_VIOLIN == TRUE) {
    p <- p +
      geom_point(
        data = cm_status_emm %>% filter(tissue == tt), aes(y = emmean),
        pch = 18,
        size = 5,
        color = "black"
      )
  }
  
  p <- p +
    geom_hline(yintercept = 50, lwd = 1, linetype = "dotted") +
    scale_color_manual(values = cm_colors, drop = FALSE) +
    scale_fill_manual(values = cm_colors, drop = FALSE) +
    scale_y_continuous(
      limits = c(0, ymax),
      breaks = c(2.5, 25, 50, 75, 97.5),
      labels = c("2.5th", "25th", "50th", "75th", "97.5th")
    ) +
    charite::theme_sci(font = "Helvetica", font_size = 10, aspect_ratio = 4/5, tiny_margins = TRUE) +
    theme(
      axis.ticks.x = element_blank(),
      plot.title = element_text(face = "bold", size = 18, margin = margin(b = + 10))
    ) +
    guides(fill = "none", color = "none") +
    labs(title = tt, x = NULL, y = NULL)
  
  if (nrow(ann_tt) > 0) {
    p <- p +
      stat_pvalue_manual(
        ann_tt,
        label = "p.adj.signif",
        y.position = "y.position",
        xmin = "group1",
        xmax = "group2",
        tip.length = 0.01,
        inherit.aes = FALSE,
        size = 5
      )
  }
  
  if (nrow(pop_tt) > 0) {
    p <- p +
      geom_text(
        data = pop_tt,
        aes(
          x = cm_group,
          y = y.position,
          label = label
        ),
        inherit.aes = FALSE,
        #fontface = "bold",
        size = 5,
        vjust = 0
      )
  }
  
  p
})

names(fig1_list) <- tissues_labels
fig1_list


# --- figure assembly ---
fig1a <- fig1_list[["cGM"]] + theme(axis.ticks.y = element_blank())
fig1b <- fig1_list[["WM"]] + theme(axis.ticks.y = element_blank())
fig1c <- fig1_list[["sGM"]] + theme(axis.ticks.y = element_blank())

figure1 <- fig1a + fig1b + fig1c +
  plot_layout(ncol = 3, widths = c(1, 1, 1))


# --- figure export ---
ggsave(
  filename = "figure1_cm_groups.pdf",
  plot = figure1,
  path = path2figures,
  device = grDevices::pdf,
  width = 14.4, height = 3.6, units = "in",
  useDingbats = FALSE,
  bg = "white"
)


# Maltreatment severity comparisons against population mean --------------------

# .. note:: Severity analyses are limited to the sample group with substantiated
#           maltreatment exposure to separate severity-related effects from
#           global group differences between CM+ and controls.

# --- data for analysis ---
df_sev_long <- df_long %>%
  filter(cm_group == "CM+") %>%
  filter(micm_total_max_severity != 1) %>%
  mutate(
    sev_group = factor(
      case_when(
        micm_total_max_severity == 2 ~ "2",
        micm_total_max_severity == 3 ~ "3",
        micm_total_max_severity == 4 ~ "4",
        micm_total_max_severity == 5 ~ "5",
        TRUE ~ NA
      ),
    levels = c("2", "3", "4", "5")),
    sev_group_num = as.numeric(sev_group)
  )

# N per group
df_sev_long %>% filter(tissue == "cGM") %>% count(sev_group)


# --- comparisons against population mean ---
# tests against population mean without adjustment for covariates
severity_popmean_tests_unadjusted <- purrr::map_dfr(
  tissues_labels,
  ~ run_popmean_tests(
    data = df_sev_long,
    tissue_name = .x,
    group_var = "sev_group",
    covariates = NULL,
    pop_mean = POP_REF,
    adjust_method = ADJUSTMENT_METHOD
  )
)

# test against population mean with same adjustment as in group comparisons
severity_popmean_tests_adjusted <- purrr::map_dfr(
  tissues_labels,
  ~ run_popmean_tests(
    data = df_sev_long,
    tissue_name = .x,
    group_var = "sev_group",
    covariates = COVARIATES,
    pop_mean = POP_REF,
    adjust_method = ADJUSTMENT_METHOD
  )
)

# run global adjustment for multiple comparisons across all p-values, else it is
# only performed within group x tissue
if (ADJUSTMENT_METHOD != "none") {
  severity_popmean_tests_adjusted <- severity_popmean_tests_adjusted %>%
    mutate(
      p.adj = p.adjust(p.value, method = ADJUSTMENT_METHOD),
      p.adj.signif = case_when(
        p.adj < 0.001 ~ "***",
        p.adj < 0.01  ~ "**",
        p.adj < 0.05  ~ "*",
        TRUE ~ "ns"
      )
    )
}

# combined test results against population means
severity_popmean_tests_all <- bind_rows(
  severity_popmean_tests_unadjusted,
  severity_popmean_tests_adjusted
) %>%
  mutate(
    across(
      where(is.numeric) & !c(p.value, p.adj),
      ~ round(.x, 2)
    ),
    across(
      c(p.value, p.adj),
      ~ round(.x, 3)
    )
  )

# --- export results tables ---
write_csv(severity_popmean_tests_all, file.path(path2results_global, "results_severity_vs_population_mean.csv"))


# Maltreatment load group differences ------------------------------------------

N_SEV_GROUPS <- length(unique(df_sev_long$sev_group))

# --- associations with total number of subtypes experienced ---
severity_results <- set_names(tissues_labels) %>%
  map(
    ~ run_global_centile_model(
      data = df_sev_long,
      tissue_name = .x,
      group_var = sev_group,
      covariates = COVARIATES,
      ses_covariate = SES_VAR,
      random_effect = RANDOM_INTERCEPT,
      model_type = MODEL_TYPE,
      adjust_for_ses = ADJUST_FOR_SES,
      adjustment_method = ADJUSTMENT_METHOD
    )
  )

# estimated marginal means
severity_emm <- map_dfr(severity_results, "emm") %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# omnibus tests for parameters
severity_parameters <- map_dfr(
  severity_results,
  ~ as.data.frame(.x$joint_tests),
  .id = "tissue"
) %>%
  rename(
    "model.term" = "model term"
  )

adj <- severity_parameters %>%
  filter(model.term == "sev_group") %>%
  mutate(
    p.adj = p.adjust(p.value, method = ADJUSTMENT_METHOD)
  ) %>%
  select(tissue, model.term, p.adj)

severity_parameters <- severity_parameters %>%
  left_join(adj, by = c("tissue", "model.term")) %>%
  mutate(
    p.adj.signif = case_when(
      is.na(p.adj)  ~ NA_character_,
      p.adj < 0.001 ~ "***",
      p.adj < 0.01  ~ "**",
      p.adj < 0.05  ~ "*",
      TRUE          ~ "ns"
    )
  ) %>%
  mutate(
    across(
      where(is.numeric) & !c(p.value, p.adj),
      ~ round(.x, 2)
    ),
    across(
      c(p.value, p.adj),
      ~ round(.x, 3)
    )
  )

# all pairwise comparisons
severity_pairwise <- purrr::map_dfr(
  names(severity_results),
  ~ get_pairwise(
    model = severity_results[[.x]]$model,
    tissue_label = .x,
    group_var = "sev_group",
    adjust = ADJUSTMENT_METHOD,
    ystack = YSTACK,
    n_groups = N_SEV_GROUPS
  )
)


# --- export results tables ---
write_csv(severity_emm, file.path(path2results_global, "results_severity_estimated_marginal_means.csv"))
write_csv(severity_pairwise, file.path(path2results_global, "results_severity_pairwise_comparisons.csv"))
write_csv(severity_parameters, file.path(path2results_global, "results_severity_joint_omnibus_tests.csv"))


# Maltreatment load results figure ---------------------------------------------

SHOW_BOXPLOT  <- FALSE
SHOW_VIOLIN   <- FALSE

fig2_list <- map(tissues_labels, \(tt) {
  
  df_tt <- df_sev_long %>% filter(tissue == tt)
  
  ann_tt <- severity_pairwise %>%
    filter(tissue == tt)
  
  if (HIDE_NS) {
    ann_tt <- ann_tt %>%
      filter(p.adj.signif != "ns")
  }
  
  ann_tt <- ann_tt %>%
    arrange(group2, group1) %>%
    mutate(
      y.position = rep(YSTACK[2:n()], length.out = n()),
      y.position = y.position + 6,
      group1 = gsub("sev_group", "", group1),
      group2 = gsub("sev_group", "", group2)
    )
  
  pop_tt <- severity_popmean_tests_adjusted %>%
    filter(tissue == tt) %>% 
    mutate(
      y.position = YSTACK[1],
      label = p.adj.signif
    )
  
  if (HIDE_NS) {
    pop_tt <- pop_tt %>%
      filter(p.adj.signif != "ns")
  }
  
  # ymax <- ifelse(HIDE_NS, YSTACK[N_GROUPS] - 4, max(ann_tt$y.position + 6))
  ymax <- YSTACK[N_GROUPS] - 4
  
  if (SHOW_VIOLIN) {
    p <- ggplot(df_tt, aes(x = sev_group, y = centile, color = sev_group, fill = sev_group)) +
      geom_flat_violin(
        aes(group = sev_group, color = sev_group, fill = sev_group),
        position = position_nudge(x = 0, y = 0),
        adjust = 1,
        color = "white",
        alpha = 0.4,
        width = 1
      ) +
      ggbeeswarm::geom_quasirandom(
        aes(x = as.numeric(sev_group) - 0),
        width = 0.2,
        size = 2,
        alpha = 1,
        stroke = NA
      )
  } else {
    p <- ggplot(df_tt, aes(x = sev_group, y = centile, color = sev_group, fill = sev_group)) +
      ggbeeswarm::geom_quasirandom(
        width = 0.2,
        size = 2,
        alpha = 1,
        stroke = NA
      )
  }
  
  if (SHOW_BOXPLOT) {
    p <- p +
      geom_boxplot(
        outlier.shape = NA,
        fill = "white",
        color = "black",
        alpha = 0,
        lwd = 1,
        width = 0.2
      )
  }
  
  if (SHOW_MARGINAL_MEANS) {
    p <- p +
      geom_point(
        data = severity_emm %>% filter(tissue == tt), aes(y = emmean),
        pch = 18,
        size = 4,
        color = "black"
      ) +
      geom_errorbar(
        data = severity_emm %>% filter(tissue == tt),
        aes(x = sev_group, ymin = lower.CL, ymax = upper.CL),
        width = 0.12,
        size = 0.75,
        color = "black",
        inherit.aes = FALSE
      )
  }
  
  p <- p +
    geom_hline(yintercept = 50, lwd = 1, linetype = "dotted") +
    scale_color_manual(values = sev_colors, drop = FALSE) +
    scale_fill_manual(values = sev_colors, drop = FALSE) +
    scale_y_continuous(
      limits = c(0, ymax),
      breaks = c(2.5, 25, 50, 75, 97.5),
      labels = c("2.5th", "25th", "50th", "75th", "97.5th")
    ) +
    charite::theme_sci(font = "Helvetica", font_size = 10, aspect_ratio = 4/5, tiny_margins = TRUE) +
    theme(
      axis.ticks.x = element_blank(),
      plot.title = element_text(face = "bold", size = 18, margin = margin(b = + 10))
    ) +
    guides(fill = "none", color = "none") +
    labs(title = tt, x = NULL, y = NULL)
  
  # if (nrow(ann_tt) > 0) {
  #   p <- p +
  #     stat_pvalue_manual(
  #       ann_tt,
  #       label = "p.adj.signif",
  #       y.position = "y.position",
  #       xmin = "group1",
  #       xmax = "group2",
  #       tip.length = 0.01,
  #       inherit.aes = FALSE,
  #       size = 4
  #     )
  # }
  
  if (nrow(pop_tt) > 0) {
    p <- p +
      geom_text(
        data = pop_tt,
        aes(
          x = sev_group,
          y = y.position,
          label = label
        ),
        inherit.aes = FALSE,
        #fontface = "bold",
        size = 5,
        vjust = 0
      )
  }
  
  p
})

names(fig2_list) <- tissues_labels
fig2_list


# --- figure assembly ---
fig2a <- fig2_list[["cGM"]] + theme(axis.ticks.y = element_blank())
fig2b <- fig2_list[["WM"]] + theme(axis.ticks.y = element_blank())
fig2c <- fig2_list[["sGM"]] + theme(axis.ticks.y = element_blank())


figure2 <- fig2a + fig2b + fig2c +
  plot_layout(ncol = 3, widths = c(1, 1, 1))


# --- figure export ---
ggsave(
  filename = "figure2_severity.pdf",
  plot = figure2,
  path = path2figures,
  device = grDevices::pdf,
  width = 14.4, height = 3.6, units = "in",
  useDingbats = FALSE,
  bg = "white"
)


# Exploration of associations with maximum maltreatment severity ---------------

model <- lmer(cgm_c ~ micm_total_max_severity + age + sex_factor + (1|family_id), data = df %>% filter(cm_group == "CM+", micm_total_max_severity >= 2))
summary(model)
confint(model, method = "Wald")

model <- lmer(wm_c ~ micm_total_max_severity + age + sex_factor + (1|family_id), data = df %>% filter(cm_group == "CM+", micm_total_max_severity >= 2))
summary(model)
confint(model, method = "Wald")

model <- lmer(sgm_c ~ micm_total_max_severity + age + sex_factor + (1|family_id), data = df %>% filter(cm_group == "CM+", micm_total_max_severity >= 2))
summary(model)
confint(model, method = "Wald")

p.adjust(c(0.044, 0.022, 0.172), method = "fdr")


# Moderation of maltreatment-brain centiles associations by SES ----------------

model <- lmer(cgm_c ~ cm_group * ses_z + age_centred + sex_factor + (1|family_id), data = df)
summary(model)
confint(model, method = "Wald")
joint_tests(model)

model <- lmer(wm_c ~ cm_group * ses_z + age_centred + sex_factor + (1|family_id), data = df)
summary(model)
confint(model, method = "Wald")
joint_tests(model)

model <- lmer(sgm_c ~ cm_group * ses_z + age_centred + sex_factor + (1|family_id), data = df)
summary(model)
confint(model, method = "Wald")
joint_tests(model)

p.adjust(c(.046, .226, .643), method = "fdr")
