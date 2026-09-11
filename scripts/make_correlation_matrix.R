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
library(cowplot)

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

names(df) <- c("Nonverbal IQ", "Verbal IQ", "Externalizing", "Internalizing", "cGM Centile", "WM Centile", "sGM Centile")

corr_results <- calculate_ppcorr_matrix(
  df = df,
  covariates = NULL,
  pcor.method = "pearson",
  plot = TRUE,
  pal = charite_pal
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
  insig   = "n",
  col     = charite_pal
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
  insig   = "n",
  col     = charite_pal
)

dev.off()

# individual plots of bivariate correlations
for (plot_name in names(corr_results$plots)) {
  
  plot_outname <- gsub(" ", "_", tolower(plot_name))
  
  charite::nice_save(paste0(plot_outname, ".png"), corr_results$plots[[plot_name]], layout = "half col", bg = "white")

}


# Correlation matrix of scatter plots ------------------------------------------

# define sparse axis breaks for less clutter
axis_breaks <- list(
  "Nonverbal IQ" = c(70, 100, 130),
  "Verbal IQ" = c(70, 100, 130),
  "Externalizing" = c(5, 10, 15),
  "Internalizing" = c(5, 10, 15),
  "cGM Centile" = c(.25, .5, .75),
  "WM Centile" = c(.25, .5, .75),
  "sGM Centile" = c(.25, .5, .75)
)

# helper function to format axis numbers
format_axis <- function(x) {
  sub("^(-?)0\\.", "\\1.", format(x, trim = TRUE))
}


for (plot_name in names(corr_results$plots)) {
  
  p <- corr_results$plots[[plot_name]]
  
  x_var <- p$labels$x
  y_var <- p$labels$y
  
  if (x_var %in% names(axis_breaks)) {
    
    x_breaks <- axis_breaks[[x_var]]
    
    p <- p +
      scale_x_continuous(
        breaks = x_breaks,
        labels = format_axis,
        expand = expansion(mult = c(0, 0.05))
      )
  }
  
  if (y_var %in% names(axis_breaks)) {
    
    y_breaks <- axis_breaks[[y_var]]
    
    p <- p +
      scale_y_continuous(
        breaks = y_breaks,
        labels = format_axis,
        expand = expansion(mult = c(0, 0.05))
      )
  }
  
  corr_results$plots[[plot_name]] <- p
}

vars <- colnames(corr_results$correlations)
n <- length(vars)

# order the plots accoring to their appearance in the correlation matrix
# strip axis labels and breaks for plots in the inner triangle
ordered_plots <- list()

for (i in 2:n) {
  for (j in 1:(i - 1)) {
    
    name1 <- paste0(
      "ppcorrplot_", vars[j], "_x_", vars[i]
    )
    
    name2 <- paste0(
      "ppcorrplot_", vars[i], "_x_", vars[j]
    )
    
    if (name1 %in% names(corr_results$plots)) {
      p <- corr_results$plots[[name1]]
    } else {
      p <- corr_results$plots[[name2]]
    }
    
    # only first column keeps y-axis numbers and title
    if (j > 1) {
      p <- p +
        theme(
          axis.text.y  = element_blank(),
          axis.title.y = element_blank()
        )
    }
    
    # only bottom row keeps x-axis numbers and title
    if (i < n) {
      p <- p +
        theme(
          axis.text.x  = element_blank(),
          axis.title.x = element_blank()
        )
    }
    
    # remove spacing around the entire plot for more compact figure
    p <- p +
      theme(
        plot.margin = margin(0, 0, 0, 0)
      )
    
    ordered_plots[[length(ordered_plots) + 1]] <- p
  }
}

# align the plots so that y axes match despite different y value levels
aligned_plots <- cowplot::align_plots(
  plotlist = ordered_plots,
  align = "v",
  axis = "tb"
)

# construct triangular grid
grid_plots <- list()

k <- 1

for (i in 1:(n - 1)) {
  
  for (j in 1:(n - 1)) {
    
    if (j <= i) {
      
      # actual plot
      grid_plots[[length(grid_plots) + 1]] <-
        aligned_plots[[k]]
      
      k <- k + 1
      
    } else {
      
      # actual blank cell, importantly, NOT NULL
      grid_plots[[length(grid_plots) + 1]] <-
        cowplot::ggdraw()
    }
  }
}

# combined plot
combined_plot <- cowplot::plot_grid(
  plotlist = grid_plots,
  ncol = n - 1,
  nrow = n - 1
)

# figure export
ggsave(
  "correlation_matrix_of_scatterplots.pdf",
  plot = combined_plot,
  width = 1.3 * (n - 1),
  height = 1.3 * (n - 1),
  units = "in"
)
