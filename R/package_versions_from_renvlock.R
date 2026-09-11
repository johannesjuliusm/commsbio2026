packages <- c(
  "broom.mixed", "charite", "corrplot", "cowplot", "dplyr",
  "emmeans", "flextable", "ggh4x", "gghalves", "ggplot2",
  "ggpubr", "gtsummary", "Hmisc", "lme4", "lmerTest",
  "knitr", "kableExtra", "officer", "patchwork", "performance",
  "psych", "purrr", "rlang", "stringr", "tidyr"
)

lock <- renv::lockfile_read("renv.lock")

versions <- vapply(
  packages,
  function(pkg) {
    if (pkg %in% names(lock$Packages)) {
      lock$Packages[[pkg]]$Version
    } else {
      NA_character_
    }
  },
  character(1)
)

data.frame(
  Package = packages,
  Version = versions,
  row.names = NULL
)
