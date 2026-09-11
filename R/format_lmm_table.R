format_lmm_table <- function(results,
                             significance_stars = TRUE,
                             include_sample_size = TRUE,
                             digits_est = 2,
                             digits_test = 2,
                             digits_df = 2,
                             digits_p = 3) {
  
  add_stars <- function(p) {
    dplyr::case_when(
      is.na(p)   ~ "",
      p < 0.001  ~ "***",
      p < 0.01   ~ "**",
      p < 0.05   ~ "*",
      TRUE       ~ ""
    )
  }
  
  format_p <- function(p) {
    dplyr::case_when(
      is.na(p)   ~ NA_character_,
      p < 0.001  ~ "<0.001",
      TRUE       ~ sprintf(paste0("%.", digits_p, "f"), p)
    )
  }
  
  true_minus <- function(x) {
    gsub("-", "\u2212", x, fixed = TRUE)
  }
  
  table <- results %>%
    dplyr::transmute(
      `Phenotype` = phenotype,
      Term = term,
      
      Estimate = true_minus(
        sprintf(
          paste0("%.", digits_est, "f"),
          estimate
        )
      ),
      
      `95% CI` = paste0(
        true_minus(
          sprintf(
            paste0("%.", digits_est, "f"),
            conf.low
          )
        ),
        ", ",
        true_minus(
          sprintf(
            paste0("%.", digits_est, "f"),
            conf.high
          )
        )
      ),
      
      t = true_minus(
        sprintf(
          paste0("%.", digits_test, "f"),
          statistic
        )
      ),
      
      df = sprintf(
        paste0("%.", digits_df, "f"),
        df
      ),
      
      p = paste0(
        format_p(p.value),
        if (significance_stars) add_stars(p.value) else ""
      ),
      
      `Adjusted p` = paste0(
        format_p(p.adj),
        if (significance_stars) add_stars(p.adj) else ""
      ),
      
      N = n_obs,
      
      Families = n_groups
    )
  
  if (!include_sample_size) {
    table <- table %>%
      dplyr::select(-N, -Families)
  }
  
  table
}
