run_popmean_tests <- function(
    data,
    tissue_name,
    group_var = "group",
    outcome = "centile",
    tissue_var = "tissue",
    random_effect = "family_id",
    covariates = NULL,
    pop_mean = 0.5,
    adjust_method = "fdr",
    ci_level = 0.95
) {
  
  fixed_effects <- c(group_var, covariates)
  
  fml <- stats::as.formula(
    paste0(
      outcome, " ~ ",
      paste(fixed_effects, collapse = " + "),
      " + (1 | ", random_effect, ")"
    )
  )
  
  model_data <- data %>%
    dplyr::filter(.data[[tissue_var]] == tissue_name)
  
  model <- lme4::lmer(
    fml,
    data = model_data,
    REML = TRUE,
    na.action = na.exclude
  )
  
  emm <- emmeans::emmeans(
    model,
    specs = stats::as.formula(paste0("~ ", group_var))
  )
  
  emm_ci <- as.data.frame(
    summary(
      emm,
      infer = c(TRUE, TRUE),
      level = ci_level
    )
  ) %>%
    dplyr::select(-dplyr::any_of(c("t.ratio", "p.value")))
  
  raw_p <- as.data.frame(
    test(emm, null = pop_mean, adjust = "none")
  ) %>%
    dplyr::select(
      dplyr::all_of(group_var),
      p.value
    )
  
  adj_p <- as.data.frame(
    test(emm, null = pop_mean, adjust = adjust_method)
  ) %>%
    dplyr::select(
      dplyr::all_of(group_var),
      p.adj = p.value
    )
  
  r2_vals <- performance::r2_nakagawa(model)
  
  emm_ci %>%
    dplyr::left_join(raw_p, by = group_var) %>%
    dplyr::left_join(adj_p, by = group_var) %>%
    dplyr::mutate(
      tissue = tissue_name,
      model_type = ifelse(
        is.null(covariates) || length(covariates) == 0,
        "unadjusted",
        "adjusted"
      ),
      pop_mean = pop_mean,
      r2_marginal = r2_vals$R2_marginal,
      r2_conditional = r2_vals$R2_conditional,
      p.signif = dplyr::case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE ~ "ns"
      ),
      p.adj.signif = dplyr::case_when(
        p.adj < 0.001 ~ "***",
        p.adj < 0.01  ~ "**",
        p.adj < 0.05  ~ "*",
        TRUE ~ "ns"
      )
    ) %>%
    dplyr::relocate(
      tissue,
      model_type,
      pop_mean,
      dplyr::all_of(group_var)
    )
}
