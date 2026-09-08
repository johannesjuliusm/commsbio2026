get_pairwise <- function(
    model,
    tissue_label,
    group_var,
    adjust = "fdr",
    ystack = NULL,
    n_groups = NULL,
    ci_level = 0.95
) {
  
  emm_fml <- stats::as.formula(paste0("~ ", group_var))
  emm <- emmeans::emmeans(model, emm_fml)
  pw <- pairs(emm, reverse = TRUE)
  
  pw_unadj <- as.data.frame(
    summary(
      pw,
      adjust = "none",
      infer = c(TRUE, TRUE),
      level = ci_level
    )
  ) %>%
    tidyr::separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
    dplyr::transmute(
      tissue = tissue_label,
      group1 = stringr::str_remove_all(group1, "[()]"),
      group2 = stringr::str_remove_all(group2, "[()]"),
      estimate,
      SE,
      df,
      lower.CL,
      upper.CL,
      t.ratio,
      p.value,
      p.signif = dplyr::case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE ~ ""
      )
    )
  
  pw_adj <- as.data.frame(
    summary(pw, adjust = adjust)
  ) %>%
    tidyr::separate(contrast, into = c("group1", "group2"), sep = " - ") %>%
    dplyr::transmute(
      tissue = tissue_label,
      group1 = stringr::str_remove_all(group1, "[()]"),
      group2 = stringr::str_remove_all(group2, "[()]"),
      p.adj = p.value,
      p.adj.signif = dplyr::case_when(
        p.adj < 0.001 ~ "***",
        p.adj < 0.01  ~ "**",
        p.adj < 0.05  ~ "*",
        TRUE ~ ""
      )
    )
  
  if (inherits(model, "merMod")) {
    r2_vals <- performance::r2_nakagawa(model)
    r2_marginal <- r2_vals$R2_marginal
    r2_conditional <- r2_vals$R2_conditional
  } else if (inherits(model, "lm")) {
    r2_vals <- performance::r2(model)
    r2_marginal <- r2_vals$R2
    r2_conditional <- NA_real_
  }
  
  out <- dplyr::left_join(
    pw_unadj,
    pw_adj,
    by = c("tissue", "group1", "group2")
  ) %>%
    dplyr::mutate(
      r2_marginal = r2_marginal,
      r2_conditional = r2_conditional,
      p.signif = ifelse(p.signif == "", "ns", p.signif),
      p.adj.signif = ifelse(p.adj.signif == "", "ns", p.adj.signif)
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
  
  if (!is.null(ystack) && !is.null(n_groups)) {
    out <- out %>%
      dplyr::mutate(
        y.position = rep(ystack[1:n_groups], length.out = dplyr::n())
      )
  }
  
  out
}
