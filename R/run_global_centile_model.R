run_global_centile_model <- function(
    data,
    tissue_name,
    group_var,
    outcome = centile,
    tissue_var = tissue,
    covariates = c("age", "sex"),
    ses_covariate = "ses_z",
    adjust_for_ses = FALSE,
    random_effect = "family_id",
    model_type = c("lmer", "lm"),
    adjustment_method = "fdr"
) {
  
  model_type <- match.arg(model_type)
  
  group_var  <- rlang::as_name(rlang::enquo(group_var))
  outcome    <- rlang::as_name(rlang::enquo(outcome))
  tissue_var <- rlang::as_name(rlang::enquo(tissue_var))
  
  fixed_effects <- c(
    group_var,
    covariates,
    if (adjust_for_ses) ses_covariate
  )
  
  rhs <- paste(fixed_effects, collapse = " + ")
  
  model_formula <- if (model_type == "lmer") {
    stats::as.formula(
      paste0(outcome, " ~ ", rhs, " + (1 | ", random_effect, ")")
    )
  } else {
    stats::as.formula(
      paste0(outcome, " ~ ", rhs)
    )
  }
  
  model_data <- data %>%
    dplyr::filter(.data[[tissue_var]] == tissue_name)
  
  model <- if (model_type == "lmer") {
    lme4::lmer(
      model_formula,
      data = model_data,
      REML = TRUE
    )
  } else {
    stats::lm(
      model_formula,
      data = model_data
    )
  }
  
  emm <- emmeans::emmeans(
    model,
    specs = stats::as.formula(paste0("~ ", group_var))
  )
  
  list(
    model = model,
    model_type = model_type,
    joint_tests = emmeans::joint_tests(
      model,
      by = NULL,
      lmer.df = if (model_type == "lmer") "kenward-roger" else NULL
    ),
    pairwise = pairs(
      emm,
      adjust = adjustment_method
    ),
    emm = as.data.frame(emm) %>%
      dplyr::mutate(
        tissue = tissue_name,
        grouping_variable = group_var,
        model_type = model_type
      )
  )
}
