fit_lmm <- function(outcome,
                    predictor,
                    covariates,
                    random_effect,
                    data,
                    moderator = NULL,
                    reml = TRUE) {
  
  if (is.null(moderator)) {
    
    fixed_terms <- c(
      predictor,
      covariates
    )
    
  } else {
    
    fixed_terms <- c(
      paste0(predictor, " * ", moderator),
      covariates
    )
  }
  
  formula <- as.formula(
    paste0(
      outcome,
      " ~ ",
      paste(fixed_terms, collapse = " + "),
      " + (1 | ",
      random_effect,
      ")"
    )
  )
  
  lmer(
    formula = formula,
    data = data,
    REML = reml
  )
}
