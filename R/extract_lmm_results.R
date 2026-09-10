extract_lmm_results <- function(models,
                                effect,
                                p_adjust = "BH",
                                conf.level = 0.95) {
  
  extract_one <- function(model, phenotype) {
    
    # ---- map model coefficients to formula terms ----
    
    X <- model.matrix(model)
    
    term_labels <- attr(terms(model), "term.labels")
    term_assign <- attr(X, "assign")
    
    coefficient_map <- tibble::tibble(
      term = colnames(X),
      formula_term = c("(Intercept)", term_labels)[term_assign + 1]
    )
    
    # Allow interactions to be supplied in either order:
    # x:y or y:x
    effect_parts <- strsplit(effect, ":", fixed = TRUE)[[1]]
    
    if (length(effect_parts) == 2) {
      
      interaction_options <- c(
        paste(effect_parts, collapse = ":"),
        paste(rev(effect_parts), collapse = ":")
      )
      
      selected_terms <- coefficient_map %>%
        dplyr::filter(formula_term %in% interaction_options)
      
    } else {
      
      selected_terms <- coefficient_map %>%
        dplyr::filter(formula_term == effect)
    }
    
    if (nrow(selected_terms) == 0) {
      stop(
        "Effect '", effect,
        "' was not found in model for outcome '",
        phenotype, "'."
      )
    }
    
    # ---- extract fixed-effect results ----
    
    model_results <- broom.mixed::tidy(
      model,
      effects = "fixed",
      conf.int = TRUE,
      conf.level = conf.level
    ) %>%
      dplyr::filter(term %in% selected_terms$term)
    
    # ---- essential model information ----
    
    n_obs <- stats::nobs(model)
    
    group_var <- names(lme4::getME(model, "flist"))
    
    n_groups <- if (length(group_var) == 1) {
      nlevels(lme4::getME(model, "flist")[[1]])
    } else {
      NA_integer_
    }
    
    singular <- lme4::isSingular(model)
    
    conv_messages <- model@optinfo$conv$lme4$messages
    
    convergence_issue <- !is.null(conv_messages)
    
    convergence_message <- if (convergence_issue) {
      paste(conv_messages, collapse = "; ")
    } else {
      NA_character_
    }
    
    # ---- combine ----
    
    model_results %>%
      dplyr::mutate(
        phenotype = phenotype,
        formula_term = selected_terms$formula_term[
          match(term, selected_terms$term)
        ],
        n_obs = n_obs,
        n_groups = n_groups,
        singular = singular,
        convergence_issue = convergence_issue,
        convergence_message = convergence_message,
        .before = 1
      )
  }
  
  results <- purrr::imap_dfr(
    models,
    extract_one
  )
  
  results %>%
    dplyr::mutate(
      p.adj = p.adjust(p.value, method = p_adjust)
    )
}
