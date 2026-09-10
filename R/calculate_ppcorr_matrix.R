calculate_ppcorr_matrix <- function(
    df,
    covariates = c("age", "sex"),
    pcor.method = "pearson",
    plot = FALSE,
    pal = NULL
) {
  
  # validate covariates
  if (is.null(covariates)) {
    covariates <- character(0)
  }
  
  use_covariates <- length(covariates) > 0
  
  # validate a supplied color palette
  if (!is.null(pal) && (!is.character(pal) || length(pal) < 2)) {
    stop("`pal` must be NULL or a character vector containing at least two colors.")
  }
  
  numeric_vars <- setdiff(
    names(df)[sapply(df, is.numeric)],
    covariates
  )
  
  n <- length(numeric_vars)
  
  corr_matrix <- matrix(NA, n, n, dimnames = list(numeric_vars, numeric_vars))
  pval_matrix <- matrix(NA, n, n, dimnames = list(numeric_vars, numeric_vars))
  r2_matrix   <- matrix(NA, n, n, dimnames = list(numeric_vars, numeric_vars))
  
  plot_list <- list()
  df_list <- list()
  
  cov_caption <- if (use_covariates) {
    paste("Covariates:", paste(covariates, collapse = ", "))
  } else {
    "Bivariate correlation; no covariates"
  }
  
  for (i in 1:n) {
    for (j in i:n) {
      
      var1 <- numeric_vars[i]
      var2 <- numeric_vars[j]
      
      vars_to_select <- c(var1, var2, covariates)
      
      data_subset <- df %>%
        dplyr::select(all_of(vars_to_select)) %>%
        na.omit()
      
      if (nrow(data_subset) > 2) {
        
        if (use_covariates) {
          
          pcor_result <- ppcor::pcor(
            data_subset,
            method = pcor.method
          )
          
          r_value <- pcor_result$estimate[1, 2]
          p_value <- pcor_result$p.value[1, 2]
          
          formula1 <- as.formula(
            paste(var1, "~", paste(covariates, collapse = " + "))
          )
          
          formula2 <- as.formula(
            paste(var2, "~", paste(covariates, collapse = " + "))
          )
          
          residuals_var1 <- residuals(lm(formula1, data = data_subset)) +
            mean(data_subset[[var1]], na.rm = TRUE)
          
          residuals_var2 <- residuals(lm(formula2, data = data_subset)) +
            mean(data_subset[[var2]], na.rm = TRUE)
          
        } else {
          
          cor_result <- cor.test(
            data_subset[[var1]],
            data_subset[[var2]],
            method = pcor.method
          )
          
          r_value <- unname(cor_result$estimate)
          p_value <- cor_result$p.value
          
          residuals_var1 <- data_subset[[var1]]
          residuals_var2 <- data_subset[[var2]]
        }
        
        residuals_df <- data.frame(
          residuals_var1 = residuals_var1,
          residuals_var2 = residuals_var2
        )
        
        df_name <- paste0(var1, "_x_", var2)
        df_list[[df_name]] <- residuals_df
        
        r2_model <- lm(residuals_var1 ~ residuals_var2, data = residuals_df)
        r2_value <- summary(r2_model)$r.squared
        
        if (i == j) {
          r_value <- 1
          p_value <- 0.0001
          r2_value <- 1
        }
        
        corr_matrix[i, j] <- r_value
        corr_matrix[j, i] <- r_value
        
        pval_matrix[i, j] <- p_value
        pval_matrix[j, i] <- p_value
        
        r2_matrix[i, j] <- r2_value
        r2_matrix[j, i] <- r2_value
        
        if (plot && i != j) {
          
          if (is.null(pal)) {
            
            line_color <- ifelse(p_value < 0.05, "red", "darkgrey")
            point_color <- "black"
            
          } else {
            
            line_color <- ifelse(p_value < 0.05, "black", "darkgrey")
            color_index <- round(
              (r_value + 1) / 2 * (length(pal) - 1)
            ) + 1
            
            color_index <- pmax(1, pmin(length(pal), color_index))
            
            point_color <- pal[color_index]
          }
          
          p <- ggplot(
            residuals_df,
            aes(x = residuals_var1, y = residuals_var2)
          ) +
            geom_point(
              pch = 21,
              stroke = 0,
              fill = point_color,
              size = 1.5,
              alpha = 1
            ) +
            geom_smooth(
              method = "lm",
              color = line_color,
              se = FALSE
            ) +
            annotate(
              "label",
              x = -Inf,
              y = Inf,
              label = sprintf("r = %.2f", r_value),
              hjust = -0.2,
              vjust = 1.2,
              size = 2.8,
              color = "black",
              fill = scales::alpha("white", 0.7),
              linewidth = 0
            ) +
            labs(
              # title = if (use_covariates) {
              #   paste("Partial Correlation:", round(r_value, 2))
              # } else {
              #   paste("Correlation:", round(r_value, 2))
              # },
              # caption = cov_caption,
              x = var1,
              y = var2
            ) +
            scale_x_continuous(
              expand = expansion(mult = c(0, 0.05))
            ) +
            scale_y_continuous(
              expand = expansion(mult = c(0, 0.05))
            ) +
            charite::theme_sci(
              font_size = 8,
              aspect_ratio = 1
            )
          
          plot_name <- paste0("ppcorrplot_", var1, "_x_", var2)
          plot_list[[plot_name]] <- p
        }
      }
    }
  }
  
  adjusted_pval_matrix <- matrix(
    NA,
    nrow = nrow(pval_matrix),
    ncol = ncol(pval_matrix),
    dimnames = dimnames(pval_matrix)
  )
  
  upper <- upper.tri(pval_matrix)
  
  adjusted_upper <- stats::p.adjust(
    pval_matrix[upper],
    method = "fdr"
  )
  
  adjusted_pval_matrix[upper] <- adjusted_upper
  adjusted_pval_matrix[t(upper)] <- adjusted_upper
  diag(adjusted_pval_matrix) <- 0.0001
  
  return(list(
    correlations = corr_matrix,
    pvalues = pval_matrix,
    pvalues_adj = adjusted_pval_matrix,
    r2 = r2_matrix,
    plots = plot_list,
    dfs = df_list
  ))
}
