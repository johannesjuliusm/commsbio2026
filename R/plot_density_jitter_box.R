# function to create a figure consisting of density curves, jittered data points and box plots,
# optional with statistical test
plot_density_jitter_box <- function(data,
                                    x,
                                    group,
                                    title = NULL,
                                    xlab = NULL,
                                    ylab = "Density",
                                    palette = NULL,
                                    density_alpha = 0.7,
                                    density_linewidth = 1,
                                    box_width = NULL,
                                    jitter_height = NULL,
                                    point_size = 1.4,
                                    point_alpha = 0.7,
                                    box_band_top = NULL,
                                    box_band_step = NULL,
                                    add_test = TRUE,
                                    show_legend = FALSE,
                                    test_label_size = 2.5,
                                    base_size = 10,
                                    seed = 1402) {
  library(ggplot2)
  library(dplyr)
  library(scales)
  
  if (!is.character(x) || length(x) != 1) {
    stop("`x` must be a single character string naming a column.")
  }
  if (!is.character(group) || length(group) != 1) {
    stop("`group` must be a single character string naming a column.")
  }
  
  if (!x %in% names(data)) {
    stop("Column `", x, "` not found in `data`.")
  }
  if (!group %in% names(data)) {
    stop("Column `", group, "` not found in `data`.")
  }
  
  df <- data %>%
    mutate(
      .x = .data[[x]],
      .group = factor(as.character(.data[[group]]))
    ) %>%
    filter(!is.na(.x), !is.na(.group))
  
  if (nrow(df) == 0) {
    stop("No rows remain after removing missing values in `", x, "` and `", group, "`.")
  }
  
  n_groups <- nlevels(df$.group)
  
  if (n_groups < 1) {
    stop("`group` has no valid levels after filtering.")
  }
  
  group_levels <- levels(df$.group)
  
  density_max <- df %>%
    group_split(.group) %>%
    lapply(function(d) {
      if (length(d$.x) < 2 || length(unique(d$.x)) < 2) {
        return(NA_real_)
      }
      dens <- density(d$.x, na.rm = TRUE)
      max(dens$y)
    }) %>%
    unlist() %>%
    max(na.rm = TRUE)
  
  if (!is.finite(density_max)) {
    stop("Density could not be estimated. Check whether `", x, "` has enough variation.")
  }
  
  if (is.null(box_band_top)) {
    box_band_top <- -0.12 * density_max
  }
  if (is.null(box_band_step)) {
    box_band_step <- 0.14 * density_max
  }
  if (is.null(box_width)) {
    box_width <- 0.08 * density_max
  }
  if (is.null(jitter_height)) {
    jitter_height <- 0.03 * density_max
  }
  
  group_positions <- seq(
    from = box_band_top,
    by = -box_band_step,
    length.out = n_groups
  )
  
  pos_df <- data.frame(
    .group = factor(group_levels, levels = group_levels),
    .y = group_positions
  )
  
  df <- df %>%
    left_join(pos_df, by = ".group")
  
  if (is.null(palette)) {
    palette <- hue_pal()(n_groups)
    names(palette) <- group_levels
  } else {
    if (length(unique(palette)) < n_groups) {
      stop("`palette` must have at least as many colors as groups.")
    }
    
    if (is.null(names(palette)) ||
        !all(group_levels %in% names(palette))) {
      
      palette <- unname(palette)[seq_len(n_groups)]
      names(palette) <- group_levels
      
    } else {
      palette <- palette[group_levels]
    }
  }
  
  p <- ggplot(df, aes(x = .x, fill = .group, color = .group)) +
    geom_density(
      aes(y = after_stat(density)),
      alpha = density_alpha,
      color = NA,
      linewidth = density_linewidth,
      adjust = 1
    ) +
    geom_point(
      aes(y = .y),
      position = position_jitter(width = 0, height = jitter_height, seed = seed),
      size = point_size,
      alpha = point_alpha,
      stroke = 0
    ) +
    geom_boxplot(
      aes(y = .y, group = .group),
      width = box_width,
      outlier.shape = NA,
      color = "black",
      fill = NA,
      alpha = 0.95,
      linewidth = 1
    ) +
    scale_fill_manual(values = palette, drop = FALSE) +
    scale_color_manual(values = palette, drop = FALSE) +
    labs(
      title = title,
      x = if (is.null(xlab)) x else xlab,
      y = ylab
    ) +
    theme_classic(base_size = base_size, base_family = "Helvetica") +
    theme(
      legend.title = element_blank(),
      legend.position = if (show_legend) "right" else "none",
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.line = element_line(lineend = "square"),
      plot.margin = margin(t = 5.5, r = 10, b = 5.5, l = 5.5, unit = "pt")
    )
  
  if (add_test && n_groups == 2) {
    g1 <- df$.x[df$.group == group_levels[1]]
    g2 <- df$.x[df$.group == group_levels[2]]
    
    if (length(g1) > 1 && length(g2) > 1) {
      tt <- t.test(g1, g2)
      
      p_label <- if (tt$p.value < 0.001) {
        "p < 0.001"
      } else {
        paste0("p = ", formatC(tt$p.value, format = "f", digits = 3))
      }
      
      test_lab <- paste0(
        "t(", round(tt$parameter), ") = ",
        formatC(tt$statistic, format = "f", digits = 2),
        ", ",
        p_label
      )
      
      x_rng <- range(df$.x, na.rm = TRUE)
      y_min <- min(group_positions) - box_band_step
      
      p <- p +
        annotate(
          "text",
          x = x_rng[2],
          y = y_min,
          label = test_lab,
          hjust = 1,
          size = test_label_size
        )
    }
  }
  
  y_upper <- density_max * 1.5
  y_lower <- min(group_positions) - 1.6 * box_band_step
  
  breaks <- pretty(c(0, y_upper))
  
  p +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0)),
      breaks = function(x) {
        pretty(c(0, max(x)), n = 5)
      }
    ) +
    coord_cartesian(
      ylim = c(y_lower, y_upper),
      clip = "off"
    )
}
