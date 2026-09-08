plot_correlation_matrix <- function(coeffs, pvals, ...) {
  
  defaults <- list(
    corr        = coeffs,
    type        = "lower",
    order       = "original",
    method      = "color",
    bg          = "white",
    tl.col      = "black",
    tl.pos      = "l",
    cl.pos      = "n",
    col         = pal,
    p.mat       = pvals,
    sig.level   = 0.05,
    insig       = "pch",
    addCoef.col = "black",
    tl.cex      = 0.8,
    number.cex  = 0.8,
    mar         = c(5, 0, 0, 0)
  )
  
  # overwrite defaults with user-supplied arguments
  user_args <- list(...)
  defaults[names(user_args)] <- user_args
  
  # draw corrplot
  do.call(corrplot::corrplot, defaults)
  
  # add column labels underneath
  n <- ncol(coeffs)
  
  text(
    x = seq_len(n),
    y = 0.25,
    labels = colnames(coeffs),
    srt = 70,
    adj = 1,
    xpd = TRUE,
    cex = defaults$tl.cex
  )
}
