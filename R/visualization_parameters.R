# Visual parameters ------------------------------------------------------------

# colors for plotting maltreatment contrasts
cm_colors <- c(
  "CM-" = charite::charite_colors$PRIME_DGREY,
  "CM+" = charite::charite_colors$KORALL
)

# colors for plotting maltreatment severity levels
sev_colors <- c(
  "1" = charite::charite_colors$RAPSGELB,
  "2" = charite::charite_colors$MANGO,
  "3" = charite::charite_colors$KORALL,
  "4" = charite::charite_colors$ROT,
  "5" = charite::charite_colors$WEINROT
)

# order of sexes
sex_order <- c("males", "females")

# order of brain compartments
tissues_order   <- c("cgm_c", "wm_c", "sgm_c")
tissues_labels  <- c(cgm_c = "cGM", wm_c = "WM", sgm_c = "sGM")
