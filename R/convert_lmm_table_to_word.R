convert_lmm_table_to_word <- function(table_results,
                                      out_path,
                                      table_number,
                                      table_title,
                                      note = NULL,
                                      orientation = c("portrait", "landscape"),
                                      font = "Times New Roman",
                                      font_size = 10,
                                      page_width = 8.3,
                                      page_height = 11.7,
                                      margin = 1) {
  
  orientation <- match.arg(orientation)
  
  
  # --- page layout ---
  
  section_properties <- officer::prop_section(
    page_size = officer::page_size(
      orient = orientation,
      width = page_width,
      height = page_height
    ),
    page_margins = officer::page_mar(
      top = margin,
      bottom = margin,
      left = margin,
      right = margin
    )
  )
  
  # calculate usable page width
  # portrait:  8.3 inches wide
  # landscape: 11.7 inches wide
  usable_width <- if (orientation == "portrait") {
    page_width - (2 * margin)
  } else {
    page_height - (2 * margin)
  }
  
  
  # --- create flextable ---
  
  ft <- flextable::flextable(table_results) %>%
    
    flextable::font(
      fontname = font,
      part = "all"
    ) %>%
    
    flextable::fontsize(
      size = font_size,
      part = "all"
    ) %>%
    
    flextable::bold(
      bold = FALSE,
      part = "body"
    ) %>%
    
    flextable::bold(
      bold = TRUE,
      part = "header"
    ) %>%
    
    flextable::border_remove() %>%
    
    flextable::hline_top(
      border = officer::fp_border(width = 1)
    ) %>%
    
    flextable::hline(
      i = 1,
      part = "header",
      border = officer::fp_border(width = 1)
    ) %>%
    
    flextable::hline_bottom(
      border = officer::fp_border(width = 1)
    ) %>%
    
    flextable::padding(
      padding = 2,
      part = "all"
    ) %>%
    
    flextable::valign(
      valign = "center",
      part = "all"
    ) %>%
    
    flextable::set_table_properties(
      layout = "fixed",
      width = 1
    )
  
  
  # --- column alignment ---
  
  text_cols <- intersect(
    c("Imaging Phenotype", "Term"),
    names(table_results)
  )
  
  numeric_cols <- setdiff(
    names(table_results),
    text_cols
  )
  
  if (length(text_cols) > 0) {
    ft <- flextable::align(
      ft,
      j = text_cols,
      align = "left",
      part = "all"
    )
  }
  
  if (length(numeric_cols) > 0) {
    ft <- flextable::align(
      ft,
      j = numeric_cols,
      align = "center",
      part = "all"
    )
  }
  
  
  # --- column widths ---
  # .. note:: these are relative desired widths and rescaled to fill the entire
  #           usable page width
  
  width_map <- c(
    "Imaging Phenotype" = 1.2,
    "Term"              = 1.2,
    "Estimate"          = 1,
    "95% CI"            = 1,
    "t"                 = 1,
    "df"                = 1,
    "p"                 = 1,
    "Adjusted p"        = 1,
    "N"                 = 1,
    "Families"          = 1
  )
  
  # keep only columns actually present in table_results
  present_cols <- names(table_results)
  
  preferred_widths <- width_map[present_cols]
  
  # fallback for any unexpected columns
  preferred_widths[is.na(preferred_widths)] <- 1
  
  # scale widths so they sum exactly to usable page width
  scaled_widths <- preferred_widths /
    sum(preferred_widths) *
    usable_width
  
  # apply scaled widths
  for (i in seq_along(present_cols)) {
    
    ft <- flextable::width(
      ft,
      j = present_cols[i],
      width = scaled_widths[i]
    )
  }
  
  
  # --- create the Word document ---
  
  doc <- officer::read_docx()
  
  doc <- officer::body_set_default_section(
    doc,
    value = section_properties
  )
  
  
  # --- table number and title ---
  
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        paste0(
          "Table ",
          table_number,
          ". ",
          table_title
        ),
        officer::fp_text(
          font.family = font,
          font.size = font_size,
          bold = TRUE
        )
      )
    )
  )
  
  
  # blank line between title and table
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        "",
        officer::fp_text(
          font.family = font,
          font.size = font_size
        )
      )
    )
  )
  
  
  # --- add table ---
  
  doc <- flextable::body_add_flextable(
    doc,
    value = ft,
    align = "left",
    split = FALSE,
    pos = "after"
  )
  
  
  # --- optional table note ---
  
  if (!is.null(note)) {
    
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(
          "Note. ",
          officer::fp_text(
            font.family = font,
            font.size = font_size,
            italic = TRUE
          )
        ),
        officer::ftext(
          note,
          officer::fp_text(
            font.family = font,
            font.size = font_size
          )
        )
      )
    )
  }
  
  
  # --- export ---
  
  print(
    doc,
    target = out_path
  )
  
  invisible(out_path)
}
