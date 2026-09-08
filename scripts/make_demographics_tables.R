# ==============================================================================
# Project      : Kids2Health, Charite - Universitatsmedizin Berlin
# Author       : Johannes Julius Mohn
# Contact      : johannes.j.mohn@maxplanckschools.de
# Date Created : 2026-05-18
# R Version    : R version 4.4.1 (2024-06-14)
# ==============================================================================
# History      : 2026-05-18: Script created, mohnjj
#                2026-09-07: Clean-up for public repository
#
# Description:
# Description of sample demographics.
#
# ==============================================================================

source(here::here("scripts", "00_setup.R"))

# --- packages ---
library(flextable)
library(officer)
library(gtsummary)

# --- directories ---
path2tables_demographics <- file.path(path2tables, "demographics")
dir.create(path2tables_demographics, recursive = TRUE)

# --- data files ---
file_demographics <- "k2h_demographics_sp-1_t-0_2026-05-19.csv"
file_maltreatment <- "k2h_maltreatment_sp-1_t-0_2026-05-19.csv"
file_iq           <- "k2h_iq_scores_sp-1_t-0_2026-05-19.csv"
file_sdq          <- "k2h_sdq_scores_sp-1_t-0_2026-05-19.csv"


# Data -------------------------------------------------------------------------

# --- demographic information ---
demos <- read.csv(file.path(path2data_demographics, file_demographics)) %>%
  select(id, age_at_mri, sex, sex_factor, ses_composite) %>%
  rename(age = age_at_mri)

# --- maltreatment coding information ---
cm <- read.csv(file.path(path2data_maltreatment, file_maltreatment))

# --- intelligence information ---
iq_scores <- read.csv(file.path(path2data_cognition, file_iq))

# --- clinical information ---
sdq_scores <- read.csv(file.path(path2data_clinical, file_sdq))

# --- combined data frame ---
df <- reduce(list(demos, cm, iq_scores, sdq_scores), full_join, by = "id")


# Tables preparation -----------------------------------------------------------

# --- demographic variables and labels ---
vars_table1 <- c("age", "sex_factor", "ses_composite")
vars_table2 <- c("sonr", "wisc", "sdq_intern", "sdq_extern")

labels_dict <- list(
  age = "Age (years)",
  sex_factor = "Sex",
  ses_composite = "SES",
  sonr = "SONR-R IQ",
  wisc = "WISC-Verbal IQ",
  sdq_total = "SDQ Total",
  sdq_extern = "SDQ Externalizing",
  sdq_intern = "SDQ Internalizing"
)

grouping_var <- "cm_group"


# Table 1. Demographics --------------------------------------------------------

# --- building table 1 ---
table_demos <- df %>%
  select(all_of(c(vars_table1, grouping_var))) %>%
  tbl_summary(
    by = !!rlang::sym(grouping_var),
    label = labels_dict,
    type = list(
      age ~ "continuous2",
      sex_factor ~ "categorical",
      ses_composite ~ "continuous2"
    ),
    statistic = list(
      all_continuous() ~ c(
        "{mean} ({sd})",
        "{median} [{min}, {max}]"
      ),
      all_categorical() ~ "{n} ({p}%)"
    )
  ) %>%
  add_overall(last = FALSE) %>%
  add_p(
    test = list(
      all_continuous() ~ "t.test", # or: t.test or wilcox.test
      all_categorical() ~ "chisq.test"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  modify_header(label ~ "**Variable**")
table_demos


# --- page layout (A4 + 1 inch margins) ---
section_properties <- officer::prop_section(
  page_size = officer::page_size(width = 8.3, height = 11.7),
  page_margins = officer::page_mar(top = 1, bottom = 1, left = 1, right = 1)
)


# --- convert tables to APA style ---
ft <- as_flex_table(table_demos) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  bold(bold = FALSE, part = "all") %>%
  border_remove() %>%
  hline_top(border = fp_border(width = 1)) %>%
  hline(i = 1, part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(border = fp_border(width = 1)) %>%
  padding(padding = 2, part = "all") %>%
  set_table_properties(layout = "fixed", width = 1)


# force full width (minus margins)
avail_width_in <- 8.3 - 1 - 1
ft <- width(ft, width = avail_width_in / ncol(ft$body$dataset))


# --- word .docx with table ---
doc <- read_docx()

# table number and title
doc <- body_add_fpar(
  doc,
  fpar(ftext("Table 1. Demographic variables and group differences.", fp_text(font.family = "Times New Roman", font.size = 10, bold = TRUE)))
)
doc <- body_add_fpar(doc, fpar(ftext("", fp_text(font.family = "Times New Roman", font.size = 10))))

# add table
doc <- body_add_flextable(
  doc,
  value = ft,
  align = "left",
  split = FALSE,
  pos = "after"
)

# table note
doc <- body_add_fpar(
  doc,
  fpar(
    ftext("Note. ", fp_text(font.family = "Times New Roman", font.size = 10, italic = TRUE)),
    ftext("Values are shown as mean (SD) for continuous variables and n (%) for categorical variables. ",
          fp_text(font.family = "Times New Roman", font.size = 10)),
    ftext("CM+, childhood maltreatment exposure; CM–, non-exposed; SES, socioeconomic status. ",
          fp_text(font.family = "Times New Roman", font.size = 10))
  )
)


# --- export ---
out_path <- file.path(path2tables_demographics, paste0("table01_demographics_", Sys.Date(), ".docx"))
print(doc, target = out_path)


# Table 2. SDQ and IQ ----------------------------------------------------------

# --- building table 2 ---
table_2 <- df %>%
  select(all_of(c(vars_table2, grouping_var))) %>%
  tbl_summary(
    by = !!rlang::sym(grouping_var),
    label = labels_dict,
    type = list(
      wisc ~ "continuous2",
      sonr ~ "continuous2",
      sdq_intern ~ "continuous2",
      sdq_extern ~ "continuous2"
    ),
    statistic = list(
      all_continuous() ~ c(
        "{mean} ({sd})",
        "{median} [{min}, {max}]"
      ),
      all_categorical() ~ "{n} ({p}%)"
    )
  ) %>%
  add_overall(last = FALSE) %>%
  add_p(
    test = list(
      all_continuous() ~ "t.test", # or: t.test or wilcox.test
      all_categorical() ~ "chisq.test"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  modify_header(label ~ "**Variable**")
table_2


# --- page layout (A4 + 1 inch margins) ---
section_properties <- officer::prop_section(
  page_size = officer::page_size(width = 8.3, height = 11.7),
  page_margins = officer::page_mar(top = 1, bottom = 1, left = 1, right = 1)
)


# --- convert tables to APA style ---
ft2 <- as_flex_table(table_2) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  bold(bold = FALSE, part = "all") %>%
  border_remove() %>%
  hline_top(border = fp_border(width = 1)) %>%
  hline(i = 1, part = "header", border = fp_border(width = 1)) %>%
  hline_bottom(border = fp_border(width = 1)) %>%
  padding(padding = 2, part = "all") %>%
  set_table_properties(layout = "fixed", width = 1)


# force full width (minus margins)
avail_width_in <- 8.3 - 1 - 1
ft2 <- width(ft2, width = avail_width_in / ncol(ft2$body$dataset))


# --- word .docx with table ---
doc2 <- read_docx()


# table number and title
doc2 <- body_add_fpar(
  doc2,
  fpar(ftext("Table 2. Intellectual ability and psychopathological symptoms.", fp_text(font.family = "Times New Roman", font.size = 10, bold = TRUE)))
)
doc2 <- body_add_fpar(doc2, fpar(ftext("", fp_text(font.family = "Times New Roman", font.size = 10))))

# add table
doc2 <- body_add_flextable(
  doc2,
  value = ft2,
  align = "left",
  split = FALSE,
  pos = "after"
)

# table note
doc2 <- body_add_fpar(
  doc2,
  fpar(
    ftext("Note. ", fp_text(font.family = "Times New Roman", font.size = 10, italic = TRUE)),
    ftext("SON-R, Snijders-Oomen Non-verbal Intelligence Test; WISC-Verbal, Verbal Comprehension Scale of the Wechsler Intelligence Scale for Children; SDQ, Strengths and Difficulties Questionnaire. ",
          fp_text(font.family = "Times New Roman", font.size = 10)),
    ftext("CM+, childhood maltreatment exposure; CM–, non-exposed.",
          fp_text(font.family = "Times New Roman", font.size = 10))
  )
)


# --- export ---
out_path <- file.path(path2tables_demographics, paste0("table02_iq_and_sdq_", Sys.Date(), ".docx"))
print(doc2, target = out_path)
