# ==============================================================================
# Project      : Kids2Health, Charite - Universitatsmedizin Berlin
# Author       : Johannes Julius Mohn
# Contact      : johannes.j.mohn@maxplanckschools.de
# Date Created : 2026-05-18
# R Version    : R version 4.4.1 (2024-06-14)
# ==============================================================================
# History      : 2026-06-07: Script created from previous .Rmd file, mohnjj
#                2026-09-08: Clean-up for public repository
#
# Description:
# Description of family structure in the data.
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


# Data -------------------------------------------------------------------------

# --- demographic information ---
demos <- read.csv(file.path(path2data_demographics, file_demographics)) %>%
  rename(age = age_at_mri)

# --- maltreatment coding information ---
cm <- read.csv(file.path(path2data_maltreatment, file_maltreatment))

# --- combined data frame ---
df <- reduce(list(demos, cm), full_join, by = "id")


# Family structure -------------------------------------------------------------

df <- df %>%
  group_by(family_id) %>%
  mutate(
    sibling_in_this_data_yn = ifelse(n() > 1, 1, 0),
    twin_in_this_data_yn    = ifelse(n() > 1 & sum(twin_sibling, na.rm = TRUE) > 1 & !is.na(twin_type), 1, 0),
    twin_type               = ifelse(twin_in_this_data_yn == 0, NA, twin_type),
    sibling_pair            = ifelse(n() == 2, 1, 0),
    sibling_trio            = ifelse(n() == 3, 1, 0),
    sibling_quartet         = ifelse(n() == 4, 1, 0)
  ) %>%
  ungroup()

# summary of siblings contained in the data
siblings_summary <- df %>%
  summarise(
    N_total             = n(),
    N_families          = n_distinct(family_id),
    N_siblings          = sum(sibling_in_this_data_yn, na.rm = TRUE),
    N_sibling_pairs     = sum(sibling_pair, na.rm = TRUE) / 2,
    N_sibling_trios     = sum(sibling_trio, na.rm = TRUE) / 3,
    N_sibling_quartets  = sum(sibling_quartet, na.rm = TRUE) / 4
  )

# summary of twins
twin_summary <- df %>%
  filter(!is.na(twin_type)) %>%
  group_by(twin_type) %>%
  summarise(
    count = n()
  ) %>%
  ungroup()

# merge information on siblings and twins
siblings_summary <- siblings_summary %>%
  mutate(
    N_twins = sum(twin_summary$count),
    N_twins_dizygotic = twin_summary %>% filter(twin_type == "dizygotic") %>% pull(count),
    N_twins_monozygotic = twin_summary %>% filter(twin_type == "monozygotic") %>% pull(count)
  )

df <- df %>%
  mutate(
    sex_factor = factor(sex_factor),
    siblings_in_this_data.factor = factor(sibling_in_this_data_yn),
    siblings_in_this_data.factor = dplyr::recode(siblings_in_this_data.factor, "0" = "Only Child", "1" = "Sibling")
  ) %>%
  group_by(cm_group, family_id) %>%
  mutate(
    family_size = n()
  ) %>%
  ungroup() %>%
  mutate(
    siblings_type = factor(case_when(
      family_size == 2 ~ "in a sibling pair",
      family_size == 3 ~ "among 3 siblings",
      family_size == 4 ~ "among 4 siblings",
      TRUE ~ NA
    ), levels = c("in a sibling pair", "among 3 siblings", "among 4 siblings")
    )
  ) %>%
  ungroup()


# Tables preparation -----------------------------------------------------------

# --- demographic variables and labels ---
vars_table <- c("siblings_in_this_data.factor", "siblings_type", "twin_type")

labels_dict <- list(
  siblings_in_this_data.factor = "Siblings",
  siblings_type = "... of which",
  twin_type = "... of which Twins"
)

grouping_var <- "cm_group"


# Table ------------------------------------------------------------------------

# --- building table 1 ---
table_siblings <- df %>%
  select(all_of(c(vars_table, grouping_var))) %>%
  tbl_summary(
    by = !!rlang::sym(grouping_var),
    label = labels_dict,
    type = list(
      siblings_in_this_data.factor ~ "categorical",
      siblings_type ~ "categorical",
      twin_type ~ "categorical"
    ),
    statistic = list(
      siblings_in_this_data.factor ~ "{n} ({p}%)",
      siblings_type ~ "{n}",
      twin_type ~ "{n}"
    ),
    missing = "no"
  ) %>%
  add_overall(last = FALSE) %>%
  add_p(
    test = list(
      all_categorical() ~ "chisq.test"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  modify_header(label ~ "**Variable**")
table_siblings


# --- page layout (A4 + 1 inch margins) ---
section_properties <- officer::prop_section(
  page_size = officer::page_size(width = 8.3, height = 11.7),
  page_margins = officer::page_mar(top = 1, bottom = 1, left = 1, right = 1)
)


# --- convert tables to APA style ---
ft <- as_flex_table(table_siblings) %>%
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
  fpar(ftext("Table Sx. Distribution of siblings and twins.", fp_text(font.family = "Times New Roman", font.size = 10, bold = TRUE)))
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
    ftext("CM+, childhood maltreatment exposure; CM–, non-exposed.",
          fp_text(font.family = "Times New Roman", font.size = 10))
  )
)


# --- export ---
out_path <- file.path(path2tables_demographics, paste0("tableSx_siblings_", Sys.Date(), ".docx"))
print(doc, target = out_path)
