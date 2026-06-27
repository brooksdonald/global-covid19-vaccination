# Table 1 - coverage, end-2023, all ---------------------------------------

ref_date <- as.Date("2023-12-01")

type_labels <- c(
  all    = "All",
  income = "Income group",
  region = "WHO region",
  covax  = "COVAX participation category"
)

# section order (subheaders)
type_order <- c("All", "Income group", "WHO region", "COVAX participation category")

# within-section order
covax_order <- c("AMC", "Self-financing")

# short labels for subcolumns
measure_labels <- c(
  "Complete primary series"   = "CPS",
  "At least one booster dose" = "Booster"
)

# desired group order
group_order <- levels_groups_short

# desired output column order
value_cols <- as.vector(
  unlist(lapply(group_order, function(g) paste(g, c("CPS", "Booster"), sep = "___")))
)

base <- data_cov_time_all %>%
  filter(
    month_name == ref_date,
    measure %in% c("Complete primary series", "At least one booster dose")
  ) %>%
  mutate(
    type_title = recode(as.character(type), !!!type_labels),
    type_title = factor(type_title, levels = type_order),
    
    Row = case_when(
      type == "all" ~ "Global",
      is.na(dimension) | dimension == "NA" ~ "All",
      TRUE ~ as.character(dimension)
    ),
    
    group = factor(group, levels = levels_groups_short),
    
    measure_short = case_when(
      measure == "Complete primary series" ~ "CPS",
      measure == "At least one booster dose" ~ "Booster"
    ),
    
    Row_order = case_when(
      type == "all"    ~ 1L,
      type == "income" ~ match(Row, levels_income),
      type == "region" ~ match(Row, levels_region),
      type == "covax"  ~ match(Row, covax_order),
      TRUE ~ NA_integer_
    ),
    Row_order = if_else(is.na(Row_order), 999L, Row_order)
  ) %>%
  filter(!(type == "covax" & Row == "Non-COVAX")) %>%
  group_by(type_title, Row, Row_order, group, measure_short) %>%
  summarise(
    cov = {
      pop_sum <- sum(pop, na.rm = TRUE)
      if (pop_sum > 0) sum(admin, na.rm = TRUE) / pop_sum else NA_real_
    },
    .groups = "drop"
  ) %>%
  mutate(
    value = if_else(is.na(cov), "—", sprintf("%.0f%%", 100 * cov))
  ) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(type_title, Row, Row_order),
    names_from = c(group, measure_short),
    names_sep = "___",
    values_from = value
  ) %>%
  arrange(type_title, Row_order, Row)

# add any missing columns so structure is stable
for (nm in value_cols) {
  if (!nm %in% names(base)) base[[nm]] <- ""
}

base <- base %>%
  select(type_title, Row, Row_order, all_of(value_cols))

make_header_row <- function(title, value_cols) {
  tibble(
    type_title = title,
    Row = as.character(title),
    Row_order = 0L,
    !!!setNames(as.list(rep("", length(value_cols))), value_cols),
    is_header = TRUE
  )
}

ft_data <- base %>%
  mutate(is_header = FALSE) %>%
  group_by(type_title) %>%
  group_split(.keep = TRUE) %>%
  lapply(function(df) bind_rows(make_header_row(unique(df$type_title), value_cols), df)) %>%
  bind_rows() %>%
  arrange(type_title, Row_order, Row) %>%
  rename(Category = Row) %>%
  select(Category, all_of(value_cols), is_header)

# header mapping for flextable
header_map <- tibble(
  col_keys = c("Category", value_cols),
  header1  = c("Category", rep(group_order, each = 2)),
  header2  = c("Category", rep(c("CPS", "Booster"), times = length(group_order)))
)

ft <- flextable(ft_data %>% select(-is_header), col_keys = header_map$col_keys) %>%
  set_header_df(mapping = header_map, key = "col_keys") %>%
  merge_h(part = "header") %>%
  merge_v(part = "header", j = "Category") %>%
  bold(part = "header") %>%
  bold(i = ft_data$is_header, bold = TRUE) %>%
  border(
    i = ft_data$is_header,
    border.top = fp_border(color = "black", width = 1)
  ) %>%
  padding(i = !ft_data$is_header, j = 1, padding.left = 10) %>%
  align(align = "left", part = "all") %>%
  valign(valign = "center", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  autofit()

ft

# Export
export_docx(ft, path = file.path(path_save, "tab_1_end-2023_coverage.docx"))


# Supplementary Table S3 - daily vaccination rate, 2021-2023, income -------

section_order <- c("All", "Income group", "WHO region", "COVAX participation category")

covax_order  <- c("AMC", "Self-financing")  # only used if present

base <- dvr_max %>%
  mutate(
    section = type,          # this is your section label
    Row = dimension,               # this is the level within the section
    adm_date = as.Date(adm_date),
    section = factor(section, levels = section_order),
    
    Row_order = case_when(
      section == "All" ~ 1L,
      section == "Income group" ~ match(Row, levels_income),
      section == "WHO region"   ~ match(Row, levels_region),
      section == "COVAX participation category" ~ match(Row, covax_order),
      TRUE ~ NA_integer_
    ),
    Row_order = if_else(is.na(Row_order), 999L, Row_order),
    
    # format DVR as percent
    value = if_else(is.na(dvr_per), "—", sprintf("%.2f%%", 100 * dvr_per))
  ) %>%
  filter(!(section == "COVAX participation category" & Row == "Non-COVAX")) %>%
  arrange(section, Row_order, Row) %>%
  transmute(section, Row, adm_date, value)

make_header_row <- function(title) {
  tibble(section = title, Row = as.character(title), adm_date = as.Date(NA), value = "", is_header = TRUE)
}

ft_data <- base %>%
  mutate(is_header = FALSE) %>%
  group_by(section) %>%
  group_split(.keep = TRUE) %>%
  lapply(function(df) bind_rows(make_header_row(unique(df$section)), df)) %>%
  bind_rows() %>%
  mutate(adm_date = if_else(is_header, as.Date(NA), adm_date)) %>%
  rename(Category = Row, `Date of peak` = adm_date, `Peak DVR` = value) %>%
  select(Category, `Date of peak`, `Peak DVR`, is_header)

tab_s3_dvr_max <- flextable(ft_data %>% select(-is_header)) %>%
  bold(part = "header") %>%
  bold(i = ft_data$is_header, bold = TRUE) %>%
  border(i = ft_data$is_header,
         border.top = fp_border(color = "black", width = 1)) %>%
  padding(i = !ft_data$is_header, j = 1, padding.left = 10) %>%
  align(align = "left", part = "all") %>%
  valign(valign = "center", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  autofit()

tab_s3_dvr_max

# Export
export_docx(tab_s3_dvr_max, path = file.path(path_save, "tab_supp_3_dvr_max.docx"))


# Table 2 - uptake, 2024, by groups ---------------------------------------

type_order   <- c("all", "income", "region", "policy")

type_titles <- c(
  all    = "All",
  income = "Income group",
  region = "WHO region",
  policy = "Policy category",
  covax  = "COVAX status"
)

base <- data_tab_2_uptake_2024 %>%
  mutate(
    type      = str_trim(tolower(as.character(type))),
    grouping  = str_trim(as.character(grouping)),
    group     = str_trim(as.character(group)),
    indicator = str_trim(as.character(indicator)),
    value     = as.numeric(value),
    
    type_title = dplyr::recode(type, !!!type_titles),
    
    row_order = case_when(
      type == "all"    ~ 0L,
      type == "income" ~ match(grouping, levels_income),
      type == "region" ~ match(grouping, levels_region),
      type == "policy" ~ match(grouping, levels_policy_b),
      TRUE ~ 999L
    ),
    row_order = if_else(is.na(row_order), 999L, row_order)
  ) %>%
  select(type, type_title, grouping, row_order, group, indicator, value)

# (optional) stable indicator ordering: counts -> proportions -> uptake, if present
ind_levels <- base %>%
  distinct(indicator) %>%
  mutate(
    ind_rank = case_when(
      str_detect(tolower(indicator), "^count")      ~ 1L,
      str_detect(tolower(indicator), "^proportion") ~ 2L,
      str_detect(tolower(indicator), "^uptake")     ~ 3L,
      TRUE ~ 99L
    )
  ) %>%
  arrange(ind_rank, indicator) %>%
  pull(indicator)

ind_map <- tibble(
  indicator = ind_levels,
  ind_key   = paste0("i", seq_along(ind_levels))
)

base2 <- base %>%
  left_join(ind_map, by = "indicator") %>%
  mutate(col = paste(group, ind_key, sep = "__")) %>%
  select(type, type_title, grouping, row_order, col, value)

wide <- base2 %>%
  pivot_wider(names_from = col, values_from = value) %>%
  arrange(factor(type, levels = type_order), row_order, grouping)

older_cols <- paste0("Older adults__", ind_map$ind_key)
hcw_cols   <- paste0("Health and care workers__", ind_map$ind_key)

# ensure missing cols exist (robust to missing group/indicator combos)
for (nm in c(older_cols, hcw_cols)) if (!nm %in% names(wide)) wide[[nm]] <- NA_real_

fmt_cell <- function(ind_label, x) {
  if (is.na(x)) return("—")
  lab <- tolower(ind_label)
  
  if (str_detect(lab, "^count")) return(formatC(round(x), format = "f", digits = 0))
  
  # values are proportions (0-1) -> show %
  sprintf("%.0f%%", 100 * x)
}

wide_fmt <- wide
for (k in seq_len(nrow(ind_map))) {
  lab <- ind_map$indicator[k]
  wide_fmt[[older_cols[k]]] <- vapply(wide_fmt[[older_cols[k]]], function(v) fmt_cell(lab, v), character(1))
  wide_fmt[[hcw_cols[k]]]   <- vapply(wide_fmt[[hcw_cols[k]]],   function(v) fmt_cell(lab, v), character(1))
}

make_header_row <- function(title) {
  out <- tibble(type_title = title, grouping = title, row_order = -999L, is_header = TRUE)
  for (nm in c(older_cols, hcw_cols)) out[[nm]] <- ""
  out
}

ft_data <- wide_fmt %>%
  select(type_title, grouping, row_order, all_of(older_cols), all_of(hcw_cols)) %>%
  mutate(is_header = FALSE) %>%
  group_by(type_title) %>%
  group_split(.keep = TRUE) %>%
  lapply(function(df) bind_rows(make_header_row(unique(df$type_title)), df)) %>%
  bind_rows() %>%
  arrange(
    factor(type_title, levels = c("All","Income group","WHO region","Policy category","COVAX status")),
    row_order, grouping
  ) %>%
  rename(Category = grouping)

tab_2_uptake_2024 <- flextable(ft_data %>% select(Category, all_of(older_cols), all_of(hcw_cols)))

labels_list <- c(
  list(Category = "Grouping"),
  setNames(as.list(ind_map$indicator), older_cols),
  setNames(as.list(ind_map$indicator), hcw_cols)
)
tab_2_uptake_2024 <- do.call(set_header_labels, c(list(tab_2_uptake_2024), labels_list))

tab_2_uptake_2024 <- tab_2_uptake_2024 %>%
  add_header_row(
    values    = c("", "Older adults", "Health and care workers"),
    colwidths = c(1, length(older_cols), length(hcw_cols))
  ) %>%
  bold(part = "header") %>%
  bold(i = ft_data$is_header, bold = TRUE) %>%
  border(i = ft_data$is_header, border.top = fp_border(color = "black", width = 1)) %>%
  padding(i = !ft_data$is_header, j = 1, padding.left = 10) %>%
  align(align = "left", part = "all") %>%
  valign(valign = "center", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  autofit()

tab_2_uptake_2024

# Export
export_docx(tab_2_uptake_2024,
             path = file.path(path_save, "tab_2_2024_uptake.docx"))
