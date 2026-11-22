# Table 1 - coverage, end-2023, all ---------------------------------------

ref_date <- as.Date("2023-12-01")

type_labels <- c(
  all    = "All",
  income = "Income group",
  region = "WHO region",
  covax  = "COVAX participation category"
)

# Table sections appear in manuscript order.
type_order <- c("All", "Income group", "WHO region", "COVAX participation category")

# COVAX table rows include participating groups only.
covax_order <- c("AMC", "Self-financing")

# Population groups are ordered consistently across tables.
group_order <- levels_groups_short

# Interleave CPS and booster columns within each population group.
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

# Add empty columns for missing population group and measure combinations.
for (nm in value_cols) {
  if (!nm %in% names(base)) base[[nm]] <- ""
}

base <- base %>%
  select(type_title, Row, Row_order, all_of(value_cols))

make_coverage_header <- function(title, value_cols) {
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
  lapply(function(df) bind_rows(make_coverage_header(unique(df$type_title), value_cols), df)) %>%
  bind_rows() %>%
  arrange(type_title, Row_order, Row) %>%
  rename(Category = Row) %>%
  select(Category, all_of(value_cols), is_header)

# Map internal column keys to the two-row flextable header.
header_map <- tibble(
  col_keys = c("Category", value_cols),
  header1  = c("Category", rep(group_order, each = 2)),
  header2  = c("Category", rep(c("CPS", "Booster"), times = length(group_order)))
)

table_1_header_rule <- fp_border(color = "black", width = 0.5)

ft <- flextable(ft_data %>% select(-is_header), col_keys = header_map$col_keys) %>%
  set_header_df(mapping = header_map, key = "col_keys") %>%
  merge_h(part = "header") %>%
  merge_v(part = "header", j = "Category") %>%
  bold(part = "header") %>%
  bold(i = ft_data$is_header, bold = TRUE) %>%
  border_remove() %>%
  hline_top(border = fp_border(color = "black", width = 1.5), part = "header") %>%
  hline(i = 1, j = value_cols, border = table_1_header_rule, part = "header") %>%
  hline_bottom(border = table_1_header_rule, part = "header") %>%
  hline_bottom(border = fp_border(color = "#666666", width = 1.5), part = "body") %>%
  padding(i = !ft_data$is_header, j = 1, padding.left = 10) %>%
  align(align = "left", part = "all") %>%
  valign(valign = "center", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  autofit()

ft

# Export
export_docx(ft, path = file.path(path_save, "tab_1_end-2023_coverage.docx"))


# Use the manuscript's A4 portrait page dimensions.
portrait_section <- prop_section(
  page_size = page_size(width = 8.27, height = 11.69, orient = "portrait"),
  page_margins = page_mar(top = 1, bottom = 1, left = 1, right = 1))

wide_table_section <- prop_section(
  page_size = page_size(width = 8.27, height = 11.69, orient = "portrait"),
  page_margins = page_mar(top = 0.75, bottom = 0.75, left = 0.75, right = 0.75))

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

# Order counts, reporting coverage and uptake within each population group.
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

# Add empty group-indicator columns if a combination is absent in the data.
for (nm in c(older_cols, hcw_cols)) if (!nm %in% names(wide)) wide[[nm]] <- NA_real_

fmt_cell <- function(ind_label, x) {
  if (is.na(x)) return("—")
  lab <- tolower(ind_label)

  if (str_detect(lab, "^count")) return(formatC(round(x), format = "f", digits = 0))

  # Proportions are stored on a 0-1 scale and displayed as percentages.
  sprintf("%.0f%%", 100 * x)
}

wide_fmt <- wide
for (k in seq_len(nrow(ind_map))) {
  lab <- ind_map$indicator[k]
  wide_fmt[[older_cols[k]]] <- vapply(wide_fmt[[older_cols[k]]], function(v) fmt_cell(lab, v), character(1))
  wide_fmt[[hcw_cols[k]]]   <- vapply(wide_fmt[[hcw_cols[k]]],   function(v) fmt_cell(lab, v), character(1))
}

make_uptake_header <- function(title) {
  out <- tibble(type_title = title, grouping = title, row_order = -999L, is_header = TRUE)
  for (nm in c(older_cols, hcw_cols)) out[[nm]] <- ""
  out
}

ft_data <- wide_fmt %>%
  select(type_title, grouping, row_order, all_of(older_cols), all_of(hcw_cols)) %>%
  mutate(is_header = FALSE) %>%
  group_by(type_title) %>%
  group_split(.keep = TRUE) %>%
  lapply(function(df) bind_rows(make_uptake_header(unique(df$type_title)), df)) %>%
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

# Fit both population groups within the portrait text width.
tab_2_uptake_2024 <- tab_2_uptake_2024 %>%
  set_table_properties(layout = "fixed") %>%
  width(j = 1, width = 1.57) %>%
  width(j = c(2, 4, 5, 7), width = 0.80) %>%
  width(j = c(3, 6), width = 0.99)

tab_2_uptake_2024

# Export
export_docx(tab_2_uptake_2024,
             path = file.path(path_save, "tab_2_2024_uptake.docx"),
             pr_section = wide_table_section)

# Table 3 - peak daily vaccination rates, 2021-2023 ----------------------

# Use the same grouping and COVAX row order as Table 1.
section_order <- unname(type_labels)

base <- dvr_max %>%
  ungroup() %>%
  left_join(
    dvr_country_peak_summary %>%
      mutate(type = unname(type_labels[type]), dimension = coalesce(dimension, "Global")),
    by = c("type", "dimension")
  ) %>%
  mutate(
    section = type,
    Row = dimension,
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

    # Express peak rates as doses administered per 100 population per day.
    value = if_else(is.na(dvr_per), "—", sprintf("%.2f", 100 * dvr_per)),
    country_peaks = if_else(is.na(peak_median), "-",
                            sprintf("%.2f (%.2f-%.2f)", peak_median, peak_q25, peak_q75))
  ) %>%
  filter(!(section == "COVAX participation category" & Row == "Non-COVAX")) %>%
  arrange(section, Row_order, Row) %>%
  transmute(section, Row, adm_date, value, country_peaks)

make_dvr_header <- function(title) {
  tibble(section = title, Row = as.character(title), adm_date = as.Date(NA),
         value = "", country_peaks = "", is_header = TRUE)
}

ft_data <- base %>%
  mutate(is_header = FALSE) %>%
  group_by(section) %>%
  group_split(.keep = TRUE) %>%
  lapply(function(df) bind_rows(make_dvr_header(unique(df$section)), df)) %>%
  bind_rows() %>%
  mutate(adm_date = if_else(is_header, as.Date(NA), adm_date)) %>%
  rename(Category = Row) %>%
  select(Category, adm_date, value, country_peaks, is_header)

tab_3_dvr_max <- flextable(ft_data %>% select(-is_header)) %>%
  set_header_labels(adm_date = "Date of population-weighted peak",
                    value = "Population-weighted", country_peaks = "Country-specific, median (IQR)") %>%
  add_header_row(values = c("Category", "Date of population-weighted peak",
                            "Peak daily vaccination rate\n(doses per 100 population per day)"),
                 colwidths = c(1, 1, 2)) %>%
  merge_v(j = c("Category", "adm_date"), part = "header") %>%
  bold(part = "header") %>%
  bold(i = ft_data$is_header, bold = TRUE) %>%
  border(i = ft_data$is_header,
         border.top = fp_border(color = "black", width = 1)) %>%
  padding(i = !ft_data$is_header, j = 1, padding.left = 10) %>%
  align(align = "left", part = "all") %>%
  valign(valign = "center", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  set_table_properties(layout = "fixed") %>%
  width(j = 1:4, width = c(1.50, 1.48, 1.55, 1.72))

tab_3_dvr_max

# Export
export_docx(tab_3_dvr_max, path = file.path(path_save, "tab_3_dvr_max.docx"),
             pr_section = portrait_section)


# Supplementary Table S4 - reporting completeness, 2024 -------------------

data_tab_s4_reporting_2024 <- data_2024_uptake_sensitivity %>%
  filter(type == "all") %>%
  arrange(factor(group, levels = c("older", "hcw"))) %>%
  transmute(
    population = recode(group, older = "Older adults", hcw = "Health and care workers"),
    countries_end_year = count_reporting_complete_year,
    population_end_year = 100 * proportion_population_reporting_complete_year,
    uptake_end_year = 100 * uptake_complete_year,
    countries_quarterly_only = count_quarterly_carry_forward,
    population_combined = 100 * proportion_population_reporting_all_reporters,
    uptake_combined = 100 * uptake_all_reporters,
    difference_pp = if_else(is.na(uptake_difference_pp), "—",
                            sprintf("%+.0f", uptake_difference_pp)),
    population_difference_pp = if_else(
      is.na(population_combined - population_end_year), "—",
      sprintf("%+.0f", population_combined - population_end_year)
    )
  )

tab_s4_reporting_2024 <- flextable(data_tab_s4_reporting_2024) %>%
  set_header_labels(
    population = "Population",
    countries_end_year = "Count of end-of-year reporting MS",
    population_end_year = "Proportion of grouping population in reporting MS",
    uptake_end_year = "Uptake (%) among reporting MS",
    countries_quarterly_only = "Count of additional MS with quarterly reports only",
    population_combined = "Proportion of grouping population in reporting MS",
    uptake_combined = "Uptake (%) among reporting MS",
    difference_pp = "Uptake",
    population_difference_pp = "Population in reporting MS"
  ) %>%
  add_header_row(
    values = c("Population", "End-of-year reporting MS",
               "End-of-year and quarterly\nreporting MS", "Difference (pp)"),
    colwidths = c(1, 3, 3, 2)
  ) %>%
  merge_v(j = "population", part = "header") %>%
  colformat_double(
    j = c("population_end_year", "uptake_end_year",
          "population_combined", "uptake_combined"),
    digits = 0, suffix = "%", na_str = "—"
  ) %>%
  theme_booktabs(bold_header = TRUE) %>%
  hline(i = 1, j = 2:9, border = fp_border(color = "#666666", width = 0.75),
        part = "header") %>%
  font(fontname = "Helvetica", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  color(color = "black", part = "all") %>%
  align(align = "left", part = "all") %>%
  valign(valign = "center", part = "all") %>%
  padding(padding = 5, part = "all") %>%
  set_table_properties(layout = "fixed", align = "left") %>%
  width(j = 1:9, width = c(0.80, 0.72, 0.82, 0.70, 0.82, 0.82, 0.70, 0.57, 0.81)) %>%
  height_all(height = 0.22, part = "body") %>%
  hrule(rule = "atleast", part = "all")

# Export
export_docx(tab_s4_reporting_2024,
             path = file.path(path_save, "tab_supp_4_2024_reporting_completeness.docx"),
             pr_section = wide_table_section)


# Supplementary Table S5 - capping by population group and period -------------------------

coverage_indicator_labels <- c(a1d = "At least one dose", cps = "Complete primary series",
                               boost = "Booster doses")

format_supplementary_table <- function(table, widths, header_columns = NULL, font_size = 10) {
  table <- table %>% theme_booktabs(bold_header = TRUE)
  if (!is.null(header_columns)) {
    table <- hline(table, i = 1, j = header_columns,
                   border = fp_border(color = "#666666", width = 0.75), part = "header")
  }
  table %>%
    font(fontname = "Helvetica", part = "all") %>%
    fontsize(size = font_size, part = "all") %>%
    color(color = "black", part = "all") %>%
    align(align = "left", part = "all") %>%
    valign(valign = "center", part = "all") %>%
    padding(padding = 5, part = "all") %>%
    set_table_properties(layout = "fixed", align = "left") %>%
    width(j = seq_along(widths), width = widths) %>%
    hrule(rule = "atleast", part = "all")
}

format_capped_count <- function(count, proportion) {
  if_else(is.finite(proportion),
          sprintf("%s (%.1f%%)", scales::comma(count, accuracy = 1), 100 * proportion),
          "—")
}

data_tab_s5_capping <- bind_rows(
  "2021–2023 (monthly estimates)" = data_capping_global,
  "2024 (annual estimates)" = data_capping_2024,
  .id = "period"
) %>%
  arrange(period, factor(population_group, levels = processing_groups),
          factor(stage, levels = c("Without HCW adjustment", "With HCW adjustment")),
          factor(indicator, levels = names(coverage_indicator_labels))) %>%
  transmute(
    period,
    population = case_when(stage == "Without HCW adjustment" ~ "HCWs without adjustment",
                           stage == "With HCW adjustment" ~ "HCWs with adjustment",
                           TRUE ~ population_group),
    indicator = recode(indicator, !!!coverage_indicator_labels, annual_uptake = "Annual uptake"),
    countries, estimates,
    countries_capped = format_capped_count(countries_capped, countries_capped / countries),
    estimates_capped = format_capped_count(estimates_capped, proportion_capped)
  ) %>%
  as_grouped_data(groups = "period")

tab_s5_capping <- as_flextable(data_tab_s5_capping, hide_grouplabel = TRUE) %>%
  set_header_labels(population = "Population group", indicator = "Indicator",
    countries = "Countries with estimates (n)", estimates = "Estimates (n)",
    countries_capped = "Countries capped, n (%)", estimates_capped = "Estimates capped, n (%)") %>%
  merge_v(j = "population", part = "body") %>%
  format_supplementary_table(widths = c(1.26, 1.27, 1.00, 0.92, 1.10, 1.20)) %>%
  bold(i = ~ !is.na(period), part = "body") %>%
  hline(i = ~ !is.na(population) & population != lead(population),
        border = fp_border(color = "#666666", width = 0.5), part = "body")

export_docx(tab_s5_capping, path = file.path(path_save, "tab_supp_5_capping.docx"),
             pr_section = wide_table_section)


# Supplementary Table S3 - processing sequence ----------------------------

data_tab_s3_processing <- data_processing_summary %>%
  filter(indicator == "All indicators", stage_id != "cps_filter") %>%
  transmute(order = if_else(stage_id == "maximum_cps", "1c", order),
            rule = paste0(step, "\n", rule), population_group,
            cell = if_else(is.na(affected), "",
                           paste0(scales::comma(affected, accuracy = 1), " (",
                                  scales::comma(countries, accuracy = 1), ")"))) %>%
  pivot_wider(names_from = population_group, values_from = cell) %>%
  bind_rows(tibble(
    order = c("1", "3", "4"),
    rule = c("Preparation of reported counts", "Data anomaly treatment", "Reconstruction of cumulative counts"),
    section_header = TRUE
  )) %>%
  mutate(order_number = parse_number(order),
         section_header = replace_na(section_header, FALSE)) %>%
  arrange(order_number, order)

tab_s3_processing <- flextable(data_tab_s3_processing,
                               col_keys = c("order", "rule", processing_groups)) %>%
  set_header_labels(order = "Order", rule = "Processing step and rule") %>%
  add_header_row(values = c("Order", "Processing step and rule", "Affected units (countries)"),
                colwidths = c(1, 1, 3)) %>%
  merge_v(j = c("order", "rule"), part = "header") %>%
  format_supplementary_table(
    widths = c(0.53, 3.42, 0.96, 0.91, 0.93), header_columns = 3:5, font_size = 9.5) %>%
  italic(i = data_tab_s3_processing$section_header, part = "body") %>%
  hline(i = which(diff(data_tab_s3_processing$order_number) != 0),
        border = fp_border(color = "#666666", width = 0.5), part = "body") %>%
  set_table_properties(opts_word = list(split = FALSE, keep_with_next = FALSE,
                                        repeat_headers = TRUE)) %>%
  paginate(init = FALSE, hdr_ftr = TRUE)

export_docx(tab_s3_processing,
  path = file.path(path_save, "tab_supp_3_processing_sequence.docx"),
  pr_section = wide_table_section, align = "left")

# Supplementary Table S6 - reports underlying end-2023 estimates ------------

recency_indicator_labels <- c(td = "Total doses administered", coverage_indicator_labels)

data_tab_s6_recency <- data_processing_year_end %>%
  filter(month_name == ref_date, indicator %in% names(recency_indicator_labels)) %>%
  arrange(factor(population_group, levels = processing_groups),
          factor(indicator, levels = names(recency_indicator_labels))) %>%
  transmute(population_group, indicator = recode(indicator, !!!recency_indicator_labels),
            carried = paste0(carried_forward, "/", countries_with_estimates),
            days = if_else(is.na(median_carry_forward_days), "-",
                           sprintf("%.0f (%.0f-%.0f)", median_carry_forward_days,
                                   q25_carry_forward_days, q75_carry_forward_days)))

tab_s6_recency <- flextable(data_tab_s6_recency) %>%
  set_header_labels(population_group = "Population group", indicator = "Indicator",
    carried = "Estimates carried forward (n/N)", days = "Days since last report, median (IQR)") %>%
  merge_v(j = "population_group", part = "body") %>%
  format_supplementary_table(widths = c(1.46, 1.68, 1.44, 1.67)) %>%
  hline(i = which(!duplicated(data_tab_s6_recency$population_group))[-1] - 1L,
        border = fp_border(color = "#666666", width = 0.5), part = "body")

export_docx(tab_s6_recency, path = file.path(path_save, "tab_supp_6_processing_year_end.docx"),
             pr_section = portrait_section)


# End ---------------------------------------------------------------------
