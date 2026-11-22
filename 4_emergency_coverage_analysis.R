# 1. Calculate coverage from treated and reconstructed uptake -------------

# Add the available HCW adjustment, then cap the resulting count.
adjust_hcw_count <- function(count, population, gap, total_coverage) {
  addition <- if_else(!is.na(gap) & gap > 0 & !is.na(total_coverage),
                       gap * total_coverage, 0)
  pmin(count + addition, population)
}

# Coverage uses cleaned cumulative administration data and caps values at 100%.
data_cov_time_country <- data_admin_time_interp %>%

  # Calculate coverage in total population.
  mutate(cov_tot_a1d = pmin(adm_tot_a1d_monotone_interp / pop_2021, 1),
         cov_tot_cps = pmin(adm_tot_cps_monotone_interp / pop_2021, 1),
         cov_tot_boost = pmin(adm_tot_boost_monotone_interp / pop_2021, 1),

  # Adjust reconstructed HCW counts and apply the final population cap.
         hcw_flag = if_else(pop_hcw > adm_target_hcw, "Yes", NA_character_),
         hcw_diff = pmax(pop_hcw - adm_target_hcw, 0, na.rm = TRUE),
         adm_hcw_a1d_monotone_interp = adjust_hcw_count(
           adm_hcw_a1d_monotone_interp, pop_hcw, hcw_diff, cov_tot_a1d),
         adm_hcw_cps_monotone_interp = adjust_hcw_count(
           adm_hcw_cps_monotone_interp, pop_hcw, hcw_diff, cov_tot_cps),
         adm_hcw_boost_monotone_interp = adjust_hcw_count(
           adm_hcw_boost_monotone_interp, pop_hcw, hcw_diff, cov_tot_boost),

  # Calculate coverage in priority populations.
         cov_hcw_a1d = adm_hcw_a1d_monotone_interp / pop_hcw,
         cov_hcw_cps = adm_hcw_cps_monotone_interp / pop_hcw,
         cov_hcw_boost = adm_hcw_boost_monotone_interp / pop_hcw,

         cov_old_a1d = adm_old_a1d_monotone_interp / pop_2021_old,
         cov_old_cps = adm_old_cps_monotone_interp / pop_2021_old,
         cov_old_boost = adm_old_boost_monotone_interp / pop_2021_old)


# 2. Summarize country coverage distributions -----------------------------

# Convert the six coverage series to one row per country, month and indicator.
# Use country groupings defined in 3_static_values.R.
cov_acute_summary <- data_cov_time_country %>%
  select(iso, month_name, cov_tot_cps, cov_tot_boost,
         cov_old_cps, cov_old_boost, cov_hcw_cps, cov_hcw_boost) %>%
  pivot_longer(starts_with("cov_"), names_to = c("group", "indicator"),
               names_pattern = "^cov_(.*)_(cps|boost)$", values_to = "coverage") %>%
  inner_join(country_groupings, by = "iso", relationship = "many-to-many") %>%
  group_by(month_name, type, dimension, group, indicator) %>%
  summarise(avg_cov = mean(coverage, na.rm = TRUE),
            med_cov = median(coverage, na.rm = TRUE),
            perc_25 = quantile(coverage, 0.25, na.rm = TRUE),
            perc_75 = quantile(coverage, 0.75, na.rm = TRUE),
            n = sum(!is.na(coverage)), .groups = "drop") %>%
  mutate(type = if_else(type == "all", "global", type)) %>%
  arrange(match(type, c("global", "income", "region", "covax")),
          month_name, dimension, match(group, c("tot", "old", "hcw")),
          match(indicator, c("cps", "boost")))

# Retain the complete end-2023 country dataset for coverage figures.
data_cov_last <- data_cov_time_country %>%
  filter(month_name == as.Date("2023-12-01")) %>%
  select(-month_name)

# 3. Calculate aggregate coverage over time -------------------------------

# An aggregate is available when at least one country contributes an estimate.
sum_available <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

# Select reconstructed counts for aggregation, including the HCW denominator
# gap adjustment above.
data_cov_time_counts <- data_cov_time_country %>%
  select(iso, month_name,
         adm_tot_td_monotone_interp,
         adm_tot_a1d_monotone_interp,
         adm_tot_cps_monotone_interp,
         adm_tot_boost_monotone_interp,
         adm_old_a1d_monotone_interp,
         adm_old_cps_monotone_interp,
         adm_old_boost_monotone_interp,
         adm_hcw_a1d_monotone_interp,
         adm_hcw_cps_monotone_interp,
         adm_hcw_boost_monotone_interp) %>%
  pivot_longer(ends_with("_monotone_interp"),
               names_to = "measure", values_to = "admin") %>%
  mutate(measure = str_remove(measure, "_monotone_interp$"))

# Sum counts within each country grouping and join the fixed population
# of countries with an end-2023 estimate for the corresponding indicator.
data_cov_time_all <- data_cov_time_counts %>%
  inner_join(country_groupings, by = "iso", relationship = "many-to-many") %>%
  group_by(month_name, type, dimension, measure) %>%
  summarise(admin = sum_available(admin), .groups = "drop") %>%
  left_join(population_reporting %>% rename(pop = population),
            by = c("type", "dimension", "measure")) %>%
  arrange(match(type, c("all", "income", "region", "covax")),
          month_name, dimension,
          match(measure, c("adm_tot_td", "adm_tot_a1d", "adm_tot_cps", "adm_tot_boost",
                           "adm_old_a1d", "adm_old_cps", "adm_old_boost",
                           "adm_hcw_a1d", "adm_hcw_cps", "adm_hcw_boost"))) %>%
  mutate(group = case_when(
           str_detect(measure, "^adm_tot_") ~ "Total population",
           str_detect(measure, "^adm_old_") ~ "Older adults",
           str_detect(measure, "^adm_hcw_") ~ "Health and care workers"
         ),
         measure = case_when(
           str_detect(measure, "_td$") ~ "Total doses",
           str_detect(measure, "_a1d$") ~ "At least one dose",
           str_detect(measure, "_cps$") ~ "Complete primary series",
           str_detect(measure, "_boost$") ~ "At least one booster dose"
         ),
         coverage = if_else(measure == "Total doses", NA_real_, admin / pop)) %>%
  select(month_name, measure, admin, pop, group, coverage, type, dimension)

# 4. Prepare figure data --------------------------------------------------

# Figure 1, panel a: end-2023 coverage by income group.
data_fig_1_cov_end2023_inc <- data_cov_last %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "") %>%
  select(iso,
         income_group,
         cov_tot_a1d,
         cov_tot_cps,
         cov_tot_boost,
         cov_old_a1d,
         cov_old_cps,
         cov_old_boost,
         cov_hcw_a1d,
         cov_hcw_cps,
         cov_hcw_boost) %>%
  gather(key = "measure", value = "coverage", -iso, -income_group) %>%
  mutate(group = if_else(str_detect(measure, "cov_tot"), "Total population",
                         if_else(str_detect(measure, "cov_old"), "Older adults",
                                 if_else(str_detect(measure, "cov_hcw"), "Health and care workers", NA))),
         measure = if_else(str_detect(measure, "a1d"), "At least one dose",
                           if_else(str_detect(measure, "cps"), "Complete primary series",
                                   if_else(str_detect(measure, "boost"), "At least one booster dose", NA)))) %>%
  mutate(income_group = factor(income_group, levels = levels_income),
         measure = factor(measure, levels = levels_indicator),
         group = factor(group, levels = levels_groups_short))


# Figure 1, panels b and c: priority group coverage relative to total coverage.
data_fig_2_cov_cps_boost_priority_end2023 <- data_cov_last %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "") %>%
  mutate(income_group = factor(income_group, levels = levels_income)) %>%
  transmute(
    iso,
    income_group,
    # total population
    tot_cps   = cov_tot_cps,
    tot_boost = cov_tot_boost,
    # priority groups
    hcw_cps   = cov_hcw_cps,
    hcw_boost = cov_hcw_boost,
    old_cps   = cov_old_cps,
    old_boost = cov_old_boost
  ) %>%
  pivot_longer(
    cols = c(hcw_cps, hcw_boost, old_cps, old_boost),
    names_to = c("group", "dose"),
    names_pattern = "^(hcw|old)_(cps|boost)$",
    values_to = "y"
  ) %>%
  mutate(group = recode(group, hcw = "Health and care workers",
                        old = "Older adults"),
    group = factor(group, levels = c("Older adults", "Health and care workers")),
    dose = recode(dose, cps = "Complete primary series",
                  boost = "At least one booster dose"),
    dose = factor(dose, levels = c("Complete primary series",
                                   "At least one booster dose")),
    x = if_else(dose == "Complete primary series", tot_cps, tot_boost),
    diff_pp = (y - x) * 100
  ) %>%
  filter(!is.na(x), !is.na(y))


# Supp. Figure 2
data_fig_supp_2_cov_cps_priority_time <- data_cov_time_country %>%
  filter(status_who == "Member State",
         month_name %in% as.Date(c("2021-12-01", "2023-12-01")),
         !is.na(income_group),
         income_group != "") %>%
  mutate(
    time_point = case_when(
      month_name == as.Date("2021-12-01") ~ "End of 2021",
      month_name == as.Date("2023-12-01") ~ "End of 2023"
    ),
    time_point = factor(time_point, levels = c("End of 2021", "End of 2023")),
    income_group = factor(income_group, levels = levels_income)
  ) %>%
  transmute(
    iso,
    time_point,
    income_group,
    tot_cps = cov_tot_cps,
    old_cps = cov_old_cps,
    hcw_cps = cov_hcw_cps
  ) %>%
  pivot_longer(
    cols = c(old_cps, hcw_cps),
    names_to = "group",
    values_to = "y"
  ) %>%
  mutate(
    group = recode(group,
                   old_cps = "Older adults",
                   hcw_cps = "Health and care workers"),
    group = factor(group, levels = c("Older adults", "Health and care workers")),
    x = tot_cps,
    diff_pp = (y - x) * 100
  ) %>%
  filter(!is.na(x), !is.na(y))


# Figure 2, panel a: total doses per 100 population.
data_admin_per100 <- data_cov_time_all %>%
  filter(measure == "Total doses") %>%
  select(month_name, type, dimension, adm_td_tot = admin, population = pop) %>%
  mutate(coverage = (adm_td_tot / population) * 100,
         category = case_when(
           type == "income" ~ "Income group",
           type == "region" ~ "Region",
           type == "covax" ~ "COVAX"
         ))

# Repeat the global comparison curve in each of the three grouping panels.
data_fig_3_admin_time_world <- data_admin_per100 %>%
  filter(type == "all") %>%
  select(month_name, adm_td_tot, coverage) %>%
  crossing(category = c("Income group", "COVAX", "Region")) %>%
  mutate(dimension = "World")

data_fig_3_admin_time_all <- data_admin_per100 %>%
  filter(type != "all") %>%
  select(month_name, adm_td_tot, dimension, coverage, category) %>%
  bind_rows(data_fig_3_admin_time_world) %>%
  mutate(dimension = factor(dimension, levels = levels_standard)) %>%
  arrange(match(category, c("Region", "Income group", "COVAX")),
          dimension, month_name)

# Figure 2, panel b: global population weighted coverage.
data_fig_4_cov_time_world_last <- data_cov_time_all %>%
  filter(type == "all") %>%
  filter(measure != "Total doses") %>%
  mutate(measure = factor(measure, levels = levels_indicator),
         group = factor(group, levels = levels_groups_short)) %>%
  filter(!is.na(coverage)) %>%
  group_by(group, measure) %>%
  arrange(month_name, .by_group = TRUE) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(label = scales::percent(coverage, accuracy = 1))

data_fig_4_cov_time_world <- data_cov_time_all %>%
  filter(type == "all") %>%
  filter(measure != "Total doses") %>%
  mutate(measure = factor(measure, levels = levels_indicator),
         group = factor(group, levels = levels_groups_short)) %>%
  arrange(group, measure, month_name)


# Figure 2, panel c: median and interquartile range of country coverage.
data_fig_2c_global_coverage <- cov_acute_summary %>%
  filter(type == "global",
         month_name < as.Date("2024-01-01")) %>%
  mutate(
    indicator = factor(indicator, levels = c("cps","boost"),
                       labels = c("Complete primary series",
                                  "At least one booster dose")),
    group = factor(group, levels = c("tot","old","hcw"),
                   labels = levels_groups_short)
  )

# Supp. figure 3
data_fig_supp_3_cov_time_weight_all_last <- data_cov_time_all %>%
  filter(measure != "Total doses",
         type == "income") %>%
  mutate(measure = factor(measure, levels = levels_indicator),
         group = factor(group, levels = levels_groups_short),
         dimension = factor(dimension, levels = levels_income)) %>%
  filter(!is.na(coverage)) %>%
  group_by(group, measure, dimension) %>%
  arrange(month_name, .by_group = TRUE) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(label = scales::percent(coverage, accuracy = 1))

data_fig_supp_3_cov_time_weight_all <- data_cov_time_all %>%
  filter(measure != "Total doses",
         type == "income") %>%
  mutate(measure = factor(measure, levels = levels_indicator),
         group = factor(group, levels = levels_groups_short),
         dimension = factor(dimension, levels = levels_income)) %>%
  arrange(group, measure, month_name)


# Supp. figure 4
data_fig_supp_4_cov_mediqr_all <- cov_acute_summary %>%
  filter(type == "income",
         month_name < as.Date("2024-01-01")) %>%
  mutate(dimension = factor(dimension, levels = levels_income),
         indicator = factor(indicator, levels = c("cps","boost"),
                            labels = c("Complete primary series",
                                       "At least one booster dose")),
         group = factor(group, levels = c("tot","old","hcw"),
                        labels = levels_groups_short))


# End ---------------------------------------------------------------------
