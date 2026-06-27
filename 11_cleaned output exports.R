# Export cleaned analytic CSVs -------------------------------------------

# These files expose the cleaned analysis-ready datasets underlying the
# manuscript figures and tables. They are derived from the same objects used by
# the primary analysis scripts.

export_cleaned_csv <- function(data, filename) {
  if (isTRUE(export_outputs)) {
    write.csv(
      data,
      file = file.path(path_save, filename),
      row.names = FALSE,
      na = ""
    )
  }
}


# Emergency-period monthly coverage ---------------------------------------

cleaned_emergency_monthly_coverage_country <- data_cov_time_country %>%
  transmute(
    record_type = "country",
    iso,
    month = month_name,
    grouping_type = "country",
    grouping = iso,
    region_who,
    income_group,
    status_who,
    status_covax,
    hcw_denominator_adjustment_applied = hcw_flag,
    hcw_denominator_gap = hcw_diff,
    denominator_tot = pop_2021,
    denominator_old = pop_2021_old,
    denominator_hcw = pop_hcw,
    adm_tot_td = adm_tot_td_monotone_interp,
    adm_tot_a1d = adm_tot_a1d_monotone_interp,
    adm_tot_cps = adm_tot_cps_monotone_interp,
    adm_tot_boost = adm_tot_boost_monotone_interp,
    adm_old_a1d = adm_old_a1d_monotone_interp,
    adm_old_cps = adm_old_cps_monotone_interp,
    adm_old_boost = adm_old_boost_monotone_interp,
    adm_hcw_a1d = adm_hcw_a1d_monotone_interp,
    adm_hcw_cps = adm_hcw_cps_monotone_interp,
    adm_hcw_boost = adm_hcw_boost_monotone_interp,
    coverage_tot_td = NA_real_,
    coverage_tot_a1d = cov_tot_a1d,
    coverage_tot_cps = cov_tot_cps,
    coverage_tot_boost = cov_tot_boost,
    coverage_old_a1d = cov_old_a1d,
    coverage_old_cps = cov_old_cps,
    coverage_old_boost = cov_old_boost,
    coverage_hcw_a1d = cov_hcw_a1d,
    coverage_hcw_cps = cov_hcw_cps,
    coverage_hcw_boost = cov_hcw_boost
  ) %>%
  pivot_longer(
    cols = matches("^(adm|coverage)_"),
    names_to = c(".value", "measure_code"),
    names_pattern = "^(adm|coverage)_(.*)$"
  ) %>%
  mutate(
    population_group = case_when(
      str_detect(measure_code, "^tot") ~ "Total population",
      str_detect(measure_code, "^old") ~ "Older adults",
      str_detect(measure_code, "^hcw") ~ "Health and care workers",
      TRUE ~ NA_character_
    ),
    indicator = case_when(
      str_detect(measure_code, "td$") ~ "Total doses",
      str_detect(measure_code, "a1d$") ~ "At least one dose",
      str_detect(measure_code, "cps$") ~ "Complete primary series",
      str_detect(measure_code, "boost$") ~ "At least one booster dose",
      TRUE ~ NA_character_
    ),
    denominator = case_when(
      population_group == "Total population" & indicator != "Total doses" ~
        denominator_tot,
      population_group == "Older adults" ~ denominator_old,
      population_group == "Health and care workers" ~ denominator_hcw,
      TRUE ~ NA_real_
    ),
    coverage_pct = 100 * coverage
  ) %>%
  transmute(
    record_type,
    iso,
    month,
    grouping_type,
    grouping,
    region_who,
    income_group,
    status_who,
    status_covax,
    population_group,
    indicator,
    cumulative_admin = adm,
    denominator,
    coverage,
    coverage_pct,
    hcw_denominator_adjustment_applied,
    hcw_denominator_gap
  )

cleaned_emergency_monthly_coverage_group <- data_cov_time_all %>%
  transmute(
    record_type = "group",
    iso = NA_character_,
    month = month_name,
    grouping_type = type,
    grouping = if_else(is.na(dimension), "Global", dimension),
    region_who = NA_character_,
    income_group = NA_character_,
    status_who = NA_character_,
    status_covax = NA_character_,
    population_group = group,
    indicator = measure,
    cumulative_admin = admin,
    denominator = pop,
    coverage,
    coverage_pct = 100 * coverage,
    hcw_denominator_adjustment_applied = NA_character_,
    hcw_denominator_gap = NA_real_
  )

cleaned_emergency_monthly_coverage <- bind_rows(
  cleaned_emergency_monthly_coverage_country,
  cleaned_emergency_monthly_coverage_group
) %>%
  arrange(record_type, grouping_type, grouping, iso, population_group,
          indicator, month)

export_cleaned_csv(
  cleaned_emergency_monthly_coverage,
  "data_emergency_monthly_coverage.csv"
)


# Emergency-period daily vaccination rate ---------------------------------

cleaned_dvr_daily_country <- data_admin_daily %>%
  filter(adm_date <= as.Date("2023-12-31")) %>%
  transmute(
    record_type = "country",
    iso,
    adm_date,
    grouping_type = NA_character_,
    grouping = NA_character_,
    region_who,
    income_group,
    status_who,
    status_covax,
    population = pop_2021,
    dvr_4wk_doses = dvr_4wk_td,
    dvr_4wk_rate = dvr_4wk_td_per,
    dvr_4wk_rate_pct = 100 * dvr_4wk_rate,
    median_rate = NA_real_,
    percentile_10 = NA_real_,
    percentile_90 = NA_real_,
    aggregate_rate = NA_real_,
    aggregate_rate_pct = NA_real_
  )

cleaned_dvr_daily_group_weighted <- dvr_weighted %>%
  filter(adm_date <= as.Date("2023-12-31")) %>%
  transmute(
    record_type = "group_weighted",
    iso = NA_character_,
    adm_date,
    grouping_type = type,
    grouping = dimension,
    region_who = NA_character_,
    income_group = NA_character_,
    status_who = NA_character_,
    status_covax = NA_character_,
    population = pop,
    dvr_4wk_doses = dvr,
    dvr_4wk_rate = dvr_per,
    dvr_4wk_rate_pct = 100 * dvr_4wk_rate,
    median_rate = NA_real_,
    percentile_10 = NA_real_,
    percentile_90 = NA_real_,
    aggregate_rate = dvr_per,
    aggregate_rate_pct = 100 * aggregate_rate
  )

cleaned_dvr_daily_group_distribution <- dvr_summary %>%
  filter(adm_date <= as.Date("2023-12-31")) %>%
  transmute(
    record_type = "group_country_distribution",
    iso = NA_character_,
    adm_date,
    grouping_type = type,
    grouping = dimension,
    region_who = NA_character_,
    income_group = NA_character_,
    status_who = NA_character_,
    status_covax = NA_character_,
    population = NA_real_,
    dvr_4wk_doses = NA_real_,
    dvr_4wk_rate = NA_real_,
    dvr_4wk_rate_pct = NA_real_,
    median_rate,
    percentile_10 = perc_10,
    percentile_90 = perc_90,
    aggregate_rate,
    aggregate_rate_pct = 100 * aggregate_rate
  )

cleaned_dvr_daily <- bind_rows(
  cleaned_dvr_daily_country,
  cleaned_dvr_daily_group_weighted,
  cleaned_dvr_daily_group_distribution
) %>%
  arrange(record_type, grouping_type, grouping, iso, adm_date)

export_cleaned_csv(cleaned_dvr_daily, "data_dvr_daily.csv")


# Routine-period 2024 uptake ----------------------------------------------

cleaned_2024_uptake_country <- data_2024_uptake_country_audit %>%
  transmute(
    record_type = "country",
    grouping_type = "country",
    grouping = iso,
    iso,
    region_who,
    income_group,
    status_who,
    population_group = group,
    uptake_2024_raw = annual_uptake_raw,
    uptake_2024 = annual_uptake_capped,
    denominator,
    coverage,
    coverage_pct = 100 * coverage,
    capped,
    endpoint_method,
    endpoint_correction_id,
    population_all = NA_real_,
    population_reporting = NA_real_,
    countries_reporting = NA_integer_,
    proportion_population_reporting = NA_real_
  )

cleaned_2024_uptake_group <- data_2024_uptake_grouping_audit %>%
  transmute(
    record_type = "group",
    grouping_type = type,
    grouping,
    iso = NA_character_,
    region_who = NA_character_,
    income_group = NA_character_,
    status_who = NA_character_,
    population_group = group,
    uptake_2024_raw = NA_real_,
    uptake_2024 = cumulative_q4,
    denominator = population_reporting,
    coverage,
    coverage_pct = 100 * coverage,
    capped = NA,
    endpoint_method = NA_character_,
    endpoint_correction_id = NA_character_,
    population_all,
    population_reporting,
    countries_reporting = count_reporting,
    proportion_population_reporting
  )

cleaned_2024_uptake <- bind_rows(
  cleaned_2024_uptake_country,
  cleaned_2024_uptake_group
) %>%
  arrange(record_type, grouping_type, grouping, population_group, iso)

export_cleaned_csv(cleaned_2024_uptake, "data_2024_uptake.csv")


# Routine-period 2024 policy ----------------------------------------------

cleaned_2024_policy <- data_routine_policy_2024 %>%
  transmute(
    iso,
    region_who,
    income_group,
    population_group = group,
    policy_revacc_interval,
    status_policy
  ) %>%
  arrange(iso, population_group)

export_cleaned_csv(cleaned_2024_policy, "data_2024_policy.csv")
