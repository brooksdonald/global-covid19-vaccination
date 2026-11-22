# Administrative uptake preparation, treatment and reconstruction
# Maintainer: BROOKS, Donald J.
# Run through 0_master.R. Source reports remain separate from reconstructed estimates.

# 1. Stage submitted reports ----------------------------------------------

# Select reports with non-zero CPS counts. Coverage retains tied maximum
# counts; reporting summaries select one row per country, date and group.
select_target_report_rows <- function(data, with_ties = TRUE) {
  data %>%
    filter(!is.na(N_VACC_LAST_DOSE), N_VACC_LAST_DOSE != 0) %>%
    mutate(DATE = as.Date(DATE)) %>%
    group_by(ISO_3_CODE, DATE, TARGET_GROUP) %>%
    slice_max(N_VACC_LAST_DOSE, n = 1, with_ties = with_ties) %>%
    ungroup()
}

# Restrict the coverage inputs to the study period before selecting reports.
target_reports_monthly <- base_target %>%
  filter(TARGET_GROUP %in% c("HW", "OLDER_60"),
         as.Date(DATE) >= as.Date("2021-01-01"),
         as.Date(DATE) <= as.Date("2023-12-31")) %>%
  select_target_report_rows()

# Collapse equal dated indicator values after whole-report selection.
target_report_values <- target_reports_monthly %>%
  transmute(iso = ISO_3_CODE, month_name = DATE, target_group = TARGET_GROUP,
    source_id = Sys_ID, a1d = as.numeric(N_VACC_DOSE1),
    cps = as.numeric(N_VACC_LAST_DOSE), boost = as.numeric(N_VACC_BOOSTER_DOSE)) %>%
  pivot_longer(c(a1d, cps, boost), names_to = "indicator", values_to = "reported") %>%
  group_by(iso, month_name, target_group, indicator) %>%
  mutate(preparation_action = case_when(
    is.na(reported) ~ "Originally missing",
    duplicated(reported) ~ "Identical value collapsed",
    TRUE ~ "Canonical observation")) %>%
  ungroup()

target_counts_monthly <- target_report_values %>%
  semi_join(entity_pop %>%
    filter(status_who == "Member State" | iso %in% c("PSE", "XKX")), by = "iso") %>%
  distinct(iso, month_name, target_group, indicator, reported) %>%
  pivot_wider(names_from = indicator, values_from = reported, values_fn = unique)

# Select uptake fields for HCWs and older adults.
admin_hcw_monthly <- target_counts_monthly %>%
  filter(target_group == "HW") %>%
  transmute(iso, month_name, adm_hcw_a1d_raw = a1d,
            adm_hcw_cps_raw = cps, adm_hcw_boost_raw = boost)

admin_old_monthly <- target_counts_monthly %>%
  filter(target_group == "OLDER_60") %>%
  transmute(iso, month_name, adm_old_a1d_raw = a1d,
            adm_old_cps_raw = cps, adm_old_boost_raw = boost)

data_admin_target_monthly <- full_join(
  admin_hcw_monthly, admin_old_monthly, by = c("iso", "month_name"))

# Create one row per country and month, including months without reports.
data_admin_time_scaffold <- entity_pop %>%
  select(a_iso = iso) %>%
  crossing(month_name = seq(as.Date("2021-01-01"),
                            as.Date("2023-12-01"),
                            by = "1 month"))

# Join submitted priority group values to the complete monthly calendar.
# Total population estimates are reconstructed from dated reports below.
data_admin_time_staged <- data_admin_time_scaffold %>%
  left_join(data_admin_target_monthly,
            by = c("a_iso" = "iso", "month_name" = "month_name"))

# Join staged uptake data with entity details and analysis denominators.
data_admin_time <- data_admin_time_staged %>%
  left_join(., entity_pop %>% select(-pop_2024,
                                     -pop_2024_old,
                                     -pol_old,
                                     -pol_old_source),
            by = c("a_iso" = "iso")) %>%
  rename(iso = a_iso) %>%
  filter(status_who == "Member State" | iso == "PSE" | iso == "XKX",
         is.na(month_name) == FALSE) %>%
  mutate(
    # Retain priority group counts for cleaning and reconstruction before capping.
    adm_hcw_a1d = adm_hcw_a1d_raw,
    adm_hcw_cps = adm_hcw_cps_raw,
    adm_hcw_boost = adm_hcw_boost_raw,
    adm_old_a1d = adm_old_a1d_raw,
    adm_old_cps = adm_old_cps_raw,
    adm_old_boost = adm_old_boost_raw
  )

# Apply documented manual time-series corrections before monotonicity cleaning.
time_series_corrections <- data_corrections %>%
  filter(
    analysis == "acute_2021_2023",
    action == "set_missing"
  ) %>%
  transmute(
    correction_id,
    iso,
    month_name = as.Date(period),
    indicator,
    match_value = as.numeric(match_value)
  )

# Check correction variables and unique country-month-indicator combinations.
stopifnot(
  all(time_series_corrections$indicator %in% names(data_admin_time)),
  !anyDuplicated(
    time_series_corrections[c("iso", "month_name", "indicator")]
  )
)

for (i in seq_len(nrow(time_series_corrections))) {
  rule <- time_series_corrections[i, , drop = FALSE]
  matched <- data_admin_time$iso == rule$iso &
    data_admin_time$month_name == rule$month_name

  stopifnot(sum(matched) == 1L)

  observed <- data_admin_time[[rule$indicator]][matched]
  stopifnot(
    length(observed) == 1L,
    !is.na(observed),

    # Confirm the observed value matches the documented correction.
    abs(observed - rule$match_value) < 0.5
  )

  data_admin_time[[rule$indicator]][matched] <- NA_real_
}


# 2. Treat priority group counts at the end of each reporting month --------

vars_priority <- c("adm_old_a1d", "adm_old_cps", "adm_old_boost",
                   "adm_hcw_a1d", "adm_hcw_cps", "adm_hcw_boost")
priority_sources <- data_admin_time %>%
  select(iso, month_name, date_intro, pop_2021_old, pop_hcw, all_of(vars_priority)) %>%
  pivot_longer(all_of(vars_priority), names_to = "indicator", values_to = "reported") %>%
  mutate(date = ceiling_date(month_name, "month") - days(1),
         population = if_else(str_detect(indicator, "^adm_old_"), pop_2021_old, pop_hcw),
         source_id = paste(iso, indicator, month_name, sep = ":")) %>%
  arrange(iso, indicator, date)

priority_processed <- lapply(split(priority_sources,
                                   paste(priority_sources$iso, priority_sources$indicator)),
  function(source) {
    treated <- treat_count_series(source$reported, source$date)
    audit <- bind_cols(source,
      as_tibble(treated[c("after_negative", "after_zero", "after_downward")]),
      treated$flags)
    decisions <- treated$decisions
    if (nrow(decisions)) decisions <- decisions %>%
      mutate(iso = source$iso[1], indicator = source$indicator[1])
    list(audit = audit, decisions = decisions)
  })
priority_audit <- bind_rows(lapply(priority_processed, `[[`, "audit"))
priority_decisions <- bind_rows(lapply(priority_processed, `[[`, "decisions"))

# 3. Prepare dated total population reports -------------------------------

message("Reconstructing total population series from dated reports")
vars_admin <- c("adm_tot_td", "adm_tot_a1d", "adm_tot_cps", "adm_tot_boost")
# Record treatment and reconstruction settings.
retro_settings <- tibble(
  negative_treatment = "exclude", zero_treatment = "exclude_after_positive",
  interindicator_treatment = "independent", anomaly_method = "contextual_exclusion",
  reporting_history = "all_available", repetition_treatment = "retain",
  rapid_treatment = "retain", population_treatment = "retain",
  introduction_date = "documented", cps_delay_months = 2L,
  interpolation = "linear_elapsed_days", trailing_fill = "carry_forward",
  round_reconstructed = TRUE, final_person_cap = TRUE
)

# Include WHO Member States, Kosovo and occupied Palestinian territory.
# Exclude Eritrea and DPR Korea, where vaccination had not been introduced.
retro_population <- entity_pop %>%
  filter(status_who == "Member State" | iso %in% c("PSE", "XKX")) %>%
  filter(!iso %in% c("ERI", "PRK")) %>%
  select(iso, region_who, income_group, pop_2021, date_intro)
retro_test("cohort_194", nrow(retro_population) == 194L &&
             !anyDuplicated(retro_population$iso))
retro_test("positive_populations", all(is.finite(retro_population$pop_2021) &
                                       retro_population$pop_2021 > 0))

# Retain source identifiers, including missing reported values. Combine
# matching duplicates and exclude conflicting values as reconstruction anchors.
retro_raw_all <- base_total %>% transmute(
  iso = COUNTRY, date = as.Date(DATE), source_row = row_number(),
  source_id = as.character(Sys_ID),
  adm_tot_td = as.numeric(DOSES_ADMINISTERED),
  adm_tot_a1d = as.numeric(PERSONS_VACCINATED_ONE_PLUS_DOSE),
  adm_tot_cps = as.numeric(PERSONS_VACCINATED_COMPLETE),
  adm_tot_boost = as.numeric(PERSONS_VACCINATED_BOOSTER_ADD_DOSE)
) %>%
  filter(!is.na(iso), iso != "None", !is.na(date),
         date >= as.Date("2021-01-01"), date <= as.Date("2023-12-31"))
retro_raw <- retro_raw_all %>% filter(iso %in% retro_population$iso)

# Check that no manual total population corrections are specified.
retro_test("no_unresolved_manual_total_rules", !any(
  data_corrections$analysis == "acute_2021_2023" &
    data_corrections$group == "total"))
retro_preparation <- prepare_dated_reports(retro_raw, retro_population)
retro_reports <- retro_preparation$reports

# 4. Treat total population reports ---------------------------------------

retro_processed <- lapply(split(retro_reports, retro_reports$iso),
                         treat_dated_reports)
retro_audit <- summarize_treatment_actions(bind_rows(lapply(retro_processed, `[[`, "audit")))
retro_events <- bind_rows(lapply(retro_processed, `[[`, "events"))
retro_decisions <- bind_rows(lapply(names(retro_processed), function(country) {
  decisions <- retro_processed[[country]]$downward
  if (nrow(decisions)) decisions %>% mutate(iso = country) else decisions
}))

# 5. Reconstruct total population uptake at month end ---------------------

retro_targets <- crossing(
  iso = retro_population$iso, indicator = vars_admin,
  month_name = seq(as.Date("2021-01-01"), as.Date("2023-12-01"), by = "month")
) %>% left_join(retro_population, by = "iso") %>%
  mutate(target_date = ceiling_date(month_name, "month") - days(1))
retro_monthly <- reconstruct_monthly_totals(retro_audit, retro_targets)

# Use the first day as the monthly identifier; target_date records the month end
# estimation date.
retro_monthly_uncapped <- retro_monthly %>%
  select(iso, month_name, indicator, before_cap) %>%
  pivot_wider(names_from = indicator, values_from = before_cap,
              names_glue = "{indicator}_monotone")
retro_monthly_final <- retro_monthly %>%
  select(iso, month_name, indicator, estimate) %>%
  pivot_wider(names_from = indicator, values_from = estimate,
              names_glue = "{indicator}_monotone_interp")

# 6. Reconstruct priority group counts with the common dated algorithm ----

# Use the first strictly positive total population month end booster estimate
# as the zero-date proxy for priority group booster reconstruction.
priority_booster_start <- retro_monthly %>%
  filter(indicator == "adm_tot_boost", !is.na(estimate), estimate > 0) %>%
  group_by(iso) %>%
  summarise(booster_zero_date = min(target_date), .groups = "drop")

# Monthly reports represent month end observations. Retained reports take
# precedence over the programme zero dates and booster proxy.
priority_monthly <- bind_rows(lapply(split(priority_audit,
  paste(priority_audit$iso, priority_audit$indicator)), function(source) {
    source <- source %>% arrange(date)
    booster_start <- if (endsWith(source$indicator[1], "_boost"))
      priority_booster_start$booster_zero_date[
        match(source$iso[1], priority_booster_start$iso)] else as.Date(NA)
    reconstructed <- reconstruct_dated_series(source,
      target_dates = source$date, indicator = source$indicator[1],
      introduction_date = source$date_intro[1], population = source$population[1],
      cap_person_counts = FALSE, booster_zero_date = booster_start)
    bind_cols(source %>% select(iso, month_name, indicator, population, date_intro) %>%
                mutate(booster_zero_date = booster_start),
              reconstructed)
  }))

# 7. Assemble the monthly dataset before population caps -----------------

priority_retained <- priority_audit %>%
  select(iso, month_name, indicator, after_downward) %>%
  pivot_wider(names_from = indicator, values_from = after_downward,
              names_glue = "{indicator}_monotone")
priority_reconstructed <- priority_monthly %>%
  select(iso, month_name, indicator, before_cap) %>%
  pivot_wider(names_from = indicator, values_from = before_cap,
              names_glue = "{indicator}_monotone_interp")
data_admin_time_mt <- data_admin_time %>%
  left_join(priority_retained, by = c("iso", "month_name")) %>%
  left_join(retro_monthly_uncapped, by = c("iso", "month_name"))
data_admin_time_reconstructed <- data_admin_time_mt %>%
  left_join(priority_reconstructed, by = c("iso", "month_name"))

# Cap reconstructed older adult counts before coverage is calculated.
# Retain uncapped HCW counts for adjustment and capping in 4_emergency_coverage_analysis.R.
data_admin_time_interp <- data_admin_time_reconstructed %>%
  mutate(
    across(all_of(paste0(vars_priority[1:3], "_monotone_interp")),
           ~pmin(.x, pop_2021_old))
  ) %>%
  left_join(retro_monthly_final, by = c("iso", "month_name"))

# HCW counts are adjusted and capped after reconstruction, before coverage is calculated.
data_admin_last <- data_admin_time_interp %>%
  filter(month_name == as.Date("2023-12-01"))

# End ---------------------------------------------------------------------
