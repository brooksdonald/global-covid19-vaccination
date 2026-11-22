# Sensitivity analyses
# Run through 0_master.R after the primary analyses.

# 1. Compare adjusted and unadjusted HCW coverage, 2021-2023 ---------------

# Unadjusted estimates retain the same cleaning, reconstruction and denominators.
data_hcw_adjustment_country <- bind_rows(lapply(c("a1d", "cps", "boost"), function(ind) {
  measure <- paste0("adm_hcw_", ind)
  data_admin_time_interp %>%
    transmute(
      iso, month_name, region_who, income_group, status_who, status_covax,
      indicator = ind, hcw_population = pop_hcw, total_population = pop_2021,
      reported_target = adm_tar_hcw, regional_target = adm_tar_hcw_wpro,
      target = adm_target_hcw,
      reported_count = .data[[paste0(measure, "_raw")]],
      prepared_count = .data[[measure]],
      unadjusted_count = pmin(.data[[paste0(measure, "_monotone_interp")]], pop_hcw)
    ) %>%
    left_join(data_admin_time_reconstructed %>%
                transmute(iso, month_name,
                  reconstructed_before_cap = .data[[paste0(measure, "_monotone_interp")]]),
              by = c("iso", "month_name")) %>%
    left_join(data_cov_time_country %>%
                transmute(iso, month_name,
                  adjusted_count = .data[[paste0(measure, "_monotone_interp")]],
                  total_coverage = .data[[paste0("cov_tot_", ind)]]),
              by = c("iso", "month_name"))
})) %>%
  mutate(
    target_source = case_when(!is.na(regional_target) ~ "Regional target",
                             !is.na(reported_target) ~ "Reported target",
                             TRUE ~ "Not available"),
    target_gap = pmax(hcw_population - target, 0, na.rm = TRUE),
    adjustment_eligible = coalesce(target < hcw_population, FALSE),
    adjustment_status = case_when(
      !is.finite(hcw_population) | hcw_population <= 0 ~ "Denominator unavailable",
      is.na(unadjusted_count) ~ "HCW estimate unavailable",
      !adjustment_eligible ~ "Not applicable",
      is.na(total_coverage) ~ "Adjustment unavailable",
      TRUE ~ "Applied"
    ),
    adjustment_unavailable = adjustment_status == "Adjustment unavailable",
    adjustment_term = if_else(adjustment_eligible & !is.na(total_coverage),
                              target_gap * total_coverage, 0),
    adjusted_before_cap = reconstructed_before_cap + adjustment_term,
    previous_adjusted_count = pmin(reconstructed_before_cap + target_gap * total_coverage,
                                  hcw_population),
    restored_estimate = !is.na(adjusted_count) & is.na(previous_adjusted_count),
    restored_without_gap = restored_estimate & !adjustment_eligible,
    unadjusted_coverage = unadjusted_count / hcw_population,
    adjusted_coverage = adjusted_count / hcw_population,
    adjustment_difference_pp = 100 * (adjusted_coverage - unadjusted_coverage),
    reported_above_denominator = coalesce(reported_count > hcw_population, FALSE),
    unadjusted_capped = coalesce(reconstructed_before_cap > hcw_population, FALSE),
    adjustment_capped = coalesce(adjusted_before_cap > hcw_population, FALSE),
    estimate_changed = coalesce(adjusted_count > unadjusted_count + 1e-8, FALSE),
    unavailable_after_adjustment = !is.na(unadjusted_count) & is.na(adjusted_count),
    unavailable_without_gap = unavailable_after_adjustment & !adjustment_eligible,
    at_ceiling = coalesce(adjusted_count == hcw_population, FALSE),
    paired = is.finite(unadjusted_coverage) & is.finite(adjusted_coverage) &
      is.finite(total_coverage) & total_population > 0,
    hcw_total_gap_unadjusted_pp = 100 * (unadjusted_coverage - total_coverage),
    hcw_total_gap_adjusted_pp = 100 * (adjusted_coverage - total_coverage),
    hcw_total_ratio_unadjusted = if_else(total_coverage > 0,
                                        unadjusted_coverage / total_coverage, NA_real_),
    hcw_total_ratio_adjusted = if_else(total_coverage > 0,
                                      adjusted_coverage / total_coverage, NA_real_),
    crosses_above_total = paired & hcw_total_gap_unadjusted_pp < -1e-8 &
      hcw_total_gap_adjusted_pp > 1e-8,
    reaches_total = paired & hcw_total_gap_unadjusted_pp < -1e-8 &
      hcw_total_gap_adjusted_pp >= -1e-8
  ) %>%
  arrange(iso, indicator, month_name)

hcw_fixed_cohort <- reporting_population_country %>%
  filter(str_detect(measure, "^adm_hcw_")) %>%
  transmute(iso, indicator = str_remove(measure, "^adm_hcw_"),
            in_fixed_cohort = TRUE)

data_hcw_adjustment_country <- data_hcw_adjustment_country %>%
  left_join(hcw_fixed_cohort, by = c("iso", "indicator")) %>%
  mutate(in_fixed_cohort = replace_na(in_fixed_cohort, FALSE))

hcw_adjustment_grouped <- data_hcw_adjustment_country %>%
  inner_join(country_groupings, by = "iso", relationship = "many-to-many")

# Both HCW estimates use the same fixed reporting denominator.
data_hcw_adjustment_summary <- hcw_adjustment_grouped %>%
  group_by(type, dimension, month_name, indicator) %>%
  summarise(
    countries_in_cohort = sum(in_fixed_cohort),
    population_in_cohort = sum(hcw_population[in_fixed_cohort], na.rm = TRUE),
    countries_unadjusted = sum(!is.na(unadjusted_count)),
    countries_adjusted = sum(!is.na(adjusted_count)),
    countries_changed = sum(estimate_changed),
    countries_adjustment_unavailable = sum(adjustment_unavailable),
    countries_restored = sum(restored_estimate),
    countries_restored_without_gap = sum(restored_without_gap),
    countries_unavailable_after_adjustment = sum(unavailable_after_adjustment),
    countries_unavailable_without_gap = sum(unavailable_without_gap),
    unadjusted_count = sum_available(unadjusted_count),
    adjusted_count = sum_available(adjusted_count),
    .groups = "drop"
  ) %>%
  mutate(
    coverage_unadjusted = unadjusted_count / population_in_cohort,
    coverage_adjusted = adjusted_count / population_in_cohort,
    difference_pp = 100 * (coverage_adjusted - coverage_unadjusted)
  )

# Match countries and months with available HCW and total population estimates.
data_hcw_adjustment_comparison <- hcw_adjustment_grouped %>%
  filter(paired) %>%
  group_by(type, dimension, month_name, indicator) %>%
  summarise(
    countries = n(), countries_changed = sum(estimate_changed),
    coverage_hcw_unadjusted = sum(unadjusted_count) / sum(hcw_population),
    coverage_hcw_adjusted = sum(adjusted_count) / sum(hcw_population),
    coverage_total = sum(total_coverage * total_population) / sum(total_population),
    hcw_population = sum(hcw_population), total_population = sum(total_population),
    median_gap_unadjusted_pp = median(hcw_total_gap_unadjusted_pp),
    median_gap_adjusted_pp = median(hcw_total_gap_adjusted_pp),
    countries_above_total_unadjusted = sum(hcw_total_gap_unadjusted_pp > 1e-8),
    countries_above_total_adjusted = sum(hcw_total_gap_adjusted_pp > 1e-8),
    countries_crossing_above_total = sum(crosses_above_total),
    countries_reaching_total = sum(reaches_total),
    countries_with_ratio = sum(total_coverage > 0),
    median_ratio_unadjusted = if (any(total_coverage > 0))
      median(hcw_total_ratio_unadjusted[total_coverage > 0]) else NA_real_,
    median_ratio_adjusted = if (any(total_coverage > 0))
      median(hcw_total_ratio_adjusted[total_coverage > 0]) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    difference_pp = 100 * (coverage_hcw_adjusted - coverage_hcw_unadjusted),
    gap_unadjusted_pp = 100 * (coverage_hcw_unadjusted - coverage_total),
    gap_adjusted_pp = 100 * (coverage_hcw_adjusted - coverage_total)
  )

# 2. Count affected countries and country-month-indicator estimates --------

# A count at 100% is not evidence of capping; capping requires a larger input.
hcw_audit_periods <- bind_rows(
  hcw_adjustment_grouped %>% mutate(period = format(month_name, "%Y")),
  hcw_adjustment_grouped %>% mutate(period = "2021-2023")
) %>%
  bind_rows(mutate(., indicator = "All indicators"))

data_hcw_adjustment_frequency <- hcw_audit_periods %>%
  group_by(type, dimension, period, indicator) %>%
  summarise(
    countries_with_estimates = n_distinct(iso[!is.na(unadjusted_count)]),
    estimates_unadjusted = sum(!is.na(unadjusted_count)),
    countries_eligible = n_distinct(iso[adjustment_eligible & !is.na(unadjusted_count)]),
    estimates_eligible = sum(adjustment_eligible & !is.na(unadjusted_count)),
    countries_changed = n_distinct(iso[estimate_changed]),
    estimates_changed = sum(estimate_changed),
    countries_adjustment_unavailable = n_distinct(iso[adjustment_unavailable]),
    estimates_adjustment_unavailable = sum(adjustment_unavailable),
    countries_restored = n_distinct(iso[restored_estimate]),
    estimates_restored = sum(restored_estimate),
    countries_restored_without_gap = n_distinct(iso[restored_without_gap]),
    estimates_restored_without_gap = sum(restored_without_gap),
    countries_unavailable = n_distinct(iso[unavailable_after_adjustment]),
    estimates_unavailable = sum(unavailable_after_adjustment),
    countries_unavailable_without_gap = n_distinct(iso[unavailable_without_gap]),
    estimates_unavailable_without_gap = sum(unavailable_without_gap),
    .groups = "drop"
  )

hcw_cap_stages <- tibble(
  stage = c("Without HCW adjustment", "With HCW adjustment"),
  input = c("reconstructed_before_cap", "adjusted_before_cap"),
  capped = c("unadjusted_capped", "adjustment_capped")
)

data_hcw_capping_frequency <- bind_rows(lapply(seq_len(nrow(hcw_cap_stages)), function(i) {
  hcw_audit_periods %>%
    filter(!is.na(.data[[hcw_cap_stages$input[i]]]),
           is.finite(hcw_population), hcw_population > 0) %>%
    group_by(type, dimension, period, indicator) %>%
    summarise(
      countries = n_distinct(iso), estimates = n(),
      countries_capped = n_distinct(iso[.data[[hcw_cap_stages$capped[i]]]]),
      estimates_capped = sum(.data[[hcw_cap_stages$capped[i]]]),
      .groups = "drop"
    ) %>%
    mutate(stage = hcw_cap_stages$stage[i],
           proportion_capped = estimates_capped / estimates)
}))

# 3. Check agreement with the primary HCW estimates -----------------------

hcw_adjustment_check <- data_hcw_adjustment_summary %>%
  mutate(measure = recode(indicator, a1d = "At least one dose",
    cps = "Complete primary series", boost = "At least one booster dose")) %>%
  left_join(data_cov_time_all %>% filter(group == "Health and care workers") %>%
              select(type, dimension, month_name, measure, admin, pop, coverage),
            by = c("type", "dimension", "month_name", "measure"))

stopifnot(
  !anyDuplicated(data_hcw_adjustment_country[c("iso", "month_name", "indicator")]),
  isTRUE(all.equal(hcw_adjustment_check$adjusted_count, hcw_adjustment_check$admin)),
  isTRUE(all.equal(hcw_adjustment_check$population_in_cohort, hcw_adjustment_check$pop)),
  isTRUE(all.equal(hcw_adjustment_check$coverage_adjusted, hcw_adjustment_check$coverage)),
  isTRUE(all.equal(data_hcw_adjustment_country$adjusted_count,
    pmin(data_hcw_adjustment_country$adjusted_before_cap,
         data_hcw_adjustment_country$hcw_population))),
  all(data_hcw_adjustment_country$adjustment_difference_pp >= -1e-8, na.rm = TRUE),
  all(data_hcw_adjustment_comparison$difference_pp >= -1e-8),
  all(data_hcw_adjustment_comparison$coverage_total >= 0 &
        data_hcw_adjustment_comparison$coverage_total <= 1)
)

# 4. Quantify the processing sequence, 2021-2023 ---------------------------

# Restrict global processing summaries to WHO Member States.
processing_members <- entity_pop %>% filter(status_who == "Member State") %>% select(iso)
processing_groups <- c("Total population", "Older adults", "Health and care workers")

processing_stages <- tribble(
  ~stage_id, ~order, ~step, ~rule, ~unit, ~applies_to,
  "duplicates", "1a", "Identical dated values", "Keep identical counts once per country, indicator and date. For priority groups, first select the reports.", "Submitted values", "All groups",
  "conflicts", "1b", "Conflicting dated values (total population)", "Exclude counts that disagree for the same country, indicator and date.", "Submitted values", "Total population",
  "cps_filter", "1c", "Primary-series reporting prerequisite", "Exclude priority-group report rows with a missing or zero primary-series count.", "Report rows", "Priority groups",
  "maximum_cps", "1d", "Conflicting dated values (older adults and HCWs)", "For each country, month and group, retain all eligible reports with the largest primary series count.", "Report rows", "Priority groups",
  "manual", "2", "Ad hoc corrections", "Set specified counts to missing before automated cleaning.", "Observations", "Priority groups",
  "negative", "3a", "Negative counts", "Exclude negative cumulative counts.", "Observations", "All groups",
  "zero", "3b", "Zeros after positive counts", "Exclude zeros after the first positive count in the series.", "Observations", "All groups",
  "downward", "3c", "Downward steps", "Exclude conflicting observations using surrounding reports until retained counts are nondecreasing.", "Observations", "All groups",
  "internal", "4a", "Internal interpolation", "Estimate counts at month end between retained reports using elapsed days.", "Monthly estimates", "All groups",
  "introduction", "4b", "Interpolation from programme introduction", "Interpolate from zero at introduction, or two months later for primary series, to the first retained report.", "Monthly estimates", "All groups",
  "booster_proxy", "4c", "Early booster interpolation", "Interpolate from zero at the first month end with positive booster uptake in the total population to the first retained group report.", "Monthly estimates", "Priority groups",
  "carry_forward", "4d", "Carry forward", "Carry the last retained count through December 2023.", "Monthly estimates", "All groups",
  "rounding", "5", "Rounding", "Round only generated counts to the nearest integer.", "Monthly estimates", "All groups",
  "hcw_addition", "6", "Adjustment", "When the HCW denominator exceeds the reported target, add the difference multiplied by available total population coverage.", "Monthly estimates", "Health and care workers",
  "capping", "7", "Final capping", "Constrain counts of vaccinated persons to the population denominator after reconstruction and any adjustment.", "Monthly estimates", "All groups"
) %>% mutate(stage_index = row_number())

# Record why each priority group report was retained or excluded.
processing_priority_selection <- base_target %>%
  mutate(source_row = row_number(), DATE = as.Date(DATE)) %>%
  filter(TARGET_GROUP %in% c("HW", "OLDER_60"),
         DATE >= as.Date("2021-01-01"), DATE <= as.Date("2023-12-31")) %>%
  transmute(iso = ISO_3_CODE, date = DATE, source_row, source_id = Sys_ID,
    population_group = if_else(TARGET_GROUP == "HW", "Health and care workers", "Older adults"),
    a1d = as.numeric(N_VACC_DOSE1), cps = as.numeric(N_VACC_LAST_DOSE),
    boost = as.numeric(N_VACC_BOOSTER_DOSE)) %>%
  semi_join(processing_members, by = "iso") %>%
  group_by(iso, date, population_group) %>%
  mutate(eligible = !is.na(cps) & cps != 0,
    maximum_cps = if (any(eligible)) max(cps[eligible]) else NA_real_,
    selection = case_when(!eligible ~ "cps_filter", cps < maximum_cps ~ "maximum_cps",
                          TRUE ~ "Retained")) %>% ungroup()

processing_priority_raw <- data_admin_time %>%
  select(iso, month_name, all_of(paste0(vars_priority, "_raw"))) %>%
  pivot_longer(ends_with("_raw"), names_to = "indicator", values_to = "before_manual") %>%
  mutate(indicator = str_remove(indicator, "_raw$"))

data_processing_source <- bind_rows(
  retro_audit %>% transmute(iso, date, indicator, source_id,
    before_manual = reported, after_manual = reported, after_negative, after_zero, after_downward),
  priority_audit %>% left_join(processing_priority_raw, by = c("iso", "month_name", "indicator")) %>%
    transmute(iso, date, indicator, source_id,
      before_manual, after_manual = reported, after_negative, after_zero, after_downward)
) %>% semi_join(processing_members, by = "iso") %>%
  mutate(population_group = case_when(str_detect(indicator, "^adm_tot_") ~ "Total population",
    str_detect(indicator, "^adm_old_") ~ "Older adults", TRUE ~ "Health and care workers"),
    indicator = str_remove(indicator, "^adm_(tot|old|hcw)_"))

# A monthly record retains its origin and counts before and after the final steps.
data_processing_monthly <- bind_rows(
  retro_monthly %>% transmute(iso, month_name, date = target_date,
    population_group = "Total population", indicator = str_remove(indicator, "^adm_tot_"),
    population = pop_2021, origin, before_rounding, reconstructed_count = before_cap,
    after_adjustment = before_cap, final_count = estimate, left_date, right_date,
    booster_zero_date = as.Date(NA)),
  priority_monthly %>% transmute(iso, month_name, date = target_date,
    population_group = if_else(str_detect(indicator, "^adm_old_"), "Older adults", "Health and care workers"),
    indicator = str_remove(indicator, "^adm_(old|hcw)_"), population, origin,
    before_rounding, reconstructed_count = before_cap,
    after_adjustment = before_cap, final_count = pmin(before_cap, population),
    left_date, right_date, booster_zero_date)
) %>% semi_join(processing_members, by = "iso") %>%
  left_join(data_hcw_adjustment_country %>%
    transmute(iso, month_name, population_group = "Health and care workers", indicator,
      hcw_before_cap = adjusted_before_cap, hcw_final = adjusted_count),
    by = c("iso", "month_name", "population_group", "indicator")) %>%
  mutate(after_adjustment = if_else(population_group == "Health and care workers", hcw_before_cap, after_adjustment),
    final_count = if_else(population_group == "Health and care workers", hcw_final, final_count),
    adjustment_added = after_adjustment - reconstructed_count,
    cap_removed = after_adjustment - final_count,
    support_interval_days = as.numeric(right_date - left_date),
    carry_forward_days = if_else(origin == "Trailing carry-forward",
      as.numeric(date - left_date), NA_real_),
    source_id = paste(iso, population_group, indicator, month_name, sep = ":")) %>%
  select(-hcw_before_cap, -hcw_final)

# Count monthly estimates reduced to the population denominator in 2021-2023.
data_capping_global <- data_processing_monthly %>%
  filter(population_group %in% c("Total population", "Older adults"),
         indicator %in% c("a1d", "cps", "boost"),
         is.finite(population), population > 0, !is.na(cap_removed)) %>%
  group_by(population_group, indicator) %>%
  summarise(countries = n_distinct(iso), estimates = n(),
            countries_capped = n_distinct(iso[cap_removed > 0]),
            estimates_capped = sum(cap_removed > 0), .groups = "drop") %>%
  mutate(proportion_capped = estimates_capped / estimates) %>%
  bind_rows(data_hcw_capping_frequency %>%
    filter(type == "all", period == "2021-2023", indicator %in% c("a1d", "cps", "boost")) %>%
    transmute(population_group = "Health and care workers", stage, indicator,
              countries, estimates, countries_capped, estimates_capped, proportion_capped))

# Count an action only at the stage where it occurs. Reconstruction and later
# steps can affect the same estimate and are therefore reported separately.
processing_event <- function(data, stage, before = NA_real_, after = NA_real_) {
  data %>% transmute(iso, population_group, indicator, date, source_id = as.character(source_id),
    stage_id = stage, value_before = {{before}}, value_after = {{after}})
}
processing_total_mapping <- retro_preparation$mapping %>% semi_join(processing_members, by = "iso") %>%
  mutate(population_group = "Total population", indicator = str_remove(indicator, "^adm_tot_"))
processing_priority_mapping <- target_report_values %>%
  semi_join(processing_members, by = "iso") %>%
  mutate(date = month_name,
    population_group = if_else(target_group == "HW", "Health and care workers", "Older adults"))
processing_selection_events <- processing_priority_selection %>% filter(selection != "Retained") %>%
  transmute(iso, population_group, indicator = "All indicators", date, source_id,
    stage_id = selection, value_before = NA_real_, value_after = NA_real_)

data_processing_events <- bind_rows(
  processing_event(filter(processing_total_mapping, preparation_action == "Identical value collapsed"), "duplicates", reported),
  processing_event(filter(processing_priority_mapping, preparation_action == "Identical value collapsed"), "duplicates", reported),
  processing_event(filter(processing_total_mapping, preparation_action == "Conflicting values excluded as anchors"), "conflicts", reported),
  processing_selection_events,
  processing_event(filter(data_processing_source, !is.na(before_manual), is.na(after_manual)), "manual", before_manual),
  processing_event(filter(data_processing_source, !is.na(after_manual), is.na(after_negative)), "negative", after_manual),
  processing_event(filter(data_processing_source, !is.na(after_negative), is.na(after_zero)), "zero", after_negative),
  processing_event(filter(data_processing_source, !is.na(after_zero), is.na(after_downward)), "downward", after_zero),
  processing_event(filter(data_processing_monthly, origin == "Internal interpolation"), "internal", after = before_rounding),
  processing_event(filter(data_processing_monthly, origin == "Introduction ramp"), "introduction", after = before_rounding),
  processing_event(filter(data_processing_monthly, origin == "Booster proxy ramp"), "booster_proxy", after = before_rounding),
  processing_event(filter(data_processing_monthly, origin == "Trailing carry-forward"), "carry_forward", after = before_rounding),
  processing_event(filter(data_processing_monthly, before_rounding != reconstructed_count), "rounding", before_rounding, reconstructed_count),
  processing_event(filter(data_processing_monthly, adjustment_added > 0), "hcw_addition", reconstructed_count, after_adjustment),
  processing_event(filter(data_processing_monthly, cap_removed > 0), "capping", after_adjustment, final_count)
) %>% left_join(processing_stages %>% select(stage_id, stage_index, unit), by = "stage_id") %>%
  arrange(stage_index, population_group, iso, indicator, date)

processing_stage_counts <- bind_rows(
  data_processing_events,
  data_processing_events %>% filter(indicator != "All indicators") %>% mutate(indicator = "All indicators")
) %>% group_by(stage_id, population_group, indicator) %>%
  summarise(countries = n_distinct(iso), affected = n(), .groups = "drop")

data_processing_summary <- crossing(processing_stages,
  population_group = processing_groups,
  indicator = c("All indicators", "td", "a1d", "cps", "boost")) %>%
  filter(population_group == "Total population" | indicator != "td",
         !stage_id %in% c("cps_filter", "maximum_cps") | indicator == "All indicators") %>%
  mutate(applicable = applies_to == "All groups" | applies_to == population_group |
    (applies_to == "Priority groups" & population_group != "Total population"),
    applicable = applicable & !(indicator == "td" & stage_id %in% c("booster_proxy", "hcw_addition", "capping")) &
      !(indicator %in% c("a1d", "cps") & stage_id == "booster_proxy") &
      !(indicator == "boost" & stage_id == "introduction")) %>%
  left_join(processing_stage_counts, by = c("stage_id", "population_group", "indicator")) %>%
  mutate(countries = if_else(applicable, replace_na(countries, 0L), NA_integer_),
         affected = if_else(applicable, replace_na(affected, 0L), NA_integer_)) %>%
  arrange(stage_index, match(population_group, processing_groups), indicator)

# Describe the origins of monthly estimates; these categories do not overlap.
data_processing_origins <- bind_rows(
  data_processing_monthly %>% mutate(period = format(month_name, "%Y")),
  data_processing_monthly %>% mutate(period = "2021-2023")
) %>% group_by(population_group, indicator, period, origin) %>%
  summarise(countries = n_distinct(iso), estimates = n(), .groups = "drop")

# Summarize exclusions against the observations entering treatment.
data_processing_source_summary <- bind_rows(data_processing_source,
  mutate(data_processing_source, indicator = "All indicators")) %>%
  group_by(population_group, indicator) %>%
  summarise(countries_with_observations = n_distinct(iso[!is.na(before_manual)]),
    observations = sum(!is.na(before_manual)), retained = sum(!is.na(after_downward)),
    excluded = sum(!is.na(before_manual) & is.na(after_downward)),
    countries_excluded = n_distinct(iso[!is.na(before_manual) & is.na(after_downward)]),
    .groups = "drop") %>% mutate(percentage_excluded = 100 * excluded / observations)

# Use the primary analysis denominators at every final calculation stage.
processing_fixed_cohort <- reporting_population_country %>%
  transmute(iso, measure, in_fixed_cohort = TRUE)
data_processing_monthly <- data_processing_monthly %>%
  mutate(measure = paste0("adm_", recode(population_group, "Total population" = "tot",
    "Older adults" = "old", "Health and care workers" = "hcw"), "_", indicator)) %>%
  left_join(processing_fixed_cohort, by = c("iso", "measure")) %>%
  mutate(in_fixed_cohort = replace_na(in_fixed_cohort, FALSE))

processing_denominators <- population_reporting %>%
  filter(type == "all") %>%
  select(measure, denominator = population)
data_processing_year_end <- data_processing_monthly %>%
  filter(format(month_name, "%m") == "12") %>%
  mutate(carry_forward_days = if_else(in_fixed_cohort & !is.na(final_count),
                                      carry_forward_days, NA_real_)) %>%
  group_by(month_name, population_group, indicator, measure) %>%
  summarise(countries_in_cohort = sum(in_fixed_cohort),
    countries_with_estimates = sum(in_fixed_cohort & !is.na(final_count)),
    reported = sum(in_fixed_cohort & origin == "Reported" & !is.na(final_count)),
    internal = sum(in_fixed_cohort & origin == "Internal interpolation" & !is.na(final_count)),
    early = sum(in_fixed_cohort & origin %in% c("Introduction ramp", "Booster proxy ramp") & !is.na(final_count)),
    carried_forward = sum(in_fixed_cohort & origin == "Trailing carry-forward" & !is.na(final_count)),
    median_carry_forward_days = median(carry_forward_days, na.rm = TRUE),
    q25_carry_forward_days = quantile(carry_forward_days, 0.25, na.rm = TRUE, names = FALSE),
    q75_carry_forward_days = quantile(carry_forward_days, 0.75, na.rm = TRUE, names = FALSE),
    maximum_carry_forward_days = if (any(!is.na(carry_forward_days)))
      max(carry_forward_days, na.rm = TRUE) else NA_real_,
    carried_forward_over_180_days = sum(carry_forward_days > 180, na.rm = TRUE),
    unavailable = sum(in_fixed_cohort & is.na(final_count)),
    reconstructed_count = sum(reconstructed_count[in_fixed_cohort], na.rm = TRUE),
    after_adjustment_count = sum(after_adjustment[in_fixed_cohort], na.rm = TRUE),
    final_count = sum(final_count[in_fixed_cohort], na.rm = TRUE),
    .groups = "drop") %>% left_join(processing_denominators, by = "measure") %>%
  mutate(coverage_reconstructed = 100 * reconstructed_count / denominator,
    coverage_after_adjustment = 100 * after_adjustment_count / denominator,
    coverage_final = 100 * final_count / denominator,
    adjustment_pp = coverage_after_adjustment - coverage_reconstructed,
    capping_pp = coverage_final - coverage_after_adjustment,
    difference_pp = coverage_final - coverage_reconstructed)


# 5. Count annual uptake estimates reduced to the population denominator ---

data_capping_2024 <- data_2024_uptake_country_audit %>%
  filter(complete_year_report, !is.na(annual_uptake_raw),
         is.finite(denominator), denominator > 0) %>%
  group_by(population_group = recode(group, older = "Older adults",
                                     hcw = "Health and care workers")) %>%
  summarise(countries = n_distinct(iso), estimates = n(),
            countries_capped = n_distinct(iso[capped]),
            estimates_capped = sum(capped), .groups = "drop") %>%
  mutate(indicator = "annual_uptake", proportion_capped = estimates_capped / estimates)

# End ---------------------------------------------------------------------
