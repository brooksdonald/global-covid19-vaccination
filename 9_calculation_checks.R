# Check reported values and reconstructed estimates
# Maintainer: BROOKS, Donald J.
# Run through 0_master.R before the manuscript exports.

# 1. Verify source treatment and reconstruction ---------------------------

retro_test("unique_source_values", !anyDuplicated(retro_audit[c("iso", "date", "indicator")]))
retro_test("negative_counts_excluded", all(is.na(
  retro_audit$after_downward[which(retro_audit$reported < 0)])))
retro_test("zeros_after_positive_excluded", all(is.na(
  retro_audit$after_downward[which(retro_audit$zero_after_positive)])))
retro_test("no_source_value_modified", !any(retro_audit$modified))
retro_source_order <- retro_audit %>% filter(!is.na(after_downward)) %>%
  arrange(iso, indicator, date) %>% group_by(iso, indicator) %>%
  summarise(passed = all(diff(after_downward) >= 0), .groups = "drop")
retro_test("retained_sources_nondecreasing", all(retro_source_order$passed))
retro_test("complete_monthly_grid", nrow(retro_monthly) == 194L * 4L * 36L &&
  !anyDuplicated(retro_monthly[c("iso", "indicator", "month_name")]))
retro_test("targets_are_month_ends", all(
  retro_monthly$target_date == ceiling_date(retro_monthly$month_name, "month") - days(1)))
retro_monthly_order <- retro_monthly %>% filter(!is.na(estimate)) %>%
  arrange(iso, indicator, target_date) %>% group_by(iso, indicator) %>%
  summarise(passed = all(diff(estimate) >= 0), .groups = "drop")
retro_test("monthly_estimates_nondecreasing", all(retro_monthly_order$passed))
retro_test("nonnegative_estimates", all(retro_monthly$estimate >= 0, na.rm = TRUE))
retro_test("person_coverage_capped", all(
  retro_monthly$estimate_per100[retro_monthly$indicator != "adm_tot_td"] <= 100,
  na.rm = TRUE))
retro_test("total_doses_not_capped", !any(
  retro_monthly$capped[retro_monthly$indicator == "adm_tot_td"]))
retro_reconstructed <- retro_monthly %>%
  filter(!origin %in% c("Reported", "Unavailable"))
retro_test("reconstructed_counts_rounded", all(
  retro_reconstructed$before_cap == round(retro_reconstructed$before_rounding)))
retro_reported_month_ends <- retro_monthly %>% filter(origin == "Reported")
retro_test("retained_month_end_reports_preserved_before_cap", all(
  retro_reported_month_ends$before_cap == retro_reported_month_ends$left_value))
retro_test("no_early_booster_reconstruction", !any(
  retro_monthly$indicator == "adm_tot_boost" & retro_monthly$origin == "Introduction ramp"))
retro_test("no_early_reconstruction_without_intro", !any(
  is.na(retro_monthly$date_intro) & retro_monthly$origin == "Introduction ramp"))

# 2. Verify the daily and monthly outputs share a cumulative trajectory ----

retro_test("daily_doses_nonnegative", all(retro_daily$daily_doses >= 0, na.rm = TRUE))
retro_test("rates_stop_at_last_retained_report", all(
  retro_daily$target_date <= retro_daily$last_retained_report))
retro_test("no_fabricated_first_day_rate", retro_daily %>% group_by(iso) %>%
  summarise(passed = is.na(first(daily_doses)), .groups = "drop") %>%
  pull(passed) %>% all())
retro_common_targets <- retro_monthly %>% filter(indicator == "adm_tot_td") %>%
  inner_join(retro_daily %>% select(iso, target_date, daily_cumulative = estimate),
             by = c("iso", "target_date"))
retro_test("monthly_and_daily_total_doses_agree", all(
  retro_common_targets$estimate == retro_common_targets$daily_cumulative, na.rm = TRUE))
retro_integrals <- retro_daily %>% group_by(iso) %>%
  summarise(change = last(estimate) - first(estimate),
             sum_daily_doses = sum(daily_doses, na.rm = TRUE), .groups = "drop")
retro_test("daily_increments_recover_cumulative_change", all(
  abs(retro_integrals$change - retro_integrals$sum_daily_doses) < 1e-6))

# 3. Verify HCW adjustment with missing inputs ----------------------------

hcw_adjustment_cases <- tibble(
  count = c(600, 600, 600, NA_real_, 600, 950, 0, 600),
  population = c(rep(1000, 7), NA_real_),
  target = c(1200, 700, 700, 700, NA_real_, 800, 700, 700),
  total_coverage = c(NA_real_, NA_real_, 0.5, 0.5, NA_real_, 0.5, NA_real_, 0.5),
  expected = c(600, 600, 750, NA_real_, 600, 1000, 0, NA_real_)
)
retro_test("hcw_adjustment_missing_input_cases", isTRUE(all.equal(
  with(hcw_adjustment_cases, adjust_hcw_count(
    count, population, pmax(population - target, 0, na.rm = TRUE), total_coverage)),
  hcw_adjustment_cases$expected)))
retro_test("hcw_adjustment_preserves_available_counts",
  !any(data_hcw_adjustment_country$unavailable_after_adjustment))
hcw_unadjusted_fallback <- data_hcw_adjustment_country %>%
  filter(adjustment_status %in% c("Not applicable", "Adjustment unavailable"))
retro_test("hcw_unadjusted_fallback_preserves_counts", all(
  hcw_unadjusted_fallback$adjusted_count == hcw_unadjusted_fallback$unadjusted_count))
hcw_missing_input <- data_hcw_adjustment_country %>% filter(is.na(unadjusted_count))
retro_test("hcw_missing_counts_not_imputed", all(is.na(hcw_missing_input$adjusted_count)))
retro_test("hcw_adjustment_unavailable_flag", all(
  data_hcw_adjustment_country$adjustment_unavailable ==
    (!is.na(data_hcw_adjustment_country$unadjusted_count) &
       data_hcw_adjustment_country$adjustment_eligible &
       is.na(data_hcw_adjustment_country$total_coverage))))

# End ---------------------------------------------------------------------
