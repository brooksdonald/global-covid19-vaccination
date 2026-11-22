# Daily vaccination rates from reconstructed total doses
# Maintainer: BROOKS, Donald J.
# Run through 0_master.R.

# 1. Calculate trailing 28 day means --------------------------------------

trailing_mean_partial <- function(x, width = 28L) {
  # At the beginning of supported data, average the available days only.
  x_zero <- ifelse(is.na(x), 0, x)
  cumulative_sum <- cumsum(x_zero)
  cumulative_n <- cumsum(!is.na(x))
  numerator <- cumulative_sum - lag(cumulative_sum, width, default = 0)
  denominator <- cumulative_n - lag(cumulative_n, width, default = 0)
  ifelse(denominator > 0, numerator / denominator, NA_real_)
}

# 2. Evaluate the retained total dose curve on consecutive calendar days ----

# Calculate a daily rate only where two cumulative estimates are available.
# Leave the first rate missing when no earlier introduction anchor is available.
message("Deriving daily vaccination rates from the retained total dose curve")

# Daily rate estimates also include reporting territories outside the monthly
# coverage cohort. Apply total population treatment to their reports.
retro_dvr_extra_population <- retro_raw_all %>%
  filter(!iso %in% retro_population$iso,
         !is.na(adm_tot_td), adm_tot_td >= 0) %>% distinct(iso) %>%
  left_join(entity_pop %>% select(iso, pop_2021, date_intro), by = "iso")
# Retain counts for territories without a population denominator. Rates per
# 100 population remain missing for these territories.
retro_test("extra_dvr_populations_valid_where_available",
  all(is.na(retro_dvr_extra_population$pop_2021) |
        (is.finite(retro_dvr_extra_population$pop_2021) &
           retro_dvr_extra_population$pop_2021 > 0)))
retro_dvr_population <- bind_rows(retro_population %>% select(iso, pop_2021, date_intro),
                                 retro_dvr_extra_population)
retro_extra_preparation <- prepare_dated_reports(
  retro_raw_all %>% filter(iso %in% retro_dvr_extra_population$iso),
  retro_dvr_extra_population)
retro_extra_processed <- lapply(split(retro_extra_preparation$reports,
                                     retro_extra_preparation$reports$iso),
                               treat_dated_reports)
retro_extra_audit <- bind_rows(lapply(retro_extra_processed, `[[`, "audit"))
retro_td_audit <- bind_rows(retro_audit, retro_extra_audit) %>%
  filter(indicator == "adm_tot_td")
retro_td_sources <- split(retro_td_audit, retro_td_audit$iso)
retro_daily <- bind_rows(lapply(names(retro_td_sources), function(country) {
  source <- retro_td_sources[[country]]
  anchors <- source %>% filter(!is.na(after_downward)) %>% arrange(date)
  if (!nrow(anchors)) return(tibble())
  population <- retro_dvr_population %>% filter(iso == country)
  intro <- population$date_intro[1]
  first_date <- min(anchors$date)
  last_date <- max(anchors$date)
  start_date <- if (!is.na(intro) && intro < first_date) intro else first_date

  # Begin at the supported start, including dates in late 2020 if necessary
  # to calculate January rates. Never extend rates beyond the final anchor.
  daily <- reconstruct_dated_series(source,
    target_dates = seq(start_date, last_date, by = "day"),
    indicator = "adm_tot_td", introduction_date = intro,
    population = population$pop_2021[1], carry_forward = FALSE)
  daily %>% mutate(iso = country,
    daily_doses = estimate - lag(estimate),
    dvr_4wk_td = trailing_mean_partial(daily_doses, width = 28L),
    first_retained_report = first_date, last_retained_report = last_date,
    reconstruction_start = start_date)
}))

# Select the study period and fields used for daily rate summaries.
raw_data_acute_time_daily <- retro_daily %>%
  filter(target_date >= as.Date("2021-01-01"),
         target_date <= as.Date("2023-12-31")) %>%
  transmute(a_iso = iso, adm_date = target_date, dvr_4wk_td)

# Staging -----------------------------------------------------------------

# Attach entity metadata and population denominators to daily rate records.
data_admin_daily <- raw_data_acute_time_daily %>%
  transmute(iso = a_iso,
            adm_date = adm_date,
            dvr_4wk_td = dvr_4wk_td) %>%
  left_join(., entity_pop %>% select(iso, region_who, income_group, status_who,
                                     status_covax, pop_2021),
            by = "iso") %>%

  # Express daily doses per person using the 2021 population denominator.
  mutate(dvr_4wk_td_per = dvr_4wk_td / pop_2021)


# Calculate weighted average of dvr per country grouping ------------------

# Weighted figures aggregate doses first, then divide by reporting population.

dvr_weighted_all <- data_admin_daily %>%
  filter(status_who == "Member State",
         adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date) %>%
  summarize(dvr = sum_available(dvr_4wk_td)) %>%
  ungroup() %>%
  mutate(type = "All",
         dimension = "Global")

dvr_weighted_income <- data_admin_daily %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "",
         adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date, income_group) %>%
  summarize(dvr = sum_available(dvr_4wk_td)) %>%
  ungroup() %>%
  rename(dimension = income_group) %>%
  mutate(type = "Income group")

dvr_weighted_region <- data_admin_daily %>%
  filter(status_who == "Member State",
         adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date, region_who) %>%
  summarize(dvr = sum_available(dvr_4wk_td)) %>%
  ungroup() %>%
  rename(dimension = region_who) %>%
  mutate(type = "WHO region")

dvr_weighted_covax <- data_admin_daily %>%
  filter(!is.na(status_covax),
         adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date, status_covax) %>%
  summarize(dvr = sum_available(dvr_4wk_td)) %>%
  ungroup() %>%
  rename(dimension = status_covax) %>%
  mutate(type = "COVAX participation category")

dvr_weighted <- rbind(dvr_weighted_all,
                      dvr_weighted_income,
                      dvr_weighted_region,
                      dvr_weighted_covax) %>%
  # Use the fixed total population CPS reporting denominator for each grouping.
  left_join(population_reporting %>%
              filter(measure == "adm_tot_cps") %>%
              transmute(dimension = if_else(type == "all", "Global", dimension),
                        pop = population),
            by = "dimension") %>%
  mutate(dvr_per = dvr / pop)

# Filter to max aggregate dvr per country grouping.
dvr_max <- dvr_weighted %>%
  filter(!is.na(dvr_per)) %>%
  group_by(dimension) %>%
  filter(dvr_per == max(dvr_per)) %>%
  select(-pop,
         -dvr)

# Each country contributes its highest 28-day mean, irrespective of peak date.
dvr_country_peak_summary <- data_admin_daily %>%
  filter(is.finite(dvr_4wk_td_per)) %>%
  group_by(iso) %>%
  summarise(peak_per_100 = 100 * max(dvr_4wk_td_per), .groups = "drop") %>%
  inner_join(country_groupings, by = "iso") %>%
  group_by(type, dimension) %>%
  summarise(
    peak_median = median(peak_per_100),
    peak_q25 = quantile(peak_per_100, 0.25, names = FALSE),
    peak_q75 = quantile(peak_per_100, 0.75, names = FALSE),
    .groups = "drop"
  )

# Calculate cross-country percentile ranges -------------------------------

dvr_summary_all <- data_admin_daily %>%
  filter(status_who == "Member State") %>%
  group_by(adm_date) %>%
  summarise(
    median_rate = median(dvr_4wk_td_per, na.rm = TRUE),
    perc_10 = quantile(dvr_4wk_td_per, 0.10, na.rm = TRUE),
    perc_90 = quantile(dvr_4wk_td_per, 0.90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    type = "All",
    dimension = "Global"
  )

dvr_summary_income <- data_admin_daily %>%
  filter(
    status_who == "Member State",
    !is.na(income_group),
    income_group != ""
  ) %>%
  group_by(adm_date, income_group) %>%
  summarise(
    median_rate = median(dvr_4wk_td_per, na.rm = TRUE),
    perc_10 = quantile(dvr_4wk_td_per, 0.10, na.rm = TRUE),
    perc_90 = quantile(dvr_4wk_td_per, 0.90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(dimension = income_group) %>%
  mutate(type = "Income group")

dvr_summary <- bind_rows(
  dvr_summary_all,
  dvr_summary_income
) %>%
  left_join(
    dvr_weighted %>%
      select(
        adm_date,
        type,
        dimension,
        aggregate_rate = dvr_per
      ),
    by = c("adm_date", "type", "dimension")
  )


# Prepare visualizations ---------------------------------------------------

# Figure 3: global and income group daily rate distributions.
data_fig_5_dvr_summary_compA <- dvr_summary %>%
  filter(
    type == "All",
    adm_date < as.Date("2024-01-01")
  ) %>%
  mutate(dimension = factor(dimension, levels = "Global"))

data_fig_5_dvr_summary_compB <- dvr_summary %>%
  filter(type == "Income group",
         !is.na(dimension),
         dimension != "",
         adm_date < as.Date("2024-01-01")) %>%
  mutate(dimension = factor(dimension, levels = levels_standard))


# Supp. figure 5
data_fig_supp_5_dvr_time_increg <- dvr_weighted %>%
  filter(type != "COVAX participation category") %>%
  mutate(type = factor(type, levels = c("All", "Income group", "WHO region")),
         dimension = factor(dimension, levels = c("Global", "AFR", "AMR", "EMR",
                                                  "EUR", "SEAR", "WPR", "HIC",
                                                  "UMIC", "LMIC", "LIC")))


# End ---------------------------------------------------------------------
