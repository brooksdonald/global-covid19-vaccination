# Reconstruct daily vaccination rates -------------------------------------

# The CVAP DVR pipeline produced this series in an intermediate workbook.
# Reconstruct the manuscript input directly from the committed WHO weekly
# cumulative uptake snapshot, using only the total-dose fields required here.

prepare_dvr_reports <- function(raw_weekly,
                                date_start = as.Date("2021-01-01"),
                                date_end = as.Date("2023-12-31")) {
  raw_weekly %>%
    transmute(
      iso = COUNTRY,
      adm_date = as.Date(DATE),
      adm_tot_td = as.numeric(DOSES_ADMINISTERED)
    ) %>%
    filter(
      !is.na(iso),
      iso != "None",
      !is.na(adm_tot_td),
      adm_tot_td >= 0
    ) %>%
    distinct() %>%
    rename(date = adm_date) %>%
    fix_admin_weekly_dates() %>%
    rename(adm_date = date) %>%
    filter(
      !is.na(adm_date),
      adm_date >= date_start,
      adm_date <= date_end
    ) %>%
    group_by(iso, adm_date) %>%
    # Retain the largest cumulative value if duplicate country-date records
    # remain after exact duplicate removal.
    summarise(adm_tot_td = max(adm_tot_td), .groups = "drop")
}

trailing_mean_partial <- function(x, width = 28L) {
  x_zero <- ifelse(is.na(x), 0, x)
  cumulative_sum <- cumsum(x_zero)
  cumulative_n <- cumsum(!is.na(x))
  numerator <- cumulative_sum - lag(cumulative_sum, width, default = 0)
  denominator <- cumulative_n - lag(cumulative_n, width, default = 0)
  ifelse(denominator > 0, numerator / denominator, NA_real_)
}

interpolate_dvr_daily <- function(df) {
  df %>%
    group_by(iso) %>%
    arrange(adm_date, .by_group = TRUE) %>%
    group_modify(~{
      first_report_date <- min(.x$adm_date)
      last_report_date <- max(.x$adm_date)
      reported_intro_date <- as.Date(first(.x$date_intro))

      # CVAP used the programme introduction date as a zero anchor. Where it
      # was unavailable or did not precede the first report, use the preceding
      # day so the first cumulative report is retained without extrapolating
      # backwards from an invalid interval.
      interpolation_start <- if (
        is.na(reported_intro_date) ||
        reported_intro_date >= first_report_date
      ) {
        first_report_date - 1
      } else {
        reported_intro_date
      }

      daily <- tibble(
        adm_date = seq.Date(
          interpolation_start,
          last_report_date,
          by = "day"
        )
      ) %>%
        left_join(
          .x %>% select(adm_date, adm_tot_td_monotone),
          by = "adm_date"
        )

      daily$adm_tot_td_monotone[
        daily$adm_date == interpolation_start &
          is.na(daily$adm_tot_td_monotone)
      ] <- 0

      reported_idx <- which(!is.na(daily$adm_tot_td_monotone))
      daily$adm_tot_td_daily <- if (length(reported_idx) >= 2L) {
        approx(
          x = reported_idx,
          y = daily$adm_tot_td_monotone[reported_idx],
          xout = seq_len(nrow(daily)),
          method = "linear",
          rule = 2
        )$y
      } else {
        daily$adm_tot_td_monotone
      }

      daily %>%
        mutate(
          daily_doses = adm_tot_td_daily - lag(
            adm_tot_td_daily,
            default = first(adm_tot_td_daily)
          ),
          dvr_4wk_td = trailing_mean_partial(daily_doses, width = 28L),
          date_intro_dvr = interpolation_start,
          date_first_report_dvr = first_report_date,
          date_last_report_dvr = last_report_date
        )
    }) %>%
    ungroup()
}

dvr_reports <- prepare_dvr_reports(base_total)

source(file.path(path_project, "clean_monotone.R"))

dvr_reports_clean <- clean_coverage_monotone_df(
  dvr_reports,
  id_iso = "iso",
  id_time = "adm_date",
  vars_to_clean = "adm_tot_td",
  depth = 3,
  batch = TRUE,
  method = "interpolate",
  round_int = TRUE
)

dvr_monotone_log <- attr(dvr_reports_clean, "monotone_log")

dvr_reports_clean <- dvr_reports_clean %>%
  left_join(
    entity_pop %>% select(iso, date_intro),
    by = "iso"
  )

dvr_intro_fallback_log <- dvr_reports_clean %>%
  group_by(iso) %>%
  summarise(
    date_intro = first(date_intro),
    date_first_report = min(adm_date),
    .groups = "drop"
  ) %>%
  filter(is.na(date_intro) | date_intro >= date_first_report) %>%
  mutate(
    date_intro_used = date_first_report - 1,
    reason = if_else(
      is.na(date_intro),
      "programme introduction date unavailable",
      "programme introduction date did not precede first report"
    )
  )

raw_data_acute_time_daily <- dvr_reports_clean %>%
  interpolate_dvr_daily() %>%
  filter(
    adm_date >= as.Date("2021-01-01"),
    adm_date <= as.Date("2023-12-31")
  ) %>%
  transmute(
    a_iso = iso,
    adm_date,
    dvr_4wk_td
  )


# Staging -----------------------------------------------------------------

# Select relevant admin. variables, merge with relevant entity details
data_admin_daily <- raw_data_acute_time_daily %>%
  transmute(iso = a_iso,
            adm_date = adm_date,
            dvr_4wk_td = dvr_4wk_td) %>%
  left_join(., entity_pop %>% select(iso, region_who, income_group, status_who,
                                     status_covax, pop_2021),
            by = "iso") %>%
  
  # Calculate dvr as percentage of total pop.
  mutate(dvr_4wk_td_per = dvr_4wk_td / pop_2021)


# Calculate weighted average of dvr per country grouping ------------------

# Weighted figures aggregate doses first, then divide by reporting population

dvr_weighted_all <- data_admin_daily %>%
  filter(status_who == "Member State",
         adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date) %>%
  summarize(dvr = sum(dvr_4wk_td, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(type = "All",
         dimension = "Global")

dvr_weighted_income <- data_admin_daily %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "",
         adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date, income_group) %>%
  summarize(dvr = sum(dvr_4wk_td, na.rm = TRUE)) %>%
  ungroup() %>% 
  rename(dimension = income_group) %>%
  mutate(type = "Income group")

dvr_weighted_region <- data_admin_daily %>%
  filter(status_who == "Member State",
         adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date, region_who) %>%
  summarize(dvr = sum(dvr_4wk_td, na.rm = TRUE)) %>%
  ungroup() %>% 
  rename(dimension = region_who) %>%
  mutate(type = "WHO region")

dvr_weighted_covax <- data_admin_daily %>%
  filter(!is.na(status_covax),
         adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date, status_covax) %>%
  summarize(dvr = sum(dvr_4wk_td, na.rm = TRUE)) %>%
  ungroup() %>% 
  rename(dimension = status_covax) %>%
  mutate(type = "COVAX participation category")

dvr_weighted <- rbind(dvr_weighted_all,
                      dvr_weighted_income, 
                      dvr_weighted_region,
                      dvr_weighted_covax) %>%
  mutate(pop = case_when(
    dimension == "Global" ~ pop_rep_tot_2021_cps,
    dimension == "HIC" ~ pop_rep_tot_2021_cps_hic,
    dimension == "UMIC" ~ pop_rep_tot_2021_cps_umic,
    dimension == "LMIC" ~ pop_rep_tot_2021_cps_lmic,
    dimension == "LIC" ~ pop_rep_tot_2021_cps_lic,
    dimension == "AFR" ~ pop_rep_tot_2021_cps_afr,
    dimension == "AMR" ~ pop_rep_tot_2021_cps_amr,
    dimension == "EMR" ~ pop_rep_tot_2021_cps_emr,
    dimension == "EUR" ~ pop_rep_tot_2021_cps_eur,
    dimension == "SEAR" ~ pop_rep_tot_2021_cps_sear,
    dimension == "WPR" ~ pop_rep_tot_2021_cps_wpr,
    dimension == "AMC" ~ pop_rep_tot_2021_cps_amc,
    dimension == "Self-financing" ~ `pop_rep_tot_2021_cps_self-financing`,
    dimension == "Non-COVAX" ~ `pop_rep_tot_2021_cps_non-covax`,
    TRUE ~ NA_real_
  ),
  dvr_per = dvr / pop)

# Filter to max aggregate dvr per country grouping
dvr_max <- dvr_weighted %>%
  group_by(dimension) %>%
  filter(dvr_per == max(dvr_per)) %>%
  select(-pop,
         -dvr)


# Calculate cross-country percentile ranges -------------------------------

# Figure 5 uses unweighted country-level medians as the central line so the
# line and 10th–90th percentile ribbon describe the same country-level
# distribution. Population-weighted aggregate rates are retained in dvr_weighted,
# dvr_max, and the supplementary weighted DVR figure.
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

# Figure 6
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
