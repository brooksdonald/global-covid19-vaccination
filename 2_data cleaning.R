
# Staging -----------------------------------------------------------------

fix_admin_weekly_dates <- function(df) {
  out <- df %>%
    mutate(date = as.Date(date))

  date_corrections <- data_corrections %>%
    filter(
      analysis == "acute_2021_2023",
      action == "replace_date",
      indicator == "adm_date",
      dataset == "admin_weekly"
    ) %>%
    transmute(
      correction_id,
      iso,
      old_date = as.Date(period),
      replacement_date = as.Date(replacement_value)
    )

  stopifnot(
    !anyDuplicated(date_corrections[c("iso", "old_date")]),
    all(!is.na(date_corrections$replacement_date))
  )

  for (i in seq_len(nrow(date_corrections))) {
    rule <- date_corrections[i, , drop = FALSE]
    matched <- out$iso == rule$iso &
      !is.na(out$date) &
      out$date == rule$old_date

    if (any(matched, na.rm = TRUE)) {
      out$date[matched] <- rule$replacement_date
    }
  }

  out
}

prepare_admin_total_monthly <- function(raw_weekly,
                                        date_start = as.Date("2021-01-01"),
                                        date_end = as.Date("2023-12-31")) {
  raw_weekly %>%
    transmute(
      iso = COUNTRY,
      date = as.Date(DATE),
      adm_tot_td = as.numeric(DOSES_ADMINISTERED),
      adm_tot_a1d = as.numeric(PERSONS_VACCINATED_ONE_PLUS_DOSE),
      adm_tot_cps = as.numeric(PERSONS_VACCINATED_COMPLETE),
      adm_tot_boost = as.numeric(PERSONS_VACCINATED_BOOSTER_ADD_DOSE)
    ) %>%
    filter(iso != "None") %>%
    distinct() %>%
    fix_admin_weekly_dates() %>%
    filter(date >= date_start, date <= date_end) %>%
    mutate(month_name = floor_date(date, unit = "month")) %>%
    arrange(iso, month_name, date) %>%
    group_by(iso, month_name) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(
      iso,
      month_name,
      adm_tot_td,
      adm_tot_a1d,
      adm_tot_cps,
      adm_tot_boost
    )
}

prepare_admin_target_monthly <- function(raw_target,
                                         date_start = as.Date("2021-01-01"),
                                         date_end = as.Date("2023-12-31")) {
  target_hcw <- raw_target %>%
    filter(TARGET_GROUP == "HW") %>%
    mutate(
      hcw_repstat = if_else(
        is.na(N_VACC_LAST_DOSE) == FALSE & N_VACC_LAST_DOSE != 0,
        "Reporting",
        NA_character_
      ),
      DATE = as.Date(DATE)
    ) %>%
    filter(
      hcw_repstat == "Reporting",
      DATE >= date_start,
      DATE <= date_end
    ) %>%
    group_by(ISO_3_CODE, DATE) %>%
    top_n(1, N_VACC_LAST_DOSE) %>%
    ungroup() %>%
    select(
      ISO_3_CODE,
      DATE,
      N_VACC_DOSE1,
      N_VACC_LAST_DOSE,
      N_VACC_BOOSTER_DOSE
    ) %>%
    rename(
      adm_hcw_a1d_raw = N_VACC_DOSE1,
      adm_hcw_cps_raw = N_VACC_LAST_DOSE,
      adm_hcw_boost_raw = N_VACC_BOOSTER_DOSE
    )

  target_old <- raw_target %>%
    filter(TARGET_GROUP == "OLDER_60") %>%
    mutate(
      old_repstat = if_else(
        is.na(N_VACC_LAST_DOSE) == FALSE & N_VACC_LAST_DOSE != 0,
        "Reporting",
        NA_character_
      ),
      DATE = as.Date(DATE)
    ) %>%
    filter(
      old_repstat == "Reporting",
      DATE >= date_start,
      DATE <= date_end
    ) %>%
    group_by(ISO_3_CODE, DATE) %>%
    top_n(1, N_VACC_LAST_DOSE) %>%
    ungroup() %>%
    select(
      ISO_3_CODE,
      DATE,
      N_VACC_DOSE1,
      N_VACC_LAST_DOSE,
      N_VACC_BOOSTER_DOSE
    ) %>%
    rename(
      adm_old_a1d_raw = N_VACC_DOSE1,
      adm_old_cps_raw = N_VACC_LAST_DOSE,
      adm_old_boost_raw = N_VACC_BOOSTER_DOSE
    )

  full_join(
    target_hcw,
    target_old,
    by = c("ISO_3_CODE" = "ISO_3_CODE", "DATE" = "DATE")
  ) %>%
    mutate(across(-c("ISO_3_CODE", "DATE"), as.numeric)) %>%
    transmute(
      iso = ISO_3_CODE,
      month_name = DATE,
      adm_hcw_a1d_raw,
      adm_hcw_cps_raw,
      adm_hcw_boost_raw,
      adm_old_a1d_raw,
      adm_old_cps_raw,
      adm_old_boost_raw
    )
}

data_admin_time_scaffold <- entity_pop %>%
  select(a_iso = iso) %>%
  crossing(month_name = seq(as.Date("2021-01-01"),
                            as.Date("2023-12-01"),
                            by = "1 month"))

data_admin_total_monthly <- prepare_admin_total_monthly(base_total)
data_admin_target_monthly <- prepare_admin_target_monthly(base_target)

data_admin_time_staged <- data_admin_time_scaffold %>%
  left_join(
    data_admin_total_monthly,
    by = c("a_iso" = "iso", "month_name" = "month_name")
  ) %>%
  left_join(
    data_admin_target_monthly,
    by = c("a_iso" = "iso", "month_name" = "month_name")
  ) %>%
  select(
    a_iso,
    month_name,
    adm_tot_td,
    adm_tot_a1d,
    adm_tot_cps,
    adm_tot_boost,
    adm_old_a1d_raw,
    adm_old_cps_raw,
    adm_old_boost_raw,
    adm_hcw_a1d_raw,
    adm_hcw_cps_raw,
    adm_hcw_boost_raw
  )

# Join staged uptake data with entity details
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
    cov_tot_a1d = adm_tot_a1d / pop_2021,
    cov_tot_cps = adm_tot_cps / pop_2021,
    cov_tot_boost = adm_tot_boost / pop_2021,
    hcw_flag = if_else(pop_hcw > adm_target_hcw, "Yes", NA_character_),
    hcw_diff = pmax(pop_hcw - adm_target_hcw, 0, na.rm = TRUE),
    adm_hcw_a1d_cap = pmin(adm_hcw_a1d_raw, pop_hcw),
    adm_hcw_cps_cap = pmin(adm_hcw_cps_raw, pop_hcw),
    adm_hcw_boost_cap = pmin(adm_hcw_boost_raw, pop_hcw),
    cov_hcw_a1d_cap = pmin(adm_hcw_a1d_cap / pop_hcw, 1),
    cov_hcw_cps_cap = pmin(adm_hcw_cps_cap / pop_hcw, 1),
    cov_hcw_boost_cap = pmin(adm_hcw_boost_cap / pop_hcw, 1),
    adm_hcw_a1d = adm_hcw_a1d_cap,
    adm_hcw_cps = adm_hcw_cps_cap,
    adm_hcw_boost = adm_hcw_boost_cap,
    adm_old_a1d = pmin(adm_old_a1d_raw, pop_2021_old),
    adm_old_cps = pmin(adm_old_cps_raw, pop_2021_old),
    adm_old_boost = pmin(adm_old_boost_raw, pop_2021_old)
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
    abs(observed - rule$match_value) < 0.5
  )

  data_admin_time[[rule$indicator]][matched] <- NA_real_
}
  

# Clean timeseries data ---------------------------------------------------

# Identify downward breaks in cumulative time series and interpolate gaps
source(file.path(path_project, "clean_monotone.R"))

data_admin_time_mt <- clean_coverage_monotone_df(
  data_admin_time,
  id_iso = "iso",
  id_time = "month_name",
  vars_to_clean = c("adm_tot_td",
                    "adm_tot_a1d",
                    "adm_tot_cps",
                    "adm_tot_boost",
                    "adm_old_a1d",
                    "adm_old_cps",
                    "adm_old_boost",
                    "adm_hcw_a1d",
                    "adm_hcw_cps",
                    "adm_hcw_boost"),
  depth = 36,               # full monthly analysis window, 2021-2023
  method = "interpolate",   # "set_na" | "carry_forward" | "interpolate"
  round_int = TRUE
  ) %>%
  
  # Fill target-group values forward after last report for end-period coverage
  group_by(iso) %>%
  arrange(month_name) %>%
  fill(adm_old_a1d_monotone,
       adm_old_cps_monotone,
       adm_old_boost_monotone,
       adm_hcw_a1d_monotone,
       adm_hcw_cps_monotone,
       adm_hcw_boost_monotone) %>%
  ungroup()

# Interpolate uptake between introduction date and first report. For
# risk-group booster series, use the country's first submitted
# total-population booster report as the interpolation start proxy.
source(file.path(path_project, "clean_interpolate.R"))

data_admin_time_interp <- interpolate_from_intro_wide_many(
  df = data_admin_time_mt,
  vars = c("adm_tot_td_monotone",
           "adm_tot_a1d_monotone",
           "adm_tot_cps_monotone",
           "adm_tot_boost_monotone",
           "adm_old_a1d_monotone",
           "adm_old_cps_monotone",
           "adm_old_boost_monotone",
           "adm_hcw_a1d_monotone",
           "adm_hcw_cps_monotone",
           "adm_hcw_boost_monotone"),
  iso_col = "iso",
  date_col = "month_name",
  intro_col = "date_intro",
  boost_reference_var = "adm_tot_boost_monotone",
  suffix = "_interp",
  cps_delay_months = 2,
  month_anchor_day = 15) %>%
  group_by(iso) %>%
  arrange(month_name) %>%
  fill(
    adm_tot_td_monotone_interp,
    adm_tot_a1d_monotone_interp,
    adm_tot_cps_monotone_interp,
    adm_tot_boost_monotone_interp
  ) %>%
  ungroup() %>%
  
  # Cap cleaned figures at population max after interpolation
  mutate(
    adm_tot_a1d_monotone_interp = pmin(adm_tot_a1d_monotone_interp, pop_2021),
    adm_tot_cps_monotone_interp = pmin(adm_tot_cps_monotone_interp, pop_2021),
    adm_tot_boost_monotone_interp = pmin(adm_tot_boost_monotone_interp, pop_2021),
    adm_old_a1d_monotone_interp = pmin(adm_old_a1d_monotone_interp, 
                                           pop_2021_old),
    adm_old_cps_monotone_interp = pmin(adm_old_cps_monotone_interp, 
                                           pop_2021_old),
    adm_old_boost_monotone_interp = pmin(adm_old_boost_monotone_interp, 
                                             pop_2021_old),
    adm_hcw_a1d_monotone_interp = pmin(adm_hcw_a1d_monotone_interp, pop_hcw),
    adm_hcw_cps_monotone_interp = pmin(adm_hcw_cps_monotone_interp, pop_hcw),
    adm_hcw_boost_monotone_interp = pmin(adm_hcw_boost_monotone_interp, pop_hcw)
  )


# Create end-2023 dataframe for reporting related calculations ------------

data_admin_last <- data_admin_time_interp %>%
  filter(month_name == as.Date("2023-12-01"))


# End ---------------------------------------------------------------------
