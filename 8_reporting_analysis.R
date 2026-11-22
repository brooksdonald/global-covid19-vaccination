# Reporting completeness from submitted values, before anomaly treatment
# Run through 0_master.R. Reporting requires a non-missing, nonzero value.
# Priority groups retain the CPS-based report selection used for coverage.

# 1. Define the reporting population and monthly calendar -----------------

reporting_members <- entity_pop %>%
  filter(status_who == "Member State") %>%
  transmute(iso,
    WBINCOME_LONG_STATUS = recode(income_group,
      HIC = "High income", UMIC = "Upper middle income",
      LMIC = "Lower middle income", LIC = "Low income"))

reporting_months <- seq(as.Date("2021-01-01"), as.Date("2023-12-01"), by = "month")
reporting_groups <- c("Total population", "Older adults", "Health and care workers")
reporting_member_state_count <- nrow(reporting_members)

# 2. Identify qualifying reports ----------------------------------------

# Keep the selected risk-group report even when other source rows contain
# additional indicators. Missing or zero CPS makes a source row ineligible.
reporting_priority <- base_target %>%
  filter(TARGET_GROUP %in% c("HW", "OLDER_60"),
         WHO_LEGAL_STATUS_TITLE == "Member State",
         as.Date(DATE) >= min(reporting_months),
         as.Date(DATE) < as.Date("2024-01-01")) %>%
  select_target_report_rows(with_ties = FALSE) %>%
  transmute(iso = ISO_3_CODE, date = as.Date(DATE),
    group = recode(TARGET_GROUP, HW = "Health and care workers", OLDER_60 = "Older adults"),
    cps_reported = !is.na(N_VACC_LAST_DOSE) & N_VACC_LAST_DOSE != 0,
    booster_reported = !is.na(N_VACC_BOOSTER_DOSE) & N_VACC_BOOSTER_DOSE != 0)

# Each total-population indicator qualifies if any dated report in the month
# contains a non-missing, nonzero value. Repeated reports count once per month.
reporting_total <- base_total %>%
  transmute(iso = COUNTRY, date = floor_date(as.Date(DATE), "month"),
    cps_reported = !is.na(PERSONS_VACCINATED_COMPLETE) & PERSONS_VACCINATED_COMPLETE != 0,
    booster_reported = !is.na(PERSONS_VACCINATED_BOOSTER_ADD_DOSE) &
      PERSONS_VACCINATED_BOOSTER_ADD_DOSE != 0) %>%
  filter(date >= min(reporting_months), date <= max(reporting_months)) %>%
  semi_join(reporting_members, by = "iso") %>%
  group_by(iso, date) %>%
  summarise(cps_reported = any(cps_reported), booster_reported = any(booster_reported),
            .groups = "drop") %>%
  mutate(group = "Total population")

# 3. Accumulate reporting status ----------------------------------------

# Once a country has reported, later missing or zero values do not remove
# its cumulative reporting status. Monthly status uses submitted values only.
reporting_country_month <- crossing(iso = reporting_members$iso,
  date = reporting_months, group = reporting_groups) %>%
  left_join(bind_rows(reporting_priority, reporting_total), by = c("iso", "date", "group")) %>%
  left_join(reporting_members, by = "iso") %>%
  mutate(across(c(cps_reported, booster_reported), ~replace_na(.x, FALSE))) %>%
  group_by(iso, group) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(cps_ever = cumany(cps_reported), booster_ever = cumany(booster_reported)) %>%
  ungroup()

reporting_country_indicator <- bind_rows(
  reporting_country_month %>%
    transmute(iso, date, group, WBINCOME_LONG_STATUS,
      type = "Complete primary series", reported = cps_reported, ever_reported = cps_ever),
  reporting_country_month %>%
    transmute(iso, date, group, WBINCOME_LONG_STATUS,
      type = "Booster", reported = booster_reported, ever_reported = booster_ever)
)

# 4. Summarize monthly categories and cumulative participation ------------

# Monthly categories are mutually exclusive and include booster-only reports.
count_time_monthly_categories <- reporting_country_month %>%
  group_by(date, group) %>%
  summarise(`CPS only` = sum(cps_reported & !booster_reported),
            `CPS and booster` = sum(cps_reported & booster_reported),
            `Booster only` = sum(!cps_reported & booster_reported), .groups = "drop") %>%
  pivot_longer(c(`CPS only`, `CPS and booster`, `Booster only`),
               names_to = "category", values_to = "count_month")

# Indicator totals include countries reporting both indicators.
count_time <- reporting_country_indicator %>%
  group_by(date, group, type) %>%
  summarise(count_month = sum(reported), count_cumul = sum(ever_reported), .groups = "drop")

# Income denominators include all Member States assigned to each income group.
reporting_income_denominators <- population_all %>%
  filter(type == "income") %>%
  transmute(WBINCOME_LONG_STATUS = recode(dimension,
              HIC = "High income", UMIC = "Upper middle income",
              LMIC = "Lower middle income", LIC = "Low income"),
            count_total = countries)

count_time_inc <- reporting_country_indicator %>%
  filter(!is.na(WBINCOME_LONG_STATUS)) %>%
  group_by(DATE = date, population = group, WBINCOME_LONG_STATUS, type) %>%
  summarise(count = sum(ever_reported), .groups = "drop") %>%
  left_join(reporting_income_denominators, by = "WBINCOME_LONG_STATUS") %>%
  mutate(count_per = count / count_total)


# Check monotonicity ------------------------------------------------------

# Cumulative counts include all countries that have reported by that month.
count_time_inc_verif <- count_time_inc %>%
  group_by(WBINCOME_LONG_STATUS,
           population,
           type) %>%
  arrange(DATE) %>%
  summarise(is_monotone = all(diff(count) >= 0), .groups = "drop")

stopifnot(all(count_time_inc_verif$is_monotone))


# End ---------------------------------------------------------------------
