# Prepare 2024 routine uptake for older adults and HCWs --------------------

# Restrict the central correction log to 2024 routine uptake decisions.
data_2024_uptake_corrections <- data_corrections %>%
  filter(analysis == "routine_2024")

required_correction_columns <- c(
  "correction_id", "analysis", "action", "iso", "group", "indicator",
  "period", "source", "dataset", "match_value", "replacement_value",
  "reporting_basis", "reason"
)
stopifnot(all(required_correction_columns %in%
                names(data_2024_uptake_corrections)))


# Prepare source records --------------------------------------------------

# Annual reports are already end-2024 values in the AD_COVERAGES snapshot.
annual_2024_source <- raw_routine_uptake_annual %>%
  filter(YEAR == 2024) %>%
  transmute(
    iso = if ("COUNTRY__CODE" %in% names(raw_routine_uptake_annual)) {
      COUNTRY__CODE
    } else {
      COUNTRY
    },
    group = case_when(
      TARGETDEFINITION == "POLICY_RECOMMENDATION_ELDERLY" ~ "older",
      TARGETDEFINITION == "POLICY_RECOMMENDATION_HCW" ~ "hcw",
      TRUE ~ NA_character_
    ),
    period = "end_2024",
    source = if_else(is.na(SOURCE) | SOURCE == "", "EJRF", SOURCE),
    dataset = "annual",
    value = as.numeric(DOSES),
    derivation = "reported_target_group",
    correction_id = NA_character_
  ) %>%
  filter(
    group %in% c("older", "hcw"),
    !is.na(value),
    value >= 0
  )

# COV_UPTAKE contains quarterly reports and annual TESSY values. Annual TESSY
# values for 2024 are stored at YEAR = 2025 and REPORTING_PERIOD = 1.
# Harmonize target groups and age bands before selecting records.
quarterly_2024_source_raw <- raw_routine_uptake_quarterly %>%
  filter(
    YEAR == 2024 & REPORTING_PERIOD %in% c(4, 7, 10) |
      YEAR == 2025 & REPORTING_PERIOD == 1
  ) %>%
  transmute(
    iso = if ("COUNTRY__CODE" %in% names(raw_routine_uptake_quarterly)) {
      COUNTRY__CODE
    } else {
      COUNTRY
    },
    period = case_when(
      YEAR == 2024 & REPORTING_PERIOD == 4 ~ "q1",
      YEAR == 2024 & REPORTING_PERIOD == 7 ~ "q2",
      YEAR == 2024 & REPORTING_PERIOD == 10 ~ "q3",
      YEAR == 2025 & REPORTING_PERIOD == 1 ~ "end_2024",
      TRUE ~ NA_character_
    ),
    source = if_else(is.na(SOURCE) | SOURCE == "", "EJRF", SOURCE),
    age_group = AGEGROUP,
    target_group = TARGET_GROUP,
    value = as.numeric(N_VACC_DOSE1)
  ) %>%
  filter(
    # Negative source values are sentinel non-response codes, not uptake counts.
    !is.na(period),
    !is.na(value),
    value != -2222,
    value != -4444,
    is.na(age_group) | age_group != "UNKNOWN"
  ) %>%
  mutate(
    reported_group = case_when(
      age_group == "GTEQ_60_YEARS" ~ "older",
      target_group == "POLICY_RECOMMENDATION_ELDERLY" ~ "older",
      target_group %in% c("POLICY_RECOMMENDATION_HCW", "HW") ~ "hcw",
      age_group == "60_69_YEARS" ~ "60_69_YEARS",
      age_group == "70_79_YEARS" ~ "70_79_YEARS",
      age_group == "GTEQ_80_YEARS" ~ "GTEQ_80_YEARS",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    reported_group %in% c(
      "older",
      "hcw",
      "60_69_YEARS",
      "70_79_YEARS",
      "GTEQ_80_YEARS"
    )
  ) %>%
  group_by(iso, period, source, reported_group) %>%
  summarise(value = sum(value),
            ages_60_plus = all(age_group %in% "GTEQ_60_YEARS"),
            .groups = "drop")

# TESSY countries sometimes reported a pre-aggregated older-adult value and
# sometimes reported the component age groups. Use the pre-aggregated value
# when present; otherwise sum only 60-69, 70-79 and >=80 years.
quarterly_2024_age_sum <- quarterly_2024_source_raw %>%
  filter(
    source == "TESSY",
    reported_group %in% c(
      "60_69_YEARS",
      "70_79_YEARS",
      "GTEQ_80_YEARS"
    )
  ) %>%
  group_by(iso, period, source) %>%
  summarise(value = sum(value),
            ages_60_plus = n_distinct(reported_group) == 3L,
            .groups = "drop") %>%
  anti_join(
    quarterly_2024_source_raw %>%
      filter(reported_group == "older") %>%
      distinct(iso, period, source),
    by = c("iso", "period", "source")
  ) %>%
  transmute(
    iso,
    group = "older",
    period,
    source,
    dataset = "quarterly",
    value,
    ages_60_plus,
    derivation = "sum_tessy_age_bands",
    correction_id = NA_character_
  )

quarterly_2024_source <- quarterly_2024_source_raw %>%
  filter(reported_group %in% c("older", "hcw")) %>%
  transmute(
    iso,
    group = reported_group,
    period,
    source,
    dataset = "quarterly",
    value,
    ages_60_plus,
    derivation = "reported_target_group",
    correction_id = NA_character_
  ) %>%
  bind_rows(quarterly_2024_age_sum)

uptake_2024_source <- bind_rows(
  annual_2024_source,
  quarterly_2024_source
)


# Apply documented corrections -------------------------------------------

# Match correction rules with "*" wildcards and value checks when supplied.
rule_matches <- function(data, rule, include_value = TRUE) {
  matched <- rep(TRUE, nrow(data))

  for (field in c("iso", "group", "period", "source", "dataset")) {
    rule_value <- rule[[field]][[1]]
    if (!is.na(rule_value) && rule_value != "*") {
      matched <- matched & data[[field]] == rule_value
    }
  }

  if (
    isTRUE(include_value) &&
      !is.na(rule$match_value[[1]])
  ) {
    matched <- matched &
      abs(data$value - as.numeric(rule$match_value[[1]])) < 0.5
  }

  matched
}

replacement_rules <- data_2024_uptake_corrections %>%
  filter(action == "add_or_replace")

# Manual replacements remove any matching source rows before adding the value.
for (i in seq_len(nrow(replacement_rules))) {
  rule <- replacement_rules[i, , drop = FALSE]
  matched <- rule_matches(uptake_2024_source, rule, include_value = FALSE)
  uptake_2024_source <- uptake_2024_source[!matched, , drop = FALSE]

  uptake_2024_source <- bind_rows(
    uptake_2024_source,
    tibble(
      iso = rule$iso,
      group = rule$group,
      period = rule$period,
      source = rule$source,
      dataset = rule$dataset,
      value = as.numeric(rule$replacement_value),
      derivation = "manual_correction",
      correction_id = rule$correction_id
    )
  )
}

exclusion_rules <- data_2024_uptake_corrections %>%
  filter(action == "exclude_record")

# Exclusion rules remove documented invalid or out-of-scope source rows.
for (i in seq_len(nrow(exclusion_rules))) {
  rule <- exclusion_rules[i, , drop = FALSE]
  matched <- rule_matches(uptake_2024_source, rule)
  uptake_2024_source <- uptake_2024_source[!matched, , drop = FALSE]
}

# When a country has both EJRF and WPR records, retain the EJRF series for the
# country/group rather than switching source between quarters.
uptake_2024_source <- uptake_2024_source %>%
  group_by(iso, group) %>%
  filter(!(any(source == "EJRF") & source == "WPR")) %>%
  ungroup()

# Resolve duplicate sources. Directly reported target group
# values are preferred to older age-band sums; within the same derivation,
# TESSY is preferred to EJRF and EJRF to WPR.
uptake_2024_selected <- uptake_2024_source %>%
  mutate(
    derivation_priority = case_when(
      derivation %in% c(
        "reported_target_group",
        "manual_correction"
      ) ~ 1L,
      derivation == "sum_tessy_age_bands" ~ 2L,
      TRUE ~ 99L
    ),
    source_priority = case_when(
      source == "TESSY" ~ 1L,
      source == "EJRF" ~ 2L,
      source == "WPR" ~ 3L,
      TRUE ~ 99L
    ),
    dataset_priority = case_when(
      dataset == "annual" ~ 1L,
      TRUE ~ 2L
    )
  ) %>%
  arrange(
    iso,
    group,
    period,
    derivation_priority,
    source_priority,
    dataset_priority
  ) %>%
  group_by(iso, group, period) %>%
  slice(1) %>%
  ungroup()

stopifnot(
  !anyDuplicated(
    uptake_2024_selected[c("iso", "group", "period")]
  )
)


# Reconcile quarterly and annual values ----------------------------------

# Combine period values with their source, input table and correction details.
uptake_2024_wide <- uptake_2024_selected %>%
  mutate(input_table = if_else(dataset == "annual", "AD_COVERAGES", "COV_UPTAKE")) %>%
  select(iso, group, period, value, source, input_table,
         derivation, correction = correction_id) %>%
  pivot_wider(names_from = period,
              values_from = c(value, source, input_table, derivation, correction)) %>%
  rename(q1 = value_q1, q2 = value_q2, q3 = value_q3, end_2024 = value_end_2024)

basis_overrides <- data_2024_uptake_corrections %>%
  filter(action == "set_reporting_basis") %>%
  select(iso, group, period, reporting_basis) %>%
  pivot_wider(
    names_from = period,
    values_from = reporting_basis,
    names_prefix = "basis_override_"
  )

# Determine whether quarterly reports are cumulative or quarter-specific.
uptake_2024_reconciled <- uptake_2024_wide %>%
  left_join(basis_overrides, by = c("iso", "group")) %>%
  mutate(
    # TESSY quarterly values are treated as cumulative unless explicitly noted.
    q2_basis_default = case_when(
      source_q2 == "TESSY" ~ "cumulative",
      !is.na(q2) & !is.na(q1) & q2 < q1 ~ "additive",
      TRUE ~ "cumulative"
    ),
    q3_basis_default = case_when(
      source_q3 == "TESSY" ~ "cumulative",
      !is.na(q3) & !is.na(q1) & q3 < q1 ~ "additive",
      TRUE ~ "cumulative"
    ),
    q2_basis = coalesce(basis_override_q2, q2_basis_default),
    q3_basis = coalesce(basis_override_q3, q3_basis_default),
    # Drop implausible additive Q2 values that would exceed the later Q3 total.
    q2_clean = case_when(
      q2_basis == "additive" &
        !is.na(q1) &
        !is.na(q2) &
        !is.na(q3) &
        (q3 - q1 - q2) < 0 ~ NA_real_,
      TRUE ~ q2
    ),
    cumulative_q2 = case_when(
      q2_basis == "additive" & (!is.na(q1) | !is.na(q2_clean)) ~
        rowSums(pick(q1, q2_clean), na.rm = TRUE),
      is.na(q2_clean) & !is.na(q1) ~ q1,
      TRUE ~ q2_clean
    ),
    # Convert Q3 to cumulative form according to the selected reporting basis.
    cumulative_q3 = case_when(
      q3_basis == "additive" &
        (!is.na(cumulative_q2) | !is.na(q3)) ~
        rowSums(pick(cumulative_q2, q3), na.rm = TRUE),
      is.na(q3) & !is.na(cumulative_q2) ~ cumulative_q2,
      TRUE ~ q3
    ),
    # For the sensitivity analysis, use the latest cumulative quarterly count when the annual count is missing.
    annual_uptake_raw = coalesce(end_2024, cumulative_q3),
    endpoint_method = case_when(
      !is.na(end_2024) ~ "direct_end_2024",
      !is.na(cumulative_q3) ~ "quarterly_carry_forward",
      TRUE ~ NA_character_
    ),
    latest_reported_period = case_when(
      !is.na(end_2024) ~ "end_2024",
      !is.na(q3) ~ "q3",
      !is.na(q2) ~ "q2",
      !is.na(q1) ~ "q1",
      TRUE ~ NA_character_
    ),
    endpoint_source = case_when(
      latest_reported_period == "end_2024" ~ source_end_2024,
      latest_reported_period == "q3" ~ source_q3,
      latest_reported_period == "q2" ~ source_q2,
      latest_reported_period == "q1" ~ source_q1,
      TRUE ~ NA_character_
    ),
    endpoint_input_table = case_when(
      latest_reported_period == "end_2024" ~ input_table_end_2024,
      latest_reported_period == "q3" ~ input_table_q3,
      latest_reported_period == "q2" ~ input_table_q2,
      latest_reported_period == "q1" ~ input_table_q1,
      TRUE ~ NA_character_
    ),
    endpoint_derivation = case_when(
      latest_reported_period == "end_2024" ~ derivation_end_2024,
      latest_reported_period == "q3" ~ derivation_q3,
      latest_reported_period == "q2" ~ derivation_q2,
      latest_reported_period == "q1" ~ derivation_q1,
      TRUE ~ NA_character_
    ),
    endpoint_correction_id = case_when(
      latest_reported_period == "end_2024" ~ correction_end_2024,
      latest_reported_period == "q3" ~ correction_q3,
      latest_reported_period == "q2" ~ correction_q2,
      latest_reported_period == "q1" ~ correction_q1,
      TRUE ~ NA_character_
    )
  )


# Construct the country-level audit dataset -------------------------------

# Match the denominator to the age range in the selected end-2024 TESSy report.
tessy_2024_older_population <- uptake_2024_selected %>%
  filter(period == "end_2024", source == "TESSY", group == "older", ages_60_plus) %>%
  select(iso, group) %>%
  left_join(pop_old %>% select(iso, pop_60p_2024), by = "iso")

# Classify the period covered independently of the table storing the report.
reporting_periods_2024 <- tibble(
  latest_reported_period = c("q1", "q2", "q3", "end_2024"),
  latest_reporting_period_end = as.Date(c(
    "2024-03-31", "2024-06-30", "2024-09-30", "2024-12-31"
  )),
  complete_year_report = c(FALSE, FALSE, FALSE, TRUE)
)

data_2024_uptake_country_audit <- entity_pop %>%
  filter(status_who == "Member State") %>%
  select(
    iso,
    region_who,
    income_group,
    status_who,
    pop_2024_old,
    pop_hcw
  ) %>%
  crossing(group = c("older", "hcw")) %>%
  left_join(
    uptake_2024_reconciled,
    by = c("iso", "group")
  ) %>%
  left_join(reporting_periods_2024, by = "latest_reported_period") %>%
  left_join(tessy_2024_older_population, by = c("iso", "group")) %>%
  mutate(
    denominator = case_when(
      group == "older" ~ coalesce(pop_60p_2024, pop_2024_old),
      group == "hcw" ~ pop_hcw,
      TRUE ~ NA_real_
    ),
    annual_uptake_capped = case_when(
      !is.na(annual_uptake_raw) & !is.na(denominator) ~
        pmin(annual_uptake_raw, denominator),
      TRUE ~ annual_uptake_raw
    ),
    capped = !is.na(annual_uptake_raw) &
      !is.na(denominator) &
      annual_uptake_raw > denominator,
    coverage = annual_uptake_capped / denominator
  ) %>%
  select(
    iso,
    region_who,
    income_group,
    status_who,
    group,
    q1,
    q2,
    q3,
    cumulative_q2,
    cumulative_q3,
    end_2024,
    q2_basis,
    q3_basis,
    annual_uptake_raw,
    annual_uptake_capped,
    denominator,
    coverage,
    capped,
    endpoint_method,
    latest_reported_period,
    latest_reporting_period_end,
    complete_year_report,
    endpoint_source,
    endpoint_input_table,
    endpoint_derivation,
    endpoint_correction_id
  ) %>%
  arrange(iso, group)

stopifnot(
  nrow(data_2024_uptake_country_audit) == 194L * 2L,
  !anyDuplicated(data_2024_uptake_country_audit[c("iso", "group")]),
  all(
    is.na(data_2024_uptake_country_audit$annual_uptake_raw) |
      data_2024_uptake_country_audit$annual_uptake_raw >= 0
  ),
  all(
    is.na(data_2024_uptake_country_audit$coverage) |
      data_2024_uptake_country_audit$coverage <= 1
  )
)

endpoint_counts_2024 <- data_2024_uptake_country_audit %>%
  filter(!is.na(endpoint_method)) %>%
  count(group, endpoint_method) %>%
  pivot_wider(
    names_from = endpoint_method,
    values_from = n,
    values_fill = 0
  )

# Check end-2024 counts against the June 2026 data extract, distinguishing
# direct reports from values carried forward.
endpoint_counts_expected <- tibble(
  group = c("hcw", "older"),
  direct_end_2024 = c(47L, 82L),
  quarterly_carry_forward = c(23L, 20L)
)

stopifnot(identical(
  endpoint_counts_2024 %>% select(group, direct_end_2024, quarterly_carry_forward) %>% arrange(group),
  endpoint_counts_expected
))


# Policy-stratified uptake uses the matching 2024 policy status by target group.
policy_2024_for_uptake <- data_routine_policy_2024 %>%
  filter(
    group %in% c("Older adults", "Health and care workers")
  ) %>%
  transmute(
    iso,
    group = recode(
      group,
      "Older adults" = "older",
      "Health and care workers" = "hcw"
    ),
    policy = status_policy
  )

stopifnot(
  !anyDuplicated(policy_2024_for_uptake[c("iso", "group")])
)

# Use the same country groups and population totals for both reporting periods.
uptake_2024_reporting_strata <- bind_rows(
  data_2024_uptake_country_audit %>%
    mutate(type = "all", grouping = "Global"),
  data_2024_uptake_country_audit %>%
    mutate(type = "region", grouping = as.character(region_who)),
  data_2024_uptake_country_audit %>%
    filter(!is.na(income_group), income_group != "") %>%
    mutate(type = "income", grouping = as.character(income_group)),
  data_2024_uptake_country_audit %>%
    left_join(policy_2024_for_uptake, by = c("iso", "group")) %>%
    filter(!is.na(policy), policy != "") %>%
    mutate(type = "policy", grouping = as.character(policy))
)

# Aggregate uptake by reporting period -----------------------------------

# Summaries use all countries for denominator availability and reporting
# countries for uptake numerators and reporting-population denominators.
# The complete-year restriction applies only to the reporting countries.
summarise_2024_uptake <- function(data, complete_year_only) {

  population_all <- data %>%
    filter(!is.na(denominator)) %>%
    group_by(type, grouping, group) %>%
    summarise(
      population_all = sum(denominator),
      .groups = "drop"
    )

  reporting <- data %>%
    filter(
      !is.na(annual_uptake_capped),
      !is.na(denominator),
      !complete_year_only | complete_year_report
    ) %>%
    group_by(type, grouping, group) %>%
    summarise(
      cumulative_q4 = sum(annual_uptake_capped),
      population_reporting = sum(denominator),
      count_reporting = n(),
      .groups = "drop"
    )

  full_join(
    population_all,
    reporting,
    by = c("type", "grouping", "group")
  ) %>%
    mutate(
      count_reporting = replace_na(count_reporting, 0L),
      population_reporting = replace_na(population_reporting, 0),
      coverage = cumulative_q4 / na_if(population_reporting, 0),
      proportion_population_reporting =
        population_reporting / population_all
    )
}

# Table 2 uses annual eJRF and TESSy values. The sensitivity analysis adds
# the latest cumulative quarterly value where an annual report is unavailable.
data_2024_uptake_grouping_audit <- summarise_2024_uptake(
  uptake_2024_reporting_strata, complete_year_only = TRUE
)
uptake_2024_all_reporters <- summarise_2024_uptake(
  uptake_2024_reporting_strata, complete_year_only = FALSE
)


# Assess reporting completeness within 2024 ------------------------------

country_names_2024 <- raw_population %>%
  filter(YEAR == 2024, GENDER == "BOTH", AGE_GROUP == "ALL") %>%
  transmute(iso = COUNTRY, country = COUNTRY_NAME) %>%
  distinct()

stopifnot(!anyDuplicated(country_names_2024$iso))

# Country and period contributions use the combined cohort, including quarterly reporters.
uptake_2024_all_reporter_totals <- uptake_2024_all_reporters %>%
  select(type, grouping, group,
         group_population_reporting = population_reporting,
         group_vaccinated = cumulative_q4,
         group_count_reporting = count_reporting)

data_2024_uptake_reporting_country <- uptake_2024_reporting_strata %>%
  filter(!is.na(annual_uptake_capped), !is.na(denominator)) %>%
  left_join(country_names_2024, by = "iso") %>%
  left_join(uptake_2024_all_reporter_totals, by = c("type", "grouping", "group")) %>%
  transmute(
    type, grouping, group, iso, country, region_who, income_group,
    latest_reported_period, latest_reporting_period_end, complete_year_report,
    endpoint_method, endpoint_source, endpoint_input_table,
    endpoint_derivation, endpoint_correction_id,
    vaccinated_raw = annual_uptake_raw,
    vaccinated = annual_uptake_capped,
    population = denominator,
    uptake = coverage,
    capped,
    group_count_reporting, group_population_reporting, group_vaccinated,
    population_share = population / group_population_reporting,
    vaccinated_share = if_else(group_vaccinated > 0,
                               vaccinated / group_vaccinated, NA_real_),
    uptake_contribution_pp = 100 * vaccinated / group_population_reporting
  ) %>%
  arrange(type, grouping, group, latest_reporting_period_end, iso)

# Summarize contributions by the latest reporting period, including zero counts.
uptake_2024_period_totals <- data_2024_uptake_reporting_country %>%
  group_by(type, grouping, group, latest_reported_period) %>%
  summarise(
    count_reporting = n(),
    population_reporting = sum(population),
    vaccinated = sum(vaccinated),
    .groups = "drop"
  )

data_2024_uptake_reporting_period <- uptake_2024_all_reporter_totals %>%
  crossing(reporting_periods_2024) %>%
  left_join(uptake_2024_period_totals,
            by = c("type", "grouping", "group", "latest_reported_period")) %>%
  mutate(
    count_reporting = replace_na(count_reporting, 0L),
    population_reporting = replace_na(population_reporting, 0),
    vaccinated = replace_na(vaccinated, 0),
    country_share = count_reporting / group_count_reporting,
    population_share = population_reporting / group_population_reporting,
    vaccinated_share = if_else(group_vaccinated > 0,
                               vaccinated / group_vaccinated, NA_real_),
    uptake_contribution_pp = 100 * vaccinated / group_population_reporting
  ) %>%
  arrange(type, grouping, group, latest_reporting_period_end)

# Express the effect of adding quarterly-only reporters relative to end-of-year reporting.
uptake_2024_sensitivity_long <- bind_rows(
  complete_year = data_2024_uptake_grouping_audit,
  all_reporters = uptake_2024_all_reporters,
  .id = "analysis"
) %>%
  transmute(
    type, grouping, group, analysis, population_all,
    count_reporting, population_reporting,
    vaccinated = replace_na(cumulative_q4, 0),
    uptake = coverage,
    proportion_population_reporting = population_reporting / population_all
  )

data_2024_uptake_sensitivity <- uptake_2024_sensitivity_long %>%
  pivot_wider(
    id_cols = c(type, grouping, group, population_all),
    names_from = analysis,
    values_from = c(count_reporting, population_reporting, vaccinated,
                    uptake, proportion_population_reporting)
  ) %>%
  mutate(
    count_quarterly_carry_forward = count_reporting_all_reporters - count_reporting_complete_year,
    population_quarterly_carry_forward =
      population_reporting_all_reporters - population_reporting_complete_year,
    vaccinated_quarterly_carry_forward = vaccinated_all_reporters - vaccinated_complete_year,
    population_share_quarterly_carry_forward =
      population_quarterly_carry_forward / population_reporting_all_reporters,
    vaccinated_share_quarterly_carry_forward = if_else(vaccinated_all_reporters > 0,
      vaccinated_quarterly_carry_forward / vaccinated_all_reporters, NA_real_),
    uptake_contribution_quarterly_pp =
      100 * vaccinated_quarterly_carry_forward / population_reporting_all_reporters,
    uptake_difference_pp = 100 * (uptake_all_reporters - uptake_complete_year)
  ) %>%
  arrange(type, grouping, group)

# Check country contributions against the aggregate for all reporters.
stopifnot(
  !anyDuplicated(data_2024_uptake_reporting_country[c("type", "grouping", "group", "iso")]),
  all(is.finite(data_2024_uptake_reporting_country$population)),
  all(data_2024_uptake_reporting_country$population > 0),
  all(!is.na(data_2024_uptake_reporting_country$complete_year_report)),
  all(data_2024_uptake_reporting_country$complete_year_report ==
        (data_2024_uptake_reporting_country$endpoint_method == "direct_end_2024")),
  all(data_2024_uptake_sensitivity$count_reporting_complete_year <=
        data_2024_uptake_sensitivity$count_reporting_all_reporters)
)

uptake_2024_contribution_check <- data_2024_uptake_reporting_period %>%
  group_by(type, grouping, group) %>%
  summarise(
    period_count = sum(count_reporting),
    period_population = sum(population_reporting),
    period_vaccinated = sum(vaccinated),
    period_contribution_pp = sum(uptake_contribution_pp),
    .groups = "drop"
  ) %>%
  left_join(data_2024_uptake_sensitivity, by = c("type", "grouping", "group"))

stopifnot(
  isTRUE(all.equal(uptake_2024_contribution_check$period_count,
                   uptake_2024_contribution_check$count_reporting_all_reporters)),
  isTRUE(all.equal(uptake_2024_contribution_check$period_population,
                   uptake_2024_contribution_check$population_reporting_all_reporters)),
  isTRUE(all.equal(uptake_2024_contribution_check$period_vaccinated,
                   uptake_2024_contribution_check$vaccinated_all_reporters)),
  isTRUE(all.equal(uptake_2024_contribution_check$period_contribution_pp,
                   100 * uptake_2024_contribution_check$uptake_all_reporters))
)


# Prepare main estimates for Table 2 -------------------------------------

# Put older adult and HCW summary fields side by side for Table 2.
prepare_group_columns <- function(data, selected_group, suffix) {
  count_suffix <- if (suffix == "old") "older" else suffix

  data %>%
    filter(group == selected_group) %>%
    transmute(
      grouping,
      type,
      !!paste0("cumulative_", suffix, "_q4") := cumulative_q4,
      !!paste0("cov_", suffix) := coverage,
      !!paste0("count_rep_", count_suffix) := count_reporting,
      !!paste0("prop_pop_", suffix) :=
        proportion_population_reporting
    )
}

raw_data_post_final <- prepare_group_columns(
  data_2024_uptake_grouping_audit,
  "older",
  "old"
) %>%
  full_join(
    prepare_group_columns(
      data_2024_uptake_grouping_audit,
      "hcw",
      "hcw"
    ),
    by = c("grouping", "type")
  ) %>%
  rename(
    cov_older = cov_old
  ) %>%
  select(
    grouping,
    type,
    cumulative_hcw_q4,
    cumulative_old_q4,
    cov_hcw,
    cov_older,
    count_rep_hcw,
    count_rep_older,
    prop_pop_hcw,
    prop_pop_old
  )

# Export the documented corrections using the common CSV convention.
export_cleaned_csv(data_corrections, "log_data_corrections.csv")


# Prepare Table 2 inputs --------------------------------------------------

data_tab_2_uptake_2024 <- raw_data_post_final %>%
  select(
    grouping,
    type,
    count_rep_older,
    count_rep_hcw,
    prop_pop_old,
    prop_pop_hcw,
    cov_older,
    cov_hcw
  ) %>%
  gather(
    key = indicator,
    value = value,
    -grouping,
    -type
  ) %>%
  mutate(
    group = case_when(
      grepl("hcw", indicator, ignore.case = TRUE) ~
        "Health and care workers",
      grepl("old", indicator, ignore.case = TRUE) ~ "Older adults",
      TRUE ~ NA_character_
    ),
    indicator = case_when(
      grepl("count", indicator, ignore.case = TRUE) ~
        "Count of end-of-year reporting countries",
      grepl("cov", indicator, ignore.case = TRUE) ~ "Uptake (%) among reporting countries",
      grepl("prop", indicator, ignore.case = TRUE) ~
        "Proportion of grouping population in reporting countries",
      TRUE ~ NA_character_
    )
  )


# End ---------------------------------------------------------------------
