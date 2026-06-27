# Reconstruct 2024 routine uptake for older adults and HCWs ----------------

# This analysis uses direct end-2024 country reports where available. When an
# end-2024 value is unavailable, Q1-Q3 reports are reconciled to a cumulative
# value and the latest available value is carried forward to 31 December 2024.
#
# The source extracts are intentionally limited to records used by this
# manuscript. Their upstream filenames and SHA-256 hashes are recorded in
# input/2024_uptake_source_manifest.csv. All manual decisions inherited from
# framework_new.R are recorded in input/data_corrections.csv.

path_uptake_annual_2024 <- file.path(
  path_input,
  "2024_uptake_annual_source.csv"
)
path_uptake_quarterly_2024 <- file.path(
  path_input,
  "2024_uptake_quarterly_source.csv"
)
stopifnot(
  file.exists(path_uptake_annual_2024),
  file.exists(path_uptake_quarterly_2024)
)

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

annual_2024_source <- read.csv(
  path_uptake_annual_2024,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
) %>%
  transmute(
    iso,
    group,
    period,
    source,
    dataset,
    value = as.numeric(value),
    derivation = "reported_target_group",
    correction_id = NA_character_
  ) %>%
  filter(
    group %in% c("older", "hcw"),
    !is.na(value),
    value >= 0
  )

quarterly_2024_source_raw <- read.csv(
  path_uptake_quarterly_2024,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
) %>%
  mutate(value = as.numeric(value))

# TESSY countries sometimes reported a pre-aggregated older-adult value and
# sometimes reported the component age groups. Use the pre-aggregated value
# when present; otherwise sum only 60-69, 70-79 and >=80 years. The previous
# framework accidentally included non-age target groups in this fallback sum.
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
  summarise(value = sum(value), .groups = "drop") %>%
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
    derivation = "reported_target_group",
    correction_id = NA_character_
  ) %>%
  bind_rows(quarterly_2024_age_sum)

uptake_2024_source <- bind_rows(
  annual_2024_source,
  quarterly_2024_source
)


# Apply documented corrections -------------------------------------------

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

# Resolve duplicate sources deterministically. Directly reported target-group
# values are preferred to older-age-band sums; within the same derivation,
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

uptake_2024_values_wide <- uptake_2024_selected %>%
  select(iso, group, period, value) %>%
  pivot_wider(names_from = period, values_from = value)

uptake_2024_sources_wide <- uptake_2024_selected %>%
  select(iso, group, period, source) %>%
  pivot_wider(
    names_from = period,
    values_from = source,
    names_prefix = "source_"
  )

uptake_2024_derivation_wide <- uptake_2024_selected %>%
  select(iso, group, period, derivation) %>%
  pivot_wider(
    names_from = period,
    values_from = derivation,
    names_prefix = "derivation_"
  )

uptake_2024_correction_wide <- uptake_2024_selected %>%
  select(iso, group, period, correction_id) %>%
  pivot_wider(
    names_from = period,
    values_from = correction_id,
    names_prefix = "correction_"
  )

basis_overrides <- data_2024_uptake_corrections %>%
  filter(action == "set_reporting_basis") %>%
  select(iso, group, period, reporting_basis) %>%
  pivot_wider(
    names_from = period,
    values_from = reporting_basis,
    names_prefix = "basis_override_"
  )

uptake_2024_reconciled <- uptake_2024_values_wide %>%
  full_join(
    uptake_2024_sources_wide,
    by = c("iso", "group")
  ) %>%
  full_join(
    uptake_2024_derivation_wide,
    by = c("iso", "group")
  ) %>%
  full_join(
    uptake_2024_correction_wide,
    by = c("iso", "group")
  ) %>%
  left_join(basis_overrides, by = c("iso", "group")) %>%
  mutate(
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
    cumulative_q3 = case_when(
      q3_basis == "additive" &
        (!is.na(cumulative_q2) | !is.na(q3)) ~
        rowSums(pick(cumulative_q2, q3), na.rm = TRUE),
      is.na(q3) & !is.na(cumulative_q2) ~ cumulative_q2,
      TRUE ~ q3
    ),
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
  mutate(
    denominator = case_when(
      group == "older" ~ pop_2024_old,
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
    endpoint_source,
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

stopifnot(
  endpoint_counts_2024$direct_end_2024[
    endpoint_counts_2024$group == "older"
  ] == 81L,
  endpoint_counts_2024$quarterly_carry_forward[
    endpoint_counts_2024$group == "older"
  ] == 21L,
  endpoint_counts_2024$direct_end_2024[
    endpoint_counts_2024$group == "hcw"
  ] == 47L,
  endpoint_counts_2024$quarterly_carry_forward[
    endpoint_counts_2024$group == "hcw"
  ] == 23L
)


# Aggregate the single capped country dataset -----------------------------

summarise_2024_uptake <- function(data, grouping, type_label) {
  grouping <- enquo(grouping)

  population_all <- data %>%
    filter(!is.na(denominator)) %>%
    group_by(grouping = !!grouping, group) %>%
    summarise(
      population_all = sum(denominator),
      .groups = "drop"
    )

  reporting <- data %>%
    filter(
      !is.na(annual_uptake_capped),
      !is.na(denominator)
    ) %>%
    group_by(grouping = !!grouping, group) %>%
    summarise(
      cumulative_q4 = sum(annual_uptake_capped),
      population_reporting = sum(denominator),
      count_reporting = n(),
      .groups = "drop"
    )

  full_join(
    population_all,
    reporting,
    by = c("grouping", "group")
  ) %>%
    mutate(
      coverage = cumulative_q4 / population_reporting,
      proportion_population_reporting =
        population_reporting / population_all,
      type = type_label
    )
}

uptake_2024_region <- summarise_2024_uptake(
  data_2024_uptake_country_audit,
  region_who,
  "region"
)

uptake_2024_income <- data_2024_uptake_country_audit %>%
  filter(!is.na(income_group), income_group != "") %>%
  summarise_2024_uptake(income_group, "income")

uptake_2024_global <- data_2024_uptake_country_audit %>%
  mutate(global = "Global") %>%
  summarise_2024_uptake(global, "all")

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

uptake_2024_policy <- data_2024_uptake_country_audit %>%
  left_join(
    policy_2024_for_uptake,
    by = c("iso", "group")
  ) %>%
  filter(!is.na(policy), policy != "") %>%
  summarise_2024_uptake(policy, "policy")

data_2024_uptake_grouping_audit <- bind_rows(
  uptake_2024_region,
  uptake_2024_income,
  uptake_2024_policy,
  uptake_2024_global
)

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

if (isTRUE(export_outputs)) {
  write.csv(
    data_corrections,
    file.path(path_save, "log_data_corrections.csv"),
    row.names = FALSE,
    na = ""
  )
}


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
        "Count of reporting MS",
      grepl("cov", indicator, ignore.case = TRUE) ~ "Uptake (%)",
      grepl("prop", indicator, ignore.case = TRUE) ~
        "Proportion of grouping population in reporting MS",
      TRUE ~ NA_character_
    )
  )


# End ---------------------------------------------------------------------
