# Prepare 2024 routine vaccination policy data -----------------------------

# Define the source policy groups retained for the figure and supplement.
policy_group_source_levels <- c(
  "Older adults",
  "Older adults with chronic conditions",
  "Adults",
  "Adults with chronic conditions",
  "Health and care workers",
  "Children & adolescents",
  "Child. & ado. with chronic conditions",
  "Pregnant women",
  "Other groups"
)

# Restrict the policy extract to vaccination and revaccination policy fields.
policy_source_indicators <- c(
  "POLICY_VACCINATION",
  "POLICY_REVACCINATION",
  "POLICY_INTERVAL",
  "POLICY_VACCINATION_COM"
)

# Map WHO indicator codes to short column names used downstream.
policy_indicator_names <- c(
  POLICY_VACCINATION = "policy_vacc",
  POLICY_REVACCINATION = "policy_revacc",
  POLICY_INTERVAL = "policy_revacc_interval",
  POLICY_VACCINATION_COM = "policy_comment"
)

# Convert source population-group codes to manuscript-facing labels.
map_policy_group <- function(group) {
  case_when(
    group == "OTHER_GROUPS" ~ "Other groups",
    group == "POLICY_RECOMMENDATION_ELDERLY" ~ "Older adults",
    group == "POLICY_RECOMMENDATION_ELDERLY_CHRONIC" ~
      "Older adults with chronic conditions",
    group == "POLICY_RECOMMENDATION_ADULTS" ~ "Adults",
    group == "POLICY_RECOMMENDATION_ADULTS_CHRONIC" ~
      "Adults with chronic conditions",
    group == "PW" ~ "Pregnant women",
    group == "POLICY_RECOMMENDATION_HCW" ~ "Health and care workers",
    group == "POLICY_RECOMMENDATION_CA" ~ "Children & adolescents",
    group == "POLICY_RECOMMENDATION_CA_CHRONIC" ~
      "Child. & ado. with chronic conditions",
    TRUE ~ group
  )
}

# Combine primary vaccination and revaccination answers into one status field.
classify_policy_status <- function(policy_vacc, policy_revacc) {
  case_when(
    policy_vacc == "YES" & policy_revacc == "YES" ~
      "Periodic revaccination & primary vaccination recommended",
    is.na(policy_vacc) & policy_revacc == "YES" ~
      "Periodic revaccination & primary vaccination recommended",
    policy_vacc == "NO" & policy_revacc == "YES" ~
      "Periodic revaccination & primary vaccination recommended",
    policy_vacc == "YES" & policy_revacc == "NO" ~
      "Primary vaccination only recommended",
    policy_vacc == "NO" & policy_revacc == "NO" ~
      "(Re-) vaccination not recommended",
    policy_vacc == "NO" & policy_revacc == "" ~
      "(Re-) vaccination not recommended",
    policy_vacc == "NO" & is.na(policy_revacc) ~
      "(Re-) vaccination not recommended",
    is.na(policy_vacc) & is.na(policy_revacc) ~ "No response provided",
    policy_vacc == "YES" & is.na(policy_revacc) ~
      "Primary vaccination only recommended",
    is.na(policy_vacc) & policy_revacc == "NO" ~
      "(Re-) vaccination not recommended",
    TRUE ~ NA_character_
  )
}

# Create one expected row for every Member State, group, and policy indicator.
data_policy_2024_scaffold <- entity_pop %>%
  filter(status_who == "Member State") %>%
  select(iso) %>%
  crossing(
    indicator = unname(policy_indicator_names),
    group = policy_group_source_levels
  ) %>%
  mutate(
    YEAR = 2024,
    REPORTING_PERIOD = 12,
    source = "EJRF"
  )

# Clean annual policy reports and retain explicit non-response rows.
data_policy_2024_annual_long <- raw_policy_annual %>%
  transmute(
    iso = if ("COUNTRY__CODE" %in% names(raw_policy_annual)) {
      COUNTRY__CODE
    } else {
      COUNTRY
    },
    YEAR = YEAR,
    indicator = INDCODE,
    group = DIMENSION2,
    value = trimws(as.character(VALUE)),
    source = SOURCE
  ) %>%
  
  # Negative sentinel and blank values indicate non-response in the source feed.
  filter(
    YEAR == 2024,
    indicator %in% policy_source_indicators,
    value != "-2222",
    value != "-4444",
    value != ""
  ) %>%
  mutate(
    REPORTING_PERIOD = 12,
    indicator = recode(indicator, !!!policy_indicator_names),
    group = map_policy_group(group)
  ) %>%
  right_join(
    # The right join keeps scaffold rows when no valid report was submitted.
    data_policy_2024_scaffold,
    by = c("iso", "indicator", "group", "YEAR",
           "REPORTING_PERIOD", "source")
  )

# The long policy table should contain one value per country-group-indicator.
stopifnot(!anyDuplicated(
  data_policy_2024_annual_long[
    c("iso", "YEAR", "REPORTING_PERIOD", "source", "group", "indicator")
  ]
))

# Pivot policy fields wide so each country-group has one classification row.
data_policy_2024_annual_wide <- data_policy_2024_annual_long %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  mutate(status_policy = classify_policy_status(policy_vacc, policy_revacc))


# Make entries friendly for visualization ---------------------------------

# Create entry orders.
levels_policy_a <- c(
  "Primary vaccination & periodic revaccination recommended",
  "Primary vaccination only recommended",
  "Vaccination not recommended"
)

levels_policy_b <- c(
  "Primary vaccination & periodic revaccination recommended",
  "Primary vaccination only recommended",
  "Vaccination not recommended",
  "No response provided"
)

levels_group <- c(
  "Older adults with chronic conditions",
  "Older adults",
  "Adults with chronic conditions",
  "Health and care workers",
  "Pregnant women",
  "Adults",
  "Child. & ado. with chronic conditions",
  "Children & adolescents"
)

# Standardize policy labels and attach grouping variables for figures and tables.
data_routine_policy_2024 <- data_policy_2024_annual_wide %>%
  select(iso,
         group,
         policy_revacc_interval,
         status_policy) %>%
  mutate(policy_revacc_interval = case_when(
    policy_revacc_interval == "12_MONTHS" ~ "12 months",
    policy_revacc_interval == "6_MONTHS" ~ "6 months",
    policy_revacc_interval == "OTHER" ~ "Other",
    TRUE ~ policy_revacc_interval
  ),
  status_policy = case_when(
    status_policy == "Periodic revaccination & primary vaccination recommended" ~ "Primary vaccination & periodic revaccination recommended",
    status_policy == "(Re-) vaccination not recommended" ~ "Vaccination not recommended",
    TRUE ~ status_policy
  )) %>%
  left_join(., entity_pop %>% select(iso, region_who, income_group), by = "iso")


# Prepare dataframes for visualization ------------------------------------

# Figure 7
wrap2 <- function(x) str_wrap(x, width = 18)  # Control policy label wrapping
pct_label_min <- 5                            # Hide small percent labels

# Data for panel a - policy count, global (responders only)
data_fig_6_policy_2024_all_compA <- data_routine_policy_2024 %>%
  filter(status_policy != "No response provided",
         group != "Other groups") %>%
  group_by(group, status_policy) %>%
  summarise(n = n_distinct(iso), .groups = "drop") %>%
  ungroup() %>%
  mutate(status_policy = factor(status_policy, levels = levels_policy_a),
         group = factor(group, levels = levels_group))

data_fig_6_policy_2024_all_compA_tot <- data_fig_6_policy_2024_all_compA %>%
  group_by(group) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  ungroup()

# Data for panel b - policy count (100%), by income group (includes non-responders)
data_fig_6_policy_2024_all_compB <- data_routine_policy_2024 %>%
  filter(!is.na(income_group),
         income_group != "",
         group != "Other groups") %>%
  group_by(group, income_group, status_policy) %>%
  summarise(n = n_distinct(iso), .groups = "drop") %>%
  group_by(group, income_group) %>%
  mutate(pct = 100 * n / sum(n),
         status_policy = factor(status_policy, levels = levels_policy_b),
         group = factor(group, levels_group),
         income_group = factor(income_group, levels = levels_income)) %>%
  ungroup()

# End ---------------------------------------------------------------------
