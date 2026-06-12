
# Make entries friendly for visualization ---------------------------------

# Create entry orders
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

# Filter & mutate relevant entries
data_routine_policy_2024 <- raw_policy_wide %>%
  filter(REPORTING_PERIOD == 12,
         a_status_who == "Member State") %>%
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

# Figure 7 preparation
wrap2 <- function(x) str_wrap(x, width = 18)  # tweak to control line breaks
pct_label_min <- 5                            # hide tiny % labels in Panel B

# Data for panel a - policy count, global (responders only)
data_fig_7_policy_2024_all_compA <- data_routine_policy_2024 %>%
  filter(status_policy != "No response provided",
         group != "Other groups") %>%
  group_by(group, status_policy) %>%
  summarise(n = n_distinct(iso), .groups = "drop") %>%
  ungroup() %>%
  mutate(status_policy = factor(status_policy, levels = levels_policy_a),
         group = factor(group, levels = levels_group))

data_fig_7_policy_2024_all_compA_tot <- data_fig_7_policy_2024_all_compA %>%
  group_by(group) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  ungroup()

# Data for panel b - policy count (100%), by income group (includes non-responders)
data_fig_7_policy_2024_all_compB <- data_routine_policy_2024 %>%
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


#Figure Supp. x preparation
data_fig_supp_7_policy_revaccint <- data_routine_policy_2024 %>%
  filter(policy_revacc_interval != "",
         group != "Other groups") %>%
  distinct(iso, group, policy_revacc_interval) %>%
  count(group, policy_revacc_interval, name = "n") %>%
  group_by(group) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(group = factor(group, levels = levels_group),
         policy_revacc_interval = factor(policy_revacc_interval, 
                                         levels = c("Other",
                                                    "6 months",
                                                    "12 months")))

data_fig_supp_7_policy_revaccint_totals <- data_fig_supp_7_policy_revaccint %>%
  distinct(group, total)


# End ---------------------------------------------------------------------