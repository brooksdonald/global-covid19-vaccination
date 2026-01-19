
# Entity details ----------------------------------------------------------

# Select, rename & modify relevant variables
base_entity <- raw_entity_details %>%
  transmute(iso = CODE,
            region_who = WHOREGIONC,
            income_group = WBINCOMESTATUS,
            status_who = WHO_LEGAL_STATUS_TITLE,
            status_covax = COVAX) %>%
  filter(!is.na(status_who)) %>%
  mutate(region_who = substr(region_who, 1, nchar(region_who) - 1),
         income_group = case_when(
           income_group == "Low income" ~ "LIC",
           income_group == "Lower middle income" ~ "LMIC",
           income_group == "Upper middle income" ~ "UMIC",
           grepl("High income", income_group) == TRUE ~ "HIC",
           TRUE ~ income_group
         ),
         status_covax = case_when(
           status_covax == "No" ~ "Non-COVAX",
           TRUE ~ status_covax
         )) %>%
  arrange(iso)


# Adhoc details -----------------------------------------------------------

# Select, rename & modify relevant variables
base_adhoc <- raw_adhoc %>%
  transmute(iso = iso,
            pol_old = pol_old,
            pol_old_source = pol_old_source,
            date_intro = min_vx_rollout_date,
            adm_tar_hcw = adm_tar_hcw,
            pop_hcw = pop_hcw)


# Population estimates ----------------------------------------------------

# Isolate required variables from raw data
raw_population_select <- raw_population %>%
  transmute(
    iso = COUNTRY,
    year = YEAR,
    group_age = AGE_GROUP,
    value = VALUE
  )

# Prepare 2021 / 2024 total population estimates
pop_tot <- raw_population_select %>%
  filter(group_age == "ALL") %>%
  select(iso,
         year,
         value) %>%
  pivot_wider(names_from = year, 
              values_from = value,
              names_prefix = "pop_")

# Prepare 2021 / 2024 older adult population estimates
cuts <- c(45, 50, 55, 60, 65, 70, 75, 80)

pop_old <- raw_population_select %>%
  filter(year %in% c(2021, 2024), group_age != "ALL") %>%
  mutate(age = as.integer(str_remove(group_age, "^Y"))) %>%
  filter(!is.na(age)) %>%
  group_by(iso, year) %>%
  summarise(
    pop_45p = sum(value[age >= 45], na.rm = TRUE),
    pop_50p = sum(value[age >= 50], na.rm = TRUE),
    pop_55p = sum(value[age >= 55], na.rm = TRUE),
    pop_60p = sum(value[age >= 60], na.rm = TRUE),
    pop_65p = sum(value[age >= 65], na.rm = TRUE),
    pop_70p = sum(value[age >= 70], na.rm = TRUE),
    pop_75p = sum(value[age >= 75], na.rm = TRUE),
    pop_80p = sum(value[age >= 80], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = year,
    values_from = starts_with("pop_"),
    names_glue  = "{.value}_{year}"
  ) %>%
  left_join(., base_adhoc %>% select(iso, pol_old), by = "iso") %>%
  mutate(pop_2021_old = case_when(
    pol_old == "45 and older" ~ pop_45p_2021,
    pol_old == "50 and older" ~ pop_50p_2021,
    pol_old == "55 and older" ~ pop_55p_2021,
    pol_old == "60 and older" ~ pop_60p_2021,
    pol_old == "65 and older" ~ pop_65p_2021,
    pol_old == "70 and older" ~ pop_70p_2021,
    pol_old == "75 and older" ~ pop_75p_2021,
    pol_old == "80 and older" ~ pop_80p_2021,
    TRUE ~ pop_60p_2021
  ),
  pop_2024_old = case_when(
    pol_old == "45 and older" ~ pop_45p_2024,
    pol_old == "50 and older" ~ pop_50p_2024,
    pol_old == "55 and older" ~ pop_55p_2024,
    pol_old == "60 and older" ~ pop_60p_2024,
    pol_old == "65 and older" ~ pop_65p_2024,
    pol_old == "70 and older" ~ pop_70p_2024,
    pol_old == "75 and older" ~ pop_75p_2024,
    pol_old == "80 and older" ~ pop_80p_2024,
    TRUE ~ pop_60p_2024
  ))


# Consolidate total and older adult population estimates
base_population <- pop_tot %>%
  left_join(pop_old %>% select(iso, pop_2021_old, pop_2024_old), by = "iso") %>%
  arrange(iso)


# Combine -----------------------------------------------------------------

# Merge entity & adhoc details and population dataframes
entity_pop <- base_entity %>%
  left_join(., base_adhoc, by = "iso") %>%
  left_join(., base_population, by = "iso")


# End ---------------------------------------------------------------------