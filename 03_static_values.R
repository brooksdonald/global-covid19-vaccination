# Static denominators used by the coverage, daily rate and reporting analyses
# Run through 00_master.R after uptake preparation and reconstruction.

# 1. Define country membership for each aggregate -------------------------

# Global, income and regional estimates use WHO Member States. COVAX groups
# include all entities with a recorded participation category.
country_groupings <- bind_rows(
  entity_pop %>%
    filter(status_who == "Member State") %>%
    transmute(iso, type = "all", dimension = NA_character_),
  entity_pop %>%
    filter(status_who == "Member State", !is.na(income_group), income_group != "") %>%
    transmute(iso, type = "income", dimension = income_group),
  entity_pop %>%
    filter(status_who == "Member State") %>%
    transmute(iso, type = "region", dimension = region_who),
  entity_pop %>%
    filter(!is.na(status_covax), status_covax != "") %>%
    transmute(iso, type = "covax", dimension = status_covax)
)

# 2. Sum all-country denominators -----------------------------------------

# Reporting percentages use the count of all eligible countries in each group.
population_all <- country_groupings %>%
  left_join(entity_pop %>% select(iso, pop_2021), by = "iso") %>%
  group_by(type, dimension) %>%
  summarise(population = sum(pop_2021, na.rm = TRUE),
            countries = n(), .groups = "drop")

# 3. Identify the countries contributing to each indicator denominator ----

# Reporting cohorts are fixed using availability at December 2023 after
# reconstruction. HCW availability is assessed before the denominator gap
# adjustment in 04_emergency_coverage_analysis.R.
reporting_population_country <- data_admin_last %>%
  select(iso, pop_2021, pop_2021_old, pop_hcw,
         adm_tot_td_monotone_interp,
         adm_tot_a1d_monotone_interp,
         adm_tot_cps_monotone_interp,
         adm_tot_boost_monotone_interp,
         adm_old_a1d_monotone_interp,
         adm_old_cps_monotone_interp,
         adm_old_boost_monotone_interp,
         adm_hcw_a1d_monotone_interp,
         adm_hcw_cps_monotone_interp,
         adm_hcw_boost_monotone_interp) %>%
  pivot_longer(ends_with("_monotone_interp"),
               names_to = "measure", values_to = "uptake") %>%
  mutate(measure = str_remove(measure, "_monotone_interp$"),
         population = case_when(
           str_detect(measure, "^adm_tot_") ~ pop_2021,
           str_detect(measure, "^adm_old_") ~ pop_2021_old,
           str_detect(measure, "^adm_hcw_") ~ pop_hcw
         )) %>%
  filter(!is.na(uptake)) %>%
  select(iso, measure, population)

# Sum the populations of countries with an available estimate for each indicator.
population_reporting <- country_groupings %>%
  inner_join(reporting_population_country, by = "iso", relationship = "many-to-many") %>%
  group_by(type, dimension, measure) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

stopifnot(
  !anyDuplicated(country_groupings[c("iso", "type")]),
  !anyDuplicated(population_all[c("type", "dimension")]),
  !anyDuplicated(population_reporting[c("type", "dimension", "measure")])
)

# End ---------------------------------------------------------------------
