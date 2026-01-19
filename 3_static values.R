
# Total - global; all - count / population --------------------------------------

world_tot <- entity_pop %>%
  filter(status_who == "Member State") %>%
  summarise(
    pop_total_2021_world = sum(pop_2021, na.rm = TRUE),
    pop_total_2024_world = sum(pop_2024, na.rm = TRUE),
    count_world = n()
  )

list2env(as.list(world_tot), envir = .GlobalEnv)


# Total - region; all - count / population -------------------------------------

region_tot <- entity_pop %>%
  filter(status_who == "Member State") %>%
  summarise(
    pop_total_2021 = sum(pop_2021, na.rm = TRUE),
    pop_total_2024 = sum(pop_2024, na.rm = TRUE),
    count_region   = n(),
    .by = region_who
  ) %>%
  mutate(region = tolower(region_who)) %>%
  select(-region_who)

region_tot_pop_scal <- region_tot %>%
  pivot_longer(starts_with("pop_total_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", region)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_tot_pop_scal), envir = .GlobalEnv)

region_tot_count_scal <- region_tot %>%
  transmute(name = paste0("count_", region), value = count_region) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_tot_count_scal), envir = .GlobalEnv)


# Total - income; all - count / population -------------------------------------

income_tot <- entity_pop %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "") %>%
  summarise(
    pop_total_2021 = sum(pop_2021, na.rm = TRUE),
    pop_total_2024 = sum(pop_2024, na.rm = TRUE),
    count_income   = n(),
    .by = income_group
  ) %>%
  mutate(income = tolower(income_group)) %>%
  select(-income_group)

income_tot_pop_scal <- income_tot %>%
  pivot_longer(starts_with("pop_total_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", income)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_tot_pop_scal), envir = .GlobalEnv)

income_tot_count_scal <- income_tot %>%
  transmute(name = paste0("count_", income), value = count_income) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_tot_count_scal), envir = .GlobalEnv)


# Total - covax; all - count / population --------------------------------------

covax_tot <- entity_pop %>%
  filter(!is.na(status_covax),
         status_covax != "") %>%
  summarise(
    pop_total_2021 = sum(pop_2021, na.rm = TRUE),
    pop_total_2024 = sum(pop_2024, na.rm = TRUE),
    count_covax = n(),
    .by = status_covax
  ) %>%
  mutate(covax = tolower(status_covax)) %>%
  select(-status_covax)

covax_tot_pop_scal <- covax_tot %>%
  pivot_longer(starts_with("pop_total_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", covax)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_tot_pop_scal), envir = .GlobalEnv)

covax_tot_count_scal <- covax_tot %>%
  transmute(name = paste0("count_", covax), value = count_covax) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_tot_count_scal), envir = .GlobalEnv)


# Total - global; reporting - count / population --------------------------

## Reporting (a1d) WHO MS
rep_tot_a1d <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_tot_a1d_monotone), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_tot_a1d_monotone)) %>%
  summarise(
    pop_rep_tot_2021_a1d = sum(pop_2021, na.rm = TRUE),
    count_rep_tot_a1d = n()
  )

list2env(as.list(rep_tot_a1d), envir = .GlobalEnv)


## Reporting (cps) WHO MS
rep_tot_cps <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_tot_cps_monotone), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_tot_cps_monotone)) %>%
  summarise(
    pop_rep_tot_2021_cps = sum(pop_2021, na.rm = TRUE),
    count_rep_tot_cps = n()
  )

list2env(as.list(rep_tot_cps), envir = .GlobalEnv)


## Reporting (booster) WHO MS
rep_tot_boost <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_tot_boost_monotone), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_tot_boost_monotone)) %>%
  summarise(
    pop_rep_tot_2021_boost = sum(pop_2021, na.rm = TRUE),
    count_rep_tot_boost = n()
  )

list2env(as.list(rep_tot_boost), envir = .GlobalEnv)


# Total - region; reporting - count / population --------------------------

## Reporting (a1d) WHO MS
region_rep_tot_a1d <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_tot_a1d_monotone), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_tot_a1d_monotone)) %>%
  summarise(
    pop_rep_tot_2021_a1d = sum(pop_2021, na.rm = TRUE),
    count_rep_tot_a1d  = n(),
    .by = region_who
  ) %>%
  mutate(region = tolower(region_who)) %>%
  select(-region_who)

region_rep_tot_a1d_pop_scal <- region_rep_tot_a1d %>%
  pivot_longer(starts_with("pop_rep_tot_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", region)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_tot_a1d_pop_scal), envir = .GlobalEnv)

region_rep_tot_a1d_count_scal <- region_rep_tot_a1d %>%
  transmute(name = paste0("count_rep_tot_a1d_", region), 
            value = count_rep_tot_a1d) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_tot_a1d_count_scal), envir = .GlobalEnv)


## Reporting (cps) WHO MS
region_rep_tot_cps <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_tot_cps_monotone), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_tot_cps_monotone)) %>%
  summarise(
    pop_rep_tot_2021_cps = sum(pop_2021, na.rm = TRUE),
    count_rep_tot_cps = n(),
    .by = region_who
  ) %>%
  mutate(region = tolower(region_who)) %>%
  select(-region_who)

region_rep_tot_cps_pop_scal <- region_rep_tot_cps %>%
  pivot_longer(starts_with("pop_rep_tot_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", region)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_tot_cps_pop_scal), envir = .GlobalEnv)

region_rep_tot_cps_count_scal <- region_rep_tot_cps %>%
  transmute(name = paste0("count_rep_tot_cps_", region), 
            value = count_rep_tot_cps) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_tot_cps_count_scal), envir = .GlobalEnv)


## Reporting (booster) WHO MS
region_rep_tot_boost <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_tot_boost_monotone), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_tot_boost_monotone)) %>%
  summarise(
    pop_rep_tot_2021_boost = sum(pop_2021, na.rm = TRUE),
    count_rep_tot_boost = n(),
    .by = region_who
  ) %>%
  mutate(region = tolower(region_who)) %>%
  select(-region_who)

region_rep_tot_boost_pop_scal <- region_rep_tot_boost %>%
  pivot_longer(starts_with("pop_rep_tot_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", region)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_tot_boost_pop_scal), envir = .GlobalEnv)

region_rep_tot_boost_count_scal <- region_rep_tot_boost %>%
  transmute(name = paste0("count_rep_tot_boost_", region), 
            value = count_rep_tot_boost) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_tot_boost_count_scal), envir = .GlobalEnv)


# Total - income; reporting - count / population --------------------------

## Reporting (a1d) WHO MS
income_rep_tot_a1d <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_tot_a1d_monotone), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "",
         !is.na(adm_tot_a1d_monotone)) %>%
  summarise(
    pop_rep_tot_2021_a1d = sum(pop_2021, na.rm = TRUE),
    count_rep_tot_a1d  = n(),
    .by = income_group
  ) %>%
  mutate(income = tolower(income_group)) %>%
  select(-income_group)

income_rep_tot_a1d_pop_scal <- income_rep_tot_a1d %>%
  pivot_longer(starts_with("pop_rep_tot_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", income)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_tot_a1d_pop_scal), envir = .GlobalEnv)

income_rep_tot_a1d_count_scal <- income_rep_tot_a1d %>%
  transmute(name = paste0("count_rep_tot_a1d_", income), 
            value = count_rep_tot_a1d) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_tot_a1d_count_scal), envir = .GlobalEnv)


## Reporting (cps) WHO MS
income_rep_tot_cps <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_tot_cps_monotone), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "",
         !is.na(adm_tot_cps_monotone)) %>%
  summarise(
    pop_rep_tot_2021_cps = sum(pop_2021, na.rm = TRUE),
    count_rep_tot_cps = n(),
    .by = income_group
  ) %>%
  mutate(income = tolower(income_group)) %>%
  select(-income_group)

income_rep_tot_cps_pop_scal <- income_rep_tot_cps %>%
  pivot_longer(starts_with("pop_rep_tot_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", income)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_tot_cps_pop_scal), envir = .GlobalEnv)

income_rep_tot_cps_count_scal <- income_rep_tot_cps %>%
  transmute(name = paste0("count_rep_tot_cps_", income), 
            value = count_rep_tot_cps) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_tot_cps_count_scal), envir = .GlobalEnv)


## Reporting (booster) WHO MS
income_rep_tot_boost <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_tot_boost_monotone), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "",
         !is.na(adm_tot_boost_monotone)) %>%
  summarise(
    pop_rep_tot_2021_boost = sum(pop_2021, na.rm = TRUE),
    count_rep_tot_boost = n(),
    .by = income_group
  ) %>%
  mutate(income = tolower(income_group)) %>%
  select(-income_group)

income_rep_tot_boost_pop_scal <- income_rep_tot_boost %>%
  pivot_longer(starts_with("pop_rep_tot_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", income)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_tot_boost_pop_scal), envir = .GlobalEnv)

income_rep_tot_boost_count_scal <- income_rep_tot_boost %>%
  transmute(name = paste0("count_rep_tot_boost_", income), 
            value = count_rep_tot_boost) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_tot_boost_count_scal), envir = .GlobalEnv)


# Total - covax; reporting - count / population ---------------------------

## Reporting (a1d) WHO MS + 2 RE
covax_rep_tot_a1d <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_tot_a1d_monotone), 
            by = "iso") %>%
  filter(!is.na(status_covax),
         !is.na(adm_tot_a1d_monotone)) %>%
  summarise(
    pop_rep_tot_2021_a1d = sum(pop_2021, na.rm = TRUE),
    count_rep_tot_a1d  = n(),
    .by = status_covax
  ) %>%
  mutate(covax = tolower(status_covax)) %>%
  select(-status_covax)

covax_rep_tot_a1d_pop_scal <- covax_rep_tot_a1d %>%
  pivot_longer(starts_with("pop_rep_tot_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", covax)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_rep_tot_a1d_pop_scal), envir = .GlobalEnv)

covax_rep_tot_a1d_count_scal <- covax_rep_tot_a1d %>%
  transmute(name = paste0("count_rep_tot_a1d_", covax), 
            value = count_rep_tot_a1d) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_rep_tot_a1d_count_scal), envir = .GlobalEnv)


## Reporting (cps) WHO MS + 2 RE
covax_rep_tot_cps <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_tot_cps_monotone), 
            by = "iso") %>%
  filter(!is.na(status_covax),
         !is.na(adm_tot_cps_monotone)) %>%
  summarise(
    pop_rep_tot_2021_cps = sum(pop_2021, na.rm = TRUE),
    count_rep_tot_cps = n(),
    .by = status_covax
  ) %>%
  mutate(covax = tolower(status_covax)) %>%
  select(-status_covax)

covax_rep_tot_cps_pop_scal <- covax_rep_tot_cps %>%
  pivot_longer(starts_with("pop_rep_tot_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", covax)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_rep_tot_cps_pop_scal), envir = .GlobalEnv)

covax_rep_tot_cps_count_scal <- covax_rep_tot_cps %>%
  transmute(name = paste0("count_rep_tot_cps_", covax), 
            value = count_rep_tot_cps) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_rep_tot_cps_count_scal), envir = .GlobalEnv)


## Reporting (booster) WHO MS + 2 RE
covax_rep_tot_boost <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_tot_boost_monotone), 
            by = "iso") %>%
  filter(!is.na(status_covax),
         !is.na(adm_tot_boost_monotone)) %>%
  summarise(
    pop_rep_tot_2021_boost = sum(pop_2021, na.rm = TRUE),
    count_rep_tot_boost = n(),
    .by = status_covax
  ) %>%
  mutate(covax = tolower(status_covax)) %>%
  select(-status_covax)

covax_rep_tot_boost_pop_scal <- covax_rep_tot_boost %>%
  pivot_longer(starts_with("pop_rep_tot_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", covax)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_rep_tot_boost_pop_scal), envir = .GlobalEnv)

covax_rep_tot_boost_count_scal <- covax_rep_tot_boost %>%
  transmute(name = paste0("count_rep_tot_boost_", covax), 
            value = count_rep_tot_boost) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_rep_tot_boost_count_scal), envir = .GlobalEnv)

# Older adults - global - population --------------------------------------

world_old <- entity_pop %>%
  filter(status_who == "Member State") %>%
  summarise(
    pop_old_2021_world = sum(pop_2021_old, na.rm = TRUE),
    pop_old_2024_world = sum(pop_2024_old, na.rm = TRUE)
  )

list2env(as.list(world_old), envir = .GlobalEnv)


# Older adults - region - population --------------------------------------

region_old <- entity_pop %>%
  filter(status_who == "Member State") %>%
  summarise(
    pop_old_2021 = sum(pop_2021_old, na.rm = TRUE),
    pop_old_2024 = sum(pop_2024_old, na.rm = TRUE),
    .by = region_who
  ) %>%
  mutate(region = tolower(region_who)) %>%
  select(-region_who)

region_old_pop_scal <- region_old %>%
  pivot_longer(starts_with("pop_old_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", region)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_old_pop_scal), envir = .GlobalEnv)


# Older adults - income - population --------------------------------------

income_old <- entity_pop %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "") %>%
  summarise(
    pop_old_2021 = sum(pop_2021_old, na.rm = TRUE),
    pop_old_2024 = sum(pop_2024_old, na.rm = TRUE),
    .by = income_group
  ) %>%
  mutate(income = tolower(income_group)) %>%
  select(-income_group)

income_old_pop_scal <- income_old %>%
  pivot_longer(starts_with("pop_old_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", income)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_old_pop_scal), envir = .GlobalEnv)


# Older adults - covax - population ---------------------------------------

covax_old <- entity_pop %>%
  filter(!is.na(status_covax),
         status_covax != "") %>%
  summarise(
    pop_old_2021 = sum(pop_2021_old, na.rm = TRUE),
    pop_old_2024 = sum(pop_2024_old, na.rm = TRUE),
    .by = status_covax
  ) %>%
  mutate(covax = tolower(status_covax)) %>%
  select(-status_covax)

covax_old_pop_scal <- covax_old %>%
  pivot_longer(starts_with("pop_old_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", covax)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_old_pop_scal), envir = .GlobalEnv)


# Older adults - global; reporting - count / population -------------------

## Reporting (a1d) WHO MS
rep_old_a1d <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_old_a1d_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_old_a1d_monotone_interp)) %>%
  summarise(
    pop_rep_old_2021_a1d = sum(pop_2021_old, na.rm = TRUE),
    count_rep_old_a1d = n()
  )

list2env(as.list(rep_old_a1d), envir = .GlobalEnv)


## Reporting (cps) WHO MS
rep_old_cps <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_old_cps_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_old_cps_monotone_interp)) %>%
  summarise(
    pop_rep_old_2021_cps = sum(pop_2021_old, na.rm = TRUE),
    count_rep_old_cps = n()
  )

list2env(as.list(rep_old_cps), envir = .GlobalEnv)


## Reporting (booster) WHO MS
rep_old_boost <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_old_boost_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_old_boost_monotone_interp)) %>%
  summarise(
    pop_rep_old_2021_boost = sum(pop_2021_old, na.rm = TRUE),
    count_rep_old_boost = n()
  )

list2env(as.list(rep_old_boost), envir = .GlobalEnv)


# Older adults - income; reporting - count / population -------------------

## Reporting (a1d) WHO MS
income_rep_old_a1d <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_old_a1d_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "",
         !is.na(adm_old_a1d_monotone_interp)) %>%
  summarise(
    pop_rep_old_2021_a1d = sum(pop_2021_old, na.rm = TRUE),
    count_rep_old_a1d  = n(),
    .by = income_group
  ) %>%
  mutate(income = tolower(income_group)) %>%
  select(-income_group)

income_rep_old_a1d_pop_scal <- income_rep_old_a1d %>%
  pivot_longer(starts_with("pop_rep_old_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", income)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_old_a1d_pop_scal), envir = .GlobalEnv)

income_rep_old_a1d_count_scal <- income_rep_old_a1d %>%
  transmute(name = paste0("count_rep_old_a1d_", income), 
            value = count_rep_old_a1d) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_old_a1d_count_scal), envir = .GlobalEnv)


## Reporting (cps) WHO MS
income_rep_old_cps <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_old_cps_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "",
         !is.na(adm_old_cps_monotone_interp)) %>%
  summarise(
    pop_rep_old_2021_cps = sum(pop_2021_old, na.rm = TRUE),
    count_rep_old_cps = n(),
    .by = income_group
  ) %>%
  mutate(income = tolower(income_group)) %>%
  select(-income_group)

income_rep_old_cps_pop_scal <- income_rep_old_cps %>%
  pivot_longer(starts_with("pop_rep_old_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", income)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_old_cps_pop_scal), envir = .GlobalEnv)

income_rep_old_cps_count_scal <- income_rep_old_cps %>%
  transmute(name = paste0("count_rep_old_cps_", income), 
            value = count_rep_old_cps) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_old_cps_count_scal), envir = .GlobalEnv)


## Reporting (booster) WHO MS
income_rep_old_boost <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_old_boost_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "",
         !is.na(adm_old_boost_monotone_interp)) %>%
  summarise(
    pop_rep_old_2021_boost = sum(pop_2021_old, na.rm = TRUE),
    count_rep_old_boost = n(),
    .by = income_group
  ) %>%
  mutate(income = tolower(income_group)) %>%
  select(-income_group)

income_rep_old_boost_pop_scal <- income_rep_old_boost %>%
  pivot_longer(starts_with("pop_rep_old_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", income)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_old_boost_pop_scal), envir = .GlobalEnv)

income_rep_old_boost_count_scal <- income_rep_old_boost %>%
  transmute(name = paste0("count_rep_old_boost_", income), 
            value = count_rep_old_boost) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_old_boost_count_scal), envir = .GlobalEnv)


# Older adults - region; reporting - count / population -----------------------

## Reporting (a1d) WHO MS
region_rep_old_a1d <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_old_a1d_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_old_a1d_monotone_interp)) %>%
  summarise(
    pop_rep_old_2021_a1d = sum(pop_2021_old, na.rm = TRUE),
    count_rep_old_a1d  = n(),
    .by = region_who
  ) %>%
  mutate(region = tolower(region_who)) %>%
  select(-region_who)

region_rep_old_a1d_pop_scal <- region_rep_old_a1d %>%
  pivot_longer(starts_with("pop_rep_old_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", region)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_old_a1d_pop_scal), envir = .GlobalEnv)

region_rep_old_a1d_count_scal <- region_rep_old_a1d %>%
  transmute(name = paste0("count_rep_old_a1d_", region), 
            value = count_rep_old_a1d) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_old_a1d_count_scal), envir = .GlobalEnv)


## Reporting (cps) WHO MS
region_rep_old_cps <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_old_cps_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_old_cps_monotone_interp)) %>%
  summarise(
    pop_rep_old_2021_cps = sum(pop_2021_old, na.rm = TRUE),
    count_rep_old_cps = n(),
    .by = region_who
  ) %>%
  mutate(region = tolower(region_who)) %>%
  select(-region_who)

region_rep_old_cps_pop_scal <- region_rep_old_cps %>%
  pivot_longer(starts_with("pop_rep_old_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", region)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_old_cps_pop_scal), envir = .GlobalEnv)

region_rep_old_cps_count_scal <- region_rep_old_cps %>%
  transmute(name = paste0("count_rep_old_cps_", region), 
            value = count_rep_old_cps) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_old_cps_count_scal), envir = .GlobalEnv)


## Reporting (booster) WHO MS
region_rep_old_boost <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_old_boost_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_old_boost_monotone_interp)) %>%
  summarise(
    pop_rep_old_2021_boost = sum(pop_2021_old, na.rm = TRUE),
    count_rep_old_boost = n(),
    .by = region_who
  ) %>%
  mutate(region = tolower(region_who)) %>%
  select(-region_who)

region_rep_old_boost_pop_scal <- region_rep_old_boost %>%
  pivot_longer(starts_with("pop_rep_old_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", region)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_old_boost_pop_scal), envir = .GlobalEnv)

region_rep_old_boost_count_scal <- region_rep_old_boost %>%
  transmute(name = paste0("count_rep_old_boost_", region), 
            value = count_rep_old_boost) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_old_boost_count_scal), envir = .GlobalEnv)


# Older adults - covax; reporting - count / population --------------------

## Reporting (a1d) WHO MS + 2 RE
covax_rep_old_a1d <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_old_a1d_monotone_interp), 
            by = "iso") %>%
  filter(!is.na(status_covax),
         !is.na(adm_old_a1d_monotone_interp)) %>%
  summarise(
    pop_rep_old_2021_a1d = sum(pop_2021_old, na.rm = TRUE),
    count_rep_old_a1d  = n(),
    .by = status_covax
  ) %>%
  mutate(covax = tolower(status_covax)) %>%
  select(-status_covax)

covax_rep_old_a1d_pop_scal <- covax_rep_old_a1d %>%
  pivot_longer(starts_with("pop_rep_old_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", covax)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_rep_old_a1d_pop_scal), envir = .GlobalEnv)

covax_rep_old_a1d_count_scal <- covax_rep_old_a1d %>%
  transmute(name = paste0("count_rep_old_a1d_", covax), 
            value = count_rep_old_a1d) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_rep_old_a1d_count_scal), envir = .GlobalEnv)


## Reporting (cps) WHO MS + 2 RE
covax_rep_old_cps <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_old_cps_monotone_interp), 
            by = "iso") %>%
  filter(!is.na(status_covax),
         !is.na(adm_old_cps_monotone_interp)) %>%
  summarise(
    pop_rep_old_2021_cps = sum(pop_2021_old, na.rm = TRUE),
    count_rep_old_cps = n(),
    .by = status_covax
  ) %>%
  mutate(covax = tolower(status_covax)) %>%
  select(-status_covax)

covax_rep_old_cps_pop_scal <- covax_rep_old_cps %>%
  pivot_longer(starts_with("pop_rep_old_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", covax)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_rep_old_cps_pop_scal), envir = .GlobalEnv)

covax_rep_old_cps_count_scal <- covax_rep_old_cps %>%
  transmute(name = paste0("count_rep_old_cps_", covax), 
            value = count_rep_old_cps) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_rep_old_cps_count_scal), envir = .GlobalEnv)


## Reporting (booster) WHO MS + 2 RE
covax_rep_old_boost <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_old_boost_monotone_interp), 
            by = "iso") %>%
  filter(!is.na(status_covax),
         !is.na(adm_old_boost_monotone_interp)) %>%
  summarise(
    pop_rep_old_2021_boost = sum(pop_2021_old, na.rm = TRUE),
    count_rep_old_boost = n(),
    .by = status_covax
  ) %>%
  mutate(covax = tolower(status_covax)) %>%
  select(-status_covax)

covax_rep_old_boost_pop_scal <- covax_rep_old_boost %>%
  pivot_longer(starts_with("pop_rep_old_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", covax)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_rep_old_boost_pop_scal), envir = .GlobalEnv)

covax_rep_old_boost_count_scal <- covax_rep_old_boost %>%
  transmute(name = paste0("count_rep_old_boost_", covax), 
            value = count_rep_old_boost) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_rep_old_boost_count_scal), envir = .GlobalEnv)


# HCW - global - population -----------------------------------------------

world_hcw <- entity_pop %>%
  filter(status_who == "Member State") %>%
  summarise(
    pop_hcw_2019_world = sum(pop_hcw, na.rm = TRUE)
  )

list2env(as.list(world_hcw), envir = .GlobalEnv)


# HCW - region - population -----------------------------------------------

region_hcw <- entity_pop %>%
  filter(status_who == "Member State") %>%
  summarise(
    pop_hcw_2019 = sum(pop_hcw, na.rm = TRUE),
    .by = region_who
  ) %>%
  mutate(region = tolower(region_who)) %>%
  select(-region_who)

region_hcw_pop_scal <- region_hcw %>%
  pivot_longer(starts_with("pop_hcw_"), names_to = "metric", values_to = "value") %>%
  mutate(name = paste0(metric, "_", region)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_hcw_pop_scal), envir = .GlobalEnv)


# HCW - income - population -----------------------------------------------

income_hcw <- entity_pop %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "") %>%
  summarise(
    pop_hcw_2019 = sum(pop_hcw, na.rm = TRUE),
    .by = income_group
  ) %>%
  mutate(income = tolower(income_group)) %>%
  select(-income_group)

income_hcw_pop_scal <- income_hcw %>%
  pivot_longer(starts_with("pop_hcw_"), names_to = "metric", values_to = "value") %>%
  mutate(name = paste0(metric, "_", income)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_hcw_pop_scal), envir = .GlobalEnv)


# HCW - covax - population ------------------------------------------------

covax_hcw <- entity_pop %>%
  filter(!is.na(status_who),
         status_covax != "") %>%
  summarise(
    pop_hcw_2019 = sum(pop_hcw, na.rm = TRUE),
    .by = status_covax
  ) %>%
  mutate(covax = tolower(status_covax)) %>%
  select(-status_covax)

covax_hcw_pop_scal <- covax_hcw %>%
  pivot_longer(starts_with("pop_hcw_"), names_to = "metric", values_to = "value") %>%
  mutate(name = paste0(metric, "_", covax)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_hcw_pop_scal), envir = .GlobalEnv)


# HCW - global; reporting - count / population ----------------------------

## Reporting (a1d) WHO MS
rep_hcw_a1d <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_hcw_a1d_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_hcw_a1d_monotone_interp)) %>%
  summarise(
    pop_rep_hcw_a1d = sum(pop_hcw, na.rm = TRUE),
    count_rep_hcw_a1d = n()
  )

list2env(as.list(rep_hcw_a1d), envir = .GlobalEnv)


## Reporting (cps) WHO MS
rep_hcw_cps <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_hcw_cps_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_hcw_cps_monotone_interp)) %>%
  summarise(
    pop_rep_hcw_cps = sum(pop_hcw, na.rm = TRUE),
    count_rep_hcw_cps = n()
  )

list2env(as.list(rep_hcw_cps), envir = .GlobalEnv)


## Reporting (booster) WHO MS
rep_hcw_boost <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_hcw_boost_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_hcw_boost_monotone_interp)) %>%
  summarise(
    pop_rep_hcw_boost = sum(pop_hcw, na.rm = TRUE),
    count_rep_hcw_boost = n()
  )

list2env(as.list(rep_hcw_boost), envir = .GlobalEnv)


# HCW - income; reporting - count / population ----------------------------

## Reporting (a1d) WHO MS
income_rep_hcw_a1d <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_hcw_a1d_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "",
         !is.na(adm_hcw_a1d_monotone_interp)) %>%
  summarise(
    pop_rep_hcw_a1d = sum(pop_hcw, na.rm = TRUE),
    count_rep_hcw_a1d  = n(),
    .by = income_group
  ) %>%
  mutate(income = tolower(income_group)) %>%
  select(-income_group)

income_rep_hcw_a1d_pop_scal <- income_rep_hcw_a1d %>%
  pivot_longer(starts_with("pop_rep_hcw_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", income)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_hcw_a1d_pop_scal), envir = .GlobalEnv)

income_rep_hcw_a1d_count_scal <- income_rep_hcw_a1d %>%
  transmute(name = paste0("count_rep_hcw_a1d_", income), 
            value = count_rep_hcw_a1d) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_hcw_a1d_count_scal), envir = .GlobalEnv)


## Reporting (cps) WHO MS
income_rep_hcw_cps <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_hcw_cps_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "",
         !is.na(adm_hcw_cps_monotone_interp)) %>%
  summarise(
    pop_rep_hcw_cps = sum(pop_hcw, na.rm = TRUE),
    count_rep_hcw_cps = n(),
    .by = income_group
  ) %>%
  mutate(income = tolower(income_group)) %>%
  select(-income_group)

income_rep_hcw_cps_pop_scal <- income_rep_hcw_cps %>%
  pivot_longer(starts_with("pop_rep_hcw_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", income)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_hcw_cps_pop_scal), envir = .GlobalEnv)

income_rep_hcw_cps_count_scal <- income_rep_hcw_cps %>%
  transmute(name = paste0("count_rep_hcw_cps_", income), 
            value = count_rep_hcw_cps) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_hcw_cps_count_scal), envir = .GlobalEnv)


## Reporting (booster) WHO MS
income_rep_hcw_boost <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_hcw_boost_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "",
         !is.na(adm_hcw_boost_monotone_interp)) %>%
  summarise(
    pop_rep_hcw_boost = sum(pop_hcw, na.rm = TRUE),
    count_rep_hcw_boost = n(),
    .by = income_group
  ) %>%
  mutate(income = tolower(income_group)) %>%
  select(-income_group)

income_rep_hcw_boost_pop_scal <- income_rep_hcw_boost %>%
  pivot_longer(starts_with("pop_rep_hcw_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", income)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_hcw_boost_pop_scal), envir = .GlobalEnv)

income_rep_hcw_boost_count_scal <- income_rep_hcw_boost %>%
  transmute(name = paste0("count_rep_hcw_boost_", income), 
            value = count_rep_hcw_boost) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(income_rep_hcw_boost_count_scal), envir = .GlobalEnv)


# HCW - region; reporting - count / population ----------------------------

## Reporting (a1d) WHO MS
region_rep_hcw_a1d <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_hcw_a1d_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_hcw_a1d_monotone_interp)) %>%
  summarise(
    pop_rep_hcw_a1d = sum(pop_hcw, na.rm = TRUE),
    count_rep_hcw_a1d  = n(),
    .by = region_who
  ) %>%
  mutate(region = tolower(region_who)) %>%
  select(-region_who)

region_rep_hcw_a1d_pop_scal <- region_rep_hcw_a1d %>%
  pivot_longer(starts_with("pop_rep_hcw_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", region)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_hcw_a1d_pop_scal), envir = .GlobalEnv)

region_rep_hcw_a1d_count_scal <- region_rep_hcw_a1d %>%
  transmute(name = paste0("count_rep_hcw_a1d_", region), 
            value = count_rep_hcw_a1d) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_hcw_a1d_count_scal), envir = .GlobalEnv)


## Reporting (cps) WHO MS
region_rep_hcw_cps <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_hcw_cps_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_hcw_cps_monotone_interp)) %>%
  summarise(
    pop_rep_hcw_cps = sum(pop_hcw, na.rm = TRUE),
    count_rep_hcw_cps = n(),
    .by = region_who
  ) %>%
  mutate(region = tolower(region_who)) %>%
  select(-region_who)

region_rep_hcw_cps_pop_scal <- region_rep_hcw_cps %>%
  pivot_longer(starts_with("pop_rep_hcw_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", region)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_hcw_cps_pop_scal), envir = .GlobalEnv)

region_rep_hcw_cps_count_scal <- region_rep_hcw_cps %>%
  transmute(name = paste0("count_rep_hcw_cps_", region), 
            value = count_rep_hcw_cps) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_hcw_cps_count_scal), envir = .GlobalEnv)


## Reporting (booster) WHO MS
region_rep_hcw_boost <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_hcw_boost_monotone_interp), 
            by = "iso") %>%
  filter(status_who == "Member State",
         !is.na(adm_hcw_boost_monotone_interp)) %>%
  summarise(
    pop_rep_hcw_boost = sum(pop_hcw, na.rm = TRUE),
    count_rep_hcw_boost = n(),
    .by = region_who
  ) %>%
  mutate(region = tolower(region_who)) %>%
  select(-region_who)

region_rep_hcw_boost_pop_scal <- region_rep_hcw_boost %>%
  pivot_longer(starts_with("pop_rep_hcw_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", region)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_hcw_boost_pop_scal), envir = .GlobalEnv)

region_rep_hcw_boost_count_scal <- region_rep_hcw_boost %>%
  transmute(name = paste0("count_rep_hcw_boost_", region), 
            value = count_rep_hcw_boost) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_hcw_boost_count_scal), envir = .GlobalEnv)

# HCW - covax; reporting - count / population -----------------------------

## Reporting (a1d) WHO MS + 2 RE
covax_rep_hcw_a1d <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_hcw_a1d_monotone_interp), 
            by = "iso") %>%
  filter(!is.na(status_covax),
         !is.na(adm_hcw_a1d_monotone_interp)) %>%
  summarise(
    pop_rep_hcw_a1d = sum(pop_hcw, na.rm = TRUE),
    count_rep_hcw_a1d  = n(),
    .by = status_covax
  ) %>%
  mutate(covax = tolower(status_covax)) %>%
  select(-status_covax)

covax_rep_hcw_a1d_pop_scal <- covax_rep_hcw_a1d %>%
  pivot_longer(starts_with("pop_rep_hcw_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", covax)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_rep_hcw_a1d_pop_scal), envir = .GlobalEnv)

covax_rep_hcw_a1d_count_scal <- covax_rep_hcw_a1d %>%
  transmute(name = paste0("count_rep_hcw_a1d_", covax), 
            value = count_rep_hcw_a1d) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(region_rep_hcw_a1d_count_scal), envir = .GlobalEnv)


## Reporting (cps) WHO MS + 2 RE
covax_rep_hcw_cps <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_hcw_cps_monotone_interp), 
            by = "iso") %>%
  filter(!is.na(status_covax),
         !is.na(adm_hcw_cps_monotone_interp)) %>%
  summarise(
    pop_rep_hcw_cps = sum(pop_hcw, na.rm = TRUE),
    count_rep_hcw_cps = n(),
    .by = status_covax
  ) %>%
  mutate(covax = tolower(status_covax)) %>%
  select(-status_covax)

covax_rep_hcw_cps_pop_scal <- covax_rep_hcw_cps %>%
  pivot_longer(starts_with("pop_rep_hcw_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", covax)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_rep_hcw_cps_pop_scal), envir = .GlobalEnv)

covax_rep_hcw_cps_count_scal <- covax_rep_hcw_cps %>%
  transmute(name = paste0("count_rep_hcw_cps_", covax), 
            value = count_rep_hcw_cps) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_rep_hcw_cps_count_scal), envir = .GlobalEnv)


## Reporting (booster) WHO MS + 2 RE
covax_rep_hcw_boost <- entity_pop %>%
  left_join(., data_admin_last %>% select(iso, adm_hcw_boost_monotone_interp), 
            by = "iso") %>%
  filter(!is.na(status_covax),
         !is.na(adm_hcw_boost_monotone_interp)) %>%
  summarise(
    pop_rep_hcw_boost = sum(pop_hcw, na.rm = TRUE),
    count_rep_hcw_boost = n(),
    .by = status_covax
  ) %>%
  mutate(covax = tolower(status_covax)) %>%
  select(-status_covax)

covax_rep_hcw_boost_pop_scal <- covax_rep_hcw_boost %>%
  pivot_longer(starts_with("pop_rep_hcw_"), names_to = "metric", 
               values_to = "value") %>%
  mutate(name = paste0(metric, "_", covax)) %>%
  select(name, value) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_rep_hcw_boost_pop_scal), envir = .GlobalEnv)

covax_rep_hcw_boost_count_scal <- covax_rep_hcw_boost %>%
  transmute(name = paste0("count_rep_hcw_boost_", covax), 
            value = count_rep_hcw_boost) %>%
  pivot_wider(names_from = name, values_from = value)

list2env(as.list(covax_rep_hcw_boost_count_scal), envir = .GlobalEnv)


# End ---------------------------------------------------------------------