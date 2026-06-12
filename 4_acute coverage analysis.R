
# Calculate coverage time series from administrative data, country --------

data_cov_time_country <- data_admin_time_interp %>%
  
  # Calculate coverage in total population
  mutate(cov_tot_a1d = pmin(adm_tot_a1d_monotone / pop_2021, 1),
         cov_tot_cps = pmin(adm_tot_cps_monotone / pop_2021, 1),
         cov_tot_boost = pmin(adm_tot_boost_monotone / pop_2021, 1),
  
  # Calculate admin. data adjustment-related fields
         hcw_flag = if_else(pop_hcw > adm_tar_hcw, "Yes", NA_character_),
         hcw_diff = pmax(pop_hcw - adm_tar_hcw, 0, na.rm = TRUE),
  
  # Adjust admin. data for HCWs
         adm_hcw_a1d_monotone_interp = pmin(
           adm_hcw_a1d_monotone_interp + (hcw_diff * cov_tot_a1d), pop_hcw),
         adm_hcw_cps_monotone_interp = pmin(
           adm_hcw_cps_monotone_interp + (hcw_diff * cov_tot_cps), pop_hcw),
         adm_hcw_boost_monotone_interp = pmin(
           adm_hcw_boost_monotone_interp + (hcw_diff * cov_tot_boost), pop_hcw),   
  
  # Calculate coverage in priority populations
         cov_hcw_a1d = pmin(adm_hcw_a1d_monotone_interp / pop_hcw, 1),
         cov_hcw_cps = pmin(adm_hcw_cps_monotone_interp / pop_hcw, 1),
         cov_hcw_boost = pmin(adm_hcw_boost_monotone_interp / pop_hcw, 1),
  
         cov_old_a1d = pmin(adm_old_a1d_monotone_interp / pop_2021_old, 1),
         cov_old_cps = pmin(adm_old_cps_monotone_interp / pop_2021_old, 1),
         cov_old_boost = pmin(adm_old_boost_monotone_interp / pop_2021_old, 1))


# Calculate 2021-2023 coverage summary figures ----------------------------

# Total - global
cov_acute_summary_all <- data_cov_time_country %>%
  filter(status_who == "Member State") %>%
  select(iso,
         month_name,
         cov_tot_cps,
         cov_tot_boost,
         cov_old_cps,
         cov_old_boost,
         cov_hcw_cps,
         cov_hcw_boost) %>%
  group_by(month_name) %>%
  summarise(
    avg_cov_tot_cps = mean(cov_tot_cps, na.rm = TRUE),
    med_cov_tot_cps = median(cov_tot_cps, na.rm = TRUE), 
    perc_25_tot_cps = quantile(cov_tot_cps, 0.25, na.rm = TRUE),
    perc_75_tot_cps = quantile(cov_tot_cps, 0.75, na.rm = TRUE),
    n_tot_cps = sum(!is.na(cov_tot_cps)),
    
    avg_cov_tot_boost = mean(cov_tot_boost, na.rm = TRUE),
    med_cov_tot_boost = median(cov_tot_boost, na.rm = TRUE), 
    perc_25_tot_boost = quantile(cov_tot_boost, 0.25, na.rm = TRUE),
    perc_75_tot_boost = quantile(cov_tot_boost, 0.75, na.rm = TRUE),
    n_tot_boost = sum(!is.na(cov_tot_boost)),
    
    avg_cov_old_cps = mean(cov_old_cps, na.rm = TRUE),
    med_cov_old_cps = median(cov_old_cps, na.rm = TRUE), 
    perc_25_old_cps = quantile(cov_old_cps, 0.25, na.rm = TRUE),
    perc_75_old_cps = quantile(cov_old_cps, 0.75, na.rm = TRUE),
    n_old_cps = sum(!is.na(cov_old_cps)),
    
    avg_cov_old_boost = mean(cov_old_boost, na.rm = TRUE),
    med_cov_old_boost = median(cov_old_boost, na.rm = TRUE), 
    perc_25_old_boost = quantile(cov_old_boost, 0.25, na.rm = TRUE),
    perc_75_old_boost = quantile(cov_old_boost, 0.75, na.rm = TRUE),
    n_old_boost = sum(!is.na(cov_old_boost)),
    
    avg_cov_hcw_cps = mean(cov_hcw_cps, na.rm = TRUE),
    med_cov_hcw_cps = median(cov_hcw_cps, na.rm = TRUE), 
    perc_25_hcw_cps = quantile(cov_hcw_cps, 0.25, na.rm = TRUE),
    perc_75_hcw_cps = quantile(cov_hcw_cps, 0.75, na.rm = TRUE),
    n_hcw_cps = sum(!is.na(cov_hcw_cps)),
    
    avg_cov_hcw_boost = mean(cov_hcw_boost, na.rm = TRUE),
    med_cov_hcw_boost = median(cov_hcw_boost, na.rm = TRUE), 
    perc_25_hcw_boost = quantile(cov_hcw_boost, 0.25, na.rm = TRUE),
    perc_75_hcw_boost = quantile(cov_hcw_boost, 0.75, na.rm = TRUE),
    n_hcw_boost = sum(!is.na(cov_hcw_boost))
  ) %>%
  mutate(
    type = "global",
    dimension = NA
  ) %>%
  ungroup()

# Total - income group
cov_acute_summary_income <- data_cov_time_country %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "") %>%
  select(iso,
         month_name,
         income_group,
         cov_tot_cps,
         cov_tot_boost,
         cov_old_cps,
         cov_old_boost,
         cov_hcw_cps,
         cov_hcw_boost) %>%
  group_by(month_name, income_group) %>%
  summarise(
    avg_cov_tot_cps = mean(cov_tot_cps, na.rm = TRUE),
    med_cov_tot_cps = median(cov_tot_cps, na.rm = TRUE), 
    perc_25_tot_cps = quantile(cov_tot_cps, 0.25, na.rm = TRUE),
    perc_75_tot_cps = quantile(cov_tot_cps, 0.75, na.rm = TRUE),
    n_tot_cps = sum(!is.na(cov_tot_cps)),
    
    avg_cov_tot_boost = mean(cov_tot_boost, na.rm = TRUE),
    med_cov_tot_boost = median(cov_tot_boost, na.rm = TRUE), 
    perc_25_tot_boost = quantile(cov_tot_boost, 0.25, na.rm = TRUE),
    perc_75_tot_boost = quantile(cov_tot_boost, 0.75, na.rm = TRUE),
    n_tot_boost = sum(!is.na(cov_tot_boost)),
    
    avg_cov_old_cps = mean(cov_old_cps, na.rm = TRUE),
    med_cov_old_cps = median(cov_old_cps, na.rm = TRUE), 
    perc_25_old_cps = quantile(cov_old_cps, 0.25, na.rm = TRUE),
    perc_75_old_cps = quantile(cov_old_cps, 0.75, na.rm = TRUE),
    n_old_cps = sum(!is.na(cov_old_cps)),
    
    avg_cov_old_boost = mean(cov_old_boost, na.rm = TRUE),
    med_cov_old_boost = median(cov_old_boost, na.rm = TRUE), 
    perc_25_old_boost = quantile(cov_old_boost, 0.25, na.rm = TRUE),
    perc_75_old_boost = quantile(cov_old_boost, 0.75, na.rm = TRUE),
    n_old_boost = sum(!is.na(cov_old_boost)),
    
    avg_cov_hcw_cps = mean(cov_hcw_cps, na.rm = TRUE),
    med_cov_hcw_cps = median(cov_hcw_cps, na.rm = TRUE), 
    perc_25_hcw_cps = quantile(cov_hcw_cps, 0.25, na.rm = TRUE),
    perc_75_hcw_cps = quantile(cov_hcw_cps, 0.75, na.rm = TRUE),
    n_hcw_cps = sum(!is.na(cov_hcw_cps)),
    
    avg_cov_hcw_boost = mean(cov_hcw_boost, na.rm = TRUE),
    med_cov_hcw_boost = median(cov_hcw_boost, na.rm = TRUE), 
    perc_25_hcw_boost = quantile(cov_hcw_boost, 0.25, na.rm = TRUE),
    perc_75_hcw_boost = quantile(cov_hcw_boost, 0.75, na.rm = TRUE),
    n_hcw_boost = sum(!is.na(cov_hcw_boost))
  ) %>%
  mutate(
    type = "income"
  ) %>%
  rename(dimension = income_group) %>%
  ungroup()

# Total - WHO region
cov_acute_summary_region <- data_cov_time_country %>%
  filter(status_who == "Member State") %>%
  select(iso,
         month_name,
         region_who,
         cov_tot_cps,
         cov_tot_boost,
         cov_old_cps,
         cov_old_boost,
         cov_hcw_cps,
         cov_hcw_boost) %>%
  group_by(month_name, region_who) %>%
  summarise(
    avg_cov_tot_cps = mean(cov_tot_cps, na.rm = TRUE),
    med_cov_tot_cps = median(cov_tot_cps, na.rm = TRUE), 
    perc_25_tot_cps = quantile(cov_tot_cps, 0.25, na.rm = TRUE),
    perc_75_tot_cps = quantile(cov_tot_cps, 0.75, na.rm = TRUE),
    n_tot_cps = sum(!is.na(cov_tot_cps)),
    
    avg_cov_tot_boost = mean(cov_tot_boost, na.rm = TRUE),
    med_cov_tot_boost = median(cov_tot_boost, na.rm = TRUE), 
    perc_25_tot_boost = quantile(cov_tot_boost, 0.25, na.rm = TRUE),
    perc_75_tot_boost = quantile(cov_tot_boost, 0.75, na.rm = TRUE),
    n_tot_boost = sum(!is.na(cov_tot_boost)),
    
    avg_cov_old_cps = mean(cov_old_cps, na.rm = TRUE),
    med_cov_old_cps = median(cov_old_cps, na.rm = TRUE), 
    perc_25_old_cps = quantile(cov_old_cps, 0.25, na.rm = TRUE),
    perc_75_old_cps = quantile(cov_old_cps, 0.75, na.rm = TRUE),
    n_old_cps = sum(!is.na(cov_old_cps)),
    
    avg_cov_old_boost = mean(cov_old_boost, na.rm = TRUE),
    med_cov_old_boost = median(cov_old_boost, na.rm = TRUE), 
    perc_25_old_boost = quantile(cov_old_boost, 0.25, na.rm = TRUE),
    perc_75_old_boost = quantile(cov_old_boost, 0.75, na.rm = TRUE),
    n_old_boost = sum(!is.na(cov_old_boost)),
    
    avg_cov_hcw_cps = mean(cov_hcw_cps, na.rm = TRUE),
    med_cov_hcw_cps = median(cov_hcw_cps, na.rm = TRUE), 
    perc_25_hcw_cps = quantile(cov_hcw_cps, 0.25, na.rm = TRUE),
    perc_75_hcw_cps = quantile(cov_hcw_cps, 0.75, na.rm = TRUE),
    n_hcw_cps = sum(!is.na(cov_hcw_cps)),
    
    avg_cov_hcw_boost = mean(cov_hcw_boost, na.rm = TRUE),
    med_cov_hcw_boost = median(cov_hcw_boost, na.rm = TRUE), 
    perc_25_hcw_boost = quantile(cov_hcw_boost, 0.25, na.rm = TRUE),
    perc_75_hcw_boost = quantile(cov_hcw_boost, 0.75, na.rm = TRUE),
    n_hcw_boost = sum(!is.na(cov_hcw_boost))
  ) %>%
  mutate(
    type = "region"
  ) %>%
  rename(dimension = region_who) %>%
  ungroup()

# Total - COVAX participation category
cov_acute_summary_covax <- data_cov_time_country %>%
  filter(!is.na(status_covax)) %>%
  select(iso,
         month_name,
         status_covax,
         cov_tot_cps,
         cov_tot_boost,
         cov_old_cps,
         cov_old_boost,
         cov_hcw_cps,
         cov_hcw_boost) %>%
  group_by(month_name, status_covax) %>%
  summarise(
    avg_cov_tot_cps = mean(cov_tot_cps, na.rm = TRUE),
    med_cov_tot_cps = median(cov_tot_cps, na.rm = TRUE), 
    perc_25_tot_cps = quantile(cov_tot_cps, 0.25, na.rm = TRUE),
    perc_75_tot_cps = quantile(cov_tot_cps, 0.75, na.rm = TRUE),
    n_tot_cps = sum(!is.na(cov_tot_cps)),
    
    avg_cov_tot_boost = mean(cov_tot_boost, na.rm = TRUE),
    med_cov_tot_boost = median(cov_tot_boost, na.rm = TRUE), 
    perc_25_tot_boost = quantile(cov_tot_boost, 0.25, na.rm = TRUE),
    perc_75_tot_boost = quantile(cov_tot_boost, 0.75, na.rm = TRUE),
    n_tot_boost = sum(!is.na(cov_tot_boost)),
    
    avg_cov_old_cps = mean(cov_old_cps, na.rm = TRUE),
    med_cov_old_cps = median(cov_old_cps, na.rm = TRUE), 
    perc_25_old_cps = quantile(cov_old_cps, 0.25, na.rm = TRUE),
    perc_75_old_cps = quantile(cov_old_cps, 0.75, na.rm = TRUE),
    n_old_cps = sum(!is.na(cov_old_cps)),
    
    avg_cov_old_boost = mean(cov_old_boost, na.rm = TRUE),
    med_cov_old_boost = median(cov_old_boost, na.rm = TRUE), 
    perc_25_old_boost = quantile(cov_old_boost, 0.25, na.rm = TRUE),
    perc_75_old_boost = quantile(cov_old_boost, 0.75, na.rm = TRUE),
    n_old_boost = sum(!is.na(cov_old_boost)),
    
    avg_cov_hcw_cps = mean(cov_hcw_cps, na.rm = TRUE),
    med_cov_hcw_cps = median(cov_hcw_cps, na.rm = TRUE), 
    perc_25_hcw_cps = quantile(cov_hcw_cps, 0.25, na.rm = TRUE),
    perc_75_hcw_cps = quantile(cov_hcw_cps, 0.75, na.rm = TRUE),
    n_hcw_cps = sum(!is.na(cov_hcw_cps)),
    
    avg_cov_hcw_boost = mean(cov_hcw_boost, na.rm = TRUE),
    med_cov_hcw_boost = median(cov_hcw_boost, na.rm = TRUE), 
    perc_25_hcw_boost = quantile(cov_hcw_boost, 0.25, na.rm = TRUE),
    perc_75_hcw_boost = quantile(cov_hcw_boost, 0.75, na.rm = TRUE),
    n_hcw_boost = sum(!is.na(cov_hcw_boost)),
  ) %>%
  mutate(
    type = "covax"
  ) %>%
  rename(dimension = status_covax) %>%
  ungroup()

# Total - combine
cov_acute_summary <- bind_rows(cov_acute_summary_all,
                               cov_acute_summary_income,
                               cov_acute_summary_region,
                               cov_acute_summary_covax) %>%
  pivot_longer(cols = matches("^(avg_cov|med_cov|perc_25|perc_75|n)_(tot|hcw|old)_(cps|boost)$"),
               names_to = c(".value", "group", "indicator"),
               names_pattern = "^(.*)_(tot|hcw|old)_(cps|boost)$")

  # Possible correction for reporting-driven artifacts in med/IQR
  # arrange(type, dimension, group, indicator, month_name) %>%
  # group_by(type, dimension, group, indicator) %>%
  # mutate(
  #   avg_cov  = cummax(replace_na(avg_cov,  -Inf)),
  #   med_cov  = cummax(replace_na(med_cov,  -Inf)),
  #   perc_25  = cummax(replace_na(perc_25,  -Inf)),
  #   perc_75  = cummax(replace_na(perc_75,  -Inf))
  # ) %>%
  # mutate(
  #   across(c(avg_cov, med_cov, perc_25, perc_75), ~na_if(.x, -Inf))
  # ) %>%
  # ungroup()


# Create end-2021 / end-2023 data frames ----------------------------------

# Filter to relevant time point & select relevant variables
data_cov_2123 <- data_cov_time_country %>%
  filter(month_name == as.Date("2021-12-01") |
           month_name == as.Date("2023-12-01")) %>%
  select(-hcw_diff,
         -hcw_flag,
         -adm_tar_hcw)

# Filter to end-2023
data_cov_last <- data_cov_time_country %>%
  filter(month_name == as.Date("2023-12-01")) %>%
  select(-month_name)


# Calculate 2021-2023 coverage timeseries, world --------------------------

data_cov_time_world_full <- data_admin_time_interp %>%
  
  # Summarize across all Member States
  filter(status_who == "Member State") %>%
  group_by(month_name) %>%
  summarize(adm_tot_td = sum(adm_tot_td, na.rm = TRUE),
            adm_tot_a1d = sum(adm_tot_a1d, na.rm = TRUE),
            adm_tot_cps = sum(adm_tot_cps, na.rm = TRUE),
            adm_tot_boost = sum(adm_tot_boost, na.rm = TRUE),
            
            adm_old_a1d = sum(adm_old_a1d_monotone_interp, na.rm = TRUE),
            adm_old_cps = sum(adm_old_cps_monotone_interp, na.rm = TRUE),
            adm_old_boost = sum(adm_old_boost_monotone_interp, na.rm = TRUE),
            
            adm_hcw_a1d = sum(adm_hcw_a1d_monotone_interp, na.rm = TRUE),
            adm_hcw_cps = sum(adm_hcw_cps_monotone_interp, na.rm = TRUE),
            adm_hcw_boost = sum(adm_hcw_boost_monotone_interp, na.rm = TRUE)) %>% 
  pivot_longer(cols = -month_name, names_to = "measure", values_to = "admin") %>%
  
  # Assign appropriate population value per group & measure
  mutate(
    pop = case_when(
      str_detect(measure, "tot_a1d") ~ pop_rep_tot_2021_a1d,
      str_detect(measure, "tot_cps") ~ pop_rep_tot_2021_cps,
      str_detect(measure, "tot_boost") ~ pop_rep_tot_2021_boost,
      str_detect(measure, "old_a1d") ~ pop_rep_old_2021_a1d,
      str_detect(measure, "old_cps") ~ pop_rep_old_2021_cps,
      str_detect(measure, "old_boost") ~ pop_rep_old_2021_boost,
      str_detect(measure, "hcw_a1d") ~ pop_rep_hcw_a1d,
      str_detect(measure, "hcw_cps") ~ pop_rep_hcw_cps,
      str_detect(measure, "hcw_boost") ~ pop_rep_hcw_boost,
      TRUE ~ NA
      ),
    
  # Assign friendly group & measure variables for visualization
    group = case_when(
      str_detect(measure, "tot") ~ "Total population",
      str_detect(measure, "old") ~ "Older adults",
      str_detect(measure, "hcw") ~ "Health and care workers",
      TRUE ~ NA
      ),
    measure = case_when(
      str_detect(measure, "td") ~ "Total doses",
      str_detect(measure, "a1d") ~ "At least one dose",
      str_detect(measure, "cps") ~ "Complete primary series",
      str_detect(measure, "boost") ~ "At least one booster dose",
      TRUE ~ NA
    ),
  
  # Calculate coverage
    coverage = case_when(
      measure != "Total doses" ~ admin / pop,
      TRUE ~ NA
    ), 
    
    # Add type and dimension for future merge
    type = "all",
    dimension = NA) %>%
  ungroup()


# Calculate 2021-2023 coverage timeseries, income -------------------------

data_cov_time_income_full <- data_admin_time_interp %>%
  
  # Summarize across all Member States
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "") %>%
  group_by(month_name,
           income_group) %>%
  summarize(adm_tot_td = sum(adm_tot_td, na.rm = TRUE),
            adm_tot_a1d = sum(adm_tot_a1d, na.rm = TRUE),
            adm_tot_cps = sum(adm_tot_cps, na.rm = TRUE),
            adm_tot_boost = sum(adm_tot_boost, na.rm = TRUE),
            
            adm_old_a1d = sum(adm_old_a1d_monotone_interp, na.rm = TRUE),
            adm_old_cps = sum(adm_old_cps_monotone_interp, na.rm = TRUE),
            adm_old_boost = sum(adm_old_boost_monotone_interp, na.rm = TRUE),
            
            adm_hcw_a1d = sum(adm_hcw_a1d_monotone_interp, na.rm = TRUE),
            adm_hcw_cps = sum(adm_hcw_cps_monotone_interp, na.rm = TRUE),
            adm_hcw_boost = sum(adm_hcw_boost_monotone_interp, na.rm = TRUE)) %>% 
  pivot_longer(cols = -c("month_name", "income_group"), names_to = "measure", 
               values_to = "admin") %>%
  rename(dimension = income_group) %>%
  
  # Assign appropriate population value per group & measure    
  mutate(
    pop = case_when(
      str_detect(measure, "tot_a1d") & dimension == "HIC" ~ pop_rep_tot_2021_a1d_hic,
      str_detect(measure, "tot_a1d") & dimension == "UMIC" ~ pop_rep_tot_2021_a1d_umic,
      str_detect(measure, "tot_a1d") & dimension == "LMIC" ~ pop_rep_tot_2021_a1d_lmic,
      str_detect(measure, "tot_a1d") & dimension == "LIC" ~ pop_rep_tot_2021_a1d_lic,
      str_detect(measure, "tot_cps") & dimension == "HIC" ~ pop_rep_tot_2021_cps_hic,
      str_detect(measure, "tot_cps") & dimension == "UMIC" ~ pop_rep_tot_2021_cps_umic,
      str_detect(measure, "tot_cps") & dimension == "LMIC" ~ pop_rep_tot_2021_cps_lmic,
      str_detect(measure, "tot_cps") & dimension == "LIC" ~ pop_rep_tot_2021_cps_lic,
      str_detect(measure, "tot_boost") & dimension == "HIC" ~ pop_rep_tot_2021_boost_hic,
      str_detect(measure, "tot_boost") & dimension == "UMIC" ~ pop_rep_tot_2021_boost_umic,
      str_detect(measure, "tot_boost") & dimension == "LMIC" ~ pop_rep_tot_2021_boost_lmic,
      str_detect(measure, "tot_boost") & dimension == "LIC" ~ pop_rep_tot_2021_boost_lic,
      str_detect(measure, "old_a1d") & dimension == "HIC" ~ pop_rep_old_2021_a1d_hic,
      str_detect(measure, "old_a1d") & dimension == "UMIC" ~ pop_rep_old_2021_a1d_umic,
      str_detect(measure, "old_a1d") & dimension == "LMIC" ~ pop_rep_old_2021_a1d_lmic,
      str_detect(measure, "old_a1d") & dimension == "LIC" ~ pop_rep_old_2021_a1d_lic,
      str_detect(measure, "old_cps") & dimension == "HIC" ~ pop_rep_old_2021_cps_hic,
      str_detect(measure, "old_cps") & dimension == "UMIC" ~ pop_rep_old_2021_cps_umic,
      str_detect(measure, "old_cps") & dimension == "LMIC" ~ pop_rep_old_2021_cps_lmic,
      str_detect(measure, "old_cps") & dimension == "LIC" ~ pop_rep_old_2021_cps_lic,
      str_detect(measure, "old_boost") & dimension == "HIC" ~ pop_rep_old_2021_boost_hic,
      str_detect(measure, "old_boost") & dimension == "UMIC" ~ pop_rep_old_2021_boost_umic,
      str_detect(measure, "old_boost") & dimension == "LMIC" ~ pop_rep_old_2021_boost_lmic,
      str_detect(measure, "old_boost") & dimension == "LIC" ~ pop_rep_old_2021_boost_lic,
      str_detect(measure, "hcw_a1d") & dimension == "HIC" ~ pop_rep_hcw_a1d_hic,
      str_detect(measure, "hcw_a1d") & dimension == "UMIC" ~ pop_rep_hcw_a1d_umic,
      str_detect(measure, "hcw_a1d") & dimension == "LMIC" ~ pop_rep_hcw_a1d_lmic,
      str_detect(measure, "hcw_a1d") & dimension == "LIC" ~ pop_rep_hcw_a1d_lic,
      str_detect(measure, "hcw_cps") & dimension == "HIC" ~ pop_rep_hcw_cps_hic,
      str_detect(measure, "hcw_cps") & dimension == "UMIC" ~ pop_rep_hcw_cps_umic,
      str_detect(measure, "hcw_cps") & dimension == "LMIC" ~ pop_rep_hcw_cps_lmic,
      str_detect(measure, "hcw_cps") & dimension == "LIC" ~ pop_rep_hcw_cps_lic,
      str_detect(measure, "hcw_boost") & dimension == "HIC" ~ pop_rep_hcw_boost_hic,
      str_detect(measure, "hcw_boost") & dimension == "UMIC" ~ pop_rep_hcw_boost_umic,
      str_detect(measure, "hcw_boost") & dimension == "LMIC" ~ pop_rep_hcw_boost_lmic,
      str_detect(measure, "hcw_boost") & dimension == "LIC" ~ pop_rep_hcw_boost_lic,
      TRUE ~ NA
    ),
    
    # Assign friendly group & measure variables for visualization
    group = case_when(
      str_detect(measure, "tot") ~ "Total population",
      str_detect(measure, "old") ~ "Older adults",
      str_detect(measure, "hcw") ~ "Health and care workers",
      TRUE ~ NA
    ),
    measure = case_when(
      str_detect(measure, "td") ~ "Total doses",
      str_detect(measure, "a1d") ~ "At least one dose",
      str_detect(measure, "cps") ~ "Complete primary series",
      str_detect(measure, "boost") ~ "At least one booster dose",
      TRUE ~ NA
    ),
    
    # Calculate coverage
    coverage = case_when(
      measure != "Total doses" ~ admin / pop,
      TRUE ~ NA
    ), 
    
    # Add type and dimension for future merge
    type = "income") %>%
  ungroup()


# Calculate 2021-2023 coverage timeseries, region ---------------------------------------------

data_cov_time_region_full <- `data_admin_time_interp` %>%
  
  # Summarize across all Member States
  filter(status_who == "Member State") %>%
  group_by(month_name,
           region_who) %>%
  summarize(adm_tot_td = sum(adm_tot_td, na.rm = TRUE),
            adm_tot_a1d = sum(adm_tot_a1d, na.rm = TRUE),
            adm_tot_cps = sum(adm_tot_cps, na.rm = TRUE),
            adm_tot_boost = sum(adm_tot_boost, na.rm = TRUE),
            
            adm_old_a1d = sum(adm_old_a1d_monotone_interp, na.rm = TRUE),
            adm_old_cps = sum(adm_old_cps_monotone_interp, na.rm = TRUE),
            adm_old_boost = sum(adm_old_boost_monotone_interp, na.rm = TRUE),
            
            adm_hcw_a1d = sum(adm_hcw_a1d_monotone_interp, na.rm = TRUE),
            adm_hcw_cps = sum(adm_hcw_cps_monotone_interp, na.rm = TRUE),
            adm_hcw_boost = sum(adm_hcw_boost_monotone_interp, na.rm = TRUE)) %>% 
  pivot_longer(cols = -c("month_name", "region_who"), names_to = "measure", 
               values_to = "admin") %>%
  rename(dimension = region_who) %>%
  
  # Assign appropriate population value per group & measure    
  mutate(
    pop = case_when(
      str_detect(measure, "tot_a1d") & dimension == "AFR" ~ pop_rep_tot_2021_a1d_afr,
      str_detect(measure, "tot_a1d") & dimension == "AMR" ~ pop_rep_tot_2021_a1d_amr,
      str_detect(measure, "tot_a1d") & dimension == "EMR" ~ pop_rep_tot_2021_a1d_emr,
      str_detect(measure, "tot_a1d") & dimension == "EUR" ~ pop_rep_tot_2021_a1d_eur,
      str_detect(measure, "tot_a1d") & dimension == "SEAR" ~ pop_rep_tot_2021_a1d_sear,
      str_detect(measure, "tot_a1d") & dimension == "WPR" ~ pop_rep_tot_2021_a1d_wpr,
      str_detect(measure, "tot_cps") & dimension == "AFR" ~ pop_rep_tot_2021_cps_afr,
      str_detect(measure, "tot_cps") & dimension == "AMR" ~ pop_rep_tot_2021_cps_amr,
      str_detect(measure, "tot_cps") & dimension == "EMR" ~ pop_rep_tot_2021_cps_emr,
      str_detect(measure, "tot_cps") & dimension == "EUR" ~ pop_rep_tot_2021_cps_eur,
      str_detect(measure, "tot_cps") & dimension == "SEAR" ~ pop_rep_tot_2021_cps_sear,
      str_detect(measure, "tot_cps") & dimension == "WPR" ~ pop_rep_tot_2021_cps_wpr,
      str_detect(measure, "tot_boost") & dimension == "AFR" ~ pop_rep_tot_2021_boost_afr,
      str_detect(measure, "tot_boost") & dimension == "AMR" ~ pop_rep_tot_2021_boost_amr,
      str_detect(measure, "tot_boost") & dimension == "EMR" ~ pop_rep_tot_2021_boost_emr,
      str_detect(measure, "tot_boost") & dimension == "EUR" ~ pop_rep_tot_2021_boost_eur,
      str_detect(measure, "tot_boost") & dimension == "SEAR" ~ pop_rep_tot_2021_boost_sear,
      str_detect(measure, "tot_boost") & dimension == "WPR" ~ pop_rep_tot_2021_boost_wpr,
      str_detect(measure, "old_a1d") & dimension == "AFR" ~ pop_rep_old_2021_a1d_afr,
      str_detect(measure, "old_a1d") & dimension == "AMR" ~ pop_rep_old_2021_a1d_amr,
      str_detect(measure, "old_a1d") & dimension == "EMR" ~ pop_rep_old_2021_a1d_emr,
      str_detect(measure, "old_a1d") & dimension == "EUR" ~ pop_rep_old_2021_a1d_eur,
      str_detect(measure, "old_a1d") & dimension == "SEAR" ~ pop_rep_old_2021_a1d_sear,
      str_detect(measure, "old_a1d") & dimension == "WPR" ~ pop_rep_old_2021_a1d_wpr,
      str_detect(measure, "old_cps") & dimension == "AFR" ~ pop_rep_old_2021_cps_afr,
      str_detect(measure, "old_cps") & dimension == "AMR" ~ pop_rep_old_2021_cps_amr,
      str_detect(measure, "old_cps") & dimension == "EMR" ~ pop_rep_old_2021_cps_emr,
      str_detect(measure, "old_cps") & dimension == "EUR" ~ pop_rep_old_2021_cps_eur,
      str_detect(measure, "old_cps") & dimension == "SEAR" ~ pop_rep_old_2021_cps_sear,
      str_detect(measure, "old_cps") & dimension == "WPR" ~ pop_rep_old_2021_cps_wpr,
      str_detect(measure, "old_boost") & dimension == "AFR" ~ pop_rep_old_2021_boost_afr,
      str_detect(measure, "old_boost") & dimension == "AMR" ~ pop_rep_old_2021_boost_amr,
      str_detect(measure, "old_boost") & dimension == "EMR" ~ pop_rep_old_2021_boost_emr,
      str_detect(measure, "old_boost") & dimension == "EUR" ~ pop_rep_old_2021_boost_eur,
      str_detect(measure, "old_boost") & dimension == "SEAR" ~ pop_rep_old_2021_boost_sear,
      str_detect(measure, "old_boost") & dimension == "WPR" ~ pop_rep_old_2021_boost_wpr,
      str_detect(measure, "hcw_a1d") & dimension == "AFR" ~ pop_rep_hcw_a1d_afr,
      str_detect(measure, "hcw_a1d") & dimension == "AMR" ~ pop_rep_hcw_a1d_amr,
      str_detect(measure, "hcw_a1d") & dimension == "EMR" ~ pop_rep_hcw_a1d_emr,
      str_detect(measure, "hcw_a1d") & dimension == "EUR" ~ pop_rep_hcw_a1d_eur,
      str_detect(measure, "hcw_a1d") & dimension == "SEAR" ~ pop_rep_hcw_a1d_sear,
      str_detect(measure, "hcw_a1d") & dimension == "WPR" ~ pop_rep_hcw_a1d_wpr,
      str_detect(measure, "hcw_cps") & dimension == "AFR" ~ pop_rep_hcw_cps_afr,
      str_detect(measure, "hcw_cps") & dimension == "AMR" ~ pop_rep_hcw_cps_amr,
      str_detect(measure, "hcw_cps") & dimension == "EMR" ~ pop_rep_hcw_cps_emr,
      str_detect(measure, "hcw_cps") & dimension == "EUR" ~ pop_rep_hcw_cps_eur,
      str_detect(measure, "hcw_cps") & dimension == "SEAR" ~ pop_rep_hcw_cps_sear,
      str_detect(measure, "hcw_cps") & dimension == "WPR" ~ pop_rep_hcw_cps_wpr,
      str_detect(measure, "hcw_boost") & dimension == "AFR" ~ pop_rep_hcw_boost_afr,
      str_detect(measure, "hcw_boost") & dimension == "AMR" ~ pop_rep_hcw_boost_amr,
      str_detect(measure, "hcw_boost") & dimension == "EMR" ~ pop_rep_hcw_boost_emr,
      str_detect(measure, "hcw_boost") & dimension == "EUR" ~ pop_rep_hcw_boost_eur,
      str_detect(measure, "hcw_boost") & dimension == "SEAR" ~ pop_rep_hcw_boost_sear,
      str_detect(measure, "hcw_boost") & dimension == "WPR" ~ pop_rep_hcw_boost_wpr,
      TRUE ~ NA
    ),
    
    # Assign friendly group & measure variables for visualization
    group = case_when(
      str_detect(measure, "tot") ~ "Total population",
      str_detect(measure, "old") ~ "Older adults",
      str_detect(measure, "hcw") ~ "Health and care workers",
      TRUE ~ NA
    ),
    measure = case_when(
      str_detect(measure, "td") ~ "Total doses",
      str_detect(measure, "a1d") ~ "At least one dose",
      str_detect(measure, "cps") ~ "Complete primary series",
      str_detect(measure, "boost") ~ "At least one booster dose",
      TRUE ~ NA
    ),
    
    # Calculate coverage
    coverage = case_when(
      measure != "Total doses" ~ admin / pop,
      TRUE ~ NA
    ), 
    
    # Add type and dimension for future merge
    type = "region") %>%
  ungroup()


# Calculate 2021-2023 coverage timeseries, covax ---------------------------------------------

data_cov_time_covax_full <- data_admin_time_interp %>%
  
  # Summarize across all WHO MS & 2 REs
  filter(!is.na(status_covax)) %>%
  group_by(month_name,
           status_covax) %>%
  summarize(adm_tot_td = sum(adm_tot_td, na.rm = TRUE),
            adm_tot_a1d = sum(adm_tot_a1d, na.rm = TRUE),
            adm_tot_cps = sum(adm_tot_cps, na.rm = TRUE),
            adm_tot_boost = sum(adm_tot_boost, na.rm = TRUE),
            
            adm_old_a1d = sum(adm_old_a1d_monotone_interp, na.rm = TRUE),
            adm_old_cps = sum(adm_old_cps_monotone_interp, na.rm = TRUE),
            adm_old_boost = sum(adm_old_boost_monotone_interp, na.rm = TRUE),
            
            adm_hcw_a1d = sum(adm_hcw_a1d_monotone_interp, na.rm = TRUE),
            adm_hcw_cps = sum(adm_hcw_cps_monotone_interp, na.rm = TRUE),
            adm_hcw_boost = sum(adm_hcw_boost_monotone_interp, na.rm = TRUE)) %>% 
  pivot_longer(cols = -c("month_name", "status_covax"), names_to = "measure", 
               values_to = "admin") %>%
  rename(dimension = status_covax) %>%
  
  # Assign appropriate population value per group & measure    
  mutate(
    pop = case_when(
      str_detect(measure, "tot_a1d") & dimension == "AMC" ~ pop_rep_tot_2021_a1d_amc,
      str_detect(measure, "tot_a1d") & dimension == "Self-financing" ~ `pop_rep_tot_2021_a1d_self-financing`,
      str_detect(measure, "tot_a1d") & dimension == "Non-COVAX" ~ `pop_rep_tot_2021_a1d_non-covax`,
      str_detect(measure, "tot_cps") & dimension == "AMC" ~ pop_rep_tot_2021_cps_amc,
      str_detect(measure, "tot_cps") & dimension == "Self-financing" ~ `pop_rep_tot_2021_cps_self-financing`,
      str_detect(measure, "tot_cps") & dimension == "Non-COVAX" ~ `pop_rep_tot_2021_cps_non-covax`,
      str_detect(measure, "tot_boost") & dimension == "AMC" ~ pop_rep_tot_2021_boost_amc,
      str_detect(measure, "tot_boost") & dimension == "Self-financing" ~ `pop_rep_tot_2021_boost_self-financing`,
      str_detect(measure, "tot_boost") & dimension == "Non-COVAX" ~ `pop_total_2021_non-covax`,
      str_detect(measure, "old_a1d") & dimension == "AMC" ~ pop_rep_old_2021_a1d_amc,
      str_detect(measure, "old_a1d") & dimension == "Self-financing" ~ `pop_rep_old_2021_a1d_self-financing`,
      str_detect(measure, "old_a1d") & dimension == "Non-COVAX" ~ `pop_rep_old_2021_a1d_non-covax`,
      str_detect(measure, "old_cps") & dimension == "AMC" ~ pop_rep_old_2021_cps_amc,
      str_detect(measure, "old_cps") & dimension == "Self-financing" ~ `pop_rep_old_2021_cps_self-financing`,
      str_detect(measure, "old_cps") & dimension == "Non-COVAX" ~ `pop_rep_old_2021_cps_non-covax`,
      str_detect(measure, "old_boost") & dimension == "AMC" ~ pop_rep_old_2021_cps_amc,
      str_detect(measure, "old_boost") & dimension == "Self-financing" ~ `pop_rep_old_2021_boost_self-financing`,
      str_detect(measure, "old_boost") & dimension == "Non-COVAX" ~ `pop_rep_old_2021_boost_non-covax`,
      str_detect(measure, "hcw_a1d") & dimension == "AMC" ~ pop_rep_hcw_a1d_amc,
      str_detect(measure, "hcw_a1d") & dimension == "Self-financing" ~ `pop_rep_hcw_a1d_self-financing`,
      str_detect(measure, "hcw_a1d") & dimension == "Non-COVAX" ~ `pop_rep_hcw_a1d_non-covax`,
      str_detect(measure, "hcw_cps") & dimension == "AMC" ~ pop_rep_hcw_cps_amc,
      str_detect(measure, "hcw_cps") & dimension == "Self-financing" ~ `pop_rep_hcw_cps_self-financing`,
      str_detect(measure, "hcw_cps") & dimension == "Non-COVAX" ~ `pop_rep_hcw_cps_non-covax`,
      str_detect(measure, "hcw_boost") & dimension == "AMC" ~ pop_rep_hcw_cps_amc,
      str_detect(measure, "hcw_boost") & dimension == "Self-financing" ~ `pop_rep_hcw_boost_self-financing`,
      str_detect(measure, "hcw_boost") & dimension == "Non-COVAX" ~ `pop_rep_hcw_boost_non-covax`,
      TRUE ~ NA
    ),
    
    # Assign friendly group & measure variables for visualization
    group = case_when(
      str_detect(measure, "tot") ~ "Total population",
      str_detect(measure, "old") ~ "Older adults",
      str_detect(measure, "hcw") ~ "Health and care workers",
      TRUE ~ NA
    ),
    measure = case_when(
      str_detect(measure, "td") ~ "Total doses",
      str_detect(measure, "a1d") ~ "At least one dose",
      str_detect(measure, "cps") ~ "Complete primary series",
      str_detect(measure, "boost") ~ "At least one booster dose",
      TRUE ~ NA
    ),
    
    # Calculate coverage
    coverage = case_when(
      measure != "Total doses" ~ admin / pop,
      TRUE ~ NA
    ), 
    
    # Add type and dimension for future merge
    type = "covax") %>%
  ungroup()


# Combine and create end-2023 aggregate estimate dataframe ----------------

data_cov_time_all <- rbind(data_cov_time_world_full, 
                           data_cov_time_income_full,
                           data_cov_time_region_full, 
                           data_cov_time_covax_full)

data_cov_last_agg <- data_cov_time_all %>%
  filter(month_name == as.Date("2023-12-01"))


# Prepare dataframes for visualization ------------------------------------

# Figure 1
data_fig_1_cov_end2023_inc <- data_cov_last %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "") %>%
  select(iso,
         income_group,
         cov_tot_a1d,
         cov_tot_cps,
         cov_tot_boost,
         cov_old_a1d,
         cov_old_cps,
         cov_old_boost,
         cov_hcw_a1d,
         cov_hcw_cps,
         cov_hcw_boost) %>%
  gather(key = "measure", value = "coverage", -iso, -income_group) %>%
  mutate(group = if_else(str_detect(measure, "cov_tot"), "Total population", 
                         if_else(str_detect(measure, "cov_old"), "Older adults",
                                 if_else(str_detect(measure, "cov_hcw"), "Health and care workers", NA))),
         measure = if_else(str_detect(measure, "a1d"), "At least one dose", 
                           if_else(str_detect(measure, "cps"), "Complete primary series", 
                                   if_else(str_detect(measure, "boost"), "At least one booster dose", NA)))) %>%
  mutate(income_group = factor(income_group, levels = levels_income),
         measure = factor(measure, levels = levels_indicator),
         group = factor(group, levels = levels_groups_short))


# Figure 2
data_fig_2_cov_cps_boost_priority_end2023 <- data_cov_last %>%
  filter(!is.na(income_group),
         income_group != "") %>%
  mutate(income_group = factor(income_group, levels = levels_income)) %>%
  transmute(
    iso,
    income_group,
    # total population
    tot_cps   = cov_tot_cps,
    tot_boost = cov_tot_boost,
    # priority groups
    hcw_cps   = cov_hcw_cps,
    hcw_boost = cov_hcw_boost,
    old_cps   = cov_old_cps,
    old_boost = cov_old_boost
  ) %>%
  pivot_longer(
    cols = c(hcw_cps, hcw_boost, old_cps, old_boost),
    names_to = c("group", "dose"),
    names_pattern = "^(hcw|old)_(cps|boost)$",
    values_to = "y"
  ) %>%
  mutate(group = recode(group, hcw = "Health and care workers",
                        old = "Older adults"),
    group = factor(group, levels = c("Older adults", "Health and care workers")),
    dose = recode(dose, cps = "Complete primary series", 
                  boost = "At least one booster dose"),
    dose = factor(dose, levels = c("Complete primary series", 
                                   "At least one booster dose")),
    x = if_else(dose == "Complete primary series", tot_cps, tot_boost),
    diff_pp = (y - x) * 100
  ) %>%
  filter(!is.na(x), !is.na(y))


# Figure 3
##  Add global figures to each category
data_fig_3_admin_time_world <- raw_data_acute_time %>%
  select(a_iso,
         month_name,
         a_status_who,
         adm_tot_td) %>%
  filter(a_status_who == "Member State") %>%
  group_by(month_name) %>%
  summarize(adm_td_tot = sum(adm_tot_td, na.rm = TRUE)) %>%
  ungroup()

data_fig_3_admin_time_all_cov <- data_fig_3_admin_time_world %>%
  mutate(dimension = "World",
         category = "COVAX")

data_fig_3_admin_time_all_reg <- data_fig_3_admin_time_world %>%
  mutate(dimension = "World",
         category = "Region")

data_fig_3_admin_time_world <- data_fig_3_admin_time_world %>%
  mutate(dimension = "World",
         category = "Income group") %>%
  rbind(., data_fig_3_admin_time_all_cov, data_fig_3_admin_time_all_reg) %>%
  mutate(coverage = (adm_td_tot / pop_total_2021_world) * 100)

##  Create grouping figures
data_fig_3_admin_time_income <- raw_data_acute_time %>%
  select(a_iso,
         month_name,
         a_status_who,
         a_income_group,
         adm_tot_td) %>%
  filter(a_status_who == "Member State",
         a_income_group != "Other") %>%
  select(-a_status_who) %>%
  group_by(month_name, a_income_group) %>%
  summarize(adm_td_tot = sum(adm_tot_td, na.rm = TRUE)) %>%
  mutate(coverage = case_when(
    a_income_group == "HIC" ~ adm_td_tot / pop_total_2021_hic * 100,
    a_income_group == "UMIC" ~ adm_td_tot / pop_total_2021_umic * 100,
    a_income_group == "LMIC" ~ adm_td_tot / pop_total_2021_lmic * 100,
    a_income_group == "LIC" ~ adm_td_tot / pop_total_2021_lic * 100,
    TRUE ~ NA
  ),
  category = "Income group") %>%
  rename(dimension = a_income_group) %>%
  ungroup()

data_fig_3_admin_time_covax <- raw_data_acute_time %>%
  select(a_iso,
         month_name,
         a_status_covax,
         adm_tot_td) %>%
  filter(!is.na(a_status_covax),
         a_status_covax != "No") %>%
  group_by(month_name, a_status_covax) %>%
  summarize(adm_td_tot = sum(adm_tot_td, na.rm = TRUE)) %>%
  mutate(coverage = case_when(
    a_status_covax == "AMC" ~ adm_td_tot / pop_total_2021_amc * 100,
    a_status_covax == "Self-financing" ~ adm_td_tot / `pop_total_2021_self-financing` * 100,
    a_status_covax == "Non-COVAX" ~ adm_td_tot / `pop_total_2021_non-covax` * 100,
    TRUE ~ NA
  ),
  category = "COVAX") %>%
  rename(dimension = a_status_covax) %>%
  ungroup()

data_fig_3_admin_time_all <- raw_data_acute_time %>%
  select(a_iso,
         month_name,
         a_status_who,
         a_region_who,
         adm_tot_td) %>%
  filter(a_status_who == "Member State") %>%
  select(-a_status_who) %>%
  group_by(month_name, a_region_who) %>%
  summarize(adm_td_tot = sum(adm_tot_td, na.rm = TRUE)) %>%
  mutate(coverage = case_when(
    a_region_who == "AFR" ~ adm_td_tot / pop_total_2021_afr * 100,
    a_region_who == "AMR" ~ adm_td_tot / pop_total_2021_amr * 100,
    a_region_who == "EMR" ~ adm_td_tot / pop_total_2021_emr * 100,
    a_region_who == "EUR" ~ adm_td_tot / pop_total_2021_eur * 100,
    a_region_who == "SEAR" ~ adm_td_tot / pop_total_2021_sear * 100,
    a_region_who == "WPR" ~ adm_td_tot / pop_total_2021_wpr * 100,
    TRUE ~ NA
  ),
  category = "Region") %>%
  rename(dimension = a_region_who) %>%
  ungroup() %>%
  rbind(., data_fig_3_admin_time_income, data_fig_3_admin_time_covax, 
        data_fig_3_admin_time_world) %>%
  mutate(dimension = factor(dimension, levels = levels_standard)) 


# Figure 4
data_fig_4_cov_time_world_last <- data_cov_time_world_full %>%
  filter(measure != "Total doses") %>%
  mutate(measure = factor(measure, levels = levels_indicator),
         group = factor(group, levels = levels_groups_short)) %>%
  group_by(group, measure) %>%
  arrange(month_name, .by_group = TRUE) %>%
  filter(row_number() == max(which(!is.na(coverage)))) %>%
  ungroup() %>%
  mutate(label = scales::percent(coverage, accuracy = 1))

data_fig_4_cov_time_world <- data_cov_time_world_full %>%
  filter(measure != "Total doses") %>%
  mutate(measure = factor(measure, levels = levels_indicator),
         group = factor(group, levels = levels_groups_short)) %>%
  arrange(group, measure, month_name)


# Figure 5

min_n <- 10

data_fig_5_cov_time_mediqr_compA_fix <- cov_acute_summary %>%
  filter(type == "global",
         month_name < as.Date("2024-01-01")) %>%
  mutate(
    med_cov = ifelse(n >= min_n, med_cov, NA_real_),
    perc_25 = ifelse(n >= min_n, perc_25, NA_real_),
    perc_75 = ifelse(n >= min_n, perc_75, NA_real_),
    dimension = factor(dimension, levels = levels_income),
    indicator = factor(indicator, levels = c("cps","boost"),
                       labels = c("Complete primary series", 
                                  "At least one booster dose")),
    group = factor(group, levels = c("tot","old","hcw"),
                   labels = levels_groups_short)
  )

data_fig_5_cov_time_mediqr_compB <- cov_acute_summary %>%
  filter(type == "income", 
         group == "tot",
         month_name < as.Date("2024-01-01")) %>%
  mutate(
    dimension = factor(dimension, levels = levels_income),
    indicator = factor(indicator, levels = c("cps","boost"),
                       labels = c("Complete primary series", 
                                  "At least one booster dose"))
  )


# Supp. figure 5
data_fig_supp_5_cov_time_weight_all_last <- data_cov_time_all %>%
  filter(measure != "Total doses",
         type == "income") %>%
  mutate(measure = factor(measure, levels = levels_indicator),
         group = factor(group, levels = levels_groups_short),
         dimension = factor(dimension, levels = levels_income)) %>%
  group_by(group, measure, dimension) %>%
  arrange(month_name, .by_group = TRUE) %>%
  filter(row_number() == max(which(!is.na(coverage)))) %>%
  ungroup() %>%
  mutate(label = scales::percent(coverage, accuracy = 1))

data_fig_supp_5_cov_time_weight_all <- data_cov_time_all %>%
  filter(measure != "Total doses",
         type == "income") %>%
  mutate(measure = factor(measure, levels = levels_indicator),
         group = factor(group, levels = levels_groups_short),
         dimension = factor(dimension, levels = levels_income)) %>%
  arrange(group, measure, month_name) 


# Supp. figure 6
data_fig_supp_6_cov_mediqr_all <- cov_acute_summary %>%
  filter(type == "income", 
         month_name < as.Date("2024-01-01")) %>%
  mutate(dimension = factor(dimension, levels = levels_income),
         indicator = factor(indicator, levels = c("cps","boost"),
                            labels = c("Complete primary series", 
                                       "At least one booster dose")),
         group = factor(group, levels = c("tot","old","hcw"),
                        labels = levels_groups_short))


# End ---------------------------------------------------------------------