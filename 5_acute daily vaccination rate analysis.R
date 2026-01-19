
# Staging -----------------------------------------------------------------

# Select relevant admin. variables, merge with relevant entity details
data_admin_daily <- raw_data_acute_time_daily %>%
  transmute(iso = a_iso,
            adm_date = adm_date,
            dvr_4wk_td = dvr_4wk_td) %>%
  left_join(., entity_pop %>% select(iso, region_who, income_group, status_who,
                                     status_covax, pop_2021),
            by = "iso") %>%
  
  # Calculate dvr as percentage of total pop.
  mutate(dvr_4wk_td_per = dvr_4wk_td / pop_2021)


# Calculate simple average dvr per country grouping -----------------------

# All
dvr_simple_all <- data_admin_daily %>%
  filter(status_who == "Member State",
         adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date) %>%
  summarize(dvr_per_mean = mean(dvr_4wk_td_per, na.rm = TRUE),
            dvr_per_median = median(dvr_4wk_td_per, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(type = "Global",
         dimension = NA)

# Income group
dvr_simple_income <- data_admin_daily %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "",
         adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date, income_group) %>%
  summarize(dvr_per_mean = mean(dvr_4wk_td_per, na.rm = TRUE),
            dvr_per_median = median(dvr_4wk_td_per, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(dimension = income_group) %>%
  mutate(type = "Income group")

# Region
dvr_simple_region <- data_admin_daily %>%
  filter(status_who == "Member State",
         adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date, region_who) %>%
  summarize(dvr_per_mean = mean(dvr_4wk_td_per, na.rm = TRUE),
            dvr_per_median = median(dvr_4wk_td_per, na.rm = TRUE)) %>%
  ungroup() %>% 
  rename(dimension = region_who) %>%
  mutate(type = "WHO region")

# COVAX 
dvr_simple_covax <- data_admin_daily %>%
  filter(!is.na(status_covax),
         adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date, status_covax) %>%
  summarize(dvr_per_mean = mean(dvr_4wk_td_per, na.rm = TRUE),
            dvr_per_median = median(dvr_4wk_td_per, na.rm = TRUE)) %>%
  ungroup() %>% 
  rename(dimension = status_covax) %>%
  mutate(type = "COVAX participation category")

# Combine
dvr_simple <- rbind(dvr_simple_all,
                    dvr_simple_income, 
                    dvr_simple_region,
                    dvr_simple_covax)


# Calculate weighted average of dvr per country grouping ------------------

dvr_weighted_all <- data_admin_daily %>%
  filter(status_who == "Member State",
         adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date) %>%
  summarize(dvr = sum(dvr_4wk_td, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(type = "All",
         dimension = "Global")

dvr_weighted_income <- data_admin_daily %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "",
         adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date, income_group) %>%
  summarize(dvr = sum(dvr_4wk_td, na.rm = TRUE)) %>%
  ungroup() %>% 
  rename(dimension = income_group) %>%
  mutate(type = "Income group")

dvr_weighted_region <- data_admin_daily %>%
  filter(status_who == "Member State",
         adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date, region_who) %>%
  summarize(dvr = sum(dvr_4wk_td, na.rm = TRUE)) %>%
  ungroup() %>% 
  rename(dimension = region_who) %>%
  mutate(type = "WHO region")

dvr_weighted_covax <- data_admin_daily %>%
  filter(!is.na(status_covax),
         adm_date <= as.Date("2023-12-31")) %>%
  group_by(adm_date, status_covax) %>%
  summarize(dvr = sum(dvr_4wk_td, na.rm = TRUE)) %>%
  ungroup() %>% 
  rename(dimension = status_covax) %>%
  mutate(type = "COVAX participation category")

dvr_weighted <- rbind(dvr_weighted_all,
                      dvr_weighted_income, 
                      dvr_weighted_region,
                      dvr_weighted_covax) %>%
  mutate(pop = case_when(
    dimension == "Global" ~ pop_rep_tot_2021_cps,
    dimension == "HIC" ~ pop_rep_tot_2021_cps_hic,
    dimension == "UMIC" ~ pop_rep_tot_2021_cps_umic,
    dimension == "LMIC" ~ pop_rep_tot_2021_cps_lmic,
    dimension == "LIC" ~ pop_rep_tot_2021_cps_lic,
    dimension == "AFR" ~ pop_rep_tot_2021_cps_afr,
    dimension == "AMR" ~ pop_rep_tot_2021_cps_amr,
    dimension == "EMR" ~ pop_rep_tot_2021_cps_emr,
    dimension == "EUR" ~ pop_rep_tot_2021_cps_eur,
    dimension == "SEAR" ~ pop_rep_tot_2021_cps_sear,
    dimension == "WPR" ~ pop_rep_tot_2021_cps_wpr,
    dimension == "AMC" ~ pop_rep_tot_2021_cps_amc,
    dimension == "Self-financing" ~ `pop_rep_tot_2021_cps_self-financing`,
    dimension == "Non-COVAX" ~ `pop_rep_tot_2021_cps_non-covax`,
    NA ~ TRUE
  ),
  dvr_per = dvr / pop)

# Filter to max aggregate dvr per country grouping
dvr_max <- dvr_weighted %>%
  group_by(dimension) %>%
  filter(dvr_per == max(dvr_per)) %>%
  select(-pop,
         -dvr)


# Calculate dvr summary statistics by country grouping --------------------

# Global
dvr_summary_all <- data_admin_daily %>%
  filter(status_who == "Member State") %>%
  select(iso,
         adm_date,
         dvr_4wk_td,
         dvr_4wk_td_per) %>%
  group_by(adm_date) %>%
  summarise(
    max_rate = max(dvr_4wk_td_per, na.rm = TRUE),
    min_rate = min(dvr_4wk_td_per, na.rm = TRUE), 
    avg_rate = mean(dvr_4wk_td_per, na.rm = TRUE),
    med_rate = median(dvr_4wk_td_per, na.rm = TRUE), 
    sd_rate = sd(dvr_4wk_td_per, na.rm = TRUE),
    perc_10 = quantile(dvr_4wk_td_per, 0.10, na.rm = TRUE),
    perc_90 = quantile(dvr_4wk_td_per, 0.90, na.rm = TRUE),
    perc_25 = quantile(dvr_4wk_td_per, 0.25, na.rm = TRUE),
    perc_75 = quantile(dvr_4wk_td_per, 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    lower_bound = avg_rate - sd_rate,
    upper_bound = avg_rate + sd_rate,
    type = "global",
    dimension = NA
  ) %>%
ungroup()

# Income group
dvr_summary_income <- data_admin_daily %>%
  filter(status_who == "Member State",
         !is.na(income_group),
         income_group != "") %>%
  select(iso,
         adm_date,
         income_group,
         dvr_4wk_td,
         dvr_4wk_td_per) %>%
  group_by(adm_date, income_group) %>%
  summarise(
    max_rate = max(dvr_4wk_td_per, na.rm = TRUE),
    min_rate = min(dvr_4wk_td_per, na.rm = TRUE), 
    avg_rate = mean(dvr_4wk_td_per, na.rm = TRUE),
    med_rate = median(dvr_4wk_td_per, na.rm = TRUE), 
    sd_rate = sd(dvr_4wk_td_per, na.rm = TRUE),
    perc_10 = quantile(dvr_4wk_td_per, 0.10, na.rm = TRUE),
    perc_90 = quantile(dvr_4wk_td_per, 0.90, na.rm = TRUE),
    perc_25 = quantile(dvr_4wk_td_per, 0.25, na.rm = TRUE),
    perc_75 = quantile(dvr_4wk_td_per, 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    lower_bound = avg_rate - sd_rate,
    upper_bound = avg_rate + sd_rate,
    type = "income"
  ) %>%
  rename(dimension = income_group) %>%
  ungroup()

# WHO region
dvr_summary_region <- data_admin_daily %>%
    filter(status_who == "Member State") %>%
    select(iso,
           adm_date,
           region_who,
           dvr_4wk_td,
           dvr_4wk_td_per) %>%
    group_by(adm_date, region_who) %>%
    summarise(
      max_rate = max(dvr_4wk_td_per, na.rm = TRUE),
      min_rate = min(dvr_4wk_td_per, na.rm = TRUE), 
      avg_rate = mean(dvr_4wk_td_per, na.rm = TRUE),
      med_rate = median(dvr_4wk_td_per, na.rm = TRUE), 
      sd_rate = sd(dvr_4wk_td_per, na.rm = TRUE),
      perc_10 = quantile(dvr_4wk_td_per, 0.10, na.rm = TRUE),
      perc_90 = quantile(dvr_4wk_td_per, 0.90, na.rm = TRUE),
      perc_25 = quantile(dvr_4wk_td_per, 0.25, na.rm = TRUE),
      perc_75 = quantile(dvr_4wk_td_per, 0.75, na.rm = TRUE)
    ) %>%
    mutate(
      lower_bound = avg_rate - sd_rate,
      upper_bound = avg_rate + sd_rate,
      type = "region"
    ) %>%
    rename(dimension = region_who) %>%
  ungroup()
  
# COVAX participation category
dvr_summary_covax <- data_admin_daily %>%
  filter(!is.na(status_covax)) %>%
  select(iso,
          adm_date,
          status_covax,
           dvr_4wk_td,
           dvr_4wk_td_per) %>%
    group_by(adm_date, status_covax) %>%
    summarise(
      max_rate = max(dvr_4wk_td_per, na.rm = TRUE),
      min_rate = min(dvr_4wk_td_per, na.rm = TRUE), 
      avg_rate = mean(dvr_4wk_td_per, na.rm = TRUE),
      med_rate = median(dvr_4wk_td_per, na.rm = TRUE), 
      sd_rate = sd(dvr_4wk_td_per, na.rm = TRUE),
      perc_10 = quantile(dvr_4wk_td_per, 0.10, na.rm = TRUE),
      perc_90 = quantile(dvr_4wk_td_per, 0.90, na.rm = TRUE),
      perc_25 = quantile(dvr_4wk_td_per, 0.25, na.rm = TRUE),
      perc_75 = quantile(dvr_4wk_td_per, 0.75, na.rm = TRUE)
    ) %>%
    mutate(
      lower_bound = avg_rate - sd_rate,
      upper_bound = avg_rate + sd_rate,
      type = "COVAX participation category"
    ) %>%
    rename(dimension = status_covax) %>%
  ungroup()
  
# Combine
dvr_summary <- rbind(dvr_summary_all,
                     dvr_summary_income,
                     dvr_summary_region,
                     dvr_summary_covax)


# Prepare visualizations ---------------------------------------------------

# Figure 6
data_fig_5_dvr_summary_compA <- dvr_summary %>%
  select(-max_rate, -min_rate, -perc_25, -perc_75) %>%
  filter(
    type == "global",
    adm_date < as.Date("2024-01-01")
  ) %>%
  mutate(dimension = "Global", dimension = factor(dimension, levels = "Global")
  )

data_fig_5_dvr_summary_compB <- dvr_summary %>%
select(-max_rate, -min_rate, -perc_25, -perc_75) %>%
  filter(type == "income",
         !is.na(dimension),
         dimension != "",
         adm_date < as.Date("2024-01-01")) %>%
  mutate(dimension = factor(dimension, levels = levels_standard))


#Supp. figure 4
data_fig_supp_4_dvr_time_increg <- dvr_weighted %>%
  filter(type != "COVAX participation category") %>%
  mutate(type = factor(type, levels = c("All", "Income group", "WHO region")),
         dimension = factor(dimension, levels = c("Global", "AFR", "AMR", "EMR", 
                                                  "EUR", "SEAR", "WPR", "HIC", 
                                                  "UMIC", "LMIC", "LIC")))


# End ---------------------------------------------------------------------