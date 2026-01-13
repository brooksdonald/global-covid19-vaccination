# Total population summary timeseries -------------------------------------

data_cov_time_full <- df_interp %>%
  group_by(month_name) %>%
  summarize(adm_tot_td = sum(adm_tot_td, na.rm = TRUE),
            adm_tot_a1d = sum(adm_tot_a1d, na.rm = TRUE),
            adm_tot_cps = sum(adm_tot_cps, na.rm = TRUE),
            adm_tot_boost = sum(adm_tot_boost, na.rm = TRUE),
            adm_old_a1d = sum(adm_old_a1d_cap_monotone_interp, na.rm = TRUE),
            adm_old_cps = sum(adm_old_cps_cap_monotone_interp, na.rm = TRUE),
            adm_old_boost = sum(adm_old_boost_cap_monotone_interp, na.rm = TRUE),
            adm_hcw_a1d = sum(adm_hcw_a1d_monotone_interp, na.rm = TRUE),
            adm_hcw_cps = sum(adm_hcw_cps_monotone_interp, na.rm = TRUE),
            adm_hcw_boost = sum(adm_hcw_boost_monotone_interp, na.rm = TRUE)) %>%
  gather(key = "measure", value = "admin", -month_name) %>%
  mutate(group = if_else(str_detect(measure, "tot"), "Total population", 
                         if_else(str_detect(measure, "old"), "Older adults",
                                 if_else(str_detect(measure, "hcw"), "Health and care workers", NA))),
         measure = if_else(str_detect(measure, "a1d"), "At least one dose", 
                           if_else(str_detect(measure, "cps"), "Complete primary series", 
                                   if_else(str_detect(measure, "boost"), "At least one booster", 
                                           if_else(str_detect(measure, "td"), "Total doses",NA)))),
         pop = case_when(
           group == "Total population" ~ pop_world_total,
           group == "Older adults" & measure == "At least one dose" ~ pop_rep_old_a1d,
           group == "Older adults" & measure == "Complete primary series" ~ pop_rep_old_cps,
           group == "Older adults" & measure == "At least one booster" ~ pop_rep_old_boost,
           group == "Health and care workers" & measure == "At least one dose" ~ pop_rep_hcw_a1d,
           group == "Health and care workers" & measure == "Complete primary series" ~ pop_rep_hcw_cps,
           group == "Health and care workers" & measure == "At least one booster" ~ pop_rep_hcw_boost,
           TRUE ~ NA
         ),
         coverage_denom_vary = case_when(
           measure != "Total doses" ~ admin / pop,
           TRUE ~ NA
         ),                     
         coverage_denom_rep = case_when(
           group == "Total population" & measure == "At least one dose" ~ admin / pop_world_total,
           group == "Total population" & measure == "Complete primary series" ~ admin / pop_world_total,
           group == "Total population" & measure == "At least one booster" ~ admin / pop_world_total,
           group == "Older adults" & measure == "At least one dose" ~ admin / pop_rep_old_cps,
           group == "Older adults" & measure == "Complete primary series" ~ admin / pop_rep_old_cps,
           group == "Older adults" & measure == "At least one booster" ~ admin / pop_rep_old_cps,
           group == "Health and care workers" & measure == "At least one dose" ~ admin / pop_rep_hcw_cps,
           group == "Health and care workers" & measure == "Complete primary series" ~ admin / pop_rep_hcw_cps,
           group == "Health and care workers" & measure == "At least one booster" ~ admin / pop_rep_hcw_cps,
           TRUE ~ NA
         )) %>%
  ungroup() %>%
  mutate(type = "all",
         dimension = NA)


# Summary timeseries - income ---------------------------------------------

data_cov_time_income_full <- df_interp %>%
  filter(a_income_group != "Other") %>%
  group_by(month_name,
           a_income_group) %>%
  summarize(adm_tot_td = sum(adm_tot_td, na.rm = TRUE),
            adm_tot_a1d = sum(adm_tot_a1d, na.rm = TRUE),
            adm_tot_cps = sum(adm_tot_cps, na.rm = TRUE),
            adm_tot_boost = sum(adm_tot_boost, na.rm = TRUE),
            adm_old_a1d = sum(adm_old_a1d_cap_monotone_interp, na.rm = TRUE),
            adm_old_cps = sum(adm_old_cps_cap_monotone_interp, na.rm = TRUE),
            adm_old_boost = sum(adm_old_boost_cap_monotone_interp, na.rm = TRUE),
            adm_hcw_a1d = sum(adm_hcw_a1d_monotone_interp, na.rm = TRUE),
            adm_hcw_cps = sum(adm_hcw_cps_monotone_interp, na.rm = TRUE),
            adm_hcw_boost = sum(adm_hcw_boost_monotone_interp, na.rm = TRUE)) %>%
  gather(key = "measure", value = "admin", -(c(month_name, a_income_group))) %>%
  mutate(type = "income") %>%
  rename(dimension = a_income_group) %>%
  mutate(group = if_else(str_detect(measure, "tot"), "Total population", 
                         if_else(str_detect(measure, "old"), "Older adults",
                                 if_else(str_detect(measure, "hcw"), "Health and care workers", NA))),
         measure = if_else(str_detect(measure, "a1d"), "At least one dose", 
                           if_else(str_detect(measure, "cps"), "Complete primary series", 
                                   if_else(str_detect(measure, "boost"), "At least one booster", 
                                           if_else(str_detect(measure, "td"), "Total doses",NA)))),
         pop = case_when(
           group == "Total population" & dimension == "HIC" ~ pop_total_hic,
           group == "Total population" & dimension == "UMIC" ~ pop_total_umic,
           group == "Total population" & dimension == "LMIC" ~ pop_total_lmic,
           group == "Total population" & dimension == "LIC" ~ pop_total_lic,
           group == "Older adults" & measure == "At least one dose" & dimension == "HIC" ~ pop_rep_old_hic_a1d,
           group == "Older adults" & measure == "At least one dose" & dimension == "UMIC" ~ pop_rep_old_umic_a1d,
           group == "Older adults" & measure == "At least one dose" & dimension == "LMIC" ~ pop_rep_old_lmic_a1d,
           group == "Older adults" & measure == "At least one dose" & dimension == "LIC" ~ pop_rep_old_lic_a1d,
           group == "Older adults" & measure == "Complete primary series" & dimension == "HIC" ~ pop_rep_old_hic_cps,
           group == "Older adults" & measure == "Complete primary series" & dimension == "UMIC" ~ pop_rep_old_umic_cps,
           group == "Older adults" & measure == "Complete primary series" & dimension == "LMIC" ~ pop_rep_old_lmic_cps,
           group == "Older adults" & measure == "Complete primary series" & dimension == "LIC" ~ pop_rep_old_lic_cps,
           group == "Older adults" & measure == "At least one booster" & dimension == "HIC" ~ pop_rep_old_hic_boost,
           group == "Older adults" & measure == "At least one booster" & dimension == "UMIC" ~ pop_rep_old_umic_boost,
           group == "Older adults" & measure == "At least one booster" & dimension == "LMIC" ~ pop_rep_old_lmic_boost,
           group == "Older adults" & measure == "At least one booster" & dimension == "LIC" ~ pop_rep_old_lic_boost,
           group == "Health and care workers" & measure == "At least one dose" & dimension == "HIC" ~ pop_rep_hcw_hic_a1d,
           group == "Health and care workers" & measure == "At least one dose" & dimension == "UMIC" ~ pop_rep_hcw_umic_a1d,
           group == "Health and care workers" & measure == "At least one dose" & dimension == "LMIC" ~ pop_rep_hcw_lmic_a1d,
           group == "Health and care workers" & measure == "At least one dose" & dimension == "LIC" ~ pop_rep_hcw_lic_a1d,
           group == "Health and care workers" & measure == "Complete primary series" & dimension == "HIC" ~ pop_rep_hcw_hic_cps,
           group == "Health and care workers" & measure == "Complete primary series" & dimension == "UMIC" ~ pop_rep_hcw_umic_cps,
           group == "Health and care workers" & measure == "Complete primary series" & dimension == "LMIC" ~ pop_rep_hcw_lmic_cps,
           group == "Health and care workers" & measure == "Complete primary series" & dimension == "LIC" ~ pop_rep_hcw_lic_cps,
           group == "Health and care workers" & measure == "At least one booster" & dimension == "HIC" ~ pop_rep_hcw_hic_boost,
           group == "Health and care workers" & measure == "At least one booster" & dimension == "UMIC" ~ pop_rep_hcw_umic_boost,
           group == "Health and care workers" & measure == "At least one booster" & dimension == "LMIC" ~ pop_rep_hcw_lmic_boost,
           group == "Health and care workers" & measure == "At least one booster" & dimension == "LIC" ~ pop_rep_hcw_lic_boost,
           TRUE ~ NA
         ),
         coverage_denom_vary = case_when(
           measure != "Total doses" ~ admin / pop,
           TRUE ~ NA
         ),
         coverage_denom_rep = case_when(
           group == "Total population" & measure == "At least one dose" ~ admin / pop_world_total,
           group == "Total population" & measure == "Complete primary series" ~ admin / pop_world_total,
           group == "Total population" & measure == "At least one booster" ~ admin / pop_world_total,
           group == "Older adults" & measure == "At least one dose" ~ admin / pop_rep_old_cps,
           group == "Older adults" & measure == "Complete primary series" ~ admin / pop_rep_old_cps,
           group == "Older adults" & measure == "At least one booster" ~ admin / pop_rep_old_cps,
           group == "Health and care workers" & measure == "At least one dose" ~ admin / pop_rep_hcw_cps,
           group == "Health and care workers" & measure == "Complete primary series" ~ admin / pop_rep_hcw_cps,
           group == "Health and care workers" & measure == "At least one booster" ~ admin / pop_rep_hcw_cps,
           TRUE ~ NA
         )) %>%
  
  # if_else(group == "Total population", admin / pop_world_total,
  #                  if_else(group == "Older adults", admin / pop_world_old,
  #                          if_else(group == "Health and care workers", admin / pop_world_hcw, NA)))) %>%
  ungroup()


# Summary timeseries - region ---------------------------------------------

data_cov_time_region_full <- df_interp %>%
  group_by(month_name,
           a_region_who) %>%
  summarize(adm_tot_td = sum(adm_tot_td, na.rm = TRUE),
            adm_tot_a1d = sum(adm_tot_a1d, na.rm = TRUE),
            adm_tot_cps = sum(adm_tot_cps, na.rm = TRUE),
            adm_tot_boost = sum(adm_tot_boost, na.rm = TRUE),
            adm_old_a1d = sum(adm_old_a1d_cap_monotone_interp, na.rm = TRUE),
            adm_old_cps = sum(adm_old_cps_cap_monotone_interp, na.rm = TRUE),
            adm_old_boost = sum(adm_old_boost_cap_monotone_interp, na.rm = TRUE),
            adm_hcw_a1d = sum(adm_hcw_a1d_monotone_interp, na.rm = TRUE),
            adm_hcw_cps = sum(adm_hcw_cps_monotone_interp, na.rm = TRUE),
            adm_hcw_boost = sum(adm_hcw_boost_monotone_interp, na.rm = TRUE)) %>%
  gather(key = "measure", value = "admin", -(c(month_name, a_region_who))) %>%
  mutate(type = "region") %>%
  rename(dimension = a_region_who) %>%
  mutate(group = if_else(str_detect(measure, "tot"), "Total population", 
                         if_else(str_detect(measure, "old"), "Older adults",
                                 if_else(str_detect(measure, "hcw"), "Health and care workers", NA))),
         measure = if_else(str_detect(measure, "a1d"), "At least one dose", 
                           if_else(str_detect(measure, "cps"), "Complete primary series", 
                                   if_else(str_detect(measure, "boost"), "At least one booster", 
                                           if_else(str_detect(measure, "td"), "Total doses",NA)))),
         pop = case_when(
           group == "Total population" & dimension == "AFR" ~ pop_total_afr,
           group == "Total population" & dimension == "AMR" ~ pop_total_amr,
           group == "Total population" & dimension == "EMR" ~ pop_total_emr,
           group == "Total population" & dimension == "EUR" ~ pop_total_eur,
           group == "Total population" & dimension == "SEAR" ~ pop_total_sear,
           group == "Total population" & dimension == "WPR" ~ pop_total_wpr,
           group == "Older adults" & measure == "At least one dose" & dimension == "AFR" ~ pop_rep_old_afr_a1d,
           group == "Older adults" & measure == "At least one dose" & dimension == "AMR" ~ pop_rep_old_amr_a1d,
           group == "Older adults" & measure == "At least one dose" & dimension == "EMR" ~ pop_rep_old_emr_a1d,
           group == "Older adults" & measure == "At least one dose" & dimension == "EUR" ~ pop_rep_old_eur_a1d,
           group == "Older adults" & measure == "At least one dose" & dimension == "SEAR" ~ pop_rep_old_sear_a1d,
           group == "Older adults" & measure == "At least one dose" & dimension == "WPR" ~ pop_rep_old_wpr_a1d,
           group == "Older adults" & measure == "Complete primary series" & dimension == "AFR" ~ pop_rep_old_afr_cps,
           group == "Older adults" & measure == "Complete primary series" & dimension == "AMR" ~ pop_rep_old_amr_cps,
           group == "Older adults" & measure == "Complete primary series" & dimension == "EMR" ~ pop_rep_old_emr_cps,
           group == "Older adults" & measure == "Complete primary series" & dimension == "EUR" ~ pop_rep_old_eur_cps,
           group == "Older adults" & measure == "Complete primary series" & dimension == "SEAR" ~ pop_rep_old_sear_cps,
           group == "Older adults" & measure == "Complete primary series" & dimension == "WPR" ~ pop_rep_old_wpr_cps,
           group == "Older adults" & measure == "At least one booster" & dimension == "AFR" ~ pop_rep_old_afr_boost,
           group == "Older adults" & measure == "At least one booster" & dimension == "AMR" ~ pop_rep_old_amr_boost,
           group == "Older adults" & measure == "At least one booster" & dimension == "EMR" ~ pop_rep_old_emr_boost,
           group == "Older adults" & measure == "At least one booster" & dimension == "EUR" ~ pop_rep_old_eur_boost,
           group == "Older adults" & measure == "At least one booster" & dimension == "SEAR" ~ pop_rep_old_sear_boost,
           group == "Older adults" & measure == "At least one booster" & dimension == "WPR" ~ pop_rep_old_wpr_boost,
           group == "Health and care workers" & measure == "At least one dose" & dimension == "AFR" ~ pop_rep_hcw_afr_a1d,
           group == "Health and care workers" & measure == "At least one dose" & dimension == "AMR" ~ pop_rep_hcw_amr_a1d,
           group == "Health and care workers" & measure == "At least one dose" & dimension == "EMR" ~ pop_rep_hcw_emr_a1d,
           group == "Health and care workers" & measure == "At least one dose" & dimension == "EUR" ~ pop_rep_hcw_eur_a1d,
           group == "Health and care workers" & measure == "At least one dose" & dimension == "SEAR" ~ pop_rep_hcw_sear_a1d,
           group == "Health and care workers" & measure == "At least one dose" & dimension == "WPR" ~ pop_rep_hcw_wpr_a1d,
           group == "Health and care workers" & measure == "Complete primary series" & dimension == "AFR" ~ pop_rep_hcw_afr_cps,
           group == "Health and care workers" & measure == "Complete primary series" & dimension == "AMR" ~ pop_rep_hcw_amr_cps,
           group == "Health and care workers" & measure == "Complete primary series" & dimension == "EMR" ~ pop_rep_hcw_emr_cps,
           group == "Health and care workers" & measure == "Complete primary series" & dimension == "EUR" ~ pop_rep_hcw_eur_cps,
           group == "Health and care workers" & measure == "Complete primary series" & dimension == "SEAR" ~ pop_rep_hcw_sear_cps,
           group == "Health and care workers" & measure == "Complete primary series" & dimension == "WPR" ~ pop_rep_hcw_wpr_cps,
           group == "Health and care workers" & measure == "At least one booster" & dimension == "AFR" ~ pop_rep_hcw_afr_boost,
           group == "Health and care workers" & measure == "At least one booster" & dimension == "AMR" ~ pop_rep_hcw_amr_boost,
           group == "Health and care workers" & measure == "At least one booster" & dimension == "EMR" ~ pop_rep_hcw_emr_boost,
           group == "Health and care workers" & measure == "At least one booster" & dimension == "EUR" ~ pop_rep_hcw_eur_boost,
           group == "Health and care workers" & measure == "At least one booster" & dimension == "SEAR" ~ pop_rep_hcw_sear_boost,
           group == "Health and care workers" & measure == "At least one booster" & dimension == "WPR" ~ pop_rep_hcw_wpr_boost,
           TRUE ~ NA
         ),
         coverage_denom_vary = case_when(
           measure != "Total doses" ~ admin / pop,
           TRUE ~ NA
         ),
         coverage_denom_rep = case_when(
           group == "Total population" & measure == "At least one dose" ~ admin / pop_world_total,
           group == "Total population" & measure == "Complete primary series" ~ admin / pop_world_total,
           group == "Total population" & measure == "At least one booster" ~ admin / pop_world_total,
           group == "Older adults" & measure == "At least one dose" ~ admin / pop_rep_old_cps,
           group == "Older adults" & measure == "Complete primary series" ~ admin / pop_rep_old_cps,
           group == "Older adults" & measure == "At least one booster" ~ admin / pop_rep_old_cps,
           group == "Health and care workers" & measure == "At least one dose" ~ admin / pop_rep_hcw_cps,
           group == "Health and care workers" & measure == "Complete primary series" ~ admin / pop_rep_hcw_cps,
           group == "Health and care workers" & measure == "At least one booster" ~ admin / pop_rep_hcw_cps,
           TRUE ~ NA
         )) %>%
  ungroup() 

 # Summary timeseries - covax ---------------------------------------------

data_cov_time_covax_full <- df_interp %>%
  group_by(month_name,
           a_status_covax) %>%
  summarize(adm_tot_td = sum(adm_tot_td, na.rm = TRUE),
            adm_tot_a1d = sum(adm_tot_a1d, na.rm = TRUE),
            adm_tot_cps = sum(adm_tot_cps, na.rm = TRUE),
            adm_tot_boost = sum(adm_tot_boost, na.rm = TRUE),
            adm_old_a1d = sum(adm_old_a1d_cap_monotone_interp, na.rm = TRUE),
            adm_old_cps = sum(adm_old_cps_cap_monotone_interp, na.rm = TRUE),
            adm_old_boost = sum(adm_old_boost_cap_monotone_interp, na.rm = TRUE),
            adm_hcw_a1d = sum(adm_hcw_a1d_monotone_interp, na.rm = TRUE),
            adm_hcw_cps = sum(adm_hcw_cps_monotone_interp, na.rm = TRUE),
            adm_hcw_boost = sum(adm_hcw_boost_monotone_interp, na.rm = TRUE)) %>%
  gather(key = "measure", value = "admin", -(c(month_name, a_status_covax))) %>%
  mutate(type = "covax") %>%
  rename(dimension = a_status_covax) %>%
  mutate(group = if_else(str_detect(measure, "tot"), "Total population", 
                         if_else(str_detect(measure, "old"), "Older adults",
                                 if_else(str_detect(measure, "hcw"), "Health and care workers", NA))),
         measure = if_else(str_detect(measure, "a1d"), "At least one dose", 
                           if_else(str_detect(measure, "cps"), "Complete primary series", 
                                   if_else(str_detect(measure, "boost"), "At least one booster", 
                                           if_else(str_detect(measure, "td"), "Total doses",NA)))),
         pop = case_when(
           group == "Total population" & dimension == "AMC" ~ pop_total_amc,
           group == "Total population" & dimension == "Self-financing" ~ pop_total_sfp,
           group == "Total population" & dimension == "No" ~ pop_total_noncovax,
           group == "Older adults" & measure == "At least one dose" & dimension == "AMC" ~ pop_rep_old_amc_a1d,
           group == "Older adults" & measure == "At least one dose" & dimension == "Self-financing" ~ pop_rep_old_sfp_a1d,
           group == "Older adults" & measure == "At least one dose" & dimension == "No" ~ pop_rep_old_noncovax_a1d,
           group == "Older adults" & measure == "Complete primary series" & dimension == "AMC" ~ pop_rep_old_amc_cps,
           group == "Older adults" & measure == "Complete primary series" & dimension == "Self-financing" ~ pop_rep_old_sfp_cps,
           group == "Older adults" & measure == "Complete primary series" & dimension == "No" ~ pop_rep_old_noncovax_cps,
           group == "Older adults" & measure == "At least one booster" & dimension == "AMC" ~ pop_rep_old_amc_boost,
           group == "Older adults" & measure == "At least one booster" & dimension == "Self-financing" ~ pop_rep_old_sfp_boost,
           group == "Older adults" & measure == "At least one booster" & dimension == "No" ~ pop_rep_old_noncovax_boost,
           group == "Health and care workers" & measure == "At least one dose" & dimension == "AMC" ~ pop_rep_hcw_amc_a1d,
           group == "Health and care workers" & measure == "At least one dose" & dimension == "Self-financing" ~ pop_rep_hcw_sfp_a1d,
           group == "Health and care workers" & measure == "At least one dose" & dimension == "No" ~ pop_rep_hcw_noncovax_a1d,
           group == "Health and care workers" & measure == "Complete primary series" & dimension == "AMC" ~ pop_rep_hcw_amc_cps,
           group == "Health and care workers" & measure == "Complete primary series" & dimension == "Self-financing" ~ pop_rep_hcw_sfp_cps,
           group == "Health and care workers" & measure == "Complete primary series" & dimension == "No" ~ pop_rep_hcw_noncovax_cps,
           group == "Health and care workers" & measure == "At least one booster" & dimension == "AMC" ~ pop_rep_hcw_amc_boost,
           group == "Health and care workers" & measure == "At least one booster" & dimension == "Self-financing" ~ pop_rep_hcw_sfp_boost,
           group == "Health and care workers" & measure == "At least one booster" & dimension == "No" ~ pop_rep_hcw_noncovax_boost,
           TRUE ~ NA
         ),
         coverage_denom_vary = case_when(
           measure != "Total doses" ~ admin / pop,
           TRUE ~ NA
         ),
         coverage_denom_rep = case_when(
           group == "Total population" & measure == "At least one dose" ~ admin / pop_world_total,
           group == "Total population" & measure == "Complete primary series" ~ admin / pop_world_total,
           group == "Total population" & measure == "At least one booster" ~ admin / pop_world_total,
           group == "Older adults" & measure == "At least one dose" ~ admin / pop_rep_old_cps,
           group == "Older adults" & measure == "Complete primary series" ~ admin / pop_rep_old_cps,
           group == "Older adults" & measure == "At least one booster" ~ admin / pop_rep_old_cps,
           group == "Health and care workers" & measure == "At least one dose" ~ admin / pop_rep_hcw_cps,
           group == "Health and care workers" & measure == "Complete primary series" ~ admin / pop_rep_hcw_cps,
           group == "Health and care workers" & measure == "At least one booster" ~ admin / pop_rep_hcw_cps,
           TRUE ~ NA
         )) %>%
  ungroup()


# Combine -----------------------------------------------------------------

data_cov_time_all <- rbind(data_cov_time_full, data_cov_time_income_full,
                           data_cov_time_region_full, data_cov_time_covax_full)

# test --------------------------------------------------------------------

# temp <- raw_data_end %>%
#   select(a_iso,
#          adm_cps_hcw,
#          adm_hcw_cps_adjust)
# 
# cov_hcw <- raw_data_time_monthly %>%
#   select(a_iso,
#          month_name,
#          adm_hcw_cps,
#          a_pop_hcw) %>%
#   filter(month_name == as.Date("2023-12-01")) %>%
#   left_join(., temp, by = "a_iso") %>%
#   mutate(same = adm_hcw_cps == adm_hcw_cps_adjust,
#          diff = adm_hcw_cps - adm_hcw_cps_adjust)
# 
# df_clean_test <- df_clean %>%
#   filter(a_iso == "JOR") %>%
#   select(a_iso,
#          month_name,
#          adm_hcw_cps,
#          adm_hcw_cps_monotone)
# 
# 
# # Month check -------------------------------------------------------------
# 
# source("paper/reporting_check.R")
# 
# res <- compare_reporting(
#   dat = data_cov_time,
#   var = "adm_old_boost_cap",                      # the variable you care about
#   m1  = "2022-10-01",
#   m2  = "2022-11-01",
#   iso_col = "a_iso",
#   date_col = "month_name",
#   require_nonzero = FALSE         # set TRUE to require x > 0
# )
# 
# res$in_m1_not_m2 
# res$in_m2_not_m1
# res$summary_table

# End ---------------------------------------------------------------------


