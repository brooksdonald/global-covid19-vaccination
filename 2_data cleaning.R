
# Staging -----------------------------------------------------------------

# Select relevant uptake variables & join with entity details
data_admin_time <- raw_data_acute_time %>%
  select(a_iso,
         month_name,
         adm_tot_td,
         adm_tot_a1d,
         adm_tot_cps,
         adm_tot_boost,
         adm_old_a1d_cap,
         adm_old_cps_cap,
         adm_old_boost_cap,
         adm_hcw_a1d,
         adm_hcw_cps,
         adm_hcw_boost) %>%
  rename(adm_old_a1d = adm_old_a1d_cap,
         adm_old_cps = adm_old_cps_cap,
         adm_old_boost = adm_old_boost_cap) %>%
  left_join(., entity_pop %>% select(-pop_2024, 
                                     -pop_2024_old, 
                                     -pol_old, 
                                     -pol_old_source), 
            by = c("a_iso" = "iso")) %>%
  rename(iso = a_iso) %>%
  filter(status_who == "Member State" | iso == "PSE" | iso == "XKX",
         is.na(month_name) == FALSE) %>%
  
  # Remove non-report 0s
  mutate(date_boost = as.Date("2021-12-01"),
         adm_old_boost = case_when(
           iso == "CHN" & as.Date(month_name) %in% 
             seq(as.Date("2022-05-01"), as.Date("2022-10-01"), 
                 by = "1 month") ~ NA,
           TRUE ~ adm_old_boost
         ))
  

# Clean timeseries data ---------------------------------------------------

# Identify outliers and interpolate
source("clean_monotone.R")

data_admin_time_mt <- clean_coverage_monotone_df(
  data_admin_time,
  id_iso = "iso",
  id_time = "month_name",
  vars_to_clean = c("adm_tot_td",
                    "adm_tot_a1d",
                    "adm_tot_cps",
                    "adm_tot_boost",
                    "adm_old_a1d",
                    "adm_old_cps",
                    "adm_old_boost",
                    "adm_hcw_a1d",
                    "adm_hcw_cps",
                    "adm_hcw_boost"),
  depth = 36,               # how far to look back/forward (non-NA points)
  batch = TRUE,             # drop runs of offenders in one shot
  method = "interpolate",   # "set_na" | "carry_forward" | "interpolate"
  round_int = TRUE
  ) %>%
  
  # Fill value forward through end of timeseries
  group_by(iso) %>%
  arrange(month_name) %>%
  fill(adm_old_a1d_monotone,
       adm_old_cps_monotone,
       adm_old_boost_monotone,
       adm_hcw_a1d_monotone,
       adm_hcw_cps_monotone,
       adm_hcw_boost_monotone) %>%
  ungroup()

# Interpolate between vaccine / booster introduction data and first report
source("clean_interpolate.R")

data_admin_time_interp <- interpolate_from_intro_wide_many(
  df = data_admin_time_mt,
  vars = c("adm_old_a1d_monotone",
           "adm_old_cps_monotone",
           "adm_old_boost_monotone",
           "adm_hcw_a1d_monotone",
           "adm_hcw_cps_monotone",
           "adm_hcw_boost_monotone"),
  iso_col = "iso",
  date_col = "month_name",
  intro_col = "date_intro",
  boost_col = "date_boost",
  suffix = "_interp",
  cps_delay_months = 2) %>%
  
  # Cap cleaned figures at population max
  mutate(
    adm_tot_a1d_monotone = pmin(adm_tot_a1d_monotone, pop_2021),
    adm_tot_cps_monotone = pmin(adm_tot_cps_monotone, pop_2021),
    adm_tot_boost_monotone = pmin(adm_tot_boost_monotone, pop_2021),
    adm_old_a1d_monotone_interp = pmin(adm_old_a1d_monotone_interp, 
                                           pop_2021_old),
    adm_old_cps_monotone_interp = pmin(adm_old_cps_monotone_interp, 
                                           pop_2021_old),
    adm_old_boost_monotone_interp = pmin(adm_old_boost_monotone_interp, 
                                             pop_2021_old),
    adm_hcw_a1d_monotone_interp = pmin(adm_hcw_a1d_monotone_interp, pop_hcw),
    adm_hcw_cps_monotone_interp = pmin(adm_hcw_cps_monotone_interp, pop_hcw),
    adm_hcw_boost_monotone_interp = pmin(adm_hcw_boost_monotone_interp, pop_hcw)
  )


# Create end-2023 dataframe for reporting related calculations ------------

data_admin_last <- data_admin_time_interp %>%
  filter(month_name == as.Date("2023-12-01"))


# End ---------------------------------------------------------------------