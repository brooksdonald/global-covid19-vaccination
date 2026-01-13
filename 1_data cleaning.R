
# Staging -----------------------------------------------------------------

data_cov_time <- raw_data_acute_time %>%
  select(a_iso,
         month_name,
         a_pop,
         a_pop_hcw,
         a_pop_old,
         date_intro,
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
  filter(a_status_who == "Member State",
         is.na(month_name) == FALSE) %>%
  mutate(date_boost = as.Date("2021-12-01"),
         adm_old_boost_cap = case_when(
           a_iso == "CHN" & as.Date(month_name) %in% 
             seq(as.Date("2022-05-01"), as.Date("2022-10-01"), by = "1 month") ~ NA,
           TRUE ~ adm_old_boost_cap
         )) %>%
  left_join(., raw_entity_details %>% select(a_iso, adm_target_hcw), by = "a_iso")
  

# Clean timeseries data ---------------------------------------------------

source("clean_monotone.R")

df_clean <- clean_coverage_monotone_df(
  data_cov_time,
  id_iso   = "a_iso",
  id_time  = "month_name",
  vars_to_clean = c("adm_tot_td",
                    "adm_tot_a1d",
                    "adm_tot_cps",
                    "adm_tot_boost",
                    "adm_old_a1d_cap",
                    "adm_old_cps_cap",
                    "adm_old_boost_cap",
                    "adm_hcw_a1d",
                    "adm_hcw_cps",
                    "adm_hcw_boost"),
  depth    = 36,               # how far to look back/forward (non-NA points)
  batch    = TRUE,             # drop runs of offenders in one shot
  method   = "interpolate",    # "set_na" | "carry_forward" | "interpolate"
  round_int = TRUE
  ) %>%
  group_by(a_iso) %>%
  arrange(month_name) %>%
  fill(adm_old_a1d_cap_monotone,
       adm_old_cps_cap_monotone,
       adm_old_boost_cap_monotone,
       adm_hcw_a1d_monotone,
       adm_hcw_cps_monotone,
       adm_hcw_boost_monotone) %>%
  ungroup()

source("clean_interpolate.R")

df_interp <- interpolate_from_intro_wide_many(
  df         = df_clean,
  vars       = c("adm_old_a1d_cap_monotone",
                "adm_old_cps_cap_monotone",
                "adm_old_boost_cap_monotone",
                "adm_hcw_a1d_monotone",
                "adm_hcw_cps_monotone",
                "adm_hcw_boost_monotone"),
  iso_col   = "a_iso",
  date_col  = "month_name",
  intro_col = "date_intro",
  boost_col = "date_boost",
  suffix    = "_interp",   # new column 'adm_old_cps_cap_introinterp'
  cps_delay_months = 2) %>%
  mutate(
    adm_tot_a1d_monotone = pmin(adm_tot_a1d_monotone, a_pop),
    adm_tot_cps_monotone = pmin(adm_tot_cps_monotone, a_pop),
    adm_tot_boost_monotone = pmin(adm_tot_boost_monotone, a_pop),
    adm_old_a1d_cap_monotone_interp = pmin(adm_old_a1d_cap_monotone_interp, a_pop_old),
    adm_old_cps_cap_monotone_interp = pmin(adm_old_cps_cap_monotone_interp, a_pop_old),
    adm_old_boost_cap_monotone_interp = pmin(adm_old_boost_cap_monotone_interp, a_pop_old),
    adm_hcw_a1d_monotone_interp = pmin(adm_hcw_a1d_monotone_interp, a_pop_hcw),
    adm_hcw_cps_monotone_interp = pmin(adm_hcw_cps_monotone_interp, a_pop_hcw),
    adm_hcw_boost_monotone_interp = pmin(adm_hcw_boost_monotone_interp, a_pop_hcw)
  )


# Final time point --------------------------------------------------------

data_cov_21andlast <- df_interp %>%
  filter(month_name == as.Date("2021-12-01") | 
           month_name == as.Date("2023-12-01")) %>%
  select(a_iso,
         a_income_group,
         a_region_who,
         a_status_covax,
         a_status_who,
         a_pop,
         a_pop_hcw,
         a_pop_old,
         month_name,
         adm_target_hcw,
         adm_tot_td_monotone,
         adm_tot_a1d_monotone,
         adm_tot_cps_monotone,
         adm_tot_boost_monotone,
         adm_old_a1d_cap_monotone_interp,
         adm_old_cps_cap_monotone_interp,
         adm_old_boost_cap_monotone_interp,
         adm_hcw_a1d_monotone_interp,
         adm_hcw_cps_monotone_interp,
         adm_hcw_boost_monotone_interp) %>%
  mutate(cov_tot_a1d = pmin(adm_tot_a1d_monotone / a_pop, 1),
         cov_tot_cps = pmin(adm_tot_cps_monotone / a_pop, 1),
         cov_tot_boost = pmin(adm_tot_boost_monotone / a_pop, 1)) %>%
  mutate(hcw_flag = if_else(a_pop_hcw > adm_target_hcw, "Yes", NA_character_),
         hcw_diff = pmax(a_pop_hcw - adm_target_hcw, 0, na.rm = TRUE)) %>%
  mutate(adm_hcw_a1d_monotone_interp = pmin(adm_hcw_a1d_monotone_interp + (hcw_diff * cov_tot_a1d), a_pop_hcw),
         cov_hcw_a1d = pmin(adm_hcw_a1d_monotone_interp / a_pop_hcw, 1),
         adm_hcw_cps_monotone_interp = pmin(adm_hcw_cps_monotone_interp + (hcw_diff * cov_tot_cps), a_pop_hcw),
         cov_hcw_cps = pmin(adm_hcw_cps_monotone_interp / a_pop_hcw, 1),
         adm_old_boost_cap_monotone_interp = pmin(adm_old_boost_cap_monotone_interp + (hcw_diff * cov_tot_boost), a_pop_hcw),   
         cov_hcw_boost = pmin(adm_old_boost_cap_monotone_interp / a_pop_hcw), 1) %>%
  mutate(cov_old_a1d = pmin(adm_old_a1d_cap_monotone_interp / a_pop_old, 1),
         cov_old_cps = pmin(adm_old_cps_cap_monotone_interp / a_pop_old, 1),
         cov_old_boost = pmin(adm_old_boost_cap_monotone_interp / a_pop_old, 1))

data_cov_last <- data_cov_21andlast %>%
  filter(month_name == as.Date("2023-12-01")) %>%
  select(-month_name)

# End ---------------------------------------------------------------------