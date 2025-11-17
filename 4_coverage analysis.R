
# Staging -----------------------------------------------------------------

data_cov_time <- raw_data_acute_time %>%
  select(a_iso,
         month_name,
         a_status_who,
         a_income_group,
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
  select(-a_status_who) %>%
  mutate(date_boost = as.Date("2021-12-01"),
         adm_old_boost_cap = case_when(
           a_iso == "CHN" & as.Date(month_name) %in% seq(as.Date("2022-05-01"), as.Date("2022-10-01"), by = "1 month") ~ NA,
           TRUE ~ adm_old_boost_cap
         ))


# Clean timeseries data ---------------------------------------------------

source("clean_monotone.R")

df_clean <- clean_coverage_monotone_df(
  data_cov_time,                    # your data.frame / tibble
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
  depth    = 36,          # how far to look back/forward (non-NA points)
  batch    = TRUE,       # drop runs of offenders in one shot
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
       adm_hcw_boost_monotone)  %>%
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
  cps_delay_months = 2 
)

big_jumps <- df_interp %>%
  filter(month_name %in% as.Date(c("2022-10-01", "2022-11-01"))) %>%
  select(a_iso, month_name, adm_old_boost_cap_monotone_interp) %>%
  pivot_wider(
    names_from = month_name,
    values_from = adm_old_boost_cap_monotone_interp
  ) %>%
  rename(
    t2022_10 = `2022-10-01`,
    t2022_11 = `2022-11-01`
  ) %>%
  mutate(diff = t2022_11 - t2022_10) %>%
  arrange(desc(diff))

# countries with the largest increases
head(big_jumps, 20)


# Final time point --------------------------------------------------------

data_cov_last <- raw_data_acute_end %>%
  select(a_iso,
         a_region_who,
         a_income_group,
         a_status_covax,
         cov_old_a1d_adjust,
         cov_old_cps,
         cov_old_boost,
         cov_hcw_a1d_adjust,
         cov_hcw_cps,
         cov_hcw_boost,
         cov_tot_a1d,
         cov_tot_cps,
         cov_tot_boost) %>%
  gather(key = "metric", value = "coverage_denom_rep", -c("a_iso",
                                                          "a_income_group",
                                                          "a_region_who",
                                                          "a_status_covax"))%>%
  mutate(measure = case_when(
    str_detect(metric, "a1d") ~ "At least one dose",
    str_detect(metric, "cps") ~ "Complete primary series",
    str_detect(metric, "boost") ~ "At least one booster",
    TRUE ~ NA
    ),
    group = case_when(
      str_detect(metric, "old") ~ "Older adults",
      str_detect(metric, "hcw") ~ "Health and care workers",
      str_detect(metric, "tot") ~ "Total population",
      TRUE ~ NA
    )
  ) %>%
  select(-metric) %>%
  rename(country = a_iso) 


data_cov_test <- raw_data_acute_end %>%
  select(a_iso,
         adm_old_a1d_homo,
         adm_old_cps_homo,
         adm_old_boost_homo)

test <- df_interp %>%
  filter(month_name == as.Date("2023-12-01")) %>%
  select(a_iso,
         adm_old_a1d_cap_monotone_interp,
         adm_old_cps_cap_monotone_interp,
         adm_old_boost_cap_monotone_interp) %>%
  left_join(., data_cov_test, by = "a_iso")


  

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
         coverage_denom_vary = case_when(
           group == "Total population" & measure == "At least one dose" ~ admin / pop_world_total,
           group == "Total population" & measure == "Complete primary series" ~ admin / pop_world_total,
           group == "Total population" & measure == "At least one booster" ~ admin / pop_world_total,
           group == "Older adults" & measure == "At least one dose" ~ admin / pop_rep_old_a1d,
           group == "Older adults" & measure == "Complete primary series" ~ admin / pop_rep_old_cps,
           group == "Older adults" & measure == "At least one booster" ~ admin / pop_rep_old_boost,
           group == "Health and care workers" & measure == "At least one dose" ~ admin / pop_rep_hcw_a1d,
           group == "Health and care workers" & measure == "Complete primary series" ~ admin / pop_rep_hcw_cps,
           group == "Health and care workers" & measure == "At least one booster" ~ admin / pop_rep_hcw_boost,
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
  ungroup() %>%
  mutate(type = "all",
         dimension = NA)


# Summary timeseries - income ---------------------------------------------

details_red <- raw_data_acute_end %>%
  select(a_iso,
         a_region_who,
         a_status_covax)

data_cov_time_income_full <- df_interp %>%
  left_join(., details_red, by = ("a_iso" = "a_iso")) %>%
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
  mutate(group = if_else(str_detect(measure, "tot"), "Total population", 
                         if_else(str_detect(measure, "old"), "Older adults",
                                 if_else(str_detect(measure, "hcw"), "Health and care workers", NA))),
         measure = if_else(str_detect(measure, "a1d"), "At least one dose", 
                           if_else(str_detect(measure, "cps"), "Complete primary series", 
                                   if_else(str_detect(measure, "boost"), "At least one booster", 
                                           if_else(str_detect(measure, "td"), "Total doses",NA)))),
         coverage_denom_vary = case_when(
           group == "Total population" & measure == "At least one dose" ~ admin / pop_world_total,
           group == "Total population" & measure == "Complete primary series" ~ admin / pop_world_total,
           group == "Total population" & measure == "At least one booster" ~ admin / pop_world_total,
           group == "Older adults" & measure == "At least one dose" ~ admin / pop_rep_old_a1d,
           group == "Older adults" & measure == "Complete primary series" ~ admin / pop_rep_old_cps,
           group == "Older adults" & measure == "At least one booster" ~ admin / pop_rep_old_boost,
           group == "Health and care workers" & measure == "At least one dose" ~ admin / pop_rep_hcw_a1d,
           group == "Health and care workers" & measure == "Complete primary series" ~ admin / pop_rep_hcw_cps,
           group == "Health and care workers" & measure == "At least one booster" ~ admin / pop_rep_hcw_boost,
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
  ungroup() %>%
  mutate(type = "income") %>%
  rename(dimension = a_income_group)


# Summary timeseries - region ---------------------------------------------

data_cov_time_region_full <- df_interp %>%
  left_join(., details_red, by = ("a_iso" = "a_iso")) %>%
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
  mutate(group = if_else(str_detect(measure, "tot"), "Total population", 
                         if_else(str_detect(measure, "old"), "Older adults",
                                 if_else(str_detect(measure, "hcw"), "Health and care workers", NA))),
         measure = if_else(str_detect(measure, "a1d"), "At least one dose", 
                           if_else(str_detect(measure, "cps"), "Complete primary series", 
                                   if_else(str_detect(measure, "boost"), "At least one booster", 
                                           if_else(str_detect(measure, "td"), "Total doses",NA)))),
         coverage_denom_vary = case_when(
           group == "Total population" & measure == "At least one dose" ~ admin / pop_world_total,
           group == "Total population" & measure == "Complete primary series" ~ admin / pop_world_total,
           group == "Total population" & measure == "At least one booster" ~ admin / pop_world_total,
           group == "Older adults" & measure == "At least one dose" ~ admin / pop_rep_old_a1d,
           group == "Older adults" & measure == "Complete primary series" ~ admin / pop_rep_old_cps,
           group == "Older adults" & measure == "At least one booster" ~ admin / pop_rep_old_boost,
           group == "Health and care workers" & measure == "At least one dose" ~ admin / pop_rep_hcw_a1d,
           group == "Health and care workers" & measure == "Complete primary series" ~ admin / pop_rep_hcw_cps,
           group == "Health and care workers" & measure == "At least one booster" ~ admin / pop_rep_hcw_boost,
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
  ungroup() %>%
  mutate(type = "region") %>%
  rename(dimension = a_region_who)

# Summary timeseries - covax ---------------------------------------------

data_cov_time_covax_full <- df_interp %>%
  left_join(., details_red, by = ("a_iso" = "a_iso")) %>%
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
  mutate(group = if_else(str_detect(measure, "tot"), "Total population", 
                         if_else(str_detect(measure, "old"), "Older adults",
                                 if_else(str_detect(measure, "hcw"), "Health and care workers", NA))),
         measure = if_else(str_detect(measure, "a1d"), "At least one dose", 
                           if_else(str_detect(measure, "cps"), "Complete primary series", 
                                   if_else(str_detect(measure, "boost"), "At least one booster", 
                                           if_else(str_detect(measure, "td"), "Total doses",NA)))),
         coverage_denom_vary = case_when(
           group == "Total population" & measure == "At least one dose" ~ admin / pop_world_total,
           group == "Total population" & measure == "Complete primary series" ~ admin / pop_world_total,
           group == "Total population" & measure == "At least one booster" ~ admin / pop_world_total,
           group == "Older adults" & measure == "At least one dose" ~ admin / pop_rep_old_a1d,
           group == "Older adults" & measure == "Complete primary series" ~ admin / pop_rep_old_cps,
           group == "Older adults" & measure == "At least one booster" ~ admin / pop_rep_old_boost,
           group == "Health and care workers" & measure == "At least one dose" ~ admin / pop_rep_hcw_a1d,
           group == "Health and care workers" & measure == "Complete primary series" ~ admin / pop_rep_hcw_cps,
           group == "Health and care workers" & measure == "At least one booster" ~ admin / pop_rep_hcw_boost,
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
  ungroup() %>%
  mutate(type = "covax") %>%
  rename(dimension = a_status_covax)


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