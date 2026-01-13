
# Total - global - count / population ---------------------------------------------

world_total <- data_cov_last %>%
  filter(a_status_who == "Member State") %>%
  summarize(pop_world_total = sum(a_pop),
            count_world = n()) 

pop_world_total <- world_total %>% pull(pop_world_total)
count_world <- world_total %>% pull(count_world)


# Total - regional - count / population -------------------------------------------

total_region <- data_cov_last %>%
  filter(a_status_who == "Member State") %>%
  group_by(a_region_who) %>%
  summarize(pop_total = sum(a_pop),
            count = n()) %>%
  ungroup()

pop_total_region  <- setNames(total_region$pop_total, total_region$a_region_who)
count_region <- setNames(total_region$count, total_region$a_region_who)

pop_total_afr  <- pop_total_region[["AFR"]]
pop_total_amr  <- pop_total_region[["AMR"]]
pop_total_emr  <- pop_total_region[["EMR"]]
pop_total_eur  <- pop_total_region[["EUR"]]
pop_total_sear <- pop_total_region[["SEAR"]]
pop_total_wpr  <- pop_total_region[["WPR"]]

count_afr  <- count_region[["AFR"]]
count_amr  <- count_region[["AMR"]]
count_emr  <- count_region[["EMR"]]
count_eur  <- count_region[["EUR"]]
count_sear <- count_region[["SEAR"]]
count_wpr  <- count_region[["WPR"]]


# Total - income - count / population ---------------------------------------------

total_income <- data_cov_last %>%
  filter(a_status_who == "Member State") %>%
  group_by(a_income_group) %>%
  summarize(pop_total = sum(a_pop),
            count = n()) %>%
  ungroup()

pop_total_income  <- setNames(total_income$pop_total, total_income$a_income_group)
count_income <- setNames(total_income$count, total_income$a_income_group)

pop_total_hic  <- pop_total_income[["HIC"]]
pop_total_umic  <- pop_total_income[["UMIC"]]
pop_total_lmic  <- pop_total_income[["LMIC"]]
pop_total_lic  <- pop_total_income[["LIC"]]

count_hic  <- count_income[["HIC"]]
count_umic  <- count_income[["UMIC"]]
count_lmic  <- count_income[["LMIC"]]
count_lic  <- count_income[["LIC"]]


# Total - covax - count / population ----------------------------------------------

total_covax <- data_cov_last %>%
  filter(is.na(a_status_covax) == FALSE) %>%
  group_by(a_status_covax) %>%
  summarize(pop_total = sum(a_pop),
            count = n()) %>%
  ungroup()

pop_total_covax  <- setNames(total_covax$pop_total, total_covax$a_status_covax)
count_covax <- setNames(total_covax$count, total_covax$a_status_covax)

pop_total_amc  <- pop_total_covax[["AMC"]]
pop_total_sfp  <- pop_total_covax[["Self-financing"]]
pop_total_noncovax  <- pop_total_covax[["No"]]

count_amc  <- count_covax[["AMC"]]
count_sfp  <- count_covax[["Self-financing"]]
count_noncovax  <- count_covax[["No"]]


# Total - global - count / population - reporting MS ------------------------------

# Reporting (a1d) WHO MS - total
rep_total_a1d <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_tot_a1d_monotone) == FALSE) %>%
  select(a_pop) %>%
  summarize(pop_rep_total_a1d = sum(a_pop),
            count_rep_total_a1d = n())

pop_rep_total_a1d <- rep_total_a1d %>% pull(pop_rep_total_a1d)
count_rep_total_a1d <- rep_total_a1d %>% pull(count_rep_total_a1d)

# Reporting (cps) WHO MS - total
rep_total_cps <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_tot_cps_monotone) == FALSE) %>%
  select(a_pop) %>%
  summarize(pop_rep_total_cps = sum(a_pop),
            count_rep_total_cps = n())

pop_rep_total_cps <- rep_total_cps %>% pull(pop_rep_total_cps)
count_rep_total_cps <- rep_total_cps %>% pull(count_rep_total_cps)

# Reporting (booster) WHO MS - total
rep_total_boost <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_tot_boost_monotone) == FALSE) %>%
  select(a_pop) %>%
  summarize(pop_rep_total_boost = sum(a_pop),
            count_rep_total_boost = n())

pop_rep_total_boost <- rep_total_boost %>% pull(pop_rep_total_boost)
count_rep_total_boost <- rep_total_boost %>% pull(count_rep_total_boost)


# Older adults - global - population --------------------------------------

pop_world_old <- data_cov_last %>%
  filter(a_status_who == "Member State") %>%
  select(a_pop_old) %>%
  summarize(pop_world_old = sum(a_pop_old)) %>%
  pull(pop_world_old)


# Older adults - regional - population -------------------------------------------

pop_old_region <- data_cov_last %>%
  filter(a_status_who == "Member State") %>%
  select(a_region_who,
         a_pop_old) %>%
  group_by(a_region_who) %>%
  summarize(pop_old = sum(a_pop_old)) %>%
  deframe()

pop_old_afr <- unname(pop_old_region["AFR"])
pop_old_amr <- unname(pop_old_region["AMR"])
pop_old_emr <- unname(pop_old_region["EMR"])
pop_old_eur <- unname(pop_old_region["EUR"])
pop_old_sear <- unname(pop_old_region["SEAR"])
pop_old_wpr <- unname(pop_old_region["WPR"])


# Older adults - income - population ---------------------------------------------

pop_old_income <- data_cov_last %>%
  filter(a_status_who == "Member State") %>%
  select(a_income_group,
         a_pop_old) %>%
  group_by(a_income_group) %>%
  summarize(pop_old = sum(a_pop_old)) %>%
  deframe()

pop_old_hic <- unname(pop_old_income["HIC"])
pop_old_umic <- unname(pop_old_income["UMIC"])
pop_old_lmic <- unname(pop_old_income["LMIC"])
pop_old_lic <- unname(pop_old_income["LIC"])


# Older adults - covax - population ----------------------------------------------

pop_old_covax <- data_cov_last %>%
  filter(is.na(a_status_covax) == FALSE) %>%
  select(a_status_covax,
         a_pop_old) %>%
  group_by(a_status_covax) %>%
  summarize(pop_old = sum(a_pop_old)) %>%
  deframe()

pop_old_amc <- unname(pop_old_covax["AMC"])
pop_old_sfp <- unname(pop_old_covax["Self-financing"])
pop_old_noncovax <- unname(pop_old_covax["No"])


# Older adults - global - population - reporting MS -----------------------

#Reporting (a1d) WHO MS older adults
pop_rep_old_a1d <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_old_a1d_cap_monotone_interp) == FALSE) %>%
  select(a_pop_old) %>%
  summarize(pop_rep_old_a1d = sum(a_pop_old)) %>%
  pull(pop_rep_old_a1d)

#Reporting (cps) WHO MS older adults
pop_rep_old_cps <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_old_cps_cap_monotone_interp) == FALSE) %>%
  select(a_pop_old) %>%
  summarize(pop_rep_old_cps = sum(a_pop_old)) %>%
  pull(pop_rep_old_cps)

#Reporting (booster) WHO MS older adults
pop_rep_old_boost <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_old_boost_cap_monotone_interp) == FALSE) %>%
  select(a_pop_old) %>%
  summarize(pop_rep_old_boost = sum(a_pop_old)) %>%
  pull(pop_rep_old_boost)


# Older adults - income - population - reporting MS -----------------------

## At least one dose
pop_rep_old_income_a1d <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_old_a1d_cap_monotone_interp) == FALSE) %>%
  select(a_income_group,
         a_pop_old) %>%
  group_by(a_income_group) %>%
  summarize(pop_old = sum(a_pop_old)) %>%
  deframe()

pop_rep_old_hic_a1d <- unname(pop_rep_old_income_a1d["HIC"])
pop_rep_old_umic_a1d <- unname(pop_rep_old_income_a1d["UMIC"])
pop_rep_old_lmic_a1d <- unname(pop_rep_old_income_a1d["LMIC"])
pop_rep_old_lic_a1d <- unname(pop_rep_old_income_a1d["LIC"])

## Complete primary series
pop_rep_old_income_cps <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_old_cps_cap_monotone_interp) == FALSE) %>%
  select(a_income_group,
         a_pop_old) %>%
  group_by(a_income_group) %>%
  summarize(pop_old = sum(a_pop_old)) %>%
  deframe()

pop_rep_old_hic_cps <- unname(pop_rep_old_income_cps["HIC"])
pop_rep_old_umic_cps <- unname(pop_rep_old_income_cps["UMIC"])
pop_rep_old_lmic_cps <- unname(pop_rep_old_income_cps["LMIC"])
pop_rep_old_lic_cps <- unname(pop_rep_old_income_cps["LIC"])

## At least one booster
pop_rep_old_income_boost <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_old_boost_cap_monotone_interp) == FALSE) %>%
  select(a_income_group,
         a_pop_old) %>%
  group_by(a_income_group) %>%
  summarize(pop_old = sum(a_pop_old)) %>%
  deframe()

pop_rep_old_hic_boost <- unname(pop_rep_old_income_boost["HIC"])
pop_rep_old_umic_boost <- unname(pop_rep_old_income_boost["UMIC"])
pop_rep_old_lmic_boost <- unname(pop_rep_old_income_boost["LMIC"])
pop_rep_old_lic_boost <- unname(pop_rep_old_income_boost["LIC"])


# Older adults - region - population - reporting MS -----------------------

## At least one dose
pop_rep_old_region_a1d <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_old_a1d_cap_monotone_interp) == FALSE) %>%
  select(a_region_who,
         a_pop_old) %>%
  group_by(a_region_who) %>%
  summarize(pop_old = sum(a_pop_old)) %>%
  deframe()

pop_rep_old_afr_a1d <- unname(pop_rep_old_region_a1d["AFR"])
pop_rep_old_amr_a1d <- unname(pop_rep_old_region_a1d["AMR"])
pop_rep_old_emr_a1d <- unname(pop_rep_old_region_a1d["EMR"])
pop_rep_old_eur_a1d <- unname(pop_rep_old_region_a1d["EUR"])
pop_rep_old_sear_a1d <- unname(pop_rep_old_region_a1d["SEAR"])
pop_rep_old_wpr_a1d <- unname(pop_rep_old_region_a1d["WPR"])

## Complete primary series
pop_rep_old_region_cps <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_old_cps_cap_monotone_interp) == FALSE) %>%
  select(a_region_who,
         a_pop_old) %>%
  group_by(a_region_who) %>%
  summarize(pop_old = sum(a_pop_old)) %>%
  deframe()

pop_rep_old_afr_cps <- unname(pop_rep_old_region_cps["AFR"])
pop_rep_old_amr_cps <- unname(pop_rep_old_region_cps["AMR"])
pop_rep_old_emr_cps <- unname(pop_rep_old_region_cps["EMR"])
pop_rep_old_eur_cps <- unname(pop_rep_old_region_cps["EUR"])
pop_rep_old_sear_cps <- unname(pop_rep_old_region_cps["SEAR"])
pop_rep_old_wpr_cps <- unname(pop_rep_old_region_cps["WPR"])

## At least one booster
pop_rep_old_region_boost <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_old_boost_cap_monotone_interp) == FALSE) %>%
  select(a_region_who,
         a_pop_old) %>%
  group_by(a_region_who) %>%
  summarize(pop_old = sum(a_pop_old)) %>%
  deframe()

pop_rep_old_afr_boost <- unname(pop_rep_old_region_boost["AFR"])
pop_rep_old_amr_boost <- unname(pop_rep_old_region_boost["AMR"])
pop_rep_old_emr_boost <- unname(pop_rep_old_region_boost["EMR"])
pop_rep_old_eur_boost <- unname(pop_rep_old_region_boost["EUR"])
pop_rep_old_sear_boost <- unname(pop_rep_old_region_boost["SEAR"])
pop_rep_old_wpr_boost <- unname(pop_rep_old_region_boost["WPR"])


# Older adults - covax - population - reporting MS -----------------------

## At least one dose
pop_rep_old_covax_a1d <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_old_a1d_cap_monotone_interp) == FALSE) %>%
  select(a_status_covax,
         a_pop_old) %>%
  group_by(a_status_covax) %>%
  summarize(pop_old = sum(a_pop_old)) %>%
  deframe()

pop_rep_old_amc_a1d <- unname(pop_rep_old_covax_a1d["AMC"])
pop_rep_old_sfp_a1d <- unname(pop_rep_old_covax_a1d["Self-financing"])
pop_rep_old_noncovax_a1d <- unname(pop_rep_old_covax_a1d["No"])

## Complete primary series
pop_rep_old_covax_cps <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_old_cps_cap_monotone_interp) == FALSE) %>%
  select(a_status_covax,
         a_pop_old) %>%
  group_by(a_status_covax) %>%
  summarize(pop_old = sum(a_pop_old)) %>%
  deframe()

pop_rep_old_amc_cps <- unname(pop_rep_old_covax_cps["AMC"])
pop_rep_old_sfp_cps <- unname(pop_rep_old_covax_cps["Self-financing"])
pop_rep_old_noncovax_cps <- unname(pop_rep_old_covax_cps["No"])

## At least one booster
pop_rep_old_covax_boost <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_old_boost_cap_monotone_interp) == FALSE) %>%
  select(a_status_covax,
         a_pop_old) %>%
  group_by(a_status_covax) %>%
  summarize(pop_old = sum(a_pop_old)) %>%
  deframe()

pop_rep_old_amc_boost <- unname(pop_rep_old_covax_boost["AMC"])
pop_rep_old_sfp_boost <- unname(pop_rep_old_covax_boost["Self-financing"])
pop_rep_old_noncovax_boost <- unname(pop_rep_old_covax_boost["No"])


# HCW - global - population -----------------------------------------------

pop_world_hcw <- data_cov_last %>%
  filter(a_status_who == "Member State") %>%
  select(a_pop_hcw) %>%
  summarize(pop_world_hcw = sum(a_pop_hcw)) %>%
  pull(pop_world_hcw)

# HCW - regional - population -------------------------------------------

pop_hcw_region <- data_cov_last %>%
  filter(a_status_who == "Member State") %>%
  select(a_region_who,
         a_pop_hcw) %>%
  group_by(a_region_who) %>%
  summarize(pop_hcw = sum(a_pop_hcw)) %>%
  deframe()

pop_hcw_afr <- unname(pop_hcw_region["AFR"])
pop_hcw_amr <- unname(pop_hcw_region["AMR"])
pop_hcw_emr <- unname(pop_hcw_region["EMR"])
pop_hcw_eur <- unname(pop_hcw_region["EUR"])
pop_hcw_sear <- unname(pop_hcw_region["SEAR"])
pop_hcw_wpr <- unname(pop_hcw_region["WPR"])


# HCW - income - population ---------------------------------------------

pop_hcw_income <- data_cov_last %>%
  filter(a_status_who == "Member State") %>%
  select(a_income_group,
         a_pop_hcw) %>%
  group_by(a_income_group) %>%
  summarize(pop_hcw = sum(a_pop_hcw)) %>%
  deframe()

pop_hcw_hic <- unname(pop_hcw_income["HIC"])
pop_hcw_umic <- unname(pop_hcw_income["UMIC"])
pop_hcw_lmic <- unname(pop_hcw_income["LMIC"])
pop_hcw_lic <- unname(pop_hcw_income["LIC"])


# HCW - covax - population ----------------------------------------------

pop_hcw_covax <- data_cov_last %>%
  filter(is.na(a_status_covax) == FALSE) %>%
  select(a_status_covax,
         a_pop_hcw) %>%
  group_by(a_status_covax) %>%
  summarize(pop_total = sum(a_pop_hcw)) %>%
  deframe()

pop_hcw_amc <- unname(pop_hcw_covax["AMC"])
pop_hcw_sfp <- unname(pop_hcw_covax["Self-financing"])
pop_hcw_noncovax <- unname(pop_hcw_covax["No"])


# HCW - global - population - reporting MS --------------------------------

#Reporting (a1d) WHO MS - HCWs
pop_rep_hcw_a1d <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_hcw_a1d_monotone_interp) == FALSE) %>%
  select(a_pop_hcw) %>%
  summarize(pop_rep_hcw_a1d = sum(a_pop_hcw)) %>%
  pull(pop_rep_hcw_a1d)

#Reporting (cps) WHO MS - HCWs
pop_rep_hcw_cps <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_hcw_cps_monotone_interp) == FALSE) %>%
  select(a_pop_hcw) %>%
  summarize(pop_rep_hcw_cps = sum(a_pop_hcw)) %>%
  pull(pop_rep_hcw_cps)

#Reporting (booster) WHO MS - HCWs
pop_rep_hcw_boost <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_hcw_boost_monotone_interp) == FALSE) %>%
  select(a_pop_hcw) %>%
  summarize(pop_rep_hcw_boost = sum(a_pop_hcw)) %>%
  pull(pop_rep_hcw_boost)


# HCW - income - population - reporting MS --------------------------------

## At least one dose
pop_rep_hcw_income_a1d <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_hcw_a1d_monotone_interp) == FALSE) %>%
  select(a_income_group,
         a_pop_hcw) %>%
  group_by(a_income_group) %>%
  summarize(pop_hcw = sum(a_pop_hcw)) %>%
  deframe()

pop_rep_hcw_hic_a1d <- unname(pop_rep_hcw_income_a1d["HIC"])
pop_rep_hcw_umic_a1d <- unname(pop_rep_hcw_income_a1d["UMIC"])
pop_rep_hcw_lmic_a1d <- unname(pop_rep_hcw_income_a1d["LMIC"])
pop_rep_hcw_lic_a1d <- unname(pop_rep_hcw_income_a1d["LIC"])

## Complete primary series
pop_rep_hcw_income_cps <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_hcw_cps_monotone_interp) == FALSE) %>%
  select(a_income_group,
         a_pop_hcw) %>%
  group_by(a_income_group) %>%
  summarize(pop_hcw = sum(a_pop_hcw)) %>%
  deframe()

pop_rep_hcw_hic_cps <- unname(pop_rep_hcw_income_cps["HIC"])
pop_rep_hcw_umic_cps <- unname(pop_rep_hcw_income_cps["UMIC"])
pop_rep_hcw_lmic_cps <- unname(pop_rep_hcw_income_cps["LMIC"])
pop_rep_hcw_lic_cps <- unname(pop_rep_hcw_income_cps["LIC"])

## At least one booster
pop_rep_hcw_income_boost <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_hcw_boost_monotone_interp) == FALSE) %>%
  select(a_income_group,
         a_pop_hcw) %>%
  group_by(a_income_group) %>%
  summarize(pop_hcw = sum(a_pop_hcw)) %>%
  deframe()

pop_rep_hcw_hic_boost <- unname(pop_rep_hcw_income_boost["HIC"])
pop_rep_hcw_umic_boost <- unname(pop_rep_hcw_income_boost["UMIC"])
pop_rep_hcw_lmic_boost <- unname(pop_rep_hcw_income_boost["LMIC"])
pop_rep_hcw_lic_boost <- unname(pop_rep_hcw_income_boost["LIC"])


# HCW - region - population - reporting MS -------------------------------

## At least one dose
pop_rep_hcw_region_a1d <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_hcw_a1d_monotone_interp) == FALSE) %>%
  select(a_region_who,
         a_pop_hcw) %>%
  group_by(a_region_who) %>%
  summarize(pop_hcw = sum(a_pop_hcw)) %>%
  deframe()

pop_rep_hcw_afr_a1d <- unname(pop_rep_hcw_region_a1d["AFR"])
pop_rep_hcw_amr_a1d <- unname(pop_rep_hcw_region_a1d["AMR"])
pop_rep_hcw_emr_a1d <- unname(pop_rep_hcw_region_a1d["EMR"])
pop_rep_hcw_eur_a1d <- unname(pop_rep_hcw_region_a1d["EUR"])
pop_rep_hcw_sear_a1d <- unname(pop_rep_hcw_region_a1d["SEAR"])
pop_rep_hcw_wpr_a1d <- unname(pop_rep_hcw_region_a1d["WPR"])

## Complete primary series
pop_rep_hcw_region_cps <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_hcw_cps_monotone_interp) == FALSE) %>%
  select(a_region_who,
         a_pop_hcw) %>%
  group_by(a_region_who) %>%
  summarize(pop_hcw = sum(a_pop_hcw)) %>%
  deframe()

pop_rep_hcw_afr_cps <- unname(pop_rep_hcw_region_cps["AFR"])
pop_rep_hcw_amr_cps <- unname(pop_rep_hcw_region_cps["AMR"])
pop_rep_hcw_emr_cps <- unname(pop_rep_hcw_region_cps["EMR"])
pop_rep_hcw_eur_cps <- unname(pop_rep_hcw_region_cps["EUR"])
pop_rep_hcw_sear_cps <- unname(pop_rep_hcw_region_cps["SEAR"])
pop_rep_hcw_wpr_cps <- unname(pop_rep_hcw_region_cps["WPR"])

## At least one booster
pop_rep_hcw_region_boost <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_hcw_boost_monotone_interp) == FALSE) %>%
  select(a_region_who,
         a_pop_hcw) %>%
  group_by(a_region_who) %>%
  summarize(pop_hcw = sum(a_pop_hcw)) %>%
  deframe()

pop_rep_hcw_afr_boost <- unname(pop_rep_hcw_region_boost["AFR"])
pop_rep_hcw_amr_boost <- unname(pop_rep_hcw_region_boost["AMR"])
pop_rep_hcw_emr_boost <- unname(pop_rep_hcw_region_boost["EMR"])
pop_rep_hcw_eur_boost <- unname(pop_rep_hcw_region_boost["EUR"])
pop_rep_hcw_sear_boost <- unname(pop_rep_hcw_region_boost["SEAR"])
pop_rep_hcw_wpr_boost <- unname(pop_rep_hcw_region_boost["WPR"])


# HCW - covax - population - reporting MS -----------------------

## At least one dose
pop_rep_hcw_covax_a1d <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_hcw_a1d_monotone_interp) == FALSE) %>%
  select(a_status_covax,
         a_pop_hcw) %>%
  group_by(a_status_covax) %>%
  summarize(pop_hcw = sum(a_pop_hcw)) %>%
  deframe()

pop_rep_hcw_amc_a1d <- unname(pop_rep_hcw_covax_a1d["AMC"])
pop_rep_hcw_sfp_a1d <- unname(pop_rep_hcw_covax_a1d["Self-financing"])
pop_rep_hcw_noncovax_a1d <- unname(pop_rep_hcw_covax_a1d["No"])

## Complete primary series
pop_rep_hcw_covax_cps <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_hcw_cps_monotone_interp) == FALSE) %>%
  select(a_status_covax,
         a_pop_hcw) %>%
  group_by(a_status_covax) %>%
  summarize(pop_hcw = sum(a_pop_hcw)) %>%
  deframe()

pop_rep_hcw_amc_cps <- unname(pop_rep_hcw_covax_cps["AMC"])
pop_rep_hcw_sfp_cps <- unname(pop_rep_hcw_covax_cps["Self-financing"])
pop_rep_hcw_noncovax_cps <- unname(pop_rep_hcw_covax_cps["No"])

## At least one booster
pop_rep_hcw_covax_boost <- data_cov_last %>%
  filter(a_status_who == "Member State",
         is.na(adm_hcw_boost_monotone_interp) == FALSE) %>%
  select(a_status_covax,
         a_pop_hcw) %>%
  group_by(a_status_covax) %>%
  summarize(pop_hcw = sum(a_pop_hcw)) %>%
  deframe()

pop_rep_hcw_amc_boost <- unname(pop_rep_hcw_covax_boost["AMC"])
pop_rep_hcw_sfp_boost <- unname(pop_rep_hcw_covax_boost["Self-financing"])
pop_rep_hcw_noncovax_boost <- unname(pop_rep_hcw_covax_boost["No"])


# End ---------------------------------------------------------------------
