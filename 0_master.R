# COVID-19 vaccination analysis
# Author: BROOKS, Donald J.

# Setup -------------------------------------------------------------------

# Clear environment
rm(list = ls())
gc()
options(scipen = 999)

# Set working directory
setwd("/Users/brooksdonald/Documents/GitHub/covid_vaccination/")

# Load packages

lib <- c("tidyverse",
         "readxl",
         "writexl",
         "lubridate",
         "countrycode",
         "data.table",
         "httr",
         "scales",
         "jsonlite",
         "ggpattern",
         "ggrepel",
         "scales",
         "treemap",
         "ggtext",
         "ggforce",
         "tibble",
         "kableExtra",
         "knitr",
         "ordering",
         "ggrepel",
         "shadowtext",
         "patchwork",
         "forcats",
         "flextable",
         "officer")

lapply(lib, library, character.only = TRUE)


# Process variables --------------------------------------------------------

path_save <- "/Users/brooksdonald/Documents/GitHub/covid_vaccination/output/"
theme_master <- theme_test

# Extract data ------------------------------------------------------------

# Load base data
raw_entity_details <- data.frame(
  read_excel("input/entity_details.xlsx",
             sheet = "Sheet1"))

raw_entity <- data.frame(
  read_excel("input/base_entitydetails.xlsx",
             sheet = "data"))

raw_entity_new <- data.frame(
  read_excel("input/data_export_WIISE_REF_COUNTRIES.xlsx",
             sheet = "REF_COUNTRIES")) %>%
  select(CODE,
         WHOREGIONC,
         WHO_LEGAL_STATUS_TITLE,
         WBINCOMESTATUS,
         COVAX) %>%
  filter(!is.na(WHO_LEGAL_STATUS_TITLE)) %>%
  rename(a_iso = CODE,
         a_region_who = WHOREGIONC,
         a_income_group = WBINCOMESTATUS,
         a_status_who = WHO_LEGAL_STATUS_TITLE,
         a_status_covax = COVAX) %>%
  mutate(a_region_who = substr(a_region_who, 1, nchar(a_region_who) - 1),
         a_income_group = case_when(
           a_income_group == "Low income" ~ "LIC",
           a_income_group == "Lower middle income" ~ "LMIC",
           a_income_group == "Upper middle income" ~ "UMIC",
           grepl("High income", a_income_group) == TRUE ~ "HIC",
           TRUE ~ a_income_group
         ),
         a_status_covax = case_when(
           a_status_covax == "No" ~ "Non-COVAX",
           TRUE ~ a_status_covax
         )) %>%
  arrange(a_iso)

raw_data_acute_time <- data.frame(
  read_excel("input/output_master_test.xlsx",
             sheet = "1_stock")) %>%
  select(-a_status_who,
         -a_status_covax,
         -a_region_who,
         -a_income_group) %>%
  left_join(., raw_entity_new, by = "a_iso")

raw_data_acute_time_daily <- data.frame(
  read_excel("input/output_master_test.xlsx",
             sheet = "1_adm_all_long")) %>%
  select(-a_status_who,
         -a_status_covax,
         -a_region_who,
         -a_income_group) %>%
  left_join(., raw_entity_new, by = "a_iso")

raw_data_post_final <- data.frame(
  read_excel("input/output_new.xlsx",
             sheet = "uptake_grouping_wide_red")) %>%
  select(grouping,
         type,
         count_rep_older,
         count_rep_hcw,
         prop_pop_old,
         prop_pop_hcw,
         cov_older,
         cov_hcw) %>%
  gather(key = indicator,
         value = value,
         -grouping,
         -type) %>%
  mutate(group = case_when(
    grepl("hcw", indicator, ignore.case = TRUE) ~ "Health and care workers",
    grepl("old", indicator, ignore.case = TRUE) ~ "Older adults",
    TRUE ~ NA
  ),
  indicator = case_when(
    grepl("count", indicator, ignore.case = TRUE) ~ "Count of reporting MS",
    grepl("cov", indicator, ignore.case = TRUE) ~ "Coverage (%)",
    grepl("prop", indicator, ignore.case = TRUE) ~ "Proportion of grouping population in reporting MS",
    TRUE ~ NA
  ))

# Load reported, disaggregated uptake data
raw_target <- GET('https://xmart-api-public.who.int/WIISE/MT_COV_UPTAKE_TARGETGROUP_LONG',
                  add_headers(`Cache-Control` = "no-cache"),
                  config = config(fresh_connect = TRUE))
raw_target_text <- content(raw_target, "text")
raw_target_json <- fromJSON(raw_target_text, flatten = TRUE)
base_target <- as.data.frame(raw_target_json)
names(base_target) <- substring(names(base_target), 7)

# Load reported, aggregate uptake data
raw_weekly <- GET('https://xmart-api-public.who.int/WIISE/COV_UPTAKE_WEEKLY')
raw_weekly_text <- content(raw_weekly, "text")
raw_weekly_json <- fromJSON(raw_weekly_text, flatten = TRUE)
base_total <- as.data.frame(raw_weekly_json)
names(base_total) <- substring(names(base_total), 7)

# Load policy data
raw_policy_wide <- data.frame(
  read_excel("input/output_new.xlsx",
             sheet = "policy_country_wide")) %>%
  filter(!is.na(a_income_group)) %>%
  mutate(status_policy = case_when(
    status_policy == "1) Periodic revaccination & primary vaccination recommended" ~ "Periodic revaccination & primary vaccination recommended",
    status_policy == "2) Vaccination recommended" ~ "Primary vaccination only recommended",
    status_policy == "3) Vaccination not recommended" ~ "(Re-) vaccination not recommended",
    status_policy == "4) No response provided" ~ "No response provided",
    TRUE ~ NA
  ),
  policy_revacc_interval = case_when(
    policy_revacc_interval == "12_MONTHS" ~ "12 months",
    policy_revacc_interval == "6_MONTHS" ~ "6 months",
    policy_revacc_interval == "OTHER" ~ "Other",
    TRUE ~ policy_revacc_interval
  ))


# Load analyses -----------------------------------------------------------

source("1_data cleaning.R")
source("2_static values.R")
source("3_acute coverage analysis.R")
source("3_reporting analysis.R")
source("4_daily vaccination rate analysis.R")

source("9_design specifications.R")


# Figure 1 - end-2023 coverage, violins, income group -----------------------

#  Prepare data
data_cov_end_inc <- data_cov_last %>%
  select(a_iso,
         a_status_who,
         a_income_group,
         cov_tot_a1d,
         cov_tot_cps,
         cov_tot_boost,
         cov_old_a1d,
         cov_old_cps,
         cov_old_boost,
         cov_hcw_a1d,
         cov_hcw_cps,
         cov_hcw_boost) %>%
  filter(a_status_who == "Member State",
         a_income_group != "Other") %>%
  select(-a_status_who) %>%
  gather(key = "measure", value = "coverage", -a_iso, -a_income_group) %>%
  mutate(group = if_else(str_detect(measure, "cov_tot"), "Total population", 
                         if_else(str_detect(measure, "cov_old"), "Older adults",
                                 if_else(str_detect(measure, "cov_hcw"), "Health and care workers", NA))),
         measure = if_else(str_detect(measure, "a1d"), "At least one dose", 
                           if_else(str_detect(measure, "cps"), "Complete primary series", 
                                   if_else(str_detect(measure, "boost"), "At least one booster dose", NA))))

# Create figure
fig_cov_end_inc <- data_cov_end_inc %>%
  mutate(a_income_group = factor(a_income_group, 
                                 levels = c("LIC", 
                                            "LMIC", 
                                            "UMIC", 
                                            "HIC")),
         measure = factor(measure, levels = c("At least one dose", 
                                              "Complete primary series", 
                                              "At least one booster dose")),
         group = factor(group, levels = c("Total population", 
                                          "Older adults", 
                                          "Health and care workers"))) %>%
  ggplot(aes(x = a_income_group, 
             y = coverage, 
             fill = a_income_group)) + 
  geom_violin(trim = TRUE) + 
  geom_boxplot(width = 0.1, color = "black") +
  facet_grid(group ~ measure, scales = "free", space = "free") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = c(color_who_cat_base_1, color_who_cat_base_2, 
                               color_who_cat_base_3, color_who_cat_base_4)) +
  theme_master() +
  labs(title = "COVID-19 vaccination coverage by dose, population group, and income group, end-2023",
       x = "Income group",
       y = "Vaccination coverage (% of population)") + 
  theme(axis.title.x = element_text(vjust = -2, size = 12),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.title.y = element_text(vjust = 4, size = 12),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        legend.position = "none")

fig_cov_end_inc

# Export
ggsave(
  path = path_save,
  filename = "fig_1_cov_end2023.jpg",
  plot = fig_cov_end_inc,
  width = 10,
  height = 7,
  dpi = 300
)


# Figure 2 - end-2023 coverage comp., scatter & violin, total vs. old / hcw ----

# Prepare data
df_parity_2023 <- data_cov_21andlast %>%
  filter(a_income_group != "Other") %>%
  filter(month_name == "2023-12-01") %>%
  mutate(
    a_income_group = forcats::fct_relevel(a_income_group, "LIC","LMIC","UMIC","HIC")
  ) %>%
  transmute(
    a_iso,
    a_income_group,
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
  mutate(
    group = recode(group,
                   hcw = "Health and care workers",
                   old = "Older adults"
    ),
    group = factor(
      group,
      levels = c("Older adults", "Health and care workers")
    ),
    dose = recode(dose,
                  cps   = "Complete primary series",
                  boost = "At least one booster dose"
    ),
    dose = factor(
      dose,
      levels = c("Complete primary series", "At least one booster dose")
    ),
    x = if_else(dose == "Complete primary series", tot_cps, tot_boost),
    diff_pp = (y - x) * 100
  ) %>%
  filter(!is.na(x), !is.na(y))

# Create figure
p_scatter <- ggplot(df_parity_2023, aes(x = x, y = y, fill = a_income_group)) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.6) +
  geom_point(shape = 21, color = "black", size = 3, alpha = 1) +
  facet_grid(group ~ dose) +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = c(color_who_cat_base_1, color_who_cat_base_2, 
                               color_who_cat_base_3, color_who_cat_base_4)) +
  coord_equal() +
  theme_master() +
  labs(
    title = "End-2023 coverage parity: total population vs priority populations",
    x = "Vaccination coverage in total population (% of pop.)",
    y = "Vaccination coverage in priority population (% of pop.)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    legend.justification = "left",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.title.x = element_text(vjust = -2, size = 12),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.title.y = element_text(vjust = 4, size = 12),
    axis.text.y = element_text(size = 11, color = "black"),
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

p_summary <- ggplot(df_parity_2023, aes(x = a_income_group, y = diff_pp, fill = a_income_group)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.18, color = "black") +
  facet_grid(group ~ dose) +
  scale_fill_manual(values = c(color_who_cat_base_1, color_who_cat_base_2, 
                               color_who_cat_base_3, color_who_cat_base_4)) +
  theme_master() +
  labs(
    title = "Difference from parity by income group, end-2023",
    x = "Income group",
    y = "Difference in coverage (% points)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(vjust = -2, size = 12),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.title.y = element_text(vjust = 4, size = 12),
    axis.text.y = element_text(size = 11, color = "black"),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )

fig_cov_parity_cps_boost_priority_2023 <- (p_scatter | p_summary) + 
  plot_layout(widths = c(1.4, 1.0)) + 
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(face = "bold", size = 12)
    )
  )

fig_cov_parity_cps_boost_priority_2023

# Export
ggsave(
  path = path_save,
  filename = "fig_2_cov comp_end2023.jpg",
  plot = fig_cov_parity_cps_boost_priority_2023,
  width = 13,
  height = 8,
  dpi = 300
)


# Figure 3 - administration over time, line, all, by reg, ig, covax -------

# Prepare data
dat_cov_time_income <- raw_data_acute_time %>%
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
  mutate(coverage = if_else(a_income_group == "HIC", adm_td_tot / pop_total_hic * 100,
                            if_else(a_income_group == "UMIC", adm_td_tot / pop_total_umic * 100,
                                    if_else(a_income_group == "LMIC", adm_td_tot / pop_total_lmic * 100, 
                                            if_else(a_income_group == "LIC", adm_td_tot / pop_total_lic * 100, NA)))),
         category = "Income group") %>%
  rename(dimension = a_income_group) %>%
  mutate(dimension = factor(dimension, levels = c("LIC",
                                                  "LMIC",
                                                  "UMIC",
                                                  "HIC"))) %>%
  ungroup()

dat_cov_time_covax <- raw_data_acute_time %>%
  select(a_iso,
         month_name,
         a_status_covax,
         adm_tot_td) %>%
  mutate(a_status_covax = case_when(
    a_status_covax == "No" ~ "Non-COVAX",
    TRUE ~ a_status_covax
  )) %>%
  filter(is.na(a_status_covax) == FALSE) %>%
  group_by(month_name, a_status_covax) %>%
  summarize(adm_td_tot = sum(adm_tot_td, na.rm = TRUE)) %>%
  mutate(coverage = if_else(a_status_covax == "AMC", adm_td_tot / pop_total_amc * 100,
                            if_else(a_status_covax == "Self-financing", adm_td_tot / pop_total_sfp * 100,
                                    if_else(a_status_covax == "Non-COVAX", adm_td_tot / pop_total_noncovax * 100, NA))),
         category = "COVAX") %>%
  rename(dimension = a_status_covax) %>%
  ungroup()

dat_cov_time_group <- raw_data_acute_time %>%
  select(a_iso,
         month_name,
         a_status_who,
         a_region_who,
         adm_tot_td) %>%
  filter(a_status_who == "Member State") %>%
  select(-a_status_who) %>%
  group_by(month_name, a_region_who) %>%
  summarize(adm_td_tot = sum(adm_tot_td, na.rm = TRUE)) %>%
  mutate(coverage = if_else(a_region_who == "AFR", adm_td_tot / pop_total_afr * 100,
                            if_else(a_region_who == "AMR", adm_td_tot / pop_total_amr * 100,
                                    if_else(a_region_who == "EMR", adm_td_tot / pop_total_emr * 100, 
                                            if_else(a_region_who == "EUR", adm_td_tot / pop_total_eur * 100, 
                                                    if_else(a_region_who == "SEAR", adm_td_tot / pop_total_sear * 100,
                                                            if_else(a_region_who == "WPR", adm_td_tot / pop_total_wpr * 100, NA)))))),
         category = "Region") %>%
  rename(dimension = a_region_who) %>%
  ungroup() %>%
  rbind(., dat_cov_time_income, dat_cov_time_covax)

# Create figure

base_theme <- theme_master() +
  theme(
    axis.title.x = element_text(vjust = -2, size = 12),
    axis.text.x  = element_text(size = 11, color = "black"),
    axis.title.y = element_text(vjust = 4, size = 11),
    axis.text.y  = element_text(size = 11, color = "black"),
    legend.position = "bottom",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "italic", vjust = 1, size = 11),
    plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
    panel.border = element_rect(
      color = "black",
      fill  = NA,
      linewidth = 0.75   # increase this (default ~0.5)
    )
  )

p_region <- dat_cov_time_group %>%
  filter(category == "Region") %>%
  mutate(dimension = factor(dimension, levels = c("AFR",
                                                  "AMR",
                                                  "EMR",
                                                  "EUR",
                                                  "SEAR",
                                                  "WPR"))) %>%
  ggplot(aes(month_name, coverage, color = dimension)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.2) +
  scale_color_manual(values = c(color_who_afr, color_who_amr, color_who_emr, 
                                color_who_eur, color_who_sear, color_who_wpr)) +
  labs(title = "WHO region", x = NULL, y = NULL, color = NULL) +
  base_theme

p_income <- dat_cov_time_group %>%
  filter(category == "Income group") %>%
  mutate(dimension = factor(dimension, levels = c("LIC",
                                                  "LMIC",
                                                  "UMIC",
                                                  "HIC"))) %>%
  ggplot(aes(month_name, coverage, color = dimension)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.2) +
  scale_color_manual(values = c(color_who_cat_base_1, color_who_cat_base_2, 
                                color_who_cat_base_3, color_who_cat_base_4)) +
  labs(title = "Income group", x = NULL, y = "Doses administered (per 100 population)", color = NULL) +
  base_theme

p_covax <- dat_cov_time_group %>%
  filter(category == "COVAX") %>%
  mutate(dimension = factor(dimension, levels = c("AMC",
                                                  "Self-financing",
                                                  "Non-COVAX"))) %>%
  ggplot(aes(month_name, coverage, color = dimension)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.2) +
  scale_color_manual(values = c(color_who_cat_base_1, color_who_cat_base_2, 
                                color_who_cat_base_3)) +
  labs(title = "COVAX participation category", x = NULL, y = NULL, color = NULL) +
  base_theme

fig <- p_income + p_region + p_covax +
  plot_layout(ncol = 3) + 
  plot_annotation(
    title = "Doses administered in total population, by income group, region, and COVAX participation category, end-2023",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0),
    )
  )
   

fig

# Export
ggsave(
  path = path_save,
  filename = "fig_3_admin_time_all_reg-inc-covax.jpg",
  plot = fig,
  width = 11,
  height = 6,
  dpi = 300
)

# Figure 4 - coverage over time, line, all --------------------------------

# Prepare data
last_pts <- data_cov_time_full %>%
  filter(measure != "Total doses") %>%
  mutate(measure = case_when(measure == "At least one booster" ~ "At least one booster dose",
                             TRUE ~ measure),
         measure = factor(measure, levels = c("At least one dose",
                                              "Complete primary series",
                                              "At least one booster dose")),
         group = factor(group, levels = c("Total population",
                                          "Older adults",
                                          "Health and care workers"))) %>%
  group_by(group, measure) %>%
  arrange(month_name, .by_group = TRUE) %>%
  filter(row_number() == max(which(!is.na(coverage_denom_vary)))) %>%
  ungroup() %>%
  mutate(label = scales::percent(coverage_denom_vary, accuracy = 1))

# Create figure
fig_cov_time_world <- data_cov_time_full %>%
  filter(measure != "Total doses") %>%
  mutate(measure = case_when(measure == "At least one booster" ~ "At least one booster dose",
                             TRUE ~ measure),
         measure = factor(measure, levels = c("At least one dose",
                                              "Complete primary series",
                                              "At least one booster dose")),
         group = factor(group, levels = c("Total population", 
                                          "Older adults",
                                          "Health and care workers"))) %>%
  ggplot(aes(x = month_name, y = coverage_denom_vary, 
             color = measure)) +
  geom_line(size = 1.5) +
  # bubble point
  geom_point(data = last_pts, aes(month_name, coverage_denom_vary),
             size = 3.5, show.legend = FALSE) +
  # boxed (outlined) label, repelled off the line
  geom_label_repel(
    data = last_pts,
    aes(x = month_name, y = coverage_denom_vary, label = label, color = measure),  # map color to measure
    inherit.aes = FALSE,
    size = 3.5, label.size = 0.5,      # box border width
    label.r = unit(0.25, "lines"),      # slight rounded corners
    fill = "white",                     # white background so text pops
    box.padding = 0.5, point.padding = 0.5,
    nudge_x = 0, nudge_y = 0,
    segment.color = "grey60", segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c(color_who_cat_base_1, color_who_cat_base_2, 
                                color_who_cat_base_3)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  theme_test() + 
  labs(title = "COVID-19 vaccination coverage by population group, 2021–2023",
       x = "Date",
       y = "Vaccination coverage (% of population)",
       color = "Legend") + 
  theme(axis.title.x = element_text(vjust = -2, size = 12),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10, color = "black"),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.spacing = unit(0.8, "lines")) +
  facet_wrap(~ group, ncol = 3)

fig_cov_time_world

# Export
ggsave(
  path = path_save,
  filename = "fig_4_cov_time_all.jpg",
  plot = fig_cov_time_world,
  width = 10,
  height = 6,
  dpi = 300
)


# Figure 5 - daily vaccination rate, line, 10/90 --------------------------

test <- data_time_mmm %>%
  select(-max_rate, -min_rate, -perc_25, -perc_75) %>%
  filter(a_income_group != "Other",
         adm_date < as.Date("2024-01-01")) %>%
  mutate(a_income_group = factor(a_income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))) %>%
  ggplot(aes(x = adm_date, y = avg_rate, color = a_income_group)) +
  geom_ribbon(aes(ymin = perc_10, ymax = perc_90, fill = a_income_group), alpha = 0.2) +  # Shaded 10th-90th percentile
  geom_line(size = 1.2) +  # Line for the average rate
  scale_y_continuous(labels = scales::percent_format()) +  # Format y-axis as percentage
  scale_color_manual(values = c(color_who_cat_base_1, color_who_cat_base_2, 
                                color_who_cat_base_3, color_who_cat_base_4)) +
  scale_fill_manual(values = c(color_who_cat_base_1, color_who_cat_base_2, 
                                color_who_cat_base_3, color_who_cat_base_4)) +
  theme_test() +
  labs(title = "Daily vaccination rate by income group, 2021-2023",
       y = "Daily vaccination rate (% of population)",
       x = "Date",
       fill = "Income Group",
       color = "Income Group") +
  theme(axis.title.x = element_text(vjust = -2, size = 12),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10, color = "black"),
        legend.position = "none",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  facet_wrap(~a_income_group, scales = "free_y")  # Separate panels for each income group

test

ggsave(
  path = path_save,
  filename = "fig_5_dvr_line_income_1090.jpg",
  plot = test,
  width = 10,
  height = 6,
  dpi = 300
)


# Figure 6 - 2024 vaccination policy, bar, all -----------------------------

groups_main <- c(
  "Older adults",
  "Health and care workers",
  "Adults with chronic conditions",
  "Pregnant women"
)

policy_levels_main <- c(
  "Periodic revaccination & primary vaccination recommended",
  "Primary vaccination only recommended",
  "(Re-) vaccination not recommended"
)

pal_policy <- c(
  "Periodic revaccination & primary vaccination recommended" = color_who_cat_base_1,  # blue
  "Primary vaccination only recommended" = color_who_cat_base_2,  # green
  "(Re-) vaccination not recommended" = color_who_cat_base_3,  # red/pink
  "No response provided" = color_who_cat_base_4   # grey
)

noresp_label <- "No response provided"
# your raw data uses "4) No response provided" sometimes; we map it to noresp_label
noresp_raw_levels <- c("No response provided", "No response provided")

policy_levels_with_noresp <- c(policy_levels_main, noresp_label)


# palette must be length 4 in the same order as policy_levels_with_noresp
# assumes you already defined these colors; otherwise replace with hex codes
# pal_policy <- c(color_who_cat_base_1, color_who_cat_base_2, color_who_cat_base_3, color_who_cat_base_4)

wrap2 <- function(x) str_wrap(x, width = 18)  # tweak to control line breaks
pct_label_min <- 5                             # hide tiny % labels in Panel B


# 1) FILTER TO END-2024 SNAPSHOT (RP==12), MEMBER STATES, GROUPS OF INTEREST

df0 <- raw_policy_wide %>%
  filter(
    YEAR == 2024,
    REPORTING_PERIOD == 12,
    source == "EJRF",
    a_status_who == "Member State",
    group %in% groups_main
  ) %>%
  mutate(
    group = factor(group, levels = groups_main),
    
    # Income ordering (you have order_inc as "1) HIC" etc; use it if present)
    income = if ("order_inc" %in% names(.)) {
      factor(a_income_group, levels = unique(a_income_group[order(order_inc)]))
    } else {
      factor(a_income_group)
    },
    
    # Harmonize policy label(s) for "no response"
    policy_raw = status_policy,
    policy_raw = if_else(policy_raw %in% noresp_raw_levels, noresp_label, policy_raw),
    
    # Enforce canonical policy categories
    policy_cat = fct_other(
      factor(policy_raw),
      keep = policy_levels_with_noresp,
      other_level = noresp_label
    ),
    policy_cat = factor(policy_cat, levels = policy_levels_with_noresp)
  )

# 2) PANEL A DATA: counts by group (responders only; exclude "No response")

dfA <- df0 %>%
  filter(policy_cat %in% policy_levels_main) %>%
  group_by(group, policy_cat) %>%
  summarise(n = n_distinct(iso), .groups = "drop")

dfA_tot <- dfA %>%
  group_by(group) %>%
  summarise(total = sum(n), .groups = "drop")


# 3) PANEL B DATA: % by income group (includes "No response")

dfB <- df0 %>%
  group_by(group, income, policy_cat) %>%
  summarise(n = n_distinct(iso), .groups = "drop") %>%
  group_by(group, income) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup()

# 4) THEMES

theme_policy <- theme_master() +
  theme(
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title.x = element_text(size = 12),
    strip.background = element_rect(fill = "grey90", color = "black"),
    strip.text = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(0.5,0.5,0.5,0.5, "cm")
  )

# 5) PANEL A PLOT

pA <- ggplot(dfA, aes(x = n, y = group, fill = policy_cat)) +
  geom_col(
    width = 0.75,
    color = "black",
    linewidth = 0.2,
    position = position_stack(reverse = TRUE)
  ) +
  geom_text(
    aes(label = ifelse(n > 0, n, "")),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 3.5,
    fontface = "bold"
  ) +
  geom_text(
    data = dfA_tot,
    aes(x = total, y = group, label = total),
    inherit.aes = FALSE,
    hjust = -0.25,
    size = 3.5,
    fontface = "bold"
  ) +
  scale_y_discrete(labels = wrap2) +
  scale_fill_manual(
    values = pal_policy[1:3],
    breaks = policy_levels_main,
    labels = policy_levels_main,
    drop = FALSE
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.10))) +
  labs(x = "Number of WHO Member States", y = NULL, fill = NULL) +
  theme_policy +
  theme(
    legend.position = "none",
    plot.margin = margin(5.5, 18, 5.5, 5.5), # room for totals
    axis.title.x = element_text(vjust = -2, size = 12)
  ) +
  coord_cartesian(clip = "off")


# 6) PANEL B PLOT

pB <- ggplot(dfB, aes(x = pct, y = income, fill = policy_cat)) +
  geom_col(
    width = 0.75,
    color = "black",
    linewidth = 0.2,
    position = position_stack(reverse = TRUE)
  ) +
  geom_text(
    aes(label = ifelse(pct >= pct_label_min, paste0(round(pct), "%"), "")),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 3.5,
    fontface = "bold"
  ) +
  facet_wrap(~ group, ncol = 2) +
  scale_fill_manual(
    values = pal_policy,
    breaks = policy_levels_with_noresp,
    drop = FALSE
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(x = "Percentage of WHO Member States (within income group)", y = NULL, fill = NULL) +
  theme_policy +
  theme(legend.position = "none",
        panel.spacing = unit(0.6, "lines"),
        axis.title.x = element_text(vjust = -2, size = 12)
        )


# 7) LEGEND-ONLY GROB (ONE, CENTERED)

p_leg <- ggplot(dfB, aes(x = pct, y = income, fill = policy_cat)) +
  geom_col() +
  scale_fill_manual(
    values = pal_policy,
    breaks = policy_levels_with_noresp,
    drop = FALSE
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.justification = "left",
    legend.box.just = "left",
    legend.text = element_text(size = 11)
  )

leg <- cowplot::get_legend(p_leg)

# 8) ASSEMBLE: aligned A/B tags + one legend

panels <- cowplot::plot_grid(
  pA, pB,
  nrow = 1,
  rel_widths = c(1, 1.25),
  align = "hv",
  axis = "tb",
  labels = c("A", "B"),
  label_fontface = "bold",
  label_size = 14,
  label_x = c(0.02, 0.02),
  label_y = c(0.98, 0.98),
  hjust = 0,
  vjust = 1
)

fig_policy_2024_main <- cowplot::plot_grid(
  panels,
  cowplot::ggdraw() +
    cowplot::draw_grob(leg, x = 0.11, hjust = 0),
  ncol = 1,
  rel_heights = c(1, 0.13)
)

fig_policy_2024_main


ggsave(
  path = path_save,
  filename = "fig_6_policy_2024_all.jpg",
  plot = fig_policy_2024_main,
  width = 12,
  height = 7,
  dpi = 300
)


# Table 1 - coverage, end-2023, all ---------------------------------------

ref_date <- as.Date("2023-12-01")

group_levels <- c("Total population", "Older adults", "Health and care workers")

type_labels <- c(
  all    = "All",
  income = "Income group",
  region = "WHO region",
  covax  = "COVAX participation category"
)

# section order (subheaders)
type_order <- c("All", "Income group", "WHO region", "COVAX participation category")

# within-section order
income_order <- c("HIC", "UMIC", "LMIC", "LIC")
region_order <- c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")
covax_order  <- c("AMC", "Self-financing", "No")  # adjust if desired

base <- data_cov_time_all %>%
  filter(
    month_name == ref_date,
    measure %in% c("Complete primary series", "At least one booster"),
    measure != "Total doses"
  ) %>%
  mutate(
    type_title = recode(as.character(type), !!!type_labels),
    type_title = factor(type_title, levels = type_order),
    
    Row = case_when(
      type == "all" ~ "Global",                         # avoid "All" duplicated
      is.na(dimension) | dimension == "NA" ~ "All",
      TRUE ~ as.character(dimension)
    ),
    
    group = factor(group, levels = group_levels),
    
    # ordering key within each section
    Row_order = case_when(
      type == "all"    ~ 1L,
      type == "income" ~ match(Row, income_order),
      type == "region" ~ match(Row, region_order),
      type == "covax"  ~ match(Row, covax_order),
      TRUE ~ NA_integer_
    ),
    Row_order = if_else(is.na(Row_order), 999L, Row_order)
  ) %>%
  group_by(type_title, Row, Row_order, group, measure) %>%
  summarise(
    cov = {
      pop_sum <- sum(pop, na.rm = TRUE)
      if (pop_sum > 0) sum(admin, na.rm = TRUE) / pop_sum else NA_real_
    },
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = measure, values_from = cov) %>%
  mutate(
    cell = paste0(
      if_else(is.na(`Complete primary series`), "—",
              sprintf("%.0f%%", 100 * `Complete primary series`)),
      " / ",
      if_else(is.na(`At least one booster`), "—",
              sprintf("%.0f%%", 100 * `At least one booster`))
    )
  ) %>%
  select(type_title, Row, Row_order, group, cell) %>%
  pivot_wider(names_from = group, values_from = cell) %>%
  arrange(type_title, Row_order, Row)

cols <- c("Total population", "Older adults", "Health and care workers")

make_header_row <- function(title) {
  tibble(
    type_title = title,
    Row = as.character(title),
    Row_order = 0L,
    `Total population` = "",
    `Older adults` = "",
    `Health and care workers` = "",
    is_header = TRUE
  )
}

ft_data <- base %>%
  mutate(is_header = FALSE) %>%
  group_by(type_title) %>%
  group_split(.keep = TRUE) %>%
  lapply(function(df) bind_rows(make_header_row(unique(df$type_title)), df)) %>%
  bind_rows() %>%
  arrange(type_title, Row_order, Row) %>%
  rename(Category = Row) %>%
  select(Category, all_of(cols), is_header)

ft <- flextable(ft_data %>% select(-is_header)) %>%
  bold(part = "header") %>%
  bold(i = ft_data$is_header, bold = TRUE) %>%
  border(
    i = ft_data$is_header,
    border.top = fp_border(color = "black", width = 1)
  ) %>%
  padding(i = !ft_data$is_header, j = 1, padding.left = 10) %>%
  align(align = "left", part = "all") %>%
  valign(valign = "center", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  autofit()

save_as_docx(ft, path = "Table1.docx")

ft


# Table 2 - daily vaccination rate, 2021-2023, income ---------------------

section_order <- c("All", "Income group", "WHO region", "COVAX participation category")

income_order <- c("HIC", "UMIC", "LMIC", "LIC")
region_order <- c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")
covax_order  <- c("AMC", "Self-financing", "No")  # only used if present

base <- dvr_max %>%
  mutate(
    type = as.character(type),
    group = as.character(group),
    
    section = group,          # this is your section label
    Row = type,               # this is the level within the section
    adm_date = as.Date(adm_date),
    section = factor(section, levels = section_order),
    
    Row_order = case_when(
      section == "All" ~ 1L,
      section == "Income group" ~ match(Row, income_order),
      section == "WHO region"   ~ match(Row, region_order),
      section == "COVAX status" ~ match(Row, covax_order),
      TRUE ~ NA_integer_
    ),
    Row_order = if_else(is.na(Row_order), 999L, Row_order),
    
    # format DVR as percent
    value = if_else(is.na(dvr_per), "—", sprintf("%.2f%%", 100 * dvr_per))
  ) %>%
  arrange(section, Row_order, Row) %>%
  transmute(section, Row, adm_date, value)

make_header_row <- function(title) {
  tibble(section = title, Row = as.character(title), adm_date = as.Date(NA), value = "", is_header = TRUE)
}

ft_data <- base %>%
  mutate(is_header = FALSE) %>%
  group_by(section) %>%
  group_split(.keep = TRUE) %>%
  lapply(function(df) bind_rows(make_header_row(unique(df$section)), df)) %>%
  bind_rows() %>%
  mutate(adm_date = if_else(is_header, as.Date(NA), adm_date)) %>%
  rename(Category = Row, `Date of peak` = adm_date, `Peak DVR` = value) %>%
  select(Category, `Date of peak`, `Peak DVR`, is_header)

ft <- flextable(ft_data %>% select(-is_header)) %>%
  bold(part = "header") %>%
  bold(i = ft_data$is_header, bold = TRUE) %>%
  border(i = ft_data$is_header,
         border.top = fp_border(color = "black", width = 1)) %>%
  padding(i = !ft_data$is_header, j = 1, padding.left = 10) %>%
  align(align = "left", part = "all") %>%
  valign(valign = "center", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  autofit()

save_as_docx(ft, path = "Table_dvr_max.docx")
ft


# Table 3 - uptake, 2024, by groups ---------------------------------------

df_long <- raw_data_post_final  # columns: grouping, type, indicator, value, group

# ---- Orders / titles ----
type_order   <- c("all", "income", "region", "policy", "covax")  # covax kept if present
income_order <- c("HIC", "LIC", "LMIC", "UMIC")
region_order <- c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")
policy_order <- c(
  "Periodic revaccination & primary vaccination recommended",
  "Primary vaccination only recommended",
  "(Re-) vaccination not recommended",
  "No response provided"
)

type_titles <- c(
  all    = "All",
  income = "Income group",
  region = "WHO region",
  policy = "Policy category",
  covax  = "COVAX status"
)

# ---- Prep ----
base <- df_long %>%
  mutate(
    type      = str_trim(tolower(as.character(type))),
    grouping  = str_trim(as.character(grouping)),
    group     = str_trim(as.character(group)),
    indicator = str_trim(as.character(indicator)),
    value     = as.numeric(value),
    
    type_title = dplyr::recode(type, !!!type_titles),
    
    row_order = case_when(
      type == "all"    ~ 0L,
      type == "income" ~ match(grouping, income_order),
      type == "region" ~ match(grouping, region_order),
      type == "policy" ~ match(grouping, policy_order),
      TRUE ~ 999L
    ),
    row_order = if_else(is.na(row_order), 999L, row_order)
  ) %>%
  select(type, type_title, grouping, row_order, group, indicator, value)

# (optional) stable indicator ordering: counts -> proportions -> coverage, if present
ind_levels <- base %>%
  distinct(indicator) %>%
  mutate(
    ind_rank = case_when(
      str_detect(tolower(indicator), "^count")      ~ 1L,
      str_detect(tolower(indicator), "^proportion") ~ 2L,
      str_detect(tolower(indicator), "^coverage")   ~ 3L,
      TRUE ~ 99L
    )
  ) %>%
  arrange(ind_rank, indicator) %>%
  pull(indicator)

ind_map <- tibble(
  indicator = ind_levels,
  ind_key   = paste0("i", seq_along(ind_levels))
)

base2 <- base %>%
  left_join(ind_map, by = "indicator") %>%
  mutate(col = paste(group, ind_key, sep = "__")) %>%
  select(type, type_title, grouping, row_order, col, value)

wide <- base2 %>%
  pivot_wider(names_from = col, values_from = value) %>%
  arrange(factor(type, levels = type_order), row_order, grouping)

older_cols <- paste0("Older adults__", ind_map$ind_key)
hcw_cols   <- paste0("Health and care workers__", ind_map$ind_key)

# ensure missing cols exist (robust to missing group/indicator combos)
for (nm in c(older_cols, hcw_cols)) if (!nm %in% names(wide)) wide[[nm]] <- NA_real_

fmt_cell <- function(ind_label, x) {
  if (is.na(x)) return("—")
  lab <- tolower(ind_label)
  
  if (str_detect(lab, "^count")) return(formatC(round(x), format = "f", digits = 0))
  
  # values are proportions (0-1) -> show %
  sprintf("%.0f%%", 100 * x)
}

wide_fmt <- wide
for (k in seq_len(nrow(ind_map))) {
  lab <- ind_map$indicator[k]
  wide_fmt[[older_cols[k]]] <- vapply(wide_fmt[[older_cols[k]]], function(v) fmt_cell(lab, v), character(1))
  wide_fmt[[hcw_cols[k]]]   <- vapply(wide_fmt[[hcw_cols[k]]],   function(v) fmt_cell(lab, v), character(1))
}

make_header_row <- function(title) {
  out <- tibble(type_title = title, grouping = title, row_order = -999L, is_header = TRUE)
  for (nm in c(older_cols, hcw_cols)) out[[nm]] <- ""
  out
}

ft_data <- wide_fmt %>%
  select(type_title, grouping, row_order, all_of(older_cols), all_of(hcw_cols)) %>%
  mutate(is_header = FALSE) %>%
  group_by(type_title) %>%
  group_split(.keep = TRUE) %>%
  lapply(function(df) bind_rows(make_header_row(unique(df$type_title)), df)) %>%
  bind_rows() %>%
  arrange(
    factor(type_title, levels = c("All","Income group","WHO region","Policy category","COVAX status")),
    row_order, grouping
  ) %>%
  rename(Category = grouping)

ft <- flextable(ft_data %>% select(Category, all_of(older_cols), all_of(hcw_cols)))

labels_list <- c(
  list(Category = "Grouping"),
  setNames(as.list(ind_map$indicator), older_cols),
  setNames(as.list(ind_map$indicator), hcw_cols)
)
ft <- do.call(set_header_labels, c(list(ft), labels_list))

ft <- ft %>%
  add_header_row(
    values    = c("", "Older adults", "Health and care workers"),
    colwidths = c(1, length(older_cols), length(hcw_cols))
  ) %>%
  bold(part = "header") %>%
  bold(i = ft_data$is_header, bold = TRUE) %>%
  border(i = ft_data$is_header, border.top = fp_border(color = "black", width = 1)) %>%
  padding(i = !ft_data$is_header, j = 1, padding.left = 10) %>%
  align(align = "left", part = "all") %>%
  valign(valign = "center", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  autofit()

save_as_docx(ft, path = "Table_3_2024 uptake.docx")
ft

# Table 4 - policy, 2024, revaccination intervals -------------------------

tab_revacc_interval_rp12 <- raw_policy_wide %>%
  filter(
    REPORTING_PERIOD == 12,
    is.na(policy_revacc_interval) == FALSE,
    status_policy == "Periodic revaccination & primary vaccination recommended"
  ) %>%
  mutate(
    policy_revacc_interval = ifelse(
      is.na(policy_revacc_interval) | policy_revacc_interval == "",
      "NA",
      as.character(policy_revacc_interval)
    )
  ) %>%
  distinct(iso, group, policy_revacc_interval) %>%
  count(group, policy_revacc_interval, name = "n_countries") %>%
  pivot_wider(
    names_from  = policy_revacc_interval,
    values_from = n_countries,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  arrange(group)

kb <- tab_revacc_interval_rp12 %>%
  mutate(across(everything(), ~replace(., is.na(.), "—"))) %>%
  kbl(
    escape = FALSE,
    align = "l",
    col.names = c("", setdiff(names(tab_revacc_interval_rp12), "group"))
  ) %>%
  kable_styling(full_width = FALSE, position = "left") %>%
  row_spec(0, color = "black", background = "white") %>%
  row_spec(1:nrow(tab_revacc_interval_rp12), color = "black", background = "white")

kb

# Supp. Figure x - reporting over time, all ---------------------------------------

combo <- count_time %>%
  filter(date < as.Date("2024-01-01")) %>%
  mutate(group = factor(group, levels = c("Total population", 
                                          "Older adults", 
                                          "Health and care workers")),
         type = factor(type, levels = c("Complete primary series", 
                                        "Booster"))) %>%
  ggplot(aes(x = date, y = count_month, fill = type)) +
  geom_col() +
  geom_line(aes(y = count_cumul, color = type), linewidth = 1.25) +
  scale_color_manual(values = c(color_who_diverg_1, color_who_diverg_5), 
                     labels = c("Complete primary series only", "Complete primary series & booster")) +
  scale_fill_manual(values = c(color_who_diverg_2, color_who_diverg_4), 
                    labels = c("Complete primary series only", "Complete primary series & booster")) +
  scale_x_date(date_labels = "%b %Y") +
  coord_cartesian(ylim = c(0, 200)) +
  geom_hline(aes(yintercept = 194), linetype = "dashed", color = "black") +
  theme_test() + 
  labs(title = "Member States reporting, cumulative and monthly",
       subtitle = "By population group",
       y = "Count of reporting Member States",
       caption = "Data source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module and WHO regional reporting systems") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  facet_wrap(~group)

combo

ggsave(
  path = path_save,
  filename = "fig_rep_disag_time.jpg",
  plot = combo,
  width = 10,
  height = 6,
  dpi = 300
)


# Supp. Figure x - reporting % over time, by income group ------------------

fig_report_cumul_inc <- count_time_inc %>%
  mutate(population = factor(population, levels = c("Total population",
                                                    "Older adults",
                                                    "Health and care workers")),
         type = factor(type, levels = c("Complete primary series",
                                        "Booster")),
         WBINCOME_LONG_STATUS = factor(WBINCOME_LONG_STATUS, levels = c("High income",
                                                                        "Upper middle income",
                                                                        "Lower middle income",
                                                                        "Low income"))) %>%
  ggplot(aes(x = DATE, y = count_per, color = WBINCOME_LONG_STATUS)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c(color_who_cat_1, 
                                color_who_cat_2, 
                                color_who_cat_3, 
                                color_who_cat_4)) +
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_test() + 
  labs(title = "Percentage of Member States reporting, cumulative",
       subtitle = "By population group, dose type, and income group",
       y = "Percentage of Member States",
       caption = "Data source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module and WHO regional reporting systems") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  facet_grid(type ~ population, scales = "free", space = "free") 
# facet_wrap(~group, ncol = 1) + 
# guides(color = guide_legend(ncol = 6, override.aes = list(size = 3)))

fig_report_cumul_inc

ggsave(
  path = path_save,
  filename = "fig_report_cumul_inc.jpg",
  plot = fig_report_cumul_inc,
  width = 10,
  height = 6,
  dpi = 300
)




# Supp. Figure x - daily vaccination rate over time, income/region, weighted ---------

fig_dvr_time <- dvr %>%
  mutate(group = factor(group, levels = c("WHO region", "Income group")),
         type = factor(type, levels = c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR",
                                        "HIC", "UMIC", "LMIC", "LIC"))) %>%
  ggplot(aes(x = adm_date, y = dvr_per, color = type)) +
  geom_line(size = 1.5) +
  geom_point(data = dvr %>% 
               group_by(type, group) %>% 
               filter(dvr_per == max(dvr_per)), 
             aes(x = adm_date, y = dvr_per, color = type), 
             size = 3, shape = 21, fill = "black", stroke = 0) +
  scale_color_manual(values = c(color_who_afr, color_who_amr, color_who_emr, 
                                color_who_eur, color_who_sear, color_who_wpr,
                                color_who_cat_base_1, color_who_cat_base_2, 
                                color_who_cat_base_3, color_who_cat_base_4)) +
  # scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_test() + 
  labs(title = "Weighted average of daily vaccination rate in total population, by region and income group",
       y = "Percentage of total population",
       x = "Date") +
  theme(axis.title.x = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) + 
  facet_wrap(~group, ncol = 1, as.table = FALSE) + 
  guides(color = guide_legend(nrow = 2, ncol = 6, byrow = TRUE))

fig_dvr_time

ggsave(
  path = path_save,
  filename = "fig_supp_x_dvr_time_group.jpg",
  plot = fig_dvr_time,
  width = 10,
  height = 6,
  dpi = 300
)


# Supp. Figure x - revaccination interval ---------------------------------

df_stack <- raw_policy_wide %>%
  filter(
    REPORTING_PERIOD == 12,
    !is.na(policy_revacc_interval),
    policy_revacc_interval != ""
  ) %>%
  mutate(
    policy_revacc_interval = as.character(policy_revacc_interval)
  ) %>%
  distinct(iso, group, policy_revacc_interval) %>%   # prevent double counting
  count(group, policy_revacc_interval, name = "n") %>%
  group_by(group) %>%
  mutate(total = sum(n)) %>%
  ungroup()

# Optional: order groups by total
df_stack <- df_stack %>%
  mutate(group = fct_reorder(group, total, .fun = unique, .desc = FALSE))

df_totals <- df_stack %>%
  distinct(group, total)

p_interval <- ggplot(
  df_stack,
  aes(x = n, y = group, fill = policy_revacc_interval)
) +
  geom_col(width = 0.72) +
  # labels inside stacks
  geom_text(
    aes(label = ifelse(n > 0, n, "")),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white"
  ) +
  # totals at end of bars
  geom_text(
    data = df_totals,
    aes(x = total, y = group, label = total),
    inherit.aes = FALSE,
    hjust = -0.15,
    size = 3,
    color = "grey20"
  ) +
  scale_fill_manual(values = c(color_who_cat_base_1, color_who_cat_base_2, 
                               color_who_cat_base_3)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Recommended interval of periodic revaccination, by population group (2024)",
    x = "Number of WHO Member States",
    y = NULL,
    fill = "Interval (months)"
  ) +
  theme_master(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.title.x = element_text(vjust = -2, color = "black"),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.justification = "left",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(size = 10, color = "black"),
    plot.margin = margin(10, 40, 10, 10)
  )

p_interval

ggsave(
  path = path_save,
  filename = "fig_supp_x_policy_revacc-interval.jpg",
  plot = p_interval,
  width = 10,
  height = 6,
  dpi = 300
)

# End ---------------------------------------------------------------------

# NOT USED - daily vaccination rate over time, income/region, simple ---------

fig_dvr_simple_time <- dvr_simple %>%
  mutate(group = factor(group, levels = c("WHO region", "Income group")),
         type = factor(type, levels = c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR",
                                        "HIC", "UMIC", "LMIC", "LIC"))) %>%
  ggplot(aes(x = adm_date, y = dvr_per, color = type)) +
  geom_line(size = 1.5) +
  geom_point(data = dvr_simple %>% 
               group_by(type, group) %>% 
               filter(dvr_per == max(dvr_per)), 
             aes(x = adm_date, y = dvr_per, color = type), 
             size = 3, shape = 21, fill = "black", stroke = 0) +
  scale_color_manual(values = c("#6363c0", "#f26829", "#bd53bd", "#008dc9", "#40bf73", "#f4a81d",
                                "#f4a81d", "#f26829", "#bd53bd", "#6363c0")) +
  # scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_test() + 
  labs(title = "Simple average of daily vaccination rate in total population",
       subtitle = "By WHO region and income group",
       y = "Percentage of total population",
       caption = "Data source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module and WHO regional reporting systems") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) + 
  facet_wrap(~group, ncol = 1, as.table = FALSE) + 
  guides(color = guide_legend(nrow = 2, ncol = 6, byrow = TRUE))

fig_dvr_simple_time









