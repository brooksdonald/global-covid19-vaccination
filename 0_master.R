# COVID-19 vaccination analysis
# Author: BROOKS, Donald J.

# Setup -------------------------------------------------------------------

# Clear environment
rm(list = ls())
gc()

# Set working directory
setwd("/Users/brooksdonald/Documents/GitHub/covid_vaccination/")

# Load packages
library("tidyverse")
library("readxl")
library("writexl")
library("lubridate")
library("countrycode")
library("data.table")
library("httr")
library("scales")
library("jsonlite")
library("ggpattern")
library("ggrepel")
library("scales")
library("treemap")
library("ggtext")
library("ggforce")
library("tibble")
library("countrycode")
library("kableExtra")
library("knitr")
library("ordering")
library("ggrepel")
library("shadowtext")


# Process variables --------------------------------------------------------

path_save <- "/Users/brooksdonald/Documents/GitHub/covid_vaccination/output/"


# Extract data ------------------------------------------------------------

# Load base data
raw_data_acute_end <- data.frame(
  read_excel("input/output_master_test.xlsx",
             sheet = "0_base_data"))

raw_data_acute_time <- data.frame(
  read_excel("input/output_master_test.xlsx",
             sheet = "1_stock"))

raw_data_acute_time_daily <- data.frame(
  read_excel("input/output_master_test.xlsx",
             sheet = "1_adm_all_long"))

# Load entity characteristic data
base_details <-
  data.frame(read_excel("input/base_entitydetails.xlsx",
                        sheet = "data"))

# Load reported, disaggregated uptake data
raw_target <- GET('https://xmart-api-public.who.int/WIISE/MT_COV_UPTAKE_TARGETGROUP_LONG')
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


# Load analyses -----------------------------------------------------------

source("1_static values.R")
source("2_reporting analysis.R")
source("3_daily vaccination rate analysis.R")
source("4_coverage analysis.R")
source("9_design specifications.R")


# Figure - reporting over time, all ---------------------------------------

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


# Figure - reporting % over time, by income group -------------------------

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


# NOT USED - Figure - reporting over time, older adults ------------------------------

# combo_old <- sum_old_cps %>%
#   ggplot(aes(x = DATE, y = count_month, 
#              fill = factor(type_cumul, levels = c("Complete primary series", "Boosters")))) +
#   geom_col() +
#   geom_line(aes(y = count_cumul, 
#                 color = factor(type_cumul, levels = c("Complete primary series", "Boosters"))), 
#             size = 1.25) +
#   scale_color_manual(values = c("#D86422", "#000066"), 
#                      labels = c("Complete primary series only", "Complete primary series & boosters")) +
#   scale_fill_manual(values = c("#A2D2F0", "#1E7FB8"), 
#                     labels = c("Complete primary series only", "Complete primary series & boosters")) +
#   scale_x_date(date_labels = "%b %Y") +
#   coord_cartesian(ylim = c(0, 200)) +
#   geom_hline(aes(yintercept = 194), linetype = "dashed", color = "black") +
#   annotate("text", x = as.Date("2023-04-01"), y = 194,
#            label = "194 (Total number of WHO Member States)",
#            color = "black", hjust = 1, vjust = -1, size = 3, fontface = "italic") +
#   theme_test() + 
#   labs(title = "Older adults",
#        y = "Count of reporting Member States ",
#        caption = "Data source: eJRF and WHO regional office reporting systems") + 
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_text(size = 10),
#         axis.title.y = element_text(vjust = 4, size = 11),
#         axis.text.y = element_text(size = 10),
#         legend.title = element_blank(),
#         legend.position = "bottom",
#         legend.justification = "left",
#         plot.title = element_text(face = "bold", vjust = 3, size = 14),
#         plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
#         plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
#         plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))


# NOT USED - Figure - reporting over time, hcw ---------------------------------------

# combo_hcw <- sum_hcw_cps %>%
#   ggplot(aes(x = DATE, y = count_month, 
#              fill = factor(type_cumul, levels = c("Complete primary series", "Boosters")))) +
#   geom_col() +
#   geom_line(aes(y = count_cumul, 
#                 color = factor(type_cumul, levels = c("Complete primary series", "Boosters"))), 
#             size = 1.25) +
#   scale_color_manual(values = c("#D86422", "#000066"), 
#                      labels = c("Complete primary series only", "Complete primary series & boosters")) +
#   scale_fill_manual(values = c("#A2D2F0", "#1E7FB8"), 
#                     labels = c("Complete primary series only", "Complete primary series & boosters")) +
#   scale_x_date(date_labels = "%b %Y") +
#   coord_cartesian(ylim = c(0, 200)) +
#   geom_hline(aes(yintercept = 194), linetype = "dashed", color = "black") +
#   annotate("text", x = as.Date("2023-04-01"), y = 194,
#            label = "194 (Total number of WHO Member States)",
#            color = "black", hjust = 1, vjust = -1, size = 3, fontface = "italic") +
#   theme_test() + 
#   labs(title = "Healthcare workers",
#        y = "Count of reporting Member States ",
#        caption = "Data source: eJRF and WHO regional office reporting systems") + 
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_text(size = 10),
#         axis.title.y = element_text(vjust = 4, size = 11),
#         axis.text.y = element_text(size = 10),
#         legend.title = element_blank(),
#         legend.position = "bottom",
#         legend.justification = "left",
#         plot.title = element_text(face = "bold", vjust = 3, size = 14),
#         plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
#         plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
#         plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))


# Figure - daily vaccination rate over time, income/region, weighted ---------

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
  scale_color_manual(values = c("#6363c0", "#f26829", "#bd53bd", "#008dc9", "#40bf73", "#f4a81d",
                                "#f4a81d", "#f26829", "#bd53bd", "#6363c0")) +
  # scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_test() + 
  labs(title = "Weighted average of daily vaccination rate in total population",
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

fig_dvr_time

ggsave(
  path = path_save,
  filename = "fig_dvr_time_group.jpg",
  plot = fig_dvr_time,
  width = 10,
  height = 6,
  dpi = 300
)


# Figure - daily vaccination rate over time, income/region, simple ---------

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


# Figure - spread daily vaccination rate, 75/25-----------------------------

test<- data_time_mmm %>%
  select(-max_rate, -min_rate, -perc_10, -perc_90) %>%
  filter(a_income_group != "Other") %>%
  mutate(a_income_group = factor(a_income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))) %>%
  ggplot(aes(x = adm_date, y = avg_rate, color = a_income_group)) +
  geom_ribbon(aes(ymin = perc_25, ymax = perc_75, fill = a_income_group), alpha = 0.2) +  # Shaded 10th-90th percentile
  geom_line(size = 1.2) +  # Line for the average rate
  scale_y_continuous(labels = scales::percent_format()) +  # Format y-axis as percentage
  theme_test() +
  labs(title = "4-Week Vaccination Rate Trend by Income Group",
       y = "Vaccination Rate",
       x = "Date",
       fill = "Income Group",
       color = "Income Group",
       caption = "Shaded area represents the 10th-90th percentile range\nData source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  facet_wrap(~a_income_group, scales = "free_y")  # Separate panels for each income group

test


# Figure - spread daily vaccination rate, sd ------------------------------

test_sd<- data_time_mmm %>%
  filter(a_income_group != "Other") %>%
  mutate(a_income_group = factor(a_income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))) %>%
  ggplot(aes(x = adm_date, y = avg_rate, color = a_income_group)) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, fill = a_income_group), alpha = 0.2) +  # Shaded 10th-90th percentile
  geom_line(size = 1.2) +  # Line for the average rate
  scale_y_continuous(labels = scales::percent_format()) +  # Format y-axis as percentage
  theme_test() +
  labs(title = "Daily vaccination rate (4-week rolling average) trend by income group",
       y = "Daily vaccination rate, as percent of total population",
       x = "Date",
       fill = "Income group",
       color = "Income group",
       caption = "Shaded area represents ±1 Standard Deviation\nData source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  facet_wrap(~a_income_group, scales = "free_y")  # Separate panels for each income group

test_sd


# Figure - spread daily vaccination rate, 75/25-----------------------------

test<- data_time_mmm %>%
  select(-max_rate, -min_rate, -perc_25, -perc_75) %>%
  filter(a_income_group != "Other") %>%
  mutate(a_income_group = factor(a_income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))) %>%
  ggplot(aes(x = adm_date, y = avg_rate, color = a_income_group)) +
  geom_ribbon(aes(ymin = perc_10, ymax = perc_90, fill = a_income_group), alpha = 0.2) +  # Shaded 10th-90th percentile
  geom_line(size = 1.2) +  # Line for the average rate
  scale_y_continuous(labels = scales::percent_format()) +  # Format y-axis as percentage
  theme_test() +
  labs(title = "Daily vaccination rate (4-week rolling average) trend by income group",
       y = "Daily vaccination rate, as percent of total population",
       x = "Date",
       fill = "Income group",
       color = "Income group",
       caption = "Shaded area represents the 25th-75th percentile range\nData source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  facet_wrap(~a_income_group, scales = "free_y")  # Separate panels for each income group

test

ggsave(
  path = path_save,
  filename = "fig_dvr_time_inc.jpg",
  plot = test,
  width = 10,
  height = 6,
  dpi = 300
)

# Figure - coverage over time, all ----------------------------------------

last_pts <- data_cov_time_full %>%
  filter(measure != "Total doses") %>%
  mutate(measure = factor(measure, levels = c("At least one dose",
                                         "Complete primary series",
                                         "At least one booster")),
         group = factor(group, levels = c("Total population",
                                          "Older adults",
                                          "Health and care workers"))) %>%
  group_by(group, measure) %>%
  arrange(month_name, .by_group = TRUE) %>%
  filter(row_number() == max(which(!is.na(coverage_denom_rep)))) %>%
  ungroup() %>%
  mutate(label = scales::percent(coverage_denom_rep, accuracy = 1))

fig_cov_time_world <- data_cov_time_full %>%
  filter(measure != "Total doses") %>%
  mutate(measure = factor(measure, levels = c("At least one dose",
                                              "Complete primary series",
                                              "At least one booster")),
         group = factor(group, levels = c("Total population", 
                                          "Older adults",
                                          "Health and care workers"))) %>%
  ggplot(aes(x = month_name, y = coverage_denom_rep, 
             color = measure)) +
  geom_line(size = 1.5) +
  # bubble point
  geom_point(data = last_pts, aes(month_name, coverage_denom_rep),
             size = 3.5, show.legend = FALSE) +
  # boxed (outlined) label, repelled off the line
  geom_label_repel(
    data = last_pts,
    aes(x = month_name, y = coverage_denom_rep, label = label, color = measure),  # map color to measure
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
  scale_color_manual(values = c("#F4a81d", "#f26829", "#bd53bd")) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  theme_test() + 
  labs(title = "Global COVID-19 vaccination coverage",
       subtitle = "By population group",
       y = "Percentage of population",
       caption = "Data source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module and WHO regional reporting systems",
       color = "Legend") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  facet_wrap(~ group, ncol = 3)

fig_cov_time_world

ggsave(
  path = path_save,
  filename = "fig_cov_time_world.jpg",
  plot = fig_cov_time_world,
  width = 10,
  height = 6,
  dpi = 300
)


# Figure - coverage over time, total --------------------------------------

fig_cov_time_total_world <- data_cov_time_full %>%
  filter(group == "Total population",
         measure != "Total doses") %>%
  ggplot(aes(x = month_name, y = coverage, 
             color = measure)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("#FFBB30", "#5200AE", "#C8D65B")) +
  # scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_test() + 
  labs(title = "Coverage in total population, global",
       y = "Percentage of total population",
       caption = "Data source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module",
       color = "Legend") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))

fig_cov_time_total_world


# Figure - coverage over time, old ---------------------------------------------

fig_cov_time_old_world <- dat_cov_time %>%
  filter(group == "Older adults",
         measure != "Total doses") %>%
  ggplot(aes(x = month_name, y = coverage, 
             color = measure)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("#FFBB30", "#5200AE", "#C8D65B")) +
  # scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_test() + 
  labs(title = "Coverage in older adults, global",
       y = "Percentage of older adults",
       caption = "Data source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module",
       color = "Legend") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))

fig_cov_time_old_world


# Figure - coverage over time, hcw ----------------------------------------

fig_cov_time_hcw_world <- dat_cov_time %>%
  filter(group == "Health and care workers",
         measure != "Total doses") %>%
  ggplot(aes(x = month_name, y = coverage, 
             color = measure)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("#FFBB30", "#5200AE", "#C8D65B")) +
  # scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_test() + 
  labs(title = "Coverage in health and care workers, global",
       y = "Percentage of health and care workers",
       caption = "Data source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module",
       color = "Legend") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))

fig_cov_time_hcw_world


# Figure - uptake over time per pop, by income group and region-------------

dat_cov_time_income <- raw_data_time %>%
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
  ungroup()

dat_cov_time_covax <- raw_data_time %>%
  select(a_iso,
         month_name,
         a_status_covax,
         adm_tot_td) %>%
  filter(is.na(a_status_covax) == FALSE) %>%
  group_by(month_name, a_status_covax) %>%
  summarize(adm_td_tot = sum(adm_tot_td, na.rm = TRUE)) %>%
  mutate(coverage = if_else(a_status_covax == "AMC", adm_td_tot / pop_total_amc * 100,
                            if_else(a_status_covax == "Self-financing", adm_td_tot / pop_total_sfp * 100,
                                    if_else(a_status_covax == "No", adm_td_tot / pop_total_noncovax * 100, NA))),
         category = "COVAX") %>%
  rename(dimension = a_status_covax) %>%
  ungroup()

dat_cov_time_group <- raw_data_time %>%
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

fig_cov_time_total_group <- dat_cov_time_group %>%
  mutate(category = factor(category, levels = c("Income group",
                                              "Region",
                                              "COVAX"))) %>%
  ggplot(aes(x = month_name, y = coverage, 
             color = dimension)) +
  geom_line(size = 1.5) +
  geom_point(size = 2.5) +
  # scale_color_manual(values = c("#FFBB30", "#5200AE", "#C8D65B")) +
  # scale_x_date(date_labels = "%b %Y") +
  # scale_y_continuous(labels = scales::percent_format()) +
  theme_test() + 
  labs(title = "Uptake in total population, global, by income group, by region",
       y = "Total doses administered per 100 population",
       caption = "Data source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module",
       color = "Legend") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left",
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  facet_wrap(~ category, ncol = 3)

fig_cov_time_total_group

ggsave(
  path = path_save,
  filename = "fig_upt_time_total_reg_inc.jpg",
  plot = fig_cov_time_total_group,
  width = 10,
  height = 6,
  dpi = 300
)

# Figure - final coverage, violins, income group ---------------------------

dat_cov_end_inc <- raw_data_end %>%
  select(a_iso,
         a_status_who,
         a_income_group,
         cov_total_a1d,
         cov_total_fv,
         cov_total_booster,
         cov_60p_a1d_adjust,
         cov_60p_fv,
         cov_60p_booster,
         cov_hcw_a1d_adjust,
         cov_hcw_fv,
         cov_hcw_booster) %>%
  filter(a_status_who == "Member State",
         a_income_group != "Other") %>%
  select(-a_status_who) %>%
  gather(key = "measure", value = "coverage", -a_iso, -a_income_group) %>%
  mutate(group = if_else(str_detect(measure, "cov_total"), "Total population", 
                         if_else(str_detect(measure, "cov_60p"), "Older adults",
                                 if_else(str_detect(measure, "cov_hcw"), "Health and care workers", NA))),
         measure = if_else(str_detect(measure, "a1d"), "At least one dose", 
                           if_else(str_detect(measure, "fv"), "Complete primary series", 
                                   if_else(str_detect(measure, "booster"), "At least one booster", NA))))

# Faceted violins
fig_cov_end_inc <- dat_cov_end_inc %>%
  mutate(a_income_group = factor(a_income_group, 
                                 levels = c("LIC", 
                                            "LMIC", 
                                            "UMIC", 
                                            "HIC")),
         measure = factor(measure, levels = c("At least one dose", 
                                              "Complete primary series", 
                                              "At least one booster")),
         group = factor(group, levels = c("Total population", 
                                          "Older adults", 
                                          "Health and care workers"))) %>%
  ggplot(aes(x = a_income_group, 
             y = coverage, 
             fill = a_income_group)) + 
  geom_violin(trim = TRUE) + 
  geom_boxplot(width = 0.1, color = "black") +
  # geom_jitter(width = 0.05, size = 3, alpha = 0.7) +
  facet_grid(group ~ measure, scales = "free", space = "free") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = c("#FFBB30", "#0A71D5", "#C8D65B", "#00AE8F")) +
  theme_bw() +
  labs(title = "COVID-19 vaccination coverage, by measure, by group, by income group",
       y = "Percentage of population") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(vjust = 4, size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

fig_cov_end_inc

ggsave(
  path = path_save,
  filename = "fig_cov_end_inc.jpg",
  plot = fig_cov_end_inc,
  width = 10,
  height = 8,
  dpi = 300
)



# Table - final coverage values -------------------------------------------

tab_final_cov <- data_cov_time_all %>%
  filter(month_name == as.Date("2023-12-01")) %>%
  select(-month_name) %>%
  filter(measure != "Total doses")

disp <- tab_final_cov %>%
  filter(measure %in% c("Complete primary series", "At least one booster")) %>%
  mutate(
    type = factor(type, levels = c("all","income","region","covax")),
    type_title = dplyr::recode(as.character(type),
                               all   = "All",
                               income= "Income group",
                               region= "WHO Region",
                               covax = "COVAX Status"),
    group = factor(group, levels = c("Total population","Older adults","Health and care workers")),
    Row = ifelse(is.na(dimension) | dimension == "NA", "All", as.character(dimension))
  ) %>%
  group_by(type, type_title, Row, group, measure) %>%
  summarise(cov = dplyr::first(coverage_denom_rep), .groups = "drop") %>%
  pivot_wider(names_from = measure, values_from = cov) %>%
  mutate(
    cps   = `Complete primary series`,
    boost = `At least one booster`,
    cell  = paste0(
      ifelse(is.na(cps),   "—", sprintf("%.1f%%", cps   * 100)), " / ",
      ifelse(is.na(boost), "—", sprintf("%.1f%%", boost * 100))
    )
  ) %>%
  select(type, type_title, Row, group, cell) %>%
  pivot_wider(names_from = group, values_from = cell) %>%
  arrange(type, Row)

# section spans
runs   <- rle(as.character(disp$type_title))
starts <- cumsum(c(1, head(runs$lengths, -1)))
ends   <- cumsum(runs$lengths)

# build the kable
kb <- disp %>%
  select(-type) %>%
  select(type_title, Row, `Total population`, `Older adults`, `Health and care workers`) %>%
  mutate(across(everything(), ~replace(., is.na(.), "—"))) %>%
  select(-type_title) %>%
  kbl(escape = FALSE,
      align = "l",
      col.names = c("", "Total population", "Older adults", "Health and care workers")) %>%
  kable_styling(full_width = FALSE, position = "left")

for (i in seq_along(runs$values)) {
  kb <- kb %>% group_rows(runs$values[i], starts[i], ends[i])
}

kb <- kb %>%
  row_spec(0, color = "black", background = "white") %>%
  row_spec(1:nrow(disp), color = "black", background = "white")

save_kable(kb, file = "table.png")

# Recycling ---------------------------------------------------------------

# Grouped violins
fig_cov_end_inc <- dat_cov_end_inc %>%
  mutate(a_income_group = factor(a_income_group, 
                                 levels = c("LIC", 
                                            "LMIC", 
                                            "UMIC", 
                                            "HIC")),
         measure = factor(measure, levels = c("cov_total_a1d", 
                                              "cov_total_fv", 
                                              "cov_total_booster"))) %>%
  ggplot(aes(x = measure, 
             y = coverage, 
             fill = a_income_group)) + 
  geom_violin(position = position_dodge(0.7), scale = "width", width = 0.7, trim = TRUE) + 
  geom_boxplot(position = position_dodge(0.7), width = 0.1, color = "black") +
  # geom_jitter(position = position_dodge(0.7), width = 0.05, size = 3, alpha = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("#FFBB30", "#0A71D5", "#C8D65B", "#00AE8F")) +
  theme_bw() +
  labs(title = "Coverage, by income group",
       y = "Coverage with a complete primary series (% of total pop.)") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(),
        axis.title.y = element_text(vjust = 4, size = 12),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", vjust = 3, size = 14),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

fig_cov_end_inc


# End ---------------------------------------------------------------------