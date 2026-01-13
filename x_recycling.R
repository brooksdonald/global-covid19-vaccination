
# Recycling ---------------------------------------------------------------


# Grouped violins ---------------------------------------------------------

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


