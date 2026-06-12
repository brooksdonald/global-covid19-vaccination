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

lib <- c("tidyverse", "readxl", "writexl", "lubridate", "countrycode",
         "data.table", "httr", "scales", "jsonlite", "ggpattern", "ggrepel", 
         "scales", "treemap", "ggtext", "ggforce", "tibble", "kableExtra", 
         "knitr", "ordering", "shadowtext", "forcats", "flextable", "officer", 
         "rlang", "cowplot","patchwork" )

lapply(lib, library, character.only = TRUE)


# Process & design variables ----------------------------------------------

path_save <- "/Users/brooksdonald/Documents/GitHub/covid_vaccination/output/"

# Theme-related settings
theme_master <- theme_test() +
  theme(
    plot.title = element_text(face = "bold", vjust = 3, size = 14),
    axis.title.x = element_text(vjust = -2, size = 12),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.title.y = element_text(vjust = 4, size = 12),
    axis.text.y = element_text(size = 10, color = "black"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )

levels_standard <- c("HIC", "UMIC", "LMIC", "LIC",
                     "AFR", "AMR", "EMR", "EUR", "SEAR", "WPR",
                     "AMC", "Self-financing", "Non-COVAX", "World")

levels_income <- c("HIC", "UMIC", "LMIC", "LIC")

levels_region <- c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")

levels_groups_short <- c("Total population", "Older adults", 
                         "Health and care workers")

levels_indicator <- c("At least one dose", "Complete primary series",
                      "At least one booster dose")

# Color-related settings
source("a_design specifications.R")

palette_3 <- c(color_who_cat_base_1, color_who_cat_base_2,
                   color_who_cat_base_3)

palette_4 <- c(color_who_cat_base_1, color_who_cat_base_2, 
                    color_who_cat_base_3, color_who_cat_base_4)

palette_5 <- c(color_who_cat_base_1, color_who_cat_base_2,
               color_who_cat_base_3, color_who_cat_base_4,
               color_who_cat_base_5)

palette_region <- c(color_who_afr, color_who_amr, color_who_emr, color_who_eur,
                    color_who_sear, color_who_wpr)


# Extract data ------------------------------------------------------------

# Load entity & population details
raw_entity_details <- data.frame(
  read.csv("input/data_export_WIISE_REF_COUNTRIES.csv"))

raw_adhoc <- data.frame(
  read_excel("input/base_adhoc.xlsx", 
             sheet = "data"))

raw_population <- data.frame(
  read.csv("input/data_export_WIISE_MT_REF_POPULATIONS.csv"))

# Load 2021-2023 uptake data
raw_data_acute_time <- data.frame(
  read_excel("input/output_master_test.xlsx",
             sheet = "1_stock")) 

raw_data_acute_time_daily <- data.frame(
  read_excel("input/output_master_test.xlsx",
             sheet = "1_adm_all_long"))

# Load 2024 policy data
raw_policy_wide <- data.frame(
  read_excel("input/output_new.xlsx",
             sheet = "policy_country_wide"))

# Load 2024 uptake data
raw_data_post_final <- data.frame(
  read_excel("input/output_new.xlsx",
             sheet = "uptake_grouping_wide_red"))

# Load uncleaned risk group uptake data
raw_target <- GET('https://xmart-api-public.who.int/WIISE/MT_COV_UPTAKE_TARGETGROUP_LONG',
                  add_headers(`Cache-Control` = "no-cache"),
                  config = config(fresh_connect = TRUE))
raw_target_text <- content(raw_target, "text")
raw_target_json <- fromJSON(raw_target_text, flatten = TRUE)
base_target <- as.data.frame(raw_target_json)
names(base_target) <- substring(names(base_target), 7)

# Load uncleaned total pop. uptake data
raw_weekly <- GET('https://xmart-api-public.who.int/WIISE/COV_UPTAKE_WEEKLY')
raw_weekly_text <- content(raw_weekly, "text")
raw_weekly_json <- fromJSON(raw_weekly_text, flatten = TRUE)
base_total <- as.data.frame(raw_weekly_json)
names(base_total) <- substring(names(base_total), 7)


# Load analyses -----------------------------------------------------------

source("1_entity_pop.R")
source("2_data cleaning.R")
source("3_static values.R")
source("4_acute coverage analysis.R")
source("5_acute daily vaccination rate analysis.R")
source("6_routine policy analysis.R")
source("7_routine coverage analysis.R")
source("8_reporting analysis.R")


# Figure 1 - end-2023 coverage, violins, income group -----------------------

fig_1_cov_end2023_inc <- data_fig_1_cov_end2023_inc %>% 
  ggplot(aes(x = income_group, y = coverage, fill = income_group)) + 
  geom_violin(trim = TRUE, na.rm = TRUE) + 
  geom_boxplot(width = 0.1, color = "black") +
  facet_grid(group ~ measure, scales = "fixed", space = "free") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = palette_4) +
  labs(title = "COVID-19 vaccination coverage by dose, population group, and income group, end-2023",
       x = "Income group",
       y = "Vaccination coverage (% of population)") + 
  theme_master +
  theme(legend.position = "none")

fig_1_cov_end2023_inc

# Export
ggsave(
  path = path_save,
  filename = "fig_1_cov_end2023_inc.jpg",
  plot = fig_1_cov_end2023_inc,
  width = 10,
  height = 7,
  dpi = 300
)


# Figure 2 - end-2023 coverage comp., scatter & violin, total vs. old / hcw ----

# Create panel a - scatter plots
fig_2_comp_scatter <- data_fig_2_cov_cps_boost_priority_end2023 %>%
  ggplot(aes(x = x, y = y, fill = income_group)) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.6) +
  geom_point(shape = 21, color = "black", size = 3, alpha = 1) +
  facet_grid(group ~ dose) +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = palette_4) +
  coord_equal() +
  labs(title = "End-2023 coverage parity: total population vs priority populations",
       x = "Vaccination coverage in total population (% of pop.)",
       y = "Vaccination coverage in priority population (% of pop.)") +
  theme_master +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

# Create panel b - violin plots
fig_2_comp_summary <- data_fig_2_cov_cps_boost_priority_end2023 %>%
  ggplot(aes(x = income_group, y = diff_pp, fill = income_group)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.18, color = "black") +
  facet_grid(group ~ dose) +
  scale_fill_manual(values = palette_4) +
  labs(title = "Difference from parity by income group, end-2023",
       x = "Income group",
       y = "Difference in coverage (% points)") +
  theme_master +
  theme(legend.position = "none")

# Combine panels
fig_2_cov_cps_boost_priority_end2023 <- (fig_2_comp_scatter | fig_2_comp_summary) + 
  plot_layout(widths = c(1.4, 1.0)) + 
  plot_annotation(tag_levels = "A",
                  theme = theme(plot.tag = 
                                  element_text(face = "bold", size = 12)))

fig_2_cov_cps_boost_priority_end2023

# Export
ggsave(
  path = path_save,
  filename = "fig_2_cov_cps_boost_priority_end2023.jpg",
  plot = fig_2_cov_cps_boost_priority_end2023,
  width = 13,
  height = 7,
  dpi = 300
)


# Figure 3 - administration over time, line, all, by reg, ig, covax -------

# Modify theme for figure
theme_fig3 <- theme_master +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size = 10),
        plot.title = element_text(face = "italic", vjust = 3, size = 12),
        panel.border = element_rect(color = "black", fill = NA, 
                                    linewidth = 0.75))

# Create component 1 - by WHO region
fig_3_comp_region <- data_fig_3_admin_time_all %>%
  filter(category == "Region") %>%
  ggplot(aes(month_name, coverage, color = dimension)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.2) +
  coord_cartesian(ylim = c(0, 235)) +
  scale_color_manual(values = c(palette_region, color_who_seq_1)) +
  labs(title = "WHO region", x = NULL, y = NULL, color = NULL) +
  theme_fig3

# Create component 2 - by income group
fig_3_comp_income <- data_fig_3_admin_time_all %>%
  filter(category == "Income group") %>%
  ggplot(aes(month_name, coverage, color = dimension)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.2) +
  coord_cartesian(ylim = c(0, 235)) +
  scale_color_manual(values = c(palette_4, color_who_seq_1)) +
  labs(title = "Income group", x = NULL, y = "Doses administered (per 100 population)", color = NULL) +
  theme_fig3

# Create component 3 - by COVAX participation category
fig_3_comp_covax <- data_fig_3_admin_time_all %>%
  filter(category == "COVAX") %>%
  ggplot(aes(month_name, coverage, color = dimension)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.2) +
  coord_cartesian(ylim = c(0, 235)) +
  scale_color_manual(values = c(color_who_cat_base_1, color_who_cat_base_2, color_who_seq_1)) +
  labs(title = "COVAX participation category", x = NULL, y = NULL, color = NULL) +
  theme_fig3

# Combine
fig_3_admin_time_all <- fig_3_comp_income + fig_3_comp_region + fig_3_comp_covax +
  plot_layout(ncol = 3) + 
  plot_annotation(
    title = "Doses administered in total population, by income group, region, and COVAX participation category, end-2023",
    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0),
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
    )

fig_3_admin_time_all

# Export
ggsave(
  path = path_save,
  filename = "fig_3_admin_time_all.jpg",
  plot = fig_3_admin_time_all,
  width = 11,
  height = 7,
  dpi = 300
)


# Figure 4 - weighted coverage over time, line, all -----------------------

# Create panel a - weighted average
fig_cov_time_global_compA <- data_fig_4_cov_time_world %>%
  ggplot(aes(x = month_name, y = coverage, color = measure)) +
  geom_line(size = 1.5) +
  geom_point(
    data = data_fig_4_cov_time_world_last,
    aes(month_name, coverage),
    size = 3.5, show.legend = FALSE
  ) +
  geom_label_repel(
    data = data_fig_4_cov_time_world_last,
    aes(x = month_name, y = coverage, label = label, color = measure),
    inherit.aes = FALSE,
    size = 3.5, label.size = 0.5,
    label.r = unit(0.25, "lines"),
    fill = "white",
    box.padding = 0.5, point.padding = 0.5,
    nudge_x = 0, nudge_y = 0,
    segment.color = "grey60", segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  facet_wrap(~group, ncol = 3) +
  scale_color_manual(values = palette_3) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(
    title = "Population-weighted coverage across reporting countries",
    x = NULL,
    y = "Vaccination coverage (% of pop.)",
    color = NULL
  ) +
  theme_master +
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    panel.spacing = unit(0.8, "lines"),
    plot.title = element_text(face = "italic", vjust = 2, size = 12),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )

fig_cov_time_global_compB <- data_fig_5_cov_time_mediqr_compA_fix %>%
  ggplot(aes(x = month_name, y = med_cov, linetype = indicator,
             fill = indicator, color = indicator)) +
  geom_ribbon(aes(ymin = perc_25, ymax = perc_75), alpha = 0.2) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c(color_who_cat_base_1, color_who_cat_base_4)) +
  scale_fill_manual(values = c(color_who_cat_base_1, color_who_cat_base_4)) +
  scale_linetype_manual(values = c(
    "Complete primary series" = "solid",
    "At least one booster dose" = "22"
  )) +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  facet_wrap(~group, ncol = 3) +
  labs(
    title = "Median (IQR) across reporting countries",
    x = "Date",
    y = "Vaccination coverage (% of pop.)",
    color = NULL, fill = NULL, linetype = NULL
  ) +
  theme_master +
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    legend.title = element_blank(),
    plot.title = element_text(face = "italic", vjust = 3, size = 12),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.spacing = unit(0.8, "lines")
  )

# Combine (stack)
fig_4_cov_time_global_bygroup <- (fig_cov_time_global_compA / fig_cov_time_global_compB) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Global COVID-19 vaccination coverage by population group, 2021-2023",
    tag_levels = "A",
    theme = theme(plot.title = element_text(face = "bold", hjust = 0, size = 14))
  ) &
  theme(
    plot.tag = element_text(face = "bold", size = 16, color = "black"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )

fig_4_cov_time_global_bygroup

# Export
ggsave(
  path = path_save,
  filename = "fig_4_cov_time_global_bygroup.jpg",
  plot = fig_4_cov_time_global_bygroup,
  width = 11,
  height = 8,
  dpi = 300
)

# Figure 5 - daily vaccination rate, line, 10/90 --------------------------

# Create panel a - global
fig_6_dvr_summary_compA <- data_fig_5_dvr_summary_compA %>%
  ggplot(aes(x = adm_date, y = avg_rate)) +
  geom_ribbon(aes(ymin = perc_10, ymax = perc_90), alpha = 0.2, color = "black") +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = percent_format(),
                     limits = c(0, 0.012),
                     expand = c(0, 0)) + 
  labs(
    title = "Global",
    y = "Daily vaccination rate (% of population)",
    x = "Date"
  ) +
  theme_master +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "italic", vjust = 3, size = 12),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )

# Create panel b - by income group
fig_6_dvr_summary_compB <- data_fig_5_dvr_summary_compB %>%
  ggplot(aes(x = adm_date, y = avg_rate, color = dimension)) +
  geom_ribbon(aes(ymin = perc_10, ymax = perc_90, fill = dimension), alpha = 0.2) +
  geom_line(size = 1.2) +  # Line for the average rate
  facet_wrap(~ dimension) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.012),
                     expand = c(0, 0)) +
  scale_color_manual(values = palette_4) +
  scale_fill_manual(values = palette_4) +
  labs(title = "Income group",
       x = "Date",
       fill = "Income Group",
       color = "Income Group") +
  theme_master +
  theme(axis.title.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "italic", vjust = 3, size = 12),
        plot.margin = margin(0, 0, 0, 0, "cm")) 

fig_6_dvr_summary <- (fig_6_dvr_summary_compA | fig_6_dvr_summary_compB) +
  plot_layout(widths = c(1, 2)) +
  plot_annotation(
    title = "Daily COVID-19 vaccination rate, global and by income group (2021–2023)",
    tag_levels = "A",
    theme = theme(plot.title = element_text(face = "bold", hjust = 0, size = 14))
    ) &
  theme(plot.tag = element_text(face="bold", size = 16, color="black"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.spacing = unit(0.35, "cm")
        ) 

fig_6_dvr_summary

# Export
ggsave(
  path = path_save,
  filename = "fig_6_dvr_summary.jpg",
  plot = fig_6_dvr_summary,
  width = 10,
  height = 7,
  dpi = 300
)


# Figure 6 - vaccination policy, 2024, bar, all ---------------------------

# Modify theme for figure
theme_fig7 <- theme_master +
  theme(
    strip.background = element_rect(fill = "grey90", color = "black"),
    strip.text = element_text(size = 11),
    panel.grid.major.y = element_blank(),
  )

# Create panel a - policy count, global
fig_7_policy_2024_all_compA <- data_fig_7_policy_2024_all_compA %>%
  ggplot(aes(x = n, y = group, fill = status_policy)) +
  geom_col(width = 0.75, color = "black", linewidth = 0.2,
           position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = ifelse(n > 0, n, "")), 
            position = position_stack(vjust = 0.5, reverse = TRUE), size = 3.5,
            fontface = "bold") +
  geom_text(data = data_fig_7_policy_2024_all_compA_tot, 
            aes(x = total, y = group, label = total), 
            inherit.aes = FALSE, hjust = -0.25, size = 3.5, fontface = "bold") +
  scale_y_discrete(labels = wrap2, limits = rev) +
  scale_fill_manual(values = palette_3, breaks = levels_policy_a,
                    labels = levels_policy_a, drop = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.10))) +
  labs(x = "Number of WHO Member States", y = NULL, fill = NULL) +
  theme_fig7 +
  theme(
    legend.position = "none",
    plot.margin = margin(5.5, 18, 5.5, 5.5), # room for totals
    axis.title.x = element_text(vjust = -2, size = 12)
  ) +
  coord_cartesian(clip = "off")

# Create panel b - policy count (100%), by income group
fig_7_policy_2024_all_compB <- data_fig_7_policy_2024_all_compB %>%
  ggplot(aes(x = pct, y = income_group, fill = status_policy)) +
  geom_col(width = 0.75, color = "black", linewidth = 0.2,
           position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = ifelse(pct >= pct_label_min, paste0(round(pct), "%"), "")),
            position = position_stack(vjust = 0.5, reverse = TRUE), size = 3.5,
            fontface = "bold") +
  facet_wrap(~ group, ncol = 2) +
  scale_fill_manual(values = palette_4, breaks = levels_policy_b, drop = FALSE) +
  scale_x_continuous(limits = c(0, 100),expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Percentage of WHO Member States (within income group)", 
       y = NULL, fill = NULL) +
  theme_fig7 +
  theme(legend.position = "none",
        panel.spacing = unit(0.6, "lines"),
        axis.title.x = element_text(vjust = -2, size = 12)
        )

# Create main legend
fig_7_policy_2024_all_compleg <- data_fig_7_policy_2024_all_compB %>%
  ggplot(aes(x = pct, y = income_group, fill = status_policy)) +
  geom_col() +
  scale_fill_manual(
    values = palette_4,
    breaks = levels_policy_b,
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

leg <- cowplot::get_legend(fig_7_policy_2024_all_compleg)

# Assemble full plot
panels <- cowplot::plot_grid(
  fig_7_policy_2024_all_compA, fig_7_policy_2024_all_compB,
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

fig_7_policy_2024_all <- cowplot::plot_grid(
  panels,
  cowplot::ggdraw() +
    cowplot::draw_grob(leg, x = 0.11, hjust = 0),
  ncol = 1,
  rel_heights = c(1, 0.13)
) +
  plot_annotation(
    title = "National COVID-19 vaccination policy recommendations, 2024",
    theme = theme(plot.title = element_text(face = "bold", hjust = 0, size = 14))
  ) &
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

fig_7_policy_2024_all

# Export
ggsave(
  path = path_save,
  filename = "fig_7_policy_2024_all.jpg",
  plot = fig_7_policy_2024_all,
  width = 11,
  height = 7,
  dpi = 300
)


# Table 1 - coverage, end-2023, all ---------------------------------------

ref_date <- as.Date("2023-12-01")

type_labels <- c(
  all    = "All",
  income = "Income group",
  region = "WHO region",
  covax  = "COVAX participation category"
)

# section order (subheaders)
type_order <- c("All", "Income group", "WHO region", "COVAX participation category")

# within-section order
covax_order <- c("AMC", "Self-financing", "No")  # adjust if desired

# short labels for subcolumns
measure_labels <- c(
  "Complete primary series"   = "CPS",
  "At least one booster dose" = "Booster"
)

# desired group order
group_order <- levels_groups_short

# desired output column order
value_cols <- as.vector(
  unlist(lapply(group_order, function(g) paste(g, c("CPS", "Booster"), sep = "___")))
)

base <- data_cov_time_all %>%
  filter(
    month_name == ref_date,
    measure %in% c("Complete primary series", "At least one booster dose")
  ) %>%
  mutate(
    type_title = recode(as.character(type), !!!type_labels),
    type_title = factor(type_title, levels = type_order),
    
    Row = case_when(
      type == "all" ~ "Global",
      is.na(dimension) | dimension == "NA" ~ "All",
      TRUE ~ as.character(dimension)
    ),
    
    group = factor(group, levels = levels_groups_short),
    
    measure_short = case_when(
      measure == "Complete primary series" ~ "CPS",
      measure == "At least one booster dose" ~ "Booster"
    ),
    
    Row_order = case_when(
      type == "all"    ~ 1L,
      type == "income" ~ match(Row, levels_income),
      type == "region" ~ match(Row, levels_region),
      type == "covax"  ~ match(Row, covax_order),
      TRUE ~ NA_integer_
    ),
    Row_order = if_else(is.na(Row_order), 999L, Row_order)
  ) %>%
  group_by(type_title, Row, Row_order, group, measure_short) %>%
  summarise(
    cov = {
      pop_sum <- sum(pop, na.rm = TRUE)
      if (pop_sum > 0) sum(admin, na.rm = TRUE) / pop_sum else NA_real_
    },
    .groups = "drop"
  ) %>%
  mutate(
    value = if_else(is.na(cov), "—", sprintf("%.0f%%", 100 * cov))
  ) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(type_title, Row, Row_order),
    names_from = c(group, measure_short),
    names_sep = "___",
    values_from = value
  ) %>%
  arrange(type_title, Row_order, Row)

# add any missing columns so structure is stable
for (nm in value_cols) {
  if (!nm %in% names(base)) base[[nm]] <- ""
}

base <- base %>%
  select(type_title, Row, Row_order, all_of(value_cols))

make_header_row <- function(title, value_cols) {
  tibble(
    type_title = title,
    Row = as.character(title),
    Row_order = 0L,
    !!!setNames(as.list(rep("", length(value_cols))), value_cols),
    is_header = TRUE
  )
}

ft_data <- base %>%
  mutate(is_header = FALSE) %>%
  group_by(type_title) %>%
  group_split(.keep = TRUE) %>%
  lapply(function(df) bind_rows(make_header_row(unique(df$type_title), value_cols), df)) %>%
  bind_rows() %>%
  arrange(type_title, Row_order, Row) %>%
  rename(Category = Row) %>%
  select(Category, all_of(value_cols), is_header)

# header mapping for flextable
header_map <- tibble(
  col_keys = c("Category", value_cols),
  header1  = c("Category", rep(group_order, each = 2)),
  header2  = c("Category", rep(c("CPS", "Booster"), times = length(group_order)))
)

ft <- flextable(ft_data %>% select(-is_header), col_keys = header_map$col_keys) %>%
  set_header_df(mapping = header_map, key = "col_keys") %>%
  merge_h(part = "header") %>%
  merge_v(part = "header", j = "Category") %>%
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

ft

# Export
save_as_docx(ft, path = "output/Table_1_end-2023_coverage.docx")


# Table 2 - daily vaccination rate, 2021-2023, income ---------------------

section_order <- c("All", "Income group", "WHO region", "COVAX participation category")

covax_order  <- c("AMC", "Self-financing", "No")  # only used if present

base <- dvr_max %>%
  mutate(
    section = type,          # this is your section label
    Row = dimension,               # this is the level within the section
    adm_date = as.Date(adm_date),
    section = factor(section, levels = section_order),
    
    Row_order = case_when(
      section == "All" ~ 1L,
      section == "Income group" ~ match(Row, levels_income),
      section == "WHO region"   ~ match(Row, levels_region),
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

ft

# Export
save_as_docx(ft, path = "output/Table_2_dvr_max.docx")


# Table 3 - uptake, 2024, by groups ---------------------------------------

type_order   <- c("all", "income", "region", "policy")

type_titles <- c(
  all    = "All",
  income = "Income group",
  region = "WHO region",
  policy = "Policy category",
  covax  = "COVAX status"
)

base <- data_tab_3_coverage_2024 %>%
  mutate(
    type      = str_trim(tolower(as.character(type))),
    grouping  = str_trim(as.character(grouping)),
    group     = str_trim(as.character(group)),
    indicator = str_trim(as.character(indicator)),
    value     = as.numeric(value),
    
    type_title = dplyr::recode(type, !!!type_titles),
    
    row_order = case_when(
      type == "all"    ~ 0L,
      type == "income" ~ match(grouping, levels_income),
      type == "region" ~ match(grouping, levels_region),
      type == "policy" ~ match(grouping, levels_policy_b),
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

tab_3_coverage_2024 <- flextable(ft_data %>% select(Category, all_of(older_cols), all_of(hcw_cols)))

labels_list <- c(
  list(Category = "Grouping"),
  setNames(as.list(ind_map$indicator), older_cols),
  setNames(as.list(ind_map$indicator), hcw_cols)
)
tab_3_coverage_2024 <- do.call(set_header_labels, c(list(tab_3_coverage_2024), labels_list))

tab_3_coverage_2024 <- tab_3_coverage_2024 %>%
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

tab_3_coverage_2024

# Export
save_as_docx(tab_3_coverage_2024, path = "output/Table_3_2024 coverage.docx")



# Supp. Figure 1 - reporting over time, all ---------------------------------------

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


# Supp. Figure 2 - reporting % over time, by income group ------------------

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
  scale_color_manual(values = c(color_who_cat_base_1, 
                                color_who_cat_base_2, 
                                color_who_cat_base_3, 
                                color_who_cat_base_4)) +
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


# Supp. Figure 3 - xxx ----------------------------------------------------


# Supp. Figure 4 - daily vaccination rate, income / region, weighted ------

dvr_time_increg_colors <- c("Global" = color_who_seq_1,
                            "AFR" = color_who_afr,
                            "AMR" = color_who_amr,
                            "EMR" = color_who_emr,
                            "EUR" = color_who_eur,
                            "SEAR" = color_who_sear,
                            "WPR" = color_who_wpr,
                            "HIC" = color_who_cat_base_1,
                            "UMIC" = color_who_cat_base_2,
                            "LMIC" = color_who_cat_base_3,
                            "LIC" = color_who_cat_base_4)

plot_dvr_time_increg_panel <- function(panel_type) {
  panel_data <- data_fig_supp_4_dvr_time_increg %>%
    filter(type == panel_type)
  
  panel_points <- dvr_weighted %>%
    filter(type == panel_type) %>%
    group_by(type, dimension) %>%
    filter(dvr_per == max(dvr_per)) %>%
    ungroup()
  
  panel_dimensions <- levels(droplevels(panel_data$dimension))
  
  panel_data %>%
    ggplot(aes(x = adm_date, y = dvr_per, color = dimension)) +
    geom_line(size = 1.5) +
    geom_point(data = panel_points,
               aes(x = adm_date, y = dvr_per, color = dimension), 
               size = 3, shape = 21, fill = "black", stroke = 0) +
    scale_color_manual(values = dvr_time_increg_colors,
                       breaks = panel_dimensions,
                       limits = panel_dimensions,
                       drop = FALSE) +
    # scale_x_date(date_labels = "%b %Y") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 0.01)) +
    theme_test() + 
    labs(title = panel_type,
         y = if (panel_type == "Income group") {
           "Percentage of total population"
         } else {
           NULL
         },
         x = if (panel_type == "All") {
           "Date"
         } else {
           NULL
         }) +
    theme(axis.title.x = element_text(size = 11),
          axis.text.x = element_text(size = 10),
          axis.title.y = element_text(vjust = 4, size = 11),
          axis.text.y = element_text(size = 10),
          legend.title = element_blank(),
          legend.position = c(0.98, 0.98),
          legend.justification = c("right", "top"),
          legend.background = element_rect(fill = alpha("white", 0.85),
                                           color = "grey80"),
          plot.title = element_text(face = "bold", vjust = 3, size = 11),
          plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
          plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
          plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) + 
    guides(color = guide_legend(nrow = 1, byrow = TRUE))
}

fig_supp_4_dvr_time_increg <- c("WHO region", "Income group", "All") %>%
  map(plot_dvr_time_increg_panel) %>%
  wrap_plots(ncol = 1) +
  plot_annotation(
    title = "Daily vaccination rate in total population (weighted average), global, region and income group",
    theme = theme(plot.title = element_text(face = "bold", vjust = 3, size = 14))
  )

fig_supp_4_dvr_time_increg

# Export
ggsave(
  path = path_save,
  filename = "fig_supp_4_dvr_time_increg.jpg",
  plot = fig_supp_4_dvr_time_increg,
  width = 10,
  height = 7,
  dpi = 300
)


# Supp. Figure 5 - coverage over time, line, group / income ---------------

fig_supp_5_cov_time_weight_all <- data_fig_supp_5_cov_time_weight_all %>%
  ggplot(aes(x = month_name, y = coverage, color = measure)) +
  geom_line(size = 1.5) +
  
  # bubble point
  geom_point(data = data_fig_supp_5_cov_time_weight_all_last, 
             aes(month_name, coverage),
             size = 3.5, show.legend = FALSE) +

  # boxed (outlined) label, repelled off the line
  geom_label_repel(
    data = data_fig_supp_5_cov_time_weight_all_last,
    aes(x = month_name, y = coverage, label = label, color = measure),  # map color to measure
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
  facet_grid(dimension ~ group) +
  scale_color_manual(values = palette_3) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(title = "COVID-19 vaccination coverage by population group, 2021–2023",
       x = "Date",
       y = "Vaccination coverage (% of population)",
       color = "Legend") + 
  theme_master + 
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        panel.spacing = unit(0.55, "lines"))

fig_supp_5_cov_time_weight_all

# Export
ggsave(
  path = path_save,
  filename = "fig_supp_5_cov_time_weight_all.jpg",
  plot = fig_supp_5_cov_time_weight_all,
  width = 9,
  height = 11,
  dpi = 300
)


# Supp. Figure 6 - summary coverage over time, line, group / income ------

fig_supp_6_cov_mediqr_all <- data_fig_supp_6_cov_mediqr_all %>%
  ggplot(aes(x = month_name, y = med_cov, linetype = indicator, 
             fill = indicator, color = indicator)) +
  geom_ribbon(aes(ymin = perc_25, ymax = perc_75, fill = indicator, 
                  color = indicator), alpha = 0.2) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c(color_who_cat_base_1, color_who_cat_base_4)) +
  scale_fill_manual(values = c(color_who_cat_base_1, color_who_cat_base_4)) +
  scale_linetype_manual(values = c("Complete primary series" = "solid", 
                                   "At least one booster dose" = "22")) + 
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "COVID-19 coverage (median, IQR), by population group and income group, 2021–2023",
       x = "Date",
       y = "Vaccination coverage (% of population)") +
  theme_master +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.justification = "left") +
  facet_grid(group ~ dimension)

fig_supp_6_cov_mediqr_all

# Export
ggsave(
  path = path_save,
  filename = "fig_supp_6_cov_mediqr_all.jpg",
  plot = fig_supp_6_cov_mediqr_all,
  width = 10,
  height = 7,
  dpi = 300
)


# Supp. Figure 7 - revaccination interval, bar, all -----------------------

fig_supp_7_policy_revaccint <- data_fig_supp_7_policy_revaccint %>%
  ggplot(aes(x = n, y = group, fill = policy_revacc_interval)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = ifelse(n > 0, n, "")), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "white") +
  # totals at end of bars
  geom_text(data = data_fig_supp_7_policy_revaccint_totals,
            aes(x = total, y = group, label = total),
            inherit.aes = FALSE, hjust = -0.15, size = 3, color = "grey20") +
  scale_fill_manual(values = palette_3, limits = rev) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 20), 
                   limits = rev) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Recommended interval of periodic revaccination, by population group (2024)",
    x = "Number of WHO Member States",
    y = NULL,
    fill = "Interval (months)"
  ) +
  theme_master +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.justification = "left",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(size = 10, color = "black")
  )

fig_supp_7_policy_revaccint

# Export
ggsave(
  path = path_save,
  filename = "fig_supp_7_policy_revaccint.jpg",
  plot = fig_supp_7_policy_revaccint,
  width = 10,
  height = 6,
  dpi = 300
)


# End ---------------------------------------------------------------------

# Create figure
fig_4_cov_time_world <- data_fig_4_cov_time_world %>%
  ggplot(aes(x = month_name, y = coverage, color = measure)) +
  geom_line(size = 1.5) +
  
  # bubble point
  geom_point(data = data_fig_4_cov_time_world_last, aes(month_name, coverage),
             size = 3.5, show.legend = FALSE) +
  
  # boxed (outlined) label, repelled off the line
  geom_label_repel(
    data = data_fig_4_cov_time_world_last,
    aes(x = month_name, y = coverage, label = label, color = measure),
    inherit.aes = FALSE,
    size = 3.5, label.size = 0.5,
    label.r = unit(0.25, "lines"),
    fill = "white",
    box.padding = 0.5, point.padding = 0.5,
    nudge_x = 0, nudge_y = 0,
    segment.color = "grey60", segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  facet_wrap(~group, ncol = 3) +
  scale_color_manual(values = palette_3) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(title = "COVID-19 vaccination coverage by population group, 2021–2023",
       x = "Date",
       y = "Vaccination coverage (% of population)",
       color = "Legend") + 
  theme_master + 
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        panel.spacing = unit(0.8, "lines"))

fig_4_cov_time_world

# Export
ggsave(
  path = path_save,
  filename = "fig_4_cov_time_world.jpg",
  plot = fig_4_cov_time_world,
  width = 10,
  height = 6,
  dpi = 300
)


# Figure 5 - summary coverage over time, line, global / income ------------

# Create panel a - global
fig_5_cov_time_mediqr_compA <- data_fig_5_cov_time_mediqr_compA %>%
  ggplot(aes(x = month_name, y = med_cov, linetype = indicator,
             fill = indicator, color = indicator)) +
  geom_ribbon(aes(ymin = perc_25, ymax = perc_75), alpha = 0.2) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c(color_who_cat_base_1, color_who_cat_base_4)) +
  scale_fill_manual(values = c(color_who_cat_base_1, color_who_cat_base_4)) +
  scale_linetype_manual(values = c(
    "Complete primary series" = "solid",
    "At least one booster dose" = "22"
  )) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.85),
                     expand = c(0, 0)) +
  labs(title = "Global",
       x = "Date",
       y = "Vaccination coverage (% of population)") +
  theme_master +
  theme(
    legend.position = "bottom",
    legend.title= element_blank(),
    plot.title = element_text(face = "italic", vjust = 3, size = 12),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )

# Create panel b - by income group
fig_5_cov_time_mediqr_compB <- data_fig_5_cov_time_mediqr_compB %>%
  ggplot(aes(x = month_name, y = med_cov, linetype = indicator,
             fill = indicator, color = indicator)) +
  geom_ribbon(aes(ymin = perc_25, ymax = perc_75), alpha = 0.2) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c(color_who_cat_base_1, color_who_cat_base_4)) +
  scale_fill_manual(values = c(color_who_cat_base_1, color_who_cat_base_4)) +
  scale_linetype_manual(values = c(
    "Complete primary series" = "solid",
    "At least one booster dose" = "22"
  )) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.85),
                     expand = c(0, 0)) +
  labs(title = "Income group",
       x = "Date",
       y = NULL) +
  theme_master +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "italic", vjust = 3, size = 12),
    plot.margin = margin(0, 0, 0, 0, "cm")
  ) +
  facet_wrap(~dimension, ncol = 2)

fig_5_cov_time_mediqr <- (fig_5_cov_time_mediqr_compA | fig_5_cov_time_mediqr_compB) +
  plot_layout(widths = c(1, 2)) +
  plot_annotation(
    title = "Total population COVID-19 coverage (median, IQR), global and by income group, 2021–2023",
    tag_levels = "A",
    theme = theme(plot.title = element_text(face = "bold", hjust = 0, size = 14))
  ) &
  theme(
    plot.tag = element_text(face="bold", size = 16, color="black"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.4, "cm")
  )

fig_5_cov_time_mediqr

# Export
ggsave(
  path = path_save,
  filename = "fig_5_cov_time_mediqr.jpg",
  plot = fig_5_cov_time_mediqr,
  width = 11,
  height = 7,
  dpi = 300
)
