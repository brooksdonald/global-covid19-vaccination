# Figure 1 - end-2023 coverage, violins, income group -----------------------

fig_1_cov_end2023_inc <- data_fig_1_cov_end2023_inc %>% 
  ggplot(aes(x = income_group, y = coverage, fill = income_group)) + 
  geom_violin(trim = TRUE, na.rm = TRUE) + 
  geom_boxplot(width = 0.1, color = "black") +
  facet_grid(group ~ measure, scales = "fixed", space = "free") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = pal_income) +
  labs(x = label_axis_income,
       y = label_axis_coverage) + 
  theme_master +
  theme(legend.position = "none")

fig_1_cov_end2023_inc

# Export
export_figure(
  filename = "fig_1_cov_end2023_inc.jpg",
  plot = fig_1_cov_end2023_inc,
  size = "wide_tall"
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
  scale_fill_manual(values = pal_income) +
  coord_equal() +
  labs(title = "Coverage parity",
       x = "Vaccination coverage in total population (% of population)",
       y = "Vaccination coverage in priority population (% of population)") +
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
  scale_fill_manual(values = pal_income) +
  labs(title = "Difference from parity",
       x = label_axis_income,
       y = "Difference in coverage (% points)") +
  theme_master +
  theme(legend.position = "none")

# Combine panels
fig_2_cov_cps_boost_priority_end2023 <- (fig_2_comp_scatter | fig_2_comp_summary) + 
  plot_layout(widths = c(1.4, 1.0)) + 
  plot_annotation(tag_levels = "a",
                  theme = panel_tag_theme)

fig_2_cov_cps_boost_priority_end2023

# Export
export_figure(
  filename = "fig_2_cov_cps_boost_priority_end2023.jpg",
  plot = fig_2_cov_cps_boost_priority_end2023,
  size = "extra_wide"
)


# Figure 3 - administration over time, line, all, by reg, ig, covax -------

data_fig_3_admin_time_plot <- data_fig_3_admin_time_all %>%
  filter(!is.na(dimension), !is.na(coverage))

data_fig_3_admin_time_labels <- data_fig_3_admin_time_plot %>%
  arrange(category, dimension, month_name) %>%
  group_by(category, dimension) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Modify theme for figure
theme_fig3 <- theme_master +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", vjust = 2, size = 11),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
    plot.margin = margin(0.2, 1.2, 0.2, 0.2, "cm")
  )

# Create component 1 - by WHO region
fig_3_comp_region <- data_fig_3_admin_time_plot %>%
  filter(category == "Region") %>%
  ggplot(aes(month_name, coverage, color = dimension)) +
  geom_line(linewidth = 1.2) +
  geom_label_repel(
    data = data_fig_3_admin_time_labels %>% filter(category == "Region"),
    aes(label = dimension),
    size = 3.5, label.size = 0.5,
    label.r = unit(0.25, "lines"),
    fill = "white",
    box.padding = 0.5, point.padding = 0.5,
    direction = "y",
    hjust = 0,
    nudge_x = 8,
    nudge_y = 0,
    segment.color = "grey60", segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  coord_cartesian(ylim = c(0, 235), clip = "off") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(0.02, 0.03))) +
  scale_color_manual(values = pal_region_world) +
  labs(title = "WHO region", x = NULL, y = NULL, color = NULL) +
  theme_fig3

# Create component 2 - by income group
fig_3_comp_income <- data_fig_3_admin_time_plot %>%
  filter(category == "Income group") %>%
  ggplot(aes(month_name, coverage, color = dimension)) +
  geom_line(linewidth = 1.2) +
  geom_label_repel(
    data = data_fig_3_admin_time_labels %>% filter(category == "Income group"),
    aes(label = dimension),
    size = 3.5, label.size = 0.5,
    label.r = unit(0.25, "lines"),
    fill = "white",
    box.padding = 0.5, point.padding = 0.5,
    direction = "y",
    hjust = 0,
    nudge_x = 8,
    nudge_y = 0,
    segment.color = "grey60", segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  coord_cartesian(ylim = c(0, 235), clip = "off") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(0.02, 0.03))) +
  scale_color_manual(values = pal_income_world) +
  labs(title = "Income group", x = NULL, y = label_axis_admin_per100, color = NULL) +
  theme_fig3

# Create component 3 - by COVAX participation category
fig_3_comp_covax <- data_fig_3_admin_time_plot %>%
  filter(category == "COVAX") %>%
  ggplot(aes(month_name, coverage, color = dimension)) +
  geom_line(linewidth = 1.2) +
  geom_label_repel(
    data = data_fig_3_admin_time_labels %>% filter(category == "COVAX"),
    aes(label = dimension),
    size = 3.5, label.size = 0.5,
    label.r = unit(0.25, "lines"),
    fill = "white",
    box.padding = 0.5, point.padding = 0.5,
    direction = "y",
    hjust = 0,
    nudge_x = 8,
    nudge_y = 0,
    segment.color = "grey60", segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  coord_cartesian(ylim = c(0, 235), clip = "off") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(0.02, 0.03))) +
  scale_color_manual(values = pal_covax) +
  labs(title = "COVAX participation", x = NULL, y = NULL, color = NULL) +
  theme_fig3

# Combine
fig_3_admin_time_all <- fig_3_comp_income + fig_3_comp_region + fig_3_comp_covax +
  plot_layout(ncol = 3) + 
  plot_annotation(
    title = "Doses administered, 2021-2023",
    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0),
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
    )

fig_3_admin_time_all

# Export
export_figure(
  filename = "fig_3_admin_time_all.jpg",
  plot = fig_3_admin_time_all,
  size = "double_wide"
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
  scale_color_manual(values = pal_dose) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(
    title = "Population-weighted coverage",
    x = NULL,
    y = label_axis_coverage,
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
  scale_colour_manual(values = pal_dose_compare) +
  scale_fill_manual(values = pal_dose_compare) +
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
    title = "Median (IQR)",
    x = label_axis_date,
    y = label_axis_coverage,
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
    title = "Vaccination coverage over time",
    tag_levels = "a",
    theme = theme(plot.title = element_text(face = "bold", hjust = 0, size = 14))
  ) &
  (panel_tag_theme +
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  ))

fig_4_cov_time_global_bygroup

# Export
export_figure(
  filename = "fig_4_cov_time_global_bygroup.jpg",
  plot = fig_4_cov_time_global_bygroup,
  size = "double_tall"
)

# Figure 5 - daily vaccination rate, line, 10/90 --------------------------

# Create panel a - global
fig_6_dvr_summary_compA <- data_fig_5_dvr_summary_compA %>%
  ggplot(aes(x = adm_date, y = aggregate_rate)) +
  geom_ribbon(aes(ymin = perc_10, ymax = perc_90), alpha = 0.2, color = "black") +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = percent_format(),
                     limits = c(0, 0.012),
                     expand = c(0, 0)) + 
  labs(
    title = "Global",
    y = "Daily vaccination rate (% of population)",
    x = label_axis_date
  ) +
  theme_master +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "italic", vjust = 3, size = 12),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )

# Create panel b - by income group
fig_6_dvr_summary_compB <- data_fig_5_dvr_summary_compB %>%
  ggplot(aes(x = adm_date, y = aggregate_rate, color = dimension)) +
  geom_ribbon(aes(ymin = perc_10, ymax = perc_90, fill = dimension), alpha = 0.2) +
  geom_line(size = 1.2) +
  facet_wrap(~ dimension) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.012),
                     expand = c(0, 0)) +
  scale_color_manual(values = pal_income) +
  scale_fill_manual(values = pal_income) +
  labs(title = "Income group",
       x = label_axis_date,
       fill = label_legend_income,
       color = label_legend_income) +
  theme_master +
  theme(axis.title.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "italic", vjust = 3, size = 12),
        plot.margin = margin(0, 0, 0, 0, "cm")) 

fig_6_dvr_summary <- (fig_6_dvr_summary_compA | fig_6_dvr_summary_compB) +
  plot_layout(widths = c(1, 2)) +
  plot_annotation(
    title = "Daily vaccination rate",
    tag_levels = "a",
    theme = theme(plot.title = element_text(face = "bold", hjust = 0, size = 14))
    ) &
  (panel_tag_theme +
  theme(
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.spacing = unit(0.35, "cm")
        ))

fig_6_dvr_summary

# Export
export_figure(
  filename = "fig_6_dvr_summary.jpg",
  plot = fig_6_dvr_summary,
  size = "wide_tall"
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
  scale_fill_manual(values = pal_policy, breaks = levels_policy_a,
                    labels = levels_policy_a, drop = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.10))) +
  labs(x = label_axis_member_states_n, y = NULL, fill = NULL) +
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
  scale_fill_manual(values = pal_policy, breaks = levels_policy_b, drop = FALSE) +
  scale_x_continuous(limits = c(0, 100),expand = expansion(mult = c(0, 0.02))) +
  labs(x = paste0(label_axis_member_states_pct, " (within income group)"), 
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
    values = pal_policy,
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
  labels = c("a", "b"),
  label_fontface = "bold",
  label_size = 10,
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
    title = "Vaccination policy recommendations, 2024",
    theme = theme(plot.title = element_text(face = "bold", hjust = 0, size = 14))
  ) &
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

fig_7_policy_2024_all

# Export
export_figure(
  filename = "fig_7_policy_2024_all.jpg",
  plot = fig_7_policy_2024_all,
  size = "double_wide"
)


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
  scale_color_manual(values = pal_reporting, 
                     labels = c("Complete primary series only",
                                "Complete primary series and booster")) +
  scale_fill_manual(values = pal_reporting_fill, 
                    labels = c("Complete primary series only",
                               "Complete primary series and booster")) +
  scale_x_date(date_labels = "%b %Y") +
  coord_cartesian(ylim = c(0, 200)) +
  geom_hline(aes(yintercept = 194), linetype = "dashed", color = "black") +
  theme_master + 
  labs(title = "Member States reporting",
       subtitle = "By population group",
       y = "Count of reporting Member States",
       caption = label_source_reporting) + 
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

export_figure(
  filename = "fig_rep_disag_time.jpg",
  plot = combo,
  size = "wide"
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
  scale_color_manual(values = pal_income_long) +
  scale_x_date(date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_master + 
  labs(title = "Reporting by income group",
       subtitle = "By population group, dose type, and income group",
       y = "Percentage of Member States",
       caption = label_source_reporting) + 
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

export_figure(
  filename = "fig_report_cumul_inc.jpg",
  plot = fig_report_cumul_inc,
  size = "wide"
)


# Supp. Figure 3 - xxx ----------------------------------------------------


# Supp. Figure 4 - daily vaccination rate, income / region, weighted ------

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
    scale_color_manual(values = pal_dvr_all,
                       breaks = panel_dimensions,
                       limits = panel_dimensions,
                       drop = FALSE) +
    # scale_x_date(date_labels = "%b %Y") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 0.01)) +
    theme_master + 
    labs(title = panel_type,
         y = if (panel_type == "Income group") {
           "Percentage of total population"
         } else {
           NULL
         },
         x = if (panel_type == "All") {
           label_axis_date
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
    title = "Daily vaccination rate by subgroup",
    theme = theme(plot.title = element_text(face = "bold", vjust = 3, size = 14))
  )

fig_supp_4_dvr_time_increg

# Export
export_figure(
  filename = "fig_supp_4_dvr_time_increg.jpg",
  plot = fig_supp_4_dvr_time_increg,
  size = "wide_tall"
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
  scale_color_manual(values = pal_dose) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(title = "Vaccination coverage over time",
       x = label_axis_date,
       y = label_axis_coverage,
       color = NULL) + 
  theme_master + 
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        panel.spacing = unit(0.55, "lines"))

fig_supp_5_cov_time_weight_all

# Export
export_figure(
  filename = "fig_supp_5_cov_time_weight_all.jpg",
  plot = fig_supp_5_cov_time_weight_all,
  size = "tall"
)


# Supp. Figure 6 - summary coverage over time, line, group / income ------

fig_supp_6_cov_mediqr_all <- data_fig_supp_6_cov_mediqr_all %>%
  ggplot(aes(x = month_name, y = med_cov, linetype = indicator, 
             fill = indicator, color = indicator)) +
  geom_ribbon(aes(ymin = perc_25, ymax = perc_75, fill = indicator, 
                  color = indicator), alpha = 0.2) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = pal_dose_compare) +
  scale_fill_manual(values = pal_dose_compare) +
  scale_linetype_manual(values = c("Complete primary series" = "solid", 
                                   "At least one booster dose" = "22")) + 
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Median coverage over time",
       x = label_axis_date,
       y = label_axis_coverage) +
  theme_master +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.justification = "left") +
  facet_grid(group ~ dimension)

fig_supp_6_cov_mediqr_all

# Export
export_figure(
  filename = "fig_supp_6_cov_mediqr_all.jpg",
  plot = fig_supp_6_cov_mediqr_all,
  size = "wide_tall"
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
  scale_fill_manual(values = unname(pal_dose), limits = rev) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 20), 
                   limits = rev) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Recommended revaccination interval, 2024",
    x = label_axis_member_states_n,
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
export_figure(
  filename = "fig_supp_7_policy_revaccint.jpg",
  plot = fig_supp_7_policy_revaccint,
  size = "wide"
)


# Additional figure - weighted coverage over time, global -----------------

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
  scale_color_manual(values = pal_dose) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(title = "Vaccination coverage over time",
       x = label_axis_date,
       y = label_axis_coverage,
       color = NULL) + 
  theme_master + 
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        panel.spacing = unit(0.8, "lines"))

fig_4_cov_time_world

# Export
export_figure(
  filename = "fig_4_cov_time_world.jpg",
  plot = fig_4_cov_time_world,
  size = "wide"
)


# Figure 5 - summary coverage over time, line, global / income ------------

# Create panel a - global
fig_5_cov_time_mediqr_compA <- data_fig_5_cov_time_mediqr_compA_fix %>%
  filter(group == "Total population") %>%
  ggplot(aes(x = month_name, y = med_cov, linetype = indicator,
             fill = indicator, color = indicator)) +
  geom_ribbon(aes(ymin = perc_25, ymax = perc_75), alpha = 0.2) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = pal_dose_compare) +
  scale_fill_manual(values = pal_dose_compare) +
  scale_linetype_manual(values = c(
    "Complete primary series" = "solid",
    "At least one booster dose" = "22"
  )) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.85),
                     expand = c(0, 0)) +
  labs(title = "Global",
       x = label_axis_date,
       y = label_axis_coverage) +
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
  scale_colour_manual(values = pal_dose_compare) +
  scale_fill_manual(values = pal_dose_compare) +
  scale_linetype_manual(values = c(
    "Complete primary series" = "solid",
    "At least one booster dose" = "22"
  )) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.85),
                     expand = c(0, 0)) +
  labs(title = "Income group",
       x = label_axis_date,
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
    title = "Total-population median coverage",
    tag_levels = "a",
    theme = theme(plot.title = element_text(face = "bold", hjust = 0, size = 14))
  ) &
  (panel_tag_theme +
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.4, "cm")
  ))

fig_5_cov_time_mediqr

# Export
export_figure(
  filename = "fig_5_cov_time_mediqr.jpg",
  plot = fig_5_cov_time_mediqr,
  size = "double_wide"
)
