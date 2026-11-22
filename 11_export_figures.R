# Set the random seed for reproducible figure label placement.
label_repel_seed <- 20240629

# Figure 1 - end-2023 coverage and priority-group parity -------------------

fig_1_cov_end2023_inc <- data_fig_1_cov_end2023_inc %>%
  ggplot(aes(x = income_group, y = coverage, fill = income_group)) +
  geom_violin(trim = TRUE, na.rm = TRUE) +
  geom_boxplot(width = 0.1, color = "black", na.rm = TRUE) +
  facet_grid(group ~ measure, scales = "fixed", space = "free") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = pal_income) +
  labs(title = "Coverage by income group",
       x = label_axis_income,
       y = label_axis_coverage) +
  theme_master +
  theme(legend.position = "none")

# Create panel b - scatter plots
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

# Create panel c - violin plots
fig_2_comp_summary <- data_fig_2_cov_cps_boost_priority_end2023 %>%
  ggplot(aes(x = income_group, y = diff_pp, fill = income_group)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.18, color = "black", na.rm = TRUE) +
  facet_grid(group ~ dose) +
  scale_fill_manual(values = pal_income) +
  labs(title = "Difference from parity",
       x = label_axis_income,
       y = "Difference in coverage (% points)") +
  theme_master +
  theme(legend.position = "none")

# Combine panels
fig_1_end2023_coverage <- (fig_1_cov_end2023_inc /
                             (fig_2_comp_scatter | fig_2_comp_summary)) +
  plot_layout(heights = c(1, 1.05)) +
  plot_annotation(tag_levels = "a",
                  theme = panel_tag_theme) &
  panel_tag_theme

fig_1_end2023_coverage

# Export
export_figure(
  filename = "fig_1_end2023_coverage",
  plot = fig_1_end2023_coverage,
  width = 11,
  height = 12
)


# Figure 2 - emergency-period coverage trends ------------------------------

data_fig_3_admin_time_plot <- data_fig_3_admin_time_all %>%
  filter(!is.na(dimension), !is.na(coverage))

data_fig_3_admin_time_labels <- data_fig_3_admin_time_plot %>%
  arrange(category, dimension, month_name) %>%
  group_by(category, dimension) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    label = paste0(dimension, ": ", scales::number(coverage, accuracy = 1)),
    label_nudge_y = case_when(
      coverage >= 205 ~ -8,
      coverage <= 65 ~ 8,
      TRUE ~ 7
    )
  )

data_fig_3_admin_time_labels_region <- data_fig_3_admin_time_labels %>%
  filter(category == "Region")

data_fig_3_admin_time_labels_income <- data_fig_3_admin_time_labels %>%
  filter(category == "Income group")

data_fig_3_admin_time_labels_covax <- data_fig_3_admin_time_labels %>%
  filter(category == "COVAX",
         dimension != "Non-COVAX")

# Modify theme for figure
theme_fig3 <- theme_master +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", vjust = 2, size = 11),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25),
    plot.margin = margin(0.2, 0.3, 0.2, 0.1, "cm")
  )

# Create component 1 - by WHO region
fig_3_comp_region <- data_fig_3_admin_time_plot %>%
  filter(category == "Region") %>%
  ggplot(aes(month_name, coverage, color = dimension)) +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  geom_point(
    data = data_fig_3_admin_time_labels_region,
    aes(month_name, coverage),
    size = 3.5, show.legend = FALSE
  ) +
  geom_label_repel(
    data = data_fig_3_admin_time_labels_region,
    aes(x = month_name, y = coverage, label = label, color = dimension),
    inherit.aes = FALSE,
    size = 3.5, label.size = 0.5,
    label.r = unit(0.25, "lines"),
    fill = "white",
    box.padding = 0.5, point.padding = 0.5,
    nudge_x = 0,
    nudge_y = data_fig_3_admin_time_labels_region$label_nudge_y,
    segment.color = "grey60", segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf,
    seed = label_repel_seed,
    show.legend = FALSE
  ) +
  coord_cartesian(ylim = c(0, 235), clip = "off") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(0.02, 0.08))) +
  scale_color_manual(values = pal_region_world) +
  labs(title = "WHO region", x = NULL, y = NULL, color = NULL) +
  theme_fig3

# Create component 2 - by income group
fig_3_comp_income <- data_fig_3_admin_time_plot %>%
  filter(category == "Income group") %>%
  ggplot(aes(month_name, coverage, color = dimension)) +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  geom_point(
    data = data_fig_3_admin_time_labels_income,
    aes(month_name, coverage),
    size = 3.5, show.legend = FALSE
  ) +
  geom_label_repel(
    data = data_fig_3_admin_time_labels_income,
    aes(x = month_name, y = coverage, label = label, color = dimension),
    inherit.aes = FALSE,
    size = 3.5, label.size = 0.5,
    label.r = unit(0.25, "lines"),
    fill = "white",
    box.padding = 0.5, point.padding = 0.5,
    nudge_x = 0,
    nudge_y = data_fig_3_admin_time_labels_income$label_nudge_y,
    segment.color = "grey60", segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf,
    seed = label_repel_seed,
    show.legend = FALSE
  ) +
  coord_cartesian(ylim = c(0, 235), clip = "off") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(0.02, 0.08))) +
  scale_color_manual(values = pal_income_world) +
  labs(title = "Income group", x = NULL, y = label_axis_admin_per100, color = NULL) +
  theme_fig3

# Create component 3 - by COVAX participation category
fig_3_comp_covax <- data_fig_3_admin_time_plot %>%
  filter(category == "COVAX",
         dimension != "Non-COVAX") %>%
  ggplot(aes(month_name, coverage, color = dimension)) +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  geom_point(
    data = data_fig_3_admin_time_labels_covax,
    aes(month_name, coverage),
    size = 3.5, show.legend = FALSE
  ) +
  geom_label_repel(
    data = data_fig_3_admin_time_labels_covax,
    aes(x = month_name, y = coverage, label = label, color = dimension),
    inherit.aes = FALSE,
    size = 3.5, label.size = 0.5,
    label.r = unit(0.25, "lines"),
    fill = "white",
    box.padding = 0.5, point.padding = 0.5,
    nudge_x = 0,
    nudge_y = data_fig_3_admin_time_labels_covax$label_nudge_y,
    segment.color = "grey60", segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf,
    seed = label_repel_seed,
    show.legend = FALSE
  ) +
  coord_cartesian(ylim = c(0, 235), clip = "off") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(0.02, 0.08))) +
  scale_color_manual(values = pal_covax) +
  labs(title = "COVAX participation", x = NULL, y = NULL, color = NULL) +
  theme_fig3

# Combine
fig_3_admin_time_all <- fig_3_comp_income + fig_3_comp_region + fig_3_comp_covax +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Doses administered",
    theme = theme(plot.title = element_text(face = "bold", size = 12, hjust = 0,
                                            margin = margin(l = 1.28, unit = "cm")),
                  plot.title.position = "panel",
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
    )

# Create panel b - weighted average
fig_cov_time_global_compA <- data_fig_4_cov_time_world %>%
  ggplot(aes(x = month_name, y = coverage, color = measure)) +
  geom_line(linewidth = 1.5, na.rm = TRUE) +
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
    seed = label_repel_seed,
    show.legend = FALSE
  ) +
  facet_wrap(~group, ncol = 3) +
  scale_color_manual(values = pal_dose) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(
    title = "Population-weighted coverage",
    x = NULL,
    y = "Coverage (%)",
    color = NULL
  ) +
  theme_master +
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    panel.spacing = unit(0.8, "lines"),
    plot.title = element_text(face = "bold", vjust = 2, size = 12),
    plot.margin = margin(0, 0.2, 0, 0, "cm")
  )

# Begin each series when 10% of the countries reporting that indicator for
# that population group during 2021-2023 have submitted their first report.
reporting_threshold_reached <- function(cumulative_reporters) {
  cumulative_reporters > 0 & cumulative_reporters >= 0.10 * max(cumulative_reporters)
}

global_coverage_display_months <- count_time %>%
  group_by(group, type) %>%
  filter(reporting_threshold_reached(count_cumul)) %>%
  ungroup() %>%
  transmute(month_name = date, group,
            indicator = recode(type, "Booster" = "At least one booster dose"))

fig_cov_time_global_compB <- data_fig_2c_global_coverage %>%
  semi_join(global_coverage_display_months, by = c("month_name", "group", "indicator")) %>%
  ggplot(aes(x = month_name, y = med_cov, linetype = indicator,
             fill = indicator, color = indicator)) +
  geom_ribbon(aes(ymin = perc_25, ymax = perc_75), alpha = 0.2, na.rm = TRUE) +
  geom_line(linewidth = 1, na.rm = TRUE) +
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
    y = "Coverage (%)",
    color = NULL, fill = NULL, linetype = NULL
  ) +
  theme_master +
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", vjust = 3, size = 12),
    plot.margin = margin(0, 0, 0, 0.2, "cm"),
    panel.spacing = unit(0.8, "lines")
  )

# Combine panels
fig_2_emergency_coverage_time <- (
  wrap_elements(full = fig_3_admin_time_all) /
    fig_cov_time_global_compA /
    fig_cov_time_global_compB
) +
  plot_layout(heights = c(1.6, 0.8, 0.8)) +
  plot_annotation(tag_levels = "a",
                  theme = panel_tag_theme) &
  panel_tag_theme

fig_2_emergency_coverage_time

# Export
export_figure(
  filename = "fig_2_emergency_coverage_time",
  plot = fig_2_emergency_coverage_time,
  width = 11,
  height = 13
)

# Figure 3 - daily vaccination rate, line, 10/90 --------------------------

# Create panel a - global
fig_5_dvr_summary_compA <- data_fig_5_dvr_summary_compA %>%
  ggplot(aes(x = adm_date, y = median_rate)) +
  geom_ribbon(aes(ymin = perc_10, ymax = perc_90), alpha = 0.2,
              color = "black", na.rm = TRUE) +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  scale_y_continuous(labels = scales::label_number(scale = 100),
                     limits = c(0, 0.012),
                     expand = c(0, 0)) +
  labs(
    title = "Global",
    y = label_axis_daily_rate,
    x = label_axis_date
  ) +
  theme_master +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", vjust = 3, size = 12),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )

# Create panel b - by income group
fig_5_dvr_summary_compB <- data_fig_5_dvr_summary_compB %>%
  ggplot(aes(x = adm_date, y = median_rate, color = dimension)) +
  geom_ribbon(aes(ymin = perc_10, ymax = perc_90, fill = dimension),
              alpha = 0.2, na.rm = TRUE) +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  facet_wrap(~ dimension) +
  scale_y_continuous(labels = scales::label_number(scale = 100),
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
        plot.title = element_text(face = "bold", vjust = 3, size = 12),
        plot.margin = margin(0, 0, 0, 0, "cm"))

fig_5_dvr_summary <- (fig_5_dvr_summary_compA | fig_5_dvr_summary_compB) +
  plot_layout(widths = c(1, 2)) +
  plot_annotation(
    tag_levels = "a",
    theme = theme(
      plot.title = element_text(face = "bold", hjust = 0, size = 14),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
    )
    ) &
  (panel_tag_theme +
  theme(
        panel.spacing = unit(0.35, "cm")
        ))

fig_5_dvr_summary

# Export
export_figure(
  filename = "fig_3_dvr_summary",
  plot = fig_5_dvr_summary,
  size = "wide_tall"
)


# Figure 4 - vaccination policy, 2024, bar, all ---------------------------

# Modify theme for figure
theme_fig6 <- theme_master +
  theme(
    strip.background = element_rect(fill = "grey90", color = "black"),
    strip.text = element_text(size = 11),
    panel.grid.major.y = element_blank(),
  )

# Create panel a - policy count, global
fig_6_policy_2024_all_compA <- data_fig_6_policy_2024_all_compA %>%
  ggplot(aes(x = n, y = group, fill = status_policy)) +
  geom_col(width = 0.75, color = "black", linewidth = 0.2,
           position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = ifelse(n > 0, n, "")),
            position = position_stack(vjust = 0.5, reverse = TRUE), size = 3.5,
            fontface = "bold") +
  geom_text(data = data_fig_6_policy_2024_all_compA_tot,
            aes(x = total, y = group, label = total),
            inherit.aes = FALSE, hjust = -0.25, size = 3.5, fontface = "bold") +
  scale_y_discrete(labels = wrap2, limits = rev) +
  scale_fill_manual(values = pal_policy, breaks = levels_policy_a,
                    labels = levels_policy_a, drop = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.10))) +
  labs(x = label_axis_reporting_countries_n, y = NULL, fill = NULL) +
  theme_fig6 +
  theme(
    legend.position = "none",
    plot.margin = margin(5.5, 18, 5.5, 5.5), # room for totals
    axis.title.x = element_text(vjust = -2, size = 12)
  ) +
  coord_cartesian(clip = "off")

# Create panel b - policy count (100%), by income group
fig_6_policy_2024_all_compB <- data_fig_6_policy_2024_all_compB %>%
  ggplot(aes(x = pct, y = income_group, fill = status_policy)) +
  geom_col(width = 0.75, color = "black", linewidth = 0.2,
           position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = ifelse(pct >= pct_label_min, paste0(round(pct), "%"), "")),
            position = position_stack(vjust = 0.5, reverse = TRUE), size = 3.5,
            fontface = "bold") +
  facet_wrap(~ group, ncol = 2) +
  scale_fill_manual(values = pal_policy, breaks = levels_policy_b, drop = FALSE) +
  scale_x_continuous(limits = c(0, 100),expand = expansion(mult = c(0, 0.02))) +
  labs(x = paste0(label_axis_countries_pct, " (within income group)"),
       y = NULL, fill = NULL) +
  theme_fig6 +
  theme(legend.position = "none",
        panel.spacing = unit(0.6, "lines"),
        axis.title.x = element_text(vjust = -2, size = 12)
        )

# Create main legend
fig_6_policy_2024_all_compleg <- data_fig_6_policy_2024_all_compB %>%
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

leg <- cowplot::get_legend(fig_6_policy_2024_all_compleg)

# Assemble full plot
panels <- cowplot::plot_grid(
  fig_6_policy_2024_all_compA, fig_6_policy_2024_all_compB,
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

fig_6_policy_2024_all <- cowplot::plot_grid(
  panels,
  cowplot::ggdraw() +
    cowplot::draw_grob(leg, x = 0.11, hjust = 0),
  ncol = 1,
  rel_heights = c(1, 0.13)
) +
  plot_annotation(
    theme = theme(
      plot.title = element_text(face = "bold", hjust = 0, size = 14),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
    )
  )

fig_6_policy_2024_all

# Export
export_figure(
  filename = "fig_4_policy_2024_all",
  plot = fig_6_policy_2024_all,
  size = "double_wide"
)


# Supp. Figure 1 - reporting completeness over time -------------------------

pal_reporting_main <- c(
  "Complete primary series" = pal_dose_compare[["Complete primary series"]],
  "Booster" = pal_dose_compare[["At least one booster dose"]]
)

pal_reporting_main_fill <- scales::alpha(c(
  "CPS only" = pal_reporting_main[["Complete primary series"]],
  "CPS and booster" = pal_reporting_main[["Booster"]],
  "Booster only" = color_who_cat_base_3
), 0.35)

data_fig_supp_1_reporting_monthly <- count_time_monthly_categories %>%
  mutate(group = factor(group, levels = c("Total population",
                                          "Older adults",
                                          "Health and care workers")),
         category = factor(category, levels = c("CPS only", "CPS and booster", "Booster only")))

data_fig_supp_1_reporting_compA <- count_time %>%
  filter(date < as.Date("2024-01-01")) %>%
  mutate(group = factor(group, levels = c("Total population",
                                          "Older adults",
                                          "Health and care workers")),
         type = factor(type, levels = c("Complete primary series",
                                          "Booster")))

data_fig_supp_1_reporting_compA_labels <- data_fig_supp_1_reporting_compA %>%
  filter(!is.na(count_cumul)) %>%
  arrange(group, type, date) %>%
  group_by(group, type) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    label = paste0(
      recode(
        as.character(type),
        "Complete primary series" = "CPS",
        "Booster" = "Booster"
      ),
      ": ",
      scales::number(count_cumul, accuracy = 1)
    )
  )

fig_supp_1_reporting_compA <- data_fig_supp_1_reporting_monthly %>%
  ggplot(aes(x = date, y = count_month, fill = category)) +
  geom_col() +
  geom_line(data = data_fig_supp_1_reporting_compA,
            aes(x = date, y = count_cumul, color = type, group = type),
            inherit.aes = FALSE, linewidth = 1.25, na.rm = TRUE) +
  geom_point(
    data = data_fig_supp_1_reporting_compA_labels,
    aes(x = date, y = count_cumul, color = type),
    inherit.aes = FALSE,
    size = 3.5,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = data_fig_supp_1_reporting_compA_labels,
    aes(x = date, y = count_cumul, label = label, color = type),
    inherit.aes = FALSE,
    size = 3.5, label.size = 0.5,
    label.r = unit(0.25, "lines"),
    fill = "white",
    box.padding = 0.5, point.padding = 0.5,
    nudge_x = 0, nudge_y = 0,
    segment.color = "grey60", segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf,
    seed = label_repel_seed,
    show.legend = FALSE
  ) +
  scale_color_manual(name = "Reported at least once",
                     values = pal_reporting_main,
                     breaks = c("Complete primary series", "Booster"),
                     labels = c("CPS", "Booster")) +
  scale_fill_manual(name = "Reported during month",
                    values = pal_reporting_main_fill,
                    breaks = c("CPS only", "CPS and booster", "Booster only"),
                    drop = FALSE) +
  guides(fill = guide_legend(order = 1, nrow = 1),
         color = guide_legend(order = 2, nrow = 1)) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  coord_cartesian(ylim = c(0, 200)) +
  geom_hline(yintercept = reporting_member_state_count, linetype = "dashed", color = "black") +
  theme_master +
  labs(
    title = "Reporting Member States",
    y = "Count of reporting Member States"
  ) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 9),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.justification = "left",
	       plot.title = element_text(face = "bold", vjust = 2, size = 12),
	       plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
	       plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
	       plot.margin = margin(0, 0, 0.65, 0, "cm")) +
  facet_wrap(~group)

data_fig_supp_1_reporting_compB <- count_time_inc %>%
  filter(
    WBINCOME_LONG_STATUS %in% c(
      "High income",
      "Upper middle income",
      "Lower middle income",
      "Low income"
    )
  ) %>%
  mutate(population = factor(population, levels = c("Total population",
                                                    "Older adults",
                                                    "Health and care workers")),
         type = factor(type, levels = c("Complete primary series",
                                        "Booster")),
         WBINCOME_LONG_STATUS = factor(WBINCOME_LONG_STATUS, levels = c("High income",
                                                                        "Upper middle income",
                                                                        "Lower middle income",
                                                                        "Low income")))

data_fig_supp_1_reporting_compB_labels <- data_fig_supp_1_reporting_compB %>%
  filter(!is.na(count_per)) %>%
  arrange(type, population, WBINCOME_LONG_STATUS, DATE) %>%
  group_by(type, population, WBINCOME_LONG_STATUS) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    label = paste0(
      recode(
        as.character(WBINCOME_LONG_STATUS),
        "High income" = "HIC",
        "Upper middle income" = "UMIC",
        "Lower middle income" = "LMIC",
        "Low income" = "LIC"
      ),
      ": ",
      scales::percent(count_per, accuracy = 1)
    )
  )

fig_supp_1_reporting_compB <- data_fig_supp_1_reporting_compB %>%
  ggplot(aes(x = DATE, y = count_per, color = WBINCOME_LONG_STATUS)) +
  geom_line(linewidth = 1.5, na.rm = TRUE) +
  geom_point(
    data = data_fig_supp_1_reporting_compB_labels,
    aes(x = DATE, y = count_per, color = WBINCOME_LONG_STATUS),
    inherit.aes = FALSE,
    size = 3.5,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = data_fig_supp_1_reporting_compB_labels,
    aes(x = DATE, y = count_per, label = label, color = WBINCOME_LONG_STATUS),
    inherit.aes = FALSE,
    size = 3.5, label.size = 0.5,
    label.r = unit(0.25, "lines"),
    fill = "white",
    box.padding = 0.5, point.padding = 0.5,
    nudge_x = 0, nudge_y = 0,
    segment.color = "grey60", segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf,
    seed = label_repel_seed,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = pal_income_long,
    labels = c(
      "High income" = "HIC",
      "Upper middle income" = "UMIC",
      "Lower middle income" = "LMIC",
      "Low income" = "LIC"
    )
  ) +
	  scale_x_date(
	    breaks = as.Date(c(
	      "2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01"
	    )),
	    date_labels = "%Y",
	    expand = expansion(mult = c(0.02, 0.08))
	  ) +
	  scale_y_continuous(labels = scales::percent_format()) +
	  theme_master +
	  labs(
	    title = "Reporting completeness by income group",
	    x = label_axis_date,
	    y = "Percentage of Member States"
	  ) +
		  theme(axis.title.x = element_blank(),
		        axis.text.x = element_text(size = 8),
		        axis.title.y = element_text(vjust = 4, size = 11),
        axis.text.y = element_text(size = 10),
	        legend.title = element_blank(),
	        legend.position = "bottom",
	        legend.justification = "left",
		        plot.title = element_text(face = "bold", vjust = 2, size = 12),
		        plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
		        plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
		        plot.margin = margin(0.45, 0, 0, 0, "cm"),
		        panel.spacing.x = unit(0.9, "lines")) +
	  facet_grid(type ~ population, scales = "free", space = "free")

fig_supp_1_reporting <- (fig_supp_1_reporting_compA / fig_supp_1_reporting_compB) +
  plot_layout(heights = c(1, 1.3)) +
  plot_annotation(
    tag_levels = "a",
    theme = theme(
      plot.title = element_text(face = "bold", hjust = 0, size = 14),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
    )
  ) &
  panel_tag_theme

fig_supp_1_reporting

export_figure(
  filename = "fig_supp_1_reporting",
  plot = fig_supp_1_reporting,
  width = 11,
  height = 11
)


# Supp. Figure 2 - end-2021 / end-2023 CPS parity, total vs. old / hcw ----

# Create panel a - scatter plots
fig_supp_2_cov_cps_priority_time_scatter <- data_fig_supp_2_cov_cps_priority_time %>%
  ggplot(aes(x = x, y = y, fill = income_group)) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.6) +
  geom_point(shape = 21, color = "black", size = 3, alpha = 1) +
  facet_grid(group ~ time_point) +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = pal_income) +
  coord_equal() +
  labs(title = "Coverage parity",
       x = "Total population CPS coverage",
       y = "Priority group CPS coverage") +
  theme_master +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "left") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

# Create panel b - violin plots
fig_supp_2_cov_cps_priority_time_summary <- data_fig_supp_2_cov_cps_priority_time %>%
  ggplot(aes(x = income_group, y = diff_pp, fill = income_group)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.18, color = "black", na.rm = TRUE) +
  facet_grid(group ~ time_point) +
  scale_fill_manual(values = pal_income) +
  labs(title = "Difference from parity",
       x = label_axis_income,
       y = "Difference in CPS coverage (percentage points)") +
  theme_master +
  theme(legend.position = "none")

# Combine panels
fig_supp_2_cov_cps_priority_time <- (fig_supp_2_cov_cps_priority_time_scatter |
                                       fig_supp_2_cov_cps_priority_time_summary) +
  plot_layout(widths = c(1.4, 1.0)) +
  plot_annotation(tag_levels = "a",
                  theme = panel_tag_theme)

fig_supp_2_cov_cps_priority_time

# Export
export_figure(
  filename = "fig_supp_2_cov_cps_priority_time",
  plot = fig_supp_2_cov_cps_priority_time,
  size = "extra_wide"
)


# Supp. Figure 3 - coverage over time, line, group / income ---------------

fig_supp_3_cov_time_weight_all <- data_fig_supp_3_cov_time_weight_all %>%
  ggplot(aes(x = month_name, y = coverage, color = measure)) +
  geom_line(linewidth = 1.5, na.rm = TRUE) +

  # Mark the final available estimate.
  geom_point(data = data_fig_supp_3_cov_time_weight_all_last,
             aes(month_name, coverage),
             size = 3.5, show.legend = FALSE) +

  # Place endpoint labels without obscuring the trajectories.
  geom_label_repel(
    data = data_fig_supp_3_cov_time_weight_all_last,
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
    seed = label_repel_seed,
    show.legend = FALSE
  ) +
  facet_grid(group ~ dimension) +
  scale_color_manual(values = pal_dose) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(x = label_axis_date,
       y = label_axis_coverage,
       color = NULL) +
  theme_master +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        panel.spacing.x = unit(0.75, "lines"),
        panel.spacing.y = unit(0.35, "lines"))

fig_supp_3_cov_time_weight_all

# Export
export_figure(
  filename = "fig_supp_3_cov_time_weight_all",
  plot = fig_supp_3_cov_time_weight_all,
  size = "wide_tall"
)


# Supp. Figure 4 - summary coverage over time, line, group / income ------

income_coverage_display_months <- count_time_inc %>%
  group_by(WBINCOME_LONG_STATUS, population, type) %>%
  filter(reporting_threshold_reached(count)) %>%
  ungroup() %>%
  transmute(month_name = DATE, group = population,
            dimension = recode(WBINCOME_LONG_STATUS, "High income" = "HIC",
                               "Upper middle income" = "UMIC", "Lower middle income" = "LMIC",
                               "Low income" = "LIC"),
            indicator = recode(type, "Booster" = "At least one booster dose"))

fig_supp_4_cov_mediqr_all <- data_fig_supp_4_cov_mediqr_all %>%
  semi_join(income_coverage_display_months, by = c("month_name", "group", "dimension", "indicator")) %>%
  ggplot(aes(x = month_name, y = med_cov, linetype = indicator,
             fill = indicator, color = indicator)) +
  geom_ribbon(aes(ymin = perc_25, ymax = perc_75, fill = indicator,
                  color = indicator), alpha = 0.2, na.rm = TRUE) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  scale_colour_manual(values = pal_dose_compare) +
  scale_fill_manual(values = pal_dose_compare) +
  scale_linetype_manual(values = c("Complete primary series" = "solid",
                                   "At least one booster dose" = "22")) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = label_axis_date,
       y = label_axis_coverage) +
  theme_master +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.justification = "left",
        panel.spacing.x = unit(0.75, "lines"),
        panel.spacing.y = unit(0.35, "lines")) +
  facet_grid(group ~ dimension)

fig_supp_4_cov_mediqr_all

# Export
export_figure(
  filename = "fig_supp_4_cov_mediqr_all",
  plot = fig_supp_4_cov_mediqr_all,
  size = "wide_tall"
)


# Supp. Figure 5 - daily vaccination rate, income / region, weighted ------

plot_dvr_time_increg_panel <- function(panel_type) {
  panel_data <- data_fig_supp_5_dvr_time_increg %>%
    filter(type == panel_type)

  panel_points <- dvr_max %>%
    filter(type == panel_type) %>%
    ungroup()

  panel_dimensions <- levels(droplevels(panel_data$dimension))

  panel_data %>%
    ggplot(aes(x = adm_date, y = dvr_per, color = dimension)) +
    geom_line(linewidth = 1.5, na.rm = TRUE) +
    geom_point(data = panel_points,
               aes(x = adm_date, y = dvr_per, color = dimension),
               size = 3, shape = 21, fill = "black", stroke = 0) +
    scale_color_manual(values = pal_dvr_all,
                       breaks = panel_dimensions,
                       limits = panel_dimensions,
                       drop = FALSE) +
    scale_y_continuous(labels = scales::label_number(scale = 100),
                       limits = c(0, 0.01)) +
    theme_master +
    labs(title = panel_type,
         y = if (panel_type == "Income group") {
           sub(" per 100", "\nper 100", label_axis_daily_rate, fixed = TRUE)
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
          legend.position = c(0.965, 0.965),
          legend.justification = c("right", "top"),
          legend.background = element_rect(fill = alpha("white", 0.85),
                                           color = "grey80"),
          legend.margin = margin(4, 6, 4, 6, "pt"),
          legend.key.width = unit(1.15, "lines"),
          legend.key.height = unit(0.9, "lines"),
          legend.spacing.x = unit(0.45, "lines"),
          plot.title = element_text(face = "bold", vjust = 3, size = 11),
          plot.subtitle = element_text(size = 8, face = "italic", vjust = 7),
          plot.caption = element_text(hjust = 0, size = 8, vjust = -3),
          plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE))
}

fig_supp_5_dvr_time_increg <- c("WHO region", "Income group", "All") %>%
  map(plot_dvr_time_increg_panel) %>%
  wrap_plots(ncol = 1) +
  plot_annotation(
    theme = theme(plot.title = element_text(face = "bold", vjust = 3, size = 14))
  )

fig_supp_5_dvr_time_increg

# Export
export_figure(
  filename = "fig_supp_5_dvr_time_increg",
  plot = fig_supp_5_dvr_time_increg,
  size = "wide_tall"
)


# Supp. Figure 6 - HCW adjustment and coverage relative to the total population ----

hcw_dose_labels <- c(a1d = "At least one dose", cps = "Complete primary series",
                     boost = "At least one booster dose")

hcw_year_end <- data_hcw_adjustment_country %>%
  filter(status_who == "Member State", month(month_name) == 12) %>%
  mutate(year = year(month_name),
         indicator = factor(indicator, names(hcw_dose_labels), hcw_dose_labels))

hcw_country_counts <- hcw_year_end %>%
  filter(is.finite(unadjusted_coverage), is.finite(adjusted_coverage)) %>%
  summarise(n_increased = n_distinct(iso[estimate_changed]), n_compared = n_distinct(iso),
            .by = c(year, indicator))

hcw_colours <- c(Unadjusted = color_who_seq_1, Adjusted = color_who_cat_base_2)
hcw_pair_width <- 0.86
hcw_dodge <- position_dodge(width = hcw_pair_width, orientation = "y", reverse = TRUE)

hcw_weighted <- data_hcw_adjustment_summary %>%
  filter(type == "all", month(month_name) == 12) %>%
  transmute(year = year(month_name),
            indicator = factor(indicator, names(hcw_dose_labels), hcw_dose_labels),
            Unadjusted = coverage_unadjusted, Adjusted = coverage_adjusted, difference_pp,
            bracket = pmax(Unadjusted, Adjusted) + 0.065, label_x = bracket + 0.075,
            year_before = year - hcw_pair_width / 4, year_after = year + hcw_pair_width / 4) %>%
  left_join(hcw_country_counts, by = c("year", "indicator"))

hcw_bars <- hcw_weighted %>%
  pivot_longer(c(Unadjusted, Adjusted), names_to = "scenario", values_to = "coverage") %>%
  mutate(scenario = factor(scenario, levels = names(hcw_colours)))

hcw_fig_theme <- theme_master +
  theme(strip.text = element_text(size = 8),
        panel.spacing.x = unit(0.44, "cm"),
        axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10, vjust = 0, margin = margin(t = 5)),
        axis.title.y = element_text(size = 10, vjust = 0, margin = margin(r = 5)),
        legend.position = "bottom", legend.justification = "left",
        legend.text = element_text(size = 8))

hcw_coverage <- ggplot(hcw_bars, aes(coverage, year)) +
  geom_col(aes(fill = scenario), orientation = "y", position = hcw_dodge, width = 0.80) +
  geom_text(aes(label = sprintf("%.1f", 100 * coverage), colour = scenario, group = scenario),
            position = hcw_dodge, hjust = 1.12, size = 2.45, show.legend = FALSE) +
  geom_segment(data = hcw_weighted,
               aes(x = Unadjusted + 0.01, xend = bracket, y = year_before, yend = year_before),
               linewidth = 0.28) +
  geom_segment(data = hcw_weighted,
               aes(x = bracket, xend = bracket, y = year_before, yend = year_after),
               linewidth = 0.28) +
  geom_segment(data = hcw_weighted,
               aes(x = bracket, xend = Adjusted + 0.01, y = year_after, yend = year_after),
               linewidth = 0.28, arrow = grid::arrow(length = unit(1.1, "mm"), type = "closed")) +
  geom_label(data = hcw_weighted,
             aes(x = label_x, y = year, label = sprintf("%+.1f pp", difference_pp)),
             hjust = 0, vjust = 0.25, size = 2.35, fontface = "bold", linewidth = 0.22,
             label.padding = unit(c(0.20, 0.40, 0.20, 0.40), "lines"),
             label.r = unit(0.40, "lines")) +
  geom_text(data = hcw_weighted,
            aes(x = label_x + 0.015, y = year, label = sprintf("%d/%d", n_increased, n_compared),
                colour = "Countries with higher adjusted coverage"),
            hjust = 0, vjust = 2.35, size = 2.30) +
  facet_wrap(vars(indicator), nrow = 1) +
  scale_fill_manual(NULL, values = hcw_colours, guide = guide_legend(order = 1)) +
  scale_colour_manual(NULL, values = c(Unadjusted = "white", Adjusted = "black",
                                      "Countries with higher adjusted coverage" = "grey25"),
                      breaks = "Countries with higher adjusted coverage",
                      guide = guide_legend(order = 2, override.aes = list(label = "n/N", hjust = 0.5, vjust = 0.5))) +
  scale_x_continuous(breaks = scales::breaks_width(0.25), labels = scales::label_number(scale = 100, accuracy = 1),
                     expand = expansion(mult = c(0, 0.40))) +
  scale_y_reverse(breaks = sort(unique(hcw_weighted$year)), minor_breaks = NULL) +
  labs(x = "Population-weighted HCW coverage (%)", y = NULL) +
  hcw_fig_theme

hcw_paired <- hcw_year_end %>%
  filter(year == 2023, paired) %>%
  arrange(crosses_above_total)

hcw_parity <- data_hcw_adjustment_comparison %>%
  filter(type == "all", month_name == as.Date("2023-12-01")) %>%
  mutate(indicator = factor(indicator, names(hcw_dose_labels), hcw_dose_labels),
         strip = sprintf("%s<br><span style='font-size:6pt'><i>%d of %d crossed above parity</i></span>",
                        indicator, countries_crossing_above_total, countries))

hcw_scatter <- ggplot(hcw_paired, aes(hcw_total_gap_unadjusted_pp, hcw_total_gap_adjusted_pp)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey65") +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey65") +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.35, linetype = "dashed") +
  geom_point(aes(fill = crosses_above_total), shape = 21, size = 1.6,
             stroke = 0.25) +
  geom_point(data = hcw_parity,
             aes(x = median_gap_unadjusted_pp, y = median_gap_adjusted_pp,
                 shape = "Country median"),
             size = 2.7, stroke = 0.6, fill = "white") +
  facet_wrap(vars(indicator), nrow = 1,
             labeller = as_labeller(setNames(hcw_parity$strip, as.character(hcw_parity$indicator)))) +
  scale_fill_manual(NULL, values = c("FALSE" = scales::alpha("grey70", 0.85), "TRUE" = color_who_cat_base_2),
                    breaks = c(FALSE, TRUE), labels = c("Other countries", "Crossed above parity")) +
  scale_shape_manual(NULL, values = c("Country median" = 23)) +
  scale_x_continuous(breaks = seq(-100, 100, 50)) +
  scale_y_continuous(breaks = seq(-100, 100, 50)) +
  coord_equal(xlim = c(-100, 100), ylim = c(-100, 100), expand = TRUE) +
  guides(fill = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  labs(x = "Coverage gap before adjustment (HCW minus total population, pp)",
       y = "Coverage gap after adjustment\n(HCW minus total population, pp)") +
  hcw_fig_theme +
  theme(strip.text.x = ggtext::element_markdown(size = 8))

fig_supp_6_hcw_adjustment <- (hcw_coverage / hcw_scatter) +
  plot_annotation(tag_levels = "a", theme = panel_tag_theme) &
  panel_tag_theme

export_figure("fig_supp_6_hcw_adjustment", fig_supp_6_hcw_adjustment,
              width = 7.25, height = 7.5, dpi = 450)
