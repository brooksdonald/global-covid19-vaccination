library(dplyr)
library(tidyr)
library(ggplot2)
library(rlang)

# df must have: country, measure, group, coverage_denom_rep
# plus your wide tags like a_income_group / a_region_who / a_status_covax
# optional: snapshot (e.g., "2021-12-31","2022-12-31",...)

measures_keep <- c("Complete primary series", "At least one booster")
groups_keep   <- c("Total population", "Older adults", "Health and care workers")
facet_groups  <- c("Older adults", "Health and care workers")  # horizontal facets

# --- Build wide and then create Δ vs Total for both facet groups ---
wide_pairs <- data_cov_last %>%
  filter(measure %in% measures_keep, group %in% groups_keep) %>%
  mutate(
    measure = factor(measure, levels = measures_keep),
    group   = factor(group, levels = groups_keep)
  ) %>%
  select(country, measure, group, coverage = coverage_denom_rep,
         a_income_group, a_region_who, a_status_covax,
         dplyr::any_of("snapshot")) %>%
  pivot_wider(names_from = group, values_from = coverage) %>%
  # create long rows for each risk group we want to compare vs Total
  pivot_longer(cols = all_of(facet_groups), names_to = "risk_group", values_to = "cov_risk") %>%
  rename(cov_total = `Total population`) %>%
  mutate(
    delta_pp = (cov_risk - cov_total) * 100
  ) %>%
  filter(!is.na(delta_pp)) %>%
  mutate(
    risk_group = factor(risk_group, levels = facet_groups)  # keep desired column order
  )

# --- Diverging histogram with negative side stacked by a category of interest ---
plot_diverging_hist_highlight_2facets <- function(data, fill_by,
                                                  binwidth = 5,
                                                  neutral_fill = "grey80") {
  fill_by <- ensym(fill_by)
  
  neg <- data %>% filter(delta_pp < 0)
  pos <- data %>% filter(delta_pp >= 0)
  
  xlim_all <- range(pretty(range(data$delta_pp, na.rm = TRUE)))
  
  p <- ggplot() +
    # non-negative side: neutral fill
    geom_histogram(
      data = pos,
      aes(x = delta_pp),
      binwidth = binwidth,
      fill = neutral_fill,
      color = NA
    ) +
    # negative side: stacked by category (e.g., a_income_group)
    geom_histogram(
      data = neg,
      aes(x = delta_pp, fill = as.factor(!!fill_by)),
      binwidth = binwidth,
      position = "stack",
      color = NA
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme_test()+
    coord_cartesian(xlim = xlim_all) +
    labs(
      title = "Δ (Older/HCW − Total) — negatives highlighted by category",
      x = "Δ (percentage points)", y = "Member States",
      fill = as_name(fill_by)
    )
  
  # Facet: rows = measure, cols = risk group; include snapshot if present
  if ("snapshot" %in% names(data)) {
    p + facet_grid(rows = vars(measure), cols = vars(risk_group, snapshot), scales = "free_y")
  } else {
    p + facet_grid(rows = vars(measure), cols = vars(risk_group), scales = "free_y")
  }
}

# ---------- Examples ----------
# 1) Highlight negative side by Income group
p_inc <- plot_diverging_hist_highlight_2facets(wide_pairs, a_income_group, binwidth = 5)

# 2) Or by WHO region
p_reg <- plot_diverging_hist_highlight_2facets(wide_pairs, a_region_who, binwidth = 5)

# 3) Or by COVAX status
p_cov <- plot_diverging_hist_highlight_2facets(wide_pairs, a_status_covax, binwidth = 5)

# Print one to view
p_inc

ggsave(
  path = path_save,
  filename = "fig_risk_total_comp.jpg",
  plot = p_inc,
  width = 10,
  height = 6,
  dpi = 300
)
