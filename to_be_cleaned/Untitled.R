library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# df must have:
# country, a_region_who, a_income_group, a_status_covax,
# coverage_denom_rep (0–1), measure, group

measures_keep <- c("Complete primary series", "At least one booster")
groups_keep   <- c("Total population", "Older adults")

# Build wide per country+measure: columns = risk groups, value = coverage_denom_rep
wide <- data_cov_last %>%
  filter(measure %in% measures_keep,
         group   %in% groups_keep) %>%
  select(country, a_region_who, a_income_group, a_status_covax,
         measure, group, coverage = coverage_denom_rep) %>%
  pivot_wider(names_from = group, values_from = coverage) %>%
  # compute Δ = Older − Total (percentage points)
  mutate(
    delta_pp = (`Older adults` - `Total population`) * 100
  ) %>%
  # keep rows with both coverages present
  filter(!is.na(delta_pp)) %>%
  mutate(
    measure = factor(measure, levels = measures_keep)
  )

plot_hist_stacked <- function(data, fill_by,
                              binwidth = 5,           # pp bin width
                              normalized = FALSE,     # TRUE → show proportions
                              drop_na = TRUE,
                              level_order = NULL) {   # e.g., c("HIC","UMIC","LMIC","LIC")
  fill_by <- ensym(fill_by)
  d <- data
  if (drop_na) d <- d %>% filter(!is.na(!!fill_by))
  if (!is.null(level_order)) {
    d <- d %>% mutate(!!fill_by := fct_relevel(as.factor(!!fill_by), !!!level_order))
  }

  pos <- if (normalized) "fill" else "stack"

  ggplot(d, aes(x = delta_pp, fill = as.factor(!!fill_by))) +
    geom_histogram(binwidth = binwidth, position = pos) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_wrap(~ measure, nrow = 1, scales = "free_y") +
    labs(
      title = "Coverage Δ (Older − Total), stacked by category",
      subtitle = if (normalized) "Bars show within-bin proportions" else "Bars show counts",
      x = "Δ (percentage points)", y = if (normalized) "Proportion" else "Count",
      fill = as_name(fill_by)
    )
}

# -------- Overall (no disaggregation) --------
p_hist_overall <- ggplot(wide, aes(x = delta_pp)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ measure, scales = "free_y") +
  labs(
    title = "Distribution of coverage differences (overall)",
    subtitle = "Δ = Older adults − Total population (percentage points)",
    x = "Δ (pp)", y = "Number of countries"
  )

p_box_overall <- ggplot(wide, aes(y = measure, x = delta_pp)) +
  geom_boxplot(outlier.alpha = 0.4, width = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Coverage gap by measure (overall)",
    x = "Δ (Older − Total) in percentage points", y = NULL
  )

# -------- Disaggregations (wide columns) --------
# Helpers so you can reuse the same pattern for income / region / covax

plot_hist_by <- function(data, by) {
  by <- ensym(by)  # turn input into a symbol
  ggplot(data, aes(x = delta_pp)) +
    geom_histogram(bins = 30) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_grid(rows = vars(measure), cols = vars(!!by), scales = "free_y") +
    labs(
      title = paste("Distribution of coverage differences by", as_name(by)),
      subtitle = "Δ = Older adults − Total population (percentage points)",
      x = "Δ (pp)", y = "Number of countries"
    )
}

# Box/whisker by a wide column (income/region/covax)
plot_box_by <- function(data, by) {
  by <- ensym(by)
  ggplot(data, aes(x = fct_infreq(as.factor(!!by)), y = delta_pp)) +
    geom_boxplot(outlier.alpha = 0.35) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_wrap(~ measure, nrow = 1) +
    coord_flip() +
    labs(
      title = paste("Coverage gap by", as_name(by)),
      x = NULL, y = "Δ (Older − Total) in percentage points"
    )
}

plot_hist_stacked <- function(data, fill_by,
                              binwidth = 5,           # pp bin width
                              normalized = FALSE,     # TRUE → show proportions
                              drop_na = TRUE,
                              level_order = NULL) {   # e.g., c("HIC","UMIC","LMIC","LIC")
  fill_by <- ensym(fill_by)
  d <- data
  if (drop_na) d <- d %>% filter(!is.na(!!fill_by))
  if (!is.null(level_order)) {
    d <- d %>% mutate(!!fill_by := fct_relevel(as.factor(!!fill_by), !!!level_order))
  }
  
  pos <- if (normalized) "fill" else "stack"
  
  ggplot(d, aes(x = delta_pp, fill = as.factor(!!fill_by))) +
    geom_histogram(binwidth = binwidth, position = pos) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_wrap(~ measure, nrow = 1, scales = "free_y") +
    labs(
      title = "Coverage Δ (Older − Total), stacked by category",
      subtitle = if (normalized) "Bars show within-bin proportions" else "Bars show counts",
      x = "Δ (percentage points)", y = if (normalized) "Proportion" else "Count",
      fill = as_name(fill_by)
    )
}

plot_bucket_bars <- function(data, breaks = c(-100, -40, -20, -10, 0, 10, 20, 40, 100),
                             by = c("overall","income","region","covax")){
  by <- match.arg(by)
  d <- data %>%
    mutate(bucket = cut(delta_pp, breaks = breaks,
                        labels = paste0(head(breaks, -1), " to ", tail(breaks, -1), " pp"),
                        include.lowest = TRUE),
           is_neg = ifelse(delta_pp < 0, "Neg", "Non-neg"))
  # choose disaggregation
  group_var <- switch(by,
                      overall = NULL,
                      income  = quo(a_income_group),
                      region  = quo(a_region_who),
                      covax   = quo(a_status_covax))
  
  if (is.null(group_var)) {
    ggplot(d, aes(x = fct_inorder(bucket), fill = is_neg)) +
      geom_bar(position = "stack") +
      facet_wrap(~ measure, nrow = 1, scales = "free_y") +
      coord_flip() +
      labs(title = "Countries by Δ buckets (pp)", x = "Δ bucket", y = "Countries", fill = NULL)
  } else {
    ggplot(d, aes(x = fct_inorder(bucket), fill = !!group_var)) +
      geom_bar(position = "stack") +
      facet_wrap(~ measure, nrow = 1, scales = "free_y") +
      coord_flip() +
      labs(title = paste("Countries by Δ buckets (pp) —", rlang::as_name(group_var)),
           x = "Δ bucket", y = "Countries", fill = rlang::as_name(group_var))
  }
}
# usage:
# plot_bucket_bars(wide, by = "overall")
plot_bucket_bars(wide, by = "income")

plot_ecdf_neg <- function(data){
  ggplot(data, aes(x = delta_pp, color = measure)) +
    stat_ecdf(geom = "step") +
    annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = 1, alpha = 0.08) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(title = "Cumulative distribution of Δ (Older − Total)",
         subtitle = "Shaded region indicates Δ < 0",
         x = "Δ (pp)", y = "Cumulative proportion", color = "Measure")
}

plot_hist_diverging <- function(data, binwidth = 5){
  data %>%
    mutate(sign = ifelse(delta_pp < 0, "Older < Total", "Older ≥ Total")) %>%
    ggplot(aes(x = delta_pp, fill = sign, alpha = sign)) +
    geom_histogram(binwidth = binwidth, position = "identity", color = NA) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme_test() +
    scale_alpha_manual(values = c("Older < Total" = 1, "Older ≥ Total" = 0.35), guide = "none") +
    facet_wrap(~ measure, scales = "free_y", nrow = 1) +
    labs(title = "Δ = Older − Total (pp), negatives emphasized",
         x = "Δ (percentage points)", y = "Count", fill = NULL)
}
plot_hist_diverging(wide, binwidth = 5)
plot_ecdf_neg(wide)



plot_diverging_hist_highlight <- function(data, fill_by,
                                          binwidth = 5,
                                          facet_by_snapshot = TRUE,
                                          neutral_fill = "grey80") {
  fill_by <- ensym(fill_by)
  
  # split negatives vs non-negatives
  neg <- data %>% filter(delta_pp < 0)
  pos <- data %>% filter(delta_pp >= 0)
  
  # fix x-limits across panels for fair comparison
  xlim_all <- range(pretty(range(data$delta_pp, na.rm = TRUE)))
  
  p <- ggplot() +
    # non-negative side (single neutral color)
    geom_histogram(
      data = pos,
      aes(x = delta_pp),
      binwidth = binwidth,
      fill = neutral_fill,
      color = NA
    ) +
    # negative side (stacked by the category of interest)
    geom_histogram(
      data = neg,
      aes(x = delta_pp, fill = as.factor(!!fill_by)),
      binwidth = binwidth,
      position = "stack",
      color = NA
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    coord_cartesian(xlim = xlim_all) +
    theme_test() +
    labs(
      title = "Δ (Older − Total) — negatives highlighted by category",
      x = "Δ (percentage points)", y = "Countries",
      fill = as_name(fill_by)
    )
  
  # faceting: measure x snapshot (if present & requested)
  if (facet_by_snapshot && "snapshot" %in% names(data)) {
    p <- p + facet_grid(rows = vars(measure), cols = vars(snapshot), scales = "free_y")
  } else {
    p <- p + facet_wrap(~ measure, nrow = 1, scales = "free_y")
  }
  
  p
}

plot_diverging_hist_highlight(wide, a_income_group, binwidth = 5)

# Income group
p_hist_income <- plot_hist_by(wide, a_income_group)
p_box_income  <- plot_box_by (wide, a_income_group)
plot_hist_stacked(wide, a_income_group, binwidth = 5, normalized = FALSE)

# WHO region
p_hist_region <- plot_hist_by(wide, a_region_who)
p_box_region  <- plot_box_by (wide, a_region_who)

# COVAX status
p_hist_covax  <- plot_hist_by(wide, a_status_covax)
p_box_covax   <- plot_box_by (wide, a_status_covax)

# ---- Print to viewer (pick what you want) ----
p_hist_overall; p_box_overall
p_hist_income;  p_box_income
p_hist_region;  p_box_region
p_hist_covax;   p_box_covax

# ---- Optional saves ----
# ggsave("hist_overall.png", p_hist_overall, width = 8, height = 5, dpi = 300)
# ggsave("box_overall.png",  p_box_overall,  width = 7, height = 4, dpi = 300)
# ggsave("hist_income.png",  p_hist_income,  width = 10, height = 6, dpi = 300)
# ggsave("box_income.png",   p_box_income,   width = 10, height = 6, dpi = 300)
# ggsave("hist_region.png",  p_hist_region,  width = 10, height = 6, dpi = 300)
# ggsave("box_region.png",   p_box_region,   width = 10, height = 6, dpi = 300)
# ggsave("hist_covax.png",   p_hist_covax,   width = 10, height = 6, dpi = 300)
# ggsave("box_covax.png",    p_box_covax,    width = 10, height = 6, dpi = 300)