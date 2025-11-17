raw_data_annual <- data.frame(
  read_excel("data/output/output_new.xlsx",
             sheet = "uptake_grouping_wide"))

data_annual <- raw_data_annual %>%
  mutate(type = case_when(
    grouping == "AFR" ~ "WHO region",
    grouping == "AMR" ~ "WHO region",
    grouping == "EUR" ~ "WHO region",
    grouping == "EMR" ~ "WHO region",
    grouping == "SEAR" ~ "WHO region",
    grouping == "WPR" ~ "WHO region",
    grouping == "HIC" ~ "Income group",
    grouping == "LIC" ~ "Income group",
    grouping == "LMIC" ~ "Income group",
    grouping == "UMIC" ~ "Income group",
    TRUE ~ NA
  )) %>%
  select(type,
         grouping,
         count_rep_hcw,
         cov_hcw,
         prop_pop_hcw,
         count_rep_older,
         cov_older,
         prop_pop_old)



