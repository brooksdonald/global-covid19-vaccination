
data_tab_3_coverage_2024 <- raw_data_post_final %>%
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
