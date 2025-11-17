library(dplyr)
library(knitr)
library(kableExtra)
library(scales)

# assume your data frame is named `df` with columns:
# type, grouping, count_rep_hcw, cov_hcw, prop_pop_hcw,
# count_rep_older, cov_older, prop_pop_old

# 1) Arrange and format display columns
disp <- data_annual %>%
  arrange(type, grouping) %>%
  mutate(
    older_count = format(round(count_rep_older), big.mark = ",", trim = TRUE),
    older_prop  = percent(prop_pop_old, accuracy = 0.01),
    older_cov   = percent(cov_older,      accuracy = 0.01),
    
    hcw_count   = format(round(count_rep_hcw), big.mark = ",", trim = TRUE),
    hcw_prop    = percent(prop_pop_hcw, accuracy = 0.01),
    hcw_cov     = percent(cov_hcw,      accuracy = 0.01)
  )

# 2) Build the table body (keep `type` only for grouping)
tbl_body <- disp %>%
  transmute(
    type,
    Grouping = grouping,
    `count` = older_count,
    `prop_pop` = older_prop,
    `cov` = older_cov,
    `count ` = hcw_count,       # trailing spaces keep names distinct
    `prop_pop ` = hcw_prop,
    `cov ` = hcw_cov
  )

# 3) Compute section row spans by `type`
run <- rle(as.character(tbl_body$type))
ends <- cumsum(run$lengths)
starts <- c(1, head(ends, -1) + 1)

# 4) Render kable with multi-level headers and group rows
table_annual <- tbl_body %>%
  select(-type) %>%
  kbl(
    align = c("l","r","r","r","r","r","r"),
    escape = FALSE,
    col.names = c("Grouping","count","prop_pop","cov","count","prop_pop","cov")
  ) %>%
  add_header_above(c(" " = 1, "Older adults" = 3, "Health and care workers" = 3)) %>%
  kable_styling(full_width = FALSE, position = "left")

for (i in seq_along(run$values)) {
  table_annual <- table_annual %>% group_rows(run$values[i], starts[i], ends[i])
}

table_annual

save_kable(table_annual, file = "table_annual.png")
