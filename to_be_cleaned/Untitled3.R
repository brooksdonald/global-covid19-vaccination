library(dplyr)
library(knitr)
library(kableExtra)
library(lubridate)

# df assumed to have: adm_date, a_income_group, avg_rate (rename if yours is avr_rate)

peak_tbl <- data_time_mmm %>%
  mutate(
    # ensure date type; adjust format if needed
    adm_date = as_date(adm_date)
  ) %>%
  group_by(a_income_group) %>%
  # keep rows hitting the group-wise max (ignore NAs)
  filter(avg_rate == max(avg_rate, na.rm = TRUE)) %>%
  # if multiple dates tie, take the most recent
  slice_max(adm_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    `Income group`   = a_income_group,
    `Max avg_rate`   = scales::percent(avg_rate, accuracy = 0.01),
    `Date of max`    = format(adm_date, "%Y-%m-%d")
  ) %>%
  arrange(`Income group`)

# Present like the last table (simple kable)
table_dvr <- kbl(peak_tbl, align = "l", caption = "Peak avg_rate by income group and date") %>%
  kable_styling(full_width = FALSE, position = "left")

save_kable(table_dvr, file = "table_dvr.png")
