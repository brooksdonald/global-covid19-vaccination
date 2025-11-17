library(dplyr)
library(lubridate)
library(rlang)

interpolate_from_intro_wide_many <- function(df,
                                             vars,
                                             date_col         = "month_name",
                                             iso_col          = "a_iso",
                                             intro_col        = "date_intro",
                                             boost_col        = "date_boost",
                                             cps_delay_months = 2,
                                             suffix           = "_interp") {
  # Coerce key date columns once
  if (!inherits(df[[date_col]],  "Date"))  df[[date_col]]  <- as.Date(df[[date_col]])
  if (!inherits(df[[intro_col]], "Date"))  df[[intro_col]] <- as.Date(df[[intro_col]])
  if (!inherits(df[[boost_col]], "Date"))  df[[boost_col]] <- as.Date(df[[boost_col]])
  
  # Resolve vars to character vector
  var_names <- if (is.character(vars)) vars else names(dplyr::select(df, {{ vars }}))
  missing_cols <- setdiff(c(iso_col, date_col, intro_col, boost_col, var_names), names(df))
  if (length(missing_cols)) stop("Missing columns in `df`: ", paste(missing_cols, collapse = ", "))
  
  df %>%
    group_by(.data[[iso_col]]) %>%
    arrange(.data[[date_col]], .by_group = TRUE) %>%
    mutate(
      # Precompute per-ISO start dates (vectors available in the data mask)
      start_intro      = first(.data[[intro_col]]),
      start_intro_cps  = ifelse(!is.na(start_intro),
                                as.Date(start_intro %m+% months(cps_delay_months)),
                                as.Date(NA)),
      start_boost      = first(.data[[boost_col]])
    ) %>%
    mutate(
      across(
        all_of(var_names),
        ~{
          var_name <- cur_column()
          v        <- .x
          
          # Choose the start date vector for this variable
          start_vec <-
            if (grepl("boost", var_name, ignore.case = TRUE)) {
              start_boost
            } else if (grepl("cps", var_name, ignore.case = TRUE)) {
              start_intro_cps
            } else if (grepl("a1d", var_name, ignore.case = TRUE)) {
              start_intro
            } else {
              start_intro
            }
          
          # Find first non-NA report for this variable (per group)
          idxs       <- which(!is.na(v))
          first_idx  <- if (length(idxs)) idxs[1] else NA_integer_
          first_val  <- if (is.na(first_idx)) NA_real_ else v[first_idx]
          first_date <- if (is.na(first_idx)) as.Date(NA) else .data[[date_col]][first_idx]
          
          # Linear ramp weights from start_vec -> first_date
          den <- as.numeric(as.Date(first_date) - as.Date(start_vec))
          num <- as.numeric(as.Date(.data[[date_col]]) - as.Date(start_vec))
          w   <- ifelse(!is.na(den) & den > 0, pmax(0, pmin(1, num/den)), NA_real_)
          
          # Interpolate from 0 at start_vec up to first_val at first_date
          ramp <- ifelse(!is.na(w), 0 + (first_val - 0) * w, NA_real_)
          
          # Use ramp only within [start_vec, first_date]; elsewhere keep original
          case_when(
            is.na(start_vec) | is.na(first_date) ~ v,
            .data[[date_col]] <  start_vec       ~ v,
            .data[[date_col]] >  first_date      ~ v,
            TRUE                                  ~ ramp
          )
        },
        .names = paste0("{.col}", suffix)
      )
    ) %>%
    # drop helper columns
    select(-start_intro, -start_intro_cps, -start_boost) %>%
    ungroup()
}