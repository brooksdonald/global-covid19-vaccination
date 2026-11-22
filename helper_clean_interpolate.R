# Reconstruction of treated vaccination counts at supplied dates
# Maintainer: BROOKS, Donald J.
# Run through 0_master.R.

# 1. Define the programme zero date for count reconstruction ----

# Total doses and at least one dose use the documented introduction date.
# CPS starts two calendar months later. Priority group boosters use a supplied
# zero-date proxy; total population boosters have no assumed zero date.
programme_zero_date <- function(introduction_date, indicator,
                                booster_zero_date = as.Date(NA)) {
  if (endsWith(indicator, "_boost")) return(as.Date(booster_zero_date))
  if (is.na(introduction_date)) return(as.Date(NA))
  start <- as.Date(introduction_date)
  if (endsWith(indicator, "_cps")) start <- start %m+% months(2L)
  start
}


# 2. Reconstruct a treated count series at supplied dates -------

# Interpolate between retained observations using elapsed calendar days.
# Month end estimates carry the final value forward; daily rates stop at the
# final retained report. Early reconstruction requires a programme zero date
# or a supplied booster zero-date proxy.
reconstruct_dated_series <- function(source, target_dates, indicator,
                                      introduction_date, population,
                                      carry_forward = TRUE, cap_person_counts = TRUE,
                                      booster_zero_date = as.Date(NA)) {
  stopifnot(inherits(target_dates, "Date"), !anyNA(target_dates),
            length(population) == 1L,
            (is.finite(population) && population > 0) ||
              ((indicator == "adm_tot_td" || !cap_person_counts) && is.na(population)),
            length(indicator) == 1L, length(introduction_date) == 1L,
            length(carry_forward) == 1L, !is.na(carry_forward))
  anchors <- source %>% filter(!is.na(after_downward)) %>% arrange(date)
  stopifnot(!anyDuplicated(anchors$date),
            all(is.finite(anchors$after_downward)),
            all(anchors$after_downward >= 0),
            all(diff(anchors$after_downward) >= 0),
            all(anchors$after_downward == anchors$reported))

  # Every estimate retains its supporting report dates, values and identifiers.
  result <- tibble::tibble(
    target_date = target_dates, before_rounding = NA_real_, before_cap = NA_real_,
    origin = "Unavailable", gap_type = "No retained anchors",
    left_date = as.Date(NA), right_date = as.Date(NA),
    left_source_id = NA_character_, right_source_id = NA_character_,
    left_value = NA_real_, right_value = NA_real_, interpolation_weight = NA_real_
  )

  if (nrow(anchors)) {
    anchor_dates <- as.numeric(anchors$date)
    anchor_values <- anchors$after_downward
    target_numeric <- as.numeric(target_dates)
    exact <- match(target_dates, anchors$date)
    previous <- findInterval(target_numeric, anchor_dates)
    exact_rows <- which(!is.na(exact))
    internal_rows <- which(is.na(exact) & previous > 0L & previous < nrow(anchors))
    leading_rows <- which(target_numeric < anchor_dates[1])
    trailing_rows <- which(target_numeric > tail(anchor_dates, 1))

    # Preserve retained observations exactly before the final coverage cap.
    if (length(exact_rows)) {
      k <- exact[exact_rows]
      result$before_rounding[exact_rows] <- anchor_values[k]
      result$origin[exact_rows] <- "Reported"
      result$gap_type[exact_rows] <- "Retained observation"
      result$left_date[exact_rows] <- result$right_date[exact_rows] <- anchors$date[k]
      result$left_source_id[exact_rows] <- result$right_source_id[exact_rows] <- anchors$source_id[k]
      result$left_value[exact_rows] <- result$right_value[exact_rows] <- anchor_values[k]
      result$interpolation_weight[exact_rows] <- 0
    }

    # Use elapsed days between the two surrounding retained observations.
    # There is no maximum gap length and excluded observations cannot be anchors.
    if (length(internal_rows)) {
      left <- previous[internal_rows]
      right <- left + 1L
      weight <- (target_numeric[internal_rows] - anchor_dates[left]) /
        (anchor_dates[right] - anchor_dates[left])
      result$gap_type[internal_rows] <- "Internal gap"
      result$left_date[internal_rows] <- anchors$date[left]
      result$right_date[internal_rows] <- anchors$date[right]
      result$left_source_id[internal_rows] <- anchors$source_id[left]
      result$right_source_id[internal_rows] <- anchors$source_id[right]
      result$left_value[internal_rows] <- anchor_values[left]
      result$right_value[internal_rows] <- anchor_values[right]
      result$before_rounding[internal_rows] <- anchor_values[left] +
        weight * (anchor_values[right] - anchor_values[left])
      result$origin[internal_rows] <- "Internal interpolation"
      result$interpolation_weight[internal_rows] <- weight
    }

    # Interpolate from the programme zero date or booster proxy only when it
    # precedes the first retained report. Earlier targets remain missing.
    result$gap_type[leading_rows] <- "Leading gap"
    intro_start <- programme_zero_date(introduction_date, indicator, booster_zero_date)
    if (!is.na(intro_start) && intro_start < anchors$date[1]) {
      eligible <- leading_rows[target_dates[leading_rows] >= intro_start]
      if (length(eligible)) {
        weight <- as.numeric(target_dates[eligible] - intro_start) /
          as.numeric(anchors$date[1] - intro_start)
        result$before_rounding[eligible] <- weight * anchor_values[1]
        result$origin[eligible] <- if (endsWith(indicator, "_boost"))
          "Booster proxy ramp" else "Introduction ramp"
        result$interpolation_weight[eligible] <- weight
        result$left_date[eligible] <- intro_start
        result$right_date[eligible] <- anchors$date[1]
        result$right_source_id[eligible] <- anchors$source_id[1]
        result$left_value[eligible] <- 0
        result$right_value[eligible] <- anchor_values[1]
      }
    }

    # Carry month end estimates forward without a duration limit. Record the
    # supporting date when carry-forward is disabled for daily rates.
    if (length(trailing_rows)) {
      last <- nrow(anchors)
      result$gap_type[trailing_rows] <- "Trailing gap"
      result$left_date[trailing_rows] <- anchors$date[last]
      result$left_source_id[trailing_rows] <- anchors$source_id[last]
      result$left_value[trailing_rows] <- anchor_values[last]
      if (carry_forward) {
        result$before_rounding[trailing_rows] <- anchor_values[last]
        result$origin[trailing_rows] <- "Trailing carry-forward"
      }
    }
  }

  # Round reconstructed counts, including early estimates, without changing reports.
  # R round() uses the nearest integer, with exact halves rounded to even.
  # Priority group counts are capped after reconstruction and any HCW adjustment.
  result$before_cap <- result$before_rounding
  round_rows <- !result$origin %in% c("Reported", "Unavailable")
  result$before_cap[round_rows] <- round(result$before_cap[round_rows])
  result %>% mutate(
    estimate = if (cap_person_counts && indicator != "adm_tot_td")
      pmin(before_cap, population) else before_cap,
    estimate_per100 = 100 * estimate / population,
    before_cap_per100 = 100 * before_cap / population,
    rounded = !is.na(before_cap) & before_cap != before_rounding,
    capped = !is.na(estimate) & estimate != before_cap,
    days_since_left = as.numeric(target_date - left_date),
    days_until_right = as.numeric(right_date - target_date),
    interval_days = as.numeric(right_date - left_date),
    n_retained_anchors = nrow(anchors)
  )
}


# 3. Reconstruct the total population month end calendar -----

reconstruct_monthly_totals <- function(audit, targets) {
  source_groups <- split(audit, paste(audit$iso, audit$indicator))
  target_groups <- split(targets, paste(targets$iso, targets$indicator))
  bind_rows(lapply(names(target_groups), function(key) {
    target <- target_groups[[key]] %>% arrange(target_date) %>% mutate(target_index = row_number())
    source <- source_groups[[key]]
    if (is.null(source)) source <- audit[FALSE, ]
    estimate <- reconstruct_dated_series(source,
      target_dates = target$target_date, indicator = target$indicator[1],
      introduction_date = target$date_intro[1], population = target$pop_2021[1])
    estimate$target_index <- target$target_index
    target %>% left_join(estimate %>% select(-target_date), by = "target_index") %>%
      select(-target_index)
  }))
}

# End ---------------------------------------------------------------------
