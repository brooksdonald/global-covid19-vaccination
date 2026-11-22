# Contextual exclusion and preparation of reported uptake
# Maintainer: BROOKS, Donald J.
# Run through 0_master.R.

# 1. Locate surrounding observations and remove conflicting runs -----------

# Locate available values before and after each observation, skipping missing values.
.prev_non_na_idx <- function(y, i) {
  j <- i - 1L
  while (j >= 1L && is.na(y[j])) j <- j - 1L
  if (j >= 1L) j else NA_integer_
}
.next_non_na_idx <- function(y, i) {
  n <- length(y)
  j <- i + 1L
  while (j <= n && is.na(y[j])) j <- j + 1L
  if (j <= n) j else NA_integer_
}
.count_prev_larger_nonconsec <- function(y, i_prev, threshold, depth) {
  cnt <- 0L
  j <- i_prev
  while (!is.na(j) && j >= 1L && cnt < depth) {
    v <- y[j]
    if (!is.na(v) && !is.na(threshold) && v > threshold) cnt <- cnt + 1L else break
    j <- .prev_non_na_idx(y, j)
  }
  cnt
}
.count_after_smaller_nonconsec <- function(y, i_curr, threshold, depth) {
  cnt <- 0L
  j <- .next_non_na_idx(y, i_curr)
  while (!is.na(j) && cnt < depth) {
    v <- y[j]
    if (!is.na(v) && !is.na(threshold) && v < threshold) cnt <- cnt + 1L else break
    j <- .next_non_na_idx(y, j)
  }
  cnt
}
.is_close_to_prior <- function(current, prior, rel_tol = 0.001) {
  if (is.na(current) || is.na(prior)) return(FALSE)
  abs(current - prior) <= max(1, abs(prior) * rel_tol)
}
.remove_prev_run <- function(y, i_prev_start, threshold, depth) {
  changed_idx <- integer(0)
  reps <- 0L
  j <- i_prev_start
  while (!is.na(j) && j >= 1L && reps < max(1L, depth)) {
    if (!is.na(y[j]) && !is.na(threshold) && y[j] > threshold) {
      y[j] <- NA_real_
      changed_idx <- c(changed_idx, j)
      reps <- reps + 1L
      j <- .prev_non_na_idx(y, j)
    } else break
  }
  list(y = y, changed_idx = changed_idx)
}
.remove_after_run <- function(y, i_curr, threshold, depth) {
  changed_idx <- integer(0)
  reps <- 0L
  j <- .next_non_na_idx(y, i_curr)
  while (!is.na(j) && reps < max(1L, depth)) {
    if (!is.na(y[j]) && !is.na(threshold) && y[j] < threshold) {
      y[j] <- NA_real_
      changed_idx <- c(changed_idx, j)
      reps <- reps + 1L
      j <- .next_non_na_idx(y, j)
    } else break
  }
  list(y = y, changed_idx = changed_idx)
}

# 2. Apply contextual exclusion and record each decision ------------------

contextual_exclusion <- function(values, dates, depth = Inf) {
  # Assess all available observations: dated reports for the total population
  # and monthly reports for priority groups.
  stopifnot(length(depth) == 1L, !is.na(depth), depth >= 1,
            is.infinite(depth) || depth == floor(depth),
            length(values) == length(dates), !anyNA(dates), all(diff(dates) > 0))
  y <- as.numeric(values)
  decisions <- list()
  round_number <- 0L
  repeat {
    available <- which(!is.na(y))
    decrease <- which(diff(y[available]) < 0)
    if (!length(decrease)) break
    # Assess the first retained decrease; repeat from the start after exclusion.
    p <- available[decrease[1]]
    i <- available[decrease[1] + 1L]
    a <- y[p]
    b <- y[i]
    pprev <- .prev_non_na_idx(y, p)
    # Earlier support includes the preceding high value; later support counts
    # subsequent values below it, excluding the first lower observation.
    prev_larger <- .count_prev_larger_nonconsec(y, p, b, depth)
    after_smaller <- .count_after_smaller_nonconsec(y, i, a, depth)
    # Prefer the sustained side of the sequence, considering whether the new
    # lower value is compatible with the value preceding the high observation.
    remove_earlier <- if (!is.na(pprev) && y[pprev] > b) {
      after_smaller >= prev_larger
    } else if (!is.na(pprev)) {
      after_smaller >= min(2L, depth) ||
        (after_smaller >= 1L && .is_close_to_prior(b, y[pprev]))
    } else after_smaller >= min(2L, depth)
    removal <- if (remove_earlier) .remove_prev_run(y, p, b, depth) else
      .remove_after_run(y, p, a, depth)
    removed <- removal$changed_idx
    stopifnot(length(removed) > 0L)
    round_number <- round_number + 1L
    decisions[[round_number]] <- tibble::tibble(
      round = round_number, high_index = p, low_index = i,
      high_date = dates[p], low_date = dates[i], high_value = a, low_value = b,
      earlier_support = prev_larger, later_support = after_smaller,
      removed_side = if (remove_earlier) "earlier_high" else "later_low",
      removed_index = removed, removed_date = dates[removed], removed_value = y[removed])
    y <- removal$y
    stopifnot(round_number <= sum(!is.na(values)))
  }
  list(values = y, decisions = bind_rows(decisions))
}

# Identify changes in value or missingness for the audit log.
reported_value_changed <- function(a, b) {
  (is.na(a) & !is.na(b)) | (!is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a != b)
}

# 3. Apply the common count treatment sequence ----------------------------

# Use the same exclusions for total population, older adult and HCW counts.
treat_count_series <- function(values, dates) {
  flags <- source_value_flags(values, dates)
  after_negative <- as.numeric(values)
  after_negative[!is.na(after_negative) & after_negative < 0] <- NA_real_
  after_zero <- after_negative
  after_zero[flags$zero_after_positive] <- NA_real_
  downward <- contextual_exclusion(after_zero, dates)
  list(after_negative = after_negative, after_zero = after_zero,
       after_downward = downward$values, flags = flags,
       decisions = downward$decisions)
}

# 4. Prepare dated total population observations --------------------------

# Equal values on the same country, indicator and date contribute one anchor.
# Conflicting values contribute no anchor. The mapping retains every submitted
# value, including duplicates, conflicts and originally missing fields.
# Record whether each value appeared in the latest whole report that month.
# All dated reports remain eligible for treatment.
prepare_dated_reports <- function(reports, population) {
  reports <- reports %>% arrange(iso, date, source_row) %>%
    mutate(month_name = floor_date(date, "month")) %>%
    group_by(iso, month_name) %>% mutate(selected_for_month = row_number() == n()) %>% ungroup()
  long <- reports %>% select(iso, date, month_name, source_row, source_id,
                             selected_for_month, all_of(vars_admin)) %>%
    pivot_longer(all_of(vars_admin), names_to = "indicator", values_to = "reported")
  canonical <- long %>% group_by(iso, date, indicator) %>%
    summarise(n_values = sum(!is.na(reported)),
      n_distinct_values = n_distinct(reported, na.rm = TRUE),
      value = if (n_distinct_values == 1L) first(reported[!is.na(reported)]) else NA_real_,
      source_id = if (any(!is.na(reported))) first(source_id[!is.na(reported)]) else first(source_id),
      source_row = if (any(!is.na(reported))) first(source_row[!is.na(reported)]) else first(source_row),
      selected = any(selected_for_month & !is.na(reported)), .groups = "drop") %>%
    mutate(conflict = n_distinct_values > 1L, selected = selected & !conflict)
  mapping <- long %>% left_join(canonical %>%
    select(iso, date, indicator, canonical_value = value, n_values, n_distinct_values, conflict),
    by = c("iso", "date", "indicator")) %>%
    group_by(iso, date, indicator) %>% mutate(
      preparation_action = case_when(is.na(reported) ~ "Originally missing",
        conflict ~ "Conflicting values excluded as anchors",
        duplicated(reported) ~ "Identical value collapsed", TRUE ~ "Canonical observation")) %>% ungroup()
  dates <- reports %>% group_by(iso, date, month_name) %>%
    summarise(source_row = first(source_row), source_id = first(source_id),
      selected_for_month = any(selected_for_month), .groups = "drop")
  wide <- dates %>%
    left_join(canonical %>% select(iso, date, indicator, value) %>%
      pivot_wider(names_from = indicator, values_from = value), by = c("iso", "date")) %>%
    left_join(canonical %>% select(iso, date, indicator, selected, source_id, source_row) %>%
      pivot_wider(names_from = indicator, values_from = c(selected, source_id, source_row)), by = c("iso", "date")) %>%
    left_join(population %>% select(iso, pop_2021), by = "iso")

  # Complementary partial reports on one date cannot create an assessed pair.
  # Where a value conflicts, that date cannot support an indicator pair check.
  for (j in 2:4) {
    lower <- vars_admin[j]
    upper <- vars_admin[j - 1L]
    pair <- reports %>% group_by(iso, date) %>%
      summarise(pair_available = any(!is.na(.data[[lower]]) & !is.na(.data[[upper]])), .groups = "drop")
    wide <- wide %>% left_join(pair, by = c("iso", "date"))
    wide[[paste0("pair_available_", lower)]] <- wide$pair_available &
      !is.na(wide[[lower]]) & !is.na(wide[[upper]])
    wide$pair_available <- NULL
  }
  list(reports = wide %>% arrange(iso, date), mapping = mapping)
}

# 5. Describe reported values before treatment ----------------------------

# Missing observations neither confirm nor end an unchanged sequence. Its
# duration starts at the first equal available value, using actual report dates.
# Flags are calculated before treatment. Repetition and rate flags are retained
# as descriptive audit fields and do not trigger exclusions in this analysis.
source_value_flags <- function(values, dates) {
  stopifnot(length(values) == length(dates), !anyNA(dates), all(diff(dates) > 0))
  n <- length(values)
  zero <- repeat_3 <- repeat_6 <- rep(FALSE, n)
  start <- as.Date(rep(NA_character_, n))
  available <- which(!is.na(values))
  if (length(available)) {
    observed <- values[available]
    new_run <- c(TRUE, diff(observed) != 0)
    start[available] <- dates[available[new_run]][cumsum(new_run)]
    positive_before <- c(FALSE, head(cumsum(observed > 0) > 0, -1L))
    zero[available] <- observed == 0 & positive_before
    # Add calendar months, using the final day where the target month is shorter.
    repeat_3[available] <- dates[available] >=
      lubridate::add_with_rollback(start[available], lubridate::period(3, "month"))
    repeat_6[available] <- dates[available] >=
      lubridate::add_with_rollback(start[available], lubridate::period(6, "month"))
  }
  tibble::tibble(zero_after_positive = zero, repeat_3 = repeat_3,
                 repeat_6 = repeat_6, run_start = start)
}

# Rates use successive available observations and the full elapsed interval.
# There is no artificial zero before the first observation and no interpolation.
reported_transitions <- function(values, dates, population) {
  available <- which(!is.na(values))
  later <- if (length(available) > 1L) available[-1L] else integer()
  earlier <- if (length(available) > 1L) head(available, -1L) else integer()
  days <- as.numeric(dates[later] - dates[earlier])
  # Retain count changes for territories without population denominators.
  # Their rates per 100 population remain missing.
  stopifnot(all(days > 0), length(population) == 1L,
            is.na(population) || (is.finite(population) && population > 0))
  tibble::tibble(earlier_index = earlier, later_index = later,
                 earlier_date = dates[earlier], later_date = dates[later],
                 earlier_value = values[earlier], later_value = values[later],
                 interval_days = days, change = values[later] - values[earlier],
                 rate_per100_day = 100 * (values[later] - values[earlier]) / population / days)
}


# 6. Treat dated total population reports ---------------------------------

treat_dated_reports <- function(reports) {
  reports <- reports %>% arrange(date, source_row)
  stopifnot(!anyDuplicated(reports$date), all(diff(reports$date) > 0),
            n_distinct(reports$pop_2021) == 1L)
  original <- as.matrix(reports[vars_admin])
  stopifnot(all(is.na(original) | is.finite(original)))
  n <- nrow(reports)
  treated <- lapply(seq_along(vars_admin), function(j) {
    treat_count_series(original[, j], reports$date)
  })
  flags <- lapply(seq_along(vars_admin), function(j) {
    result <- treated[[j]]$flags
    transitions <- reported_transitions(original[, j], reports$date, reports$pop_2021[1])
    result$original_previous_value <- result$original_rate_per100_day <- rep(NA_real_, n)
    result$original_previous_date <- as.Date(rep(NA_character_, n))
    result$original_previous_value[transitions$later_index] <- transitions$earlier_value
    result$original_previous_date[transitions$later_index] <- transitions$earlier_date
    result$original_rate_per100_day[transitions$later_index] <- transitions$rate_per100_day
    result$original_downward <- ifelse(is.na(result$original_previous_value), NA,
                                       original[, j] < result$original_previous_value)
    result$original_negative <- ifelse(is.na(original[, j]), NA, original[, j] < 0)
    result$original_population_exceedance <- if (j == 1L) rep(NA, n) else
      ifelse(is.na(original[, j]), NA, original[, j] > reports$pop_2021[1])
    result$original_interindicator <- if (j == 1L) rep(NA, n) else
      ifelse(is.na(original[, j]) | is.na(original[, j - 1L]), NA, original[, j] > original[, j - 1L])
    pair_column <- paste0("pair_available_", vars_admin[j])
    if (pair_column %in% names(reports)) {
      result$original_interindicator[!reports[[pair_column]]] <- NA
    }
    result
  })
  stages <- list(reported = original)
  for (stage in c("after_negative", "after_zero")) {
    current <- original
    for (j in seq_along(vars_admin)) current[, j] <- treated[[j]][[stage]]
    stages[[stage]] <- current
  }
  current <- stages$after_zero

  # Treat each indicator independently; exclude downward changes after the
  # negative-count and zero rules have established the available observations.
  downward <- list()
  for (j in seq_along(vars_admin)) {
    result <- list(values = treated[[j]]$after_downward, decisions = treated[[j]]$decisions)
    current[, j] <- result$values
    row_column <- paste0("source_row_", vars_admin[j])
    original_rows <- if (row_column %in% names(reports)) reports[[row_column]] else reports$source_row
    if (nrow(result$decisions)) downward[[j]] <- result$decisions %>%
      mutate(indicator = vars_admin[j], source_row = original_rows[removed_index],
             high_source_row = original_rows[high_index], low_source_row = original_rows[low_index])
  }
  stages$after_downward <- current
  # No further count exclusions are applied for repetition, rapid increases
  # or population exceedances. Person coverage is capped after reconstruction.

  # Retain missing source values in the audit so they can be distinguished
  # from reported values excluded during treatment.
  audit <- reports[rep(seq_len(n), 4L), c("iso", "date", "month_name", "source_row", "source_id",
                                        "pop_2021", "selected_for_month")] %>%
    mutate(indicator = rep(vars_admin, each = n))
  # Attach source rows, report identifiers and latest report flags by indicator.
  if (all(paste0("selected_", vars_admin) %in% names(reports))) {
    audit$selected_for_month <- as.vector(as.matrix(reports[paste0("selected_", vars_admin)]))
    audit$source_id <- as.vector(as.matrix(reports[paste0("source_id_", vars_admin)]))
    if (all(paste0("source_row_", vars_admin) %in% names(reports))) {
      audit$source_row <- as.vector(as.matrix(reports[paste0("source_row_", vars_admin)]))
    }
  }
  for (stage in names(stages)) audit[[stage]] <- as.vector(stages[[stage]])
  audit <- bind_cols(audit, bind_rows(flags)) %>%
    mutate(excluded = !is.na(reported) & is.na(after_downward),
           modified = !is.na(reported) & !is.na(after_downward) & reported != after_downward,
           action = case_when(is.na(reported) ~ "Originally missing", excluded ~ "Excluded",
                              modified ~ "Modified", TRUE ~ "Retained"))

  # Record each exclusion at the stage where it occurs. Values already excluded
  # are not counted again; retained values are recorded in the full audit above.
  events <- bind_rows(lapply(seq.int(2L, length(stages)), function(k) {
    before <- as.vector(stages[[k - 1L]])
    after <- as.vector(stages[[k]])
    changed <- reported_value_changed(before, after)
    audit[changed, c("iso", "indicator", "date", "source_row", "source_id")] %>%
      mutate(stage = names(stages)[k], value_before = before[changed], value_after = after[changed],
             action = if_else(is.na(value_after), "Excluded", "Modified"))
  }))
  list(audit = audit, events = events, downward = bind_rows(downward))
}


# The selected method excludes observations without modifying retained counts.
# Summarize actions once per reported value for the source preservation audit.
summarize_treatment_actions <- function(audit) {
  audit %>% mutate(observed = !is.na(reported), changed = excluded | modified,
    modification_per100 = if_else(modified,
      100 * abs(after_downward - reported) / pop_2021, NA_real_))
}

# End ---------------------------------------------------------------------
