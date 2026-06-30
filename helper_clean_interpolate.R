# Interpolate early cumulative uptake from programme introduction to first report.
interpolate_from_intro_wide_many <- function(df,
                                             vars,
                                             date_col         = "month_name",
                                             iso_col          = "a_iso",
                                             intro_col        = "date_intro",
                                             boost_reference_var = NULL,
                                             cps_delay_months = 2,
                                             month_anchor_day = 15,
                                             suffix           = "_interp") {
  # Coerce key date columns once before country-level interpolation.
  if (!inherits(df[[date_col]],  "Date"))  df[[date_col]]  <- as.Date(df[[date_col]])
  if (!inherits(df[[intro_col]], "Date"))  df[[intro_col]] <- as.Date(df[[intro_col]])

  if (length(month_anchor_day) != 1L ||
      is.na(month_anchor_day) ||
      month_anchor_day < 1L ||
      month_anchor_day > 28L) {
    stop("`month_anchor_day` must be a single integer between 1 and 28.")
  }
  month_anchor_day <- as.integer(month_anchor_day)

  # Month anchors allow introduction dates and monthly reports to share a scale.
  month_anchor <- function(x) {
    x <- as.Date(x)
    out <- as.Date(rep(NA_character_, length(x)))
    keep <- !is.na(x)
    out[keep] <- as.Date(sprintf(
      "%s-%02d",
      format(x[keep], "%Y-%m"),
      month_anchor_day
    ))
    out
  }
  
  # Resolve selected variables and stop early if required fields are absent.
  var_names <- if (is.character(vars)) vars else names(dplyr::select(df, {{ vars }}))
  missing_cols <- setdiff(
    c(iso_col, date_col, intro_col, boost_reference_var, var_names),
    names(df)
  )
  if (length(missing_cols)) stop("Missing columns in `df`: ", paste(missing_cols, collapse = ", "))
  if (!is.null(boost_reference_var) &&
      (length(boost_reference_var) != 1L ||
       is.na(boost_reference_var) ||
       !boost_reference_var %in% var_names)) {
    stop("`boost_reference_var` must identify one of the variables being interpolated.")
  }
  
  df %>%
    group_by(.data[[iso_col]]) %>%
    arrange(.data[[date_col]], .by_group = TRUE) %>%
    mutate(
      # Use a common within-month anchor for reported and introduction dates.
      interpolation_date = month_anchor(.data[[date_col]]),
      start_intro      = month_anchor(first(.data[[intro_col]])),
      start_intro_cps  = if_else(
        !is.na(start_intro),
        start_intro %m+% months(cps_delay_months),
        as.Date(NA)
      )
    ) %>%
    mutate(
      # Use the first submitted total-population booster report as the common
      # start proxy for interpolation of risk-group booster series.
      start_boost = if (is.null(boost_reference_var)) {
        as.Date(NA)
      } else {
        reference_values <- .data[[boost_reference_var]]
        reference_idx <- which(!is.na(reference_values))
        if (length(reference_idx)) {
          interpolation_date[reference_idx[1]]
        } else {
          as.Date(NA)
        }
      }
    ) %>%
    mutate(
      across(
        all_of(var_names),
        ~{
          var_name <- cur_column()
          v        <- .x
          
          # Choose the interpolation start date by dose type.
          start_vec <-
            if (!is.null(boost_reference_var) &&
                identical(var_name, boost_reference_var)) {
              # Preserve the observed total-population booster series.
              as.Date(NA)
            } else if (grepl("boost", var_name, ignore.case = TRUE)) {
              start_boost
            } else if (grepl("cps", var_name, ignore.case = TRUE)) {
              start_intro_cps
            } else if (grepl("a1d", var_name, ignore.case = TRUE)) {
              start_intro
            } else {
              start_intro
            }
          
          # Find the first observed value for this variable and country.
          idxs       <- which(!is.na(v))
          first_idx  <- if (length(idxs)) idxs[1] else NA_integer_
          first_val  <- if (is.na(first_idx)) NA_real_ else v[first_idx]
          first_date <- if (is.na(first_idx)) {
            as.Date(NA)
          } else {
            interpolation_date[first_idx]
          }
          
          # Build linear weights from start date to first report date.
          den <- as.numeric(as.Date(first_date) - as.Date(start_vec))
          num <- as.numeric(as.Date(interpolation_date) - as.Date(start_vec))
          w   <- ifelse(!is.na(den) & den > 0, pmax(0, pmin(1, num/den)), NA_real_)
          
          # Interpolate from zero at start date to the first observed value.
          ramp <- ifelse(!is.na(w), 0 + (first_val - 0) * w, NA_real_)
          
          # Use the ramp only before the first report; preserve observed values.
          case_when(
            is.na(start_vec) | is.na(first_date) ~ v,
            first_date <= start_vec              ~ v,
            interpolation_date < start_vec       ~ v,
            interpolation_date > first_date      ~ v,
            TRUE                                 ~ ramp
          )
        },
        .names = paste0("{.col}", suffix)
      )
    ) %>%
    # Drop temporary anchors after the interpolated variables are created.
    select(-interpolation_date, -start_intro, -start_intro_cps, -start_boost) %>%
    ungroup()
}
