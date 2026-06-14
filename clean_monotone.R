# clean_monotone.R
# Enforce non-decreasing cumulative series per country over time.
# - Dataframe in, dataframe out (original columns kept; cleaned copies get *_monotone).
# - Recursive scan with "depth" window; batch removal of consecutive offenders.
# - Post-fill method:
#     method = "set_na"        -> keep NA
#              "carry_forward" -> LOCF
#              "interpolate"   -> linear interpolation for interior gaps only
# - Interpolation is followed by a monotone clamp to prevent tiny downward steps.
# - Optional integer rounding with re-clamp.
# - A change log is attached as attr(out, "monotone_log").

suppressPackageStartupMessages({
  library(dplyr)
  library(rlang)
})

# ---------------- Var selection heuristic (edit if needed) ----------------
.default_vars_to_clean <- function(df) {
  nms <- names(df)
  sel <- grepl("^adm_tot_", nms) | grepl("_cap$", nms) | grepl("^cov_", nms, ignore.case = TRUE)
  nms[sel]
}

# ---------------- NA-safe neighbors & evidence counters ----------------
.prev_non_na_idx <- function(y, i) {
  j <- i - 1L
  while (j >= 1L && is.na(y[j])) j <- j - 1L
  if (j >= 1L) j else NA_integer_
}
.next_non_na_idx <- function(y, i) {
  n <- length(y); j <- i + 1L
  while (j <= n && is.na(y[j])) j <- j + 1L
  if (j <= n) j else NA_integer_
}
.count_prev_larger_nonconsec <- function(y, i_prev, threshold, depth) {
  cnt <- 0L; j <- i_prev
  while (!is.na(j) && j >= 1L && cnt < depth) {
    v <- y[j]
    if (!is.na(v) && !is.na(threshold) && v > threshold) cnt <- cnt + 1L else break
    j <- .prev_non_na_idx(y, j)
  }
  cnt
}
.count_after_smaller_nonconsec <- function(y, i_curr, threshold, depth) {
  cnt <- 0L; j <- .next_non_na_idx(y, i_curr)
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
  changed_idx <- integer(0); reps <- 0L; j <- i_prev_start
  while (!is.na(j) && j >= 1L && reps < max(1L, depth)) {
    if (!is.na(y[j]) && !is.na(threshold) && y[j] > threshold) {
      y[j] <- NA_real_; changed_idx <- c(changed_idx, j); reps <- reps + 1L; j <- .prev_non_na_idx(y, j)
    } else break
  }
  list(y = y, changed_idx = changed_idx)
}
.remove_after_run <- function(y, i_curr, threshold, depth) {
  changed_idx <- integer(0); reps <- 0L; j <- .next_non_na_idx(y, i_curr)
  while (!is.na(j) && reps < max(1L, depth)) {
    if (!is.na(y[j]) && !is.na(threshold) && y[j] < threshold) {
      y[j] <- NA_real_; changed_idx <- c(changed_idx, j); reps <- reps + 1L; j <- .next_non_na_idx(y, j)
    } else break
  }
  list(y = y, changed_idx = changed_idx)
}

# ---------------- Core recursive fixer (marks offenders as NA) ----------------
.fix_series_recursive <- function(x, depth = 3, batch = TRUE) {
  y <- as.numeric(x); n <- length(y); changed <- rep(FALSE, n)
  if (n <= 1) return(list(y = y, changed = changed))
  
  repeat {
    acted <- FALSE
    for (i in 2:n) {
      if (is.na(y[i])) next
      p <- .prev_non_na_idx(y, i); if (is.na(p)) next
      a <- y[p]; b <- y[i]
      if (!is.na(a) && !is.na(b) && a > b) {
        remove_prev_side <- FALSE
        pprev <- .prev_non_na_idx(y, p)
        prev_larger <- .count_prev_larger_nonconsec(y, p, b, depth)
        after_smaller <- .count_after_smaller_nonconsec(y, i, a, depth)
        # Sustained later lower reports are evidence that the preceding high
        # run was the anomaly, including isolated first reports.
        if (!is.na(pprev) && !is.na(y[pprev]) && y[pprev] > b) {
          remove_prev_side <- after_smaller >= prev_larger
        } else if (!is.na(pprev) && !is.na(y[pprev]) && y[pprev] <= b) {
          remove_prev_side <- after_smaller >= min(2L, depth) ||
            (after_smaller >= 1L && .is_close_to_prior(b, y[pprev]))
        } else if (is.na(pprev)) {
          remove_prev_side <- after_smaller >= min(2L, depth)
        }
        if (remove_prev_side) {
          rm <- .remove_prev_run(y, p, threshold = b, depth = depth)
          y <- rm$y; if (length(rm$changed_idx)) changed[rm$changed_idx] <- TRUE
        } else {
          rm <- .remove_after_run(y, p, threshold = a, depth = depth)
          y <- rm$y; if (length(rm$changed_idx)) changed[rm$changed_idx] <- TRUE
        }
        acted <- TRUE; break
      }
    }
    if (!acted) break
  }
  list(y = y, changed = changed)
}

# ---------------- Post-fill methods (with monotone clamp) ----------------
# Monotone clamp (ensures non-decreasing across non-NA values)
.enforce_monotone_post <- function(y) {
  res <- as.numeric(y); last <- NA_real_
  for (i in seq_along(res)) {
    if (!is.na(res[i])) {
      if (!is.na(last) && res[i] < last) res[i] <- last
      last <- res[i]
    }
  }
  res
}

# LOCF
.ffill_vec <- function(y) {
  last <- NA_real_
  for (i in seq_along(y)) {
    if (!is.na(y[i])) last <- y[i] else if (!is.na(last)) y[i] <- last
  }
  y
}

# Linear interpolation for interior gaps only; edges remain NA
.interpolate_internal <- function(y) {
  n <- length(y); if (all(is.na(y))) return(y)
  x <- seq_len(n); good <- which(!is.na(y))
  if (length(good) < 2) return(y)
  yi <- y
  yi[is.na(yi)] <- approx(x = good, y = y[good], xout = x, rule = 1)$y[is.na(y)]
  yi
}

.apply_fill_method <- function(y, method = c("set_na","carry_forward","interpolate"), round_int = FALSE) {
  method <- match.arg(method)
  out <- switch(method,
                set_na        = y,
                carry_forward = .enforce_monotone_post(.ffill_vec(y)),
                interpolate   = .enforce_monotone_post(.interpolate_internal(y))
  )
  if (round_int) {
    out <- round(out)
    out <- .enforce_monotone_post(out)
  }
  out
}

# Safe inequality for logging
.neq <- function(a, b) {
  (is.na(a) & !is.na(b)) | (!is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a != b)
}

# ---------------- Public API ----------------
clean_coverage_monotone_df <- function(
    df,
    vars_to_clean  = NULL,
    id_iso         = "a_iso",
    id_time        = "month_name",
    suffix         = "_monotone",
    depth          = 3,
    batch          = TRUE,
    method         = c("set_na","carry_forward","interpolate"),
    round_int      = FALSE,     # set TRUE for count series
    return_log     = TRUE
) {
  method <- match.arg(method)
  
  if (!inherits(df[[id_time]], "Date")) {
    suppressWarnings(df[[id_time]] <- as.Date(df[[id_time]]))
  }
  if (is.null(vars_to_clean)) vars_to_clean <- .default_vars_to_clean(df)
  message("Cleaning variables (depth=", depth, ", batch=", batch,
          ", method=", method, ", round_int=", round_int, "): ",
          paste(vars_to_clean, collapse = ", "))
  
  for (v in vars_to_clean) {
    if (!is.numeric(df[[v]])) suppressWarnings(df[[v]] <- as.numeric(df[[v]]))
  }
  
  base <- df %>% arrange(.data[[id_iso]], .data[[id_time]])
  logs <- list()
  
  for (v in vars_to_clean) {
    v_clean <- paste0(v, suffix)
    
    res <- base %>%
      group_by(.data[[id_iso]]) %>%
      group_modify(~{
        s_orig <- .x[[v]]
        # 1) mark offenders as NA (recursive)
        tmp <- .fix_series_recursive(s_orig, depth = depth, batch = batch)
        s_marked <- tmp$y
        # 2) post-fill per method (+ monotone clamp; + optional rounding)
        s_final <- .apply_fill_method(s_marked, method = method, round_int = round_int)
        
        .x[[v_clean]]    <- s_final
        .x$.__changed__  <- .neq(s_orig, s_final)
        .x$.__old_value  <- s_orig
        .x$.__new_value  <- s_final
        .x
      }) %>% ungroup()
    
    base[[v_clean]] <- res[[v_clean]]
    
    if (return_log) {
      idx <- which(res$.__changed__)
      if (length(idx)) {
        logs[[v]] <- tibble::tibble(
          variable  = v,
          !!id_iso  := res[[id_iso]][idx],
          !!id_time := res[[id_time]][idx],
          old_value = res$.__old_value[idx],
          new_value = res$.__new_value[idx],
          rule      = paste0("recursive_monotone -> ", method,
                             " (depth=", depth, ", batch=", batch,
                             ", round_int=", round_int, ")")
        )
      }
    }
    
    base$.__changed__ <- NULL
    base$.__old_value <- NULL
    base$.__new_value <- NULL
  }
  
  out <- df %>%
    select(all_of(c(id_iso, id_time))) %>%
    left_join(base %>% select(all_of(c(id_iso, id_time)), ends_with(suffix)),
              by = c(id_iso, id_time)) %>%
    left_join(df, by = c(id_iso, id_time)) %>%
    relocate(all_of(c(id_iso, id_time)), .before = 1)
  
  if (return_log) {
    attr(out, "monotone_log") <- if (length(logs)) dplyr::bind_rows(logs) else tibble::tibble()
  }
  out
}

# ---------------- Example (commented) ----------------
# df <- readxl::read_excel("coverage.xlsx", sheet = "Sheet1")
# df_clean <- clean_coverage_monotone_df(
#   df,
#   id_iso    = "a_iso",
#   id_time   = "month_name",
#   depth     = 6,
#   batch     = TRUE,
#   method    = "interpolate",   # "set_na" | "carry_forward" | "interpolate"
#   round_int = TRUE             # set TRUE for count variables
# )
# attr(df_clean, "monotone_log") %>% dplyr::arrange(a_iso, month_name, variable)
