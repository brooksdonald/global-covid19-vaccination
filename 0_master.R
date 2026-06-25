# COVID-19 vaccination analysis
# Maintainer: BROOKS, Donald J. 
# See README.md for manuscript and citation details

# Setup -------------------------------------------------------------------

# Clear environment
rm(list = ls())
gc()
options(scipen = 999)

# Load packages

lib <- c("tidyverse", "readxl", "writexl", "lubridate", "countrycode",
         "data.table", "httr", "scales", "jsonlite", "ggpattern", "ggrepel", 
         "scales", "treemap", "ggtext", "ggforce", "tibble", "kableExtra", 
         "knitr", "ordering", "shadowtext", "forcats", "flextable", "officer", 
         "rlang", "cowplot", "patchwork" )

lapply(lib, library, character.only = TRUE)


# Global switches ---------------------------------------------------------

# TRUE: write figures, tables, and audit outputs to the output directory.
# FALSE: run the complete analysis without overwriting exported files.
export_outputs <- TRUE

# FALSE: use the committed, dated WHO API snapshots in the input directory.
# TRUE: download fresh API data and overwrite the cached snapshots.
refresh_api <- FALSE


# Process & design variables ----------------------------------------------

# Locate the repository so the script can be run from RStudio or terminal

get_project_path <- function() {
  script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(script_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", script_arg[[1]]))))
  }

  source_files <- vapply(
    sys.frames(),
    function(frame) {
      if (!is.null(frame$ofile)) frame$ofile else NA_character_
    },
    character(1)
  )
  source_files <- source_files[!is.na(source_files)]
  if (length(source_files) > 0) {
    return(dirname(normalizePath(tail(source_files, 1))))
  }

  if (file.exists("0_master.R")) {
    return(normalizePath(getwd()))
  }

  stop("Cannot locate the project directory. Run source('0_master.R') from the covid_vaccination folder, or run Rscript with the path to 0_master.R.")
}

path_project <- get_project_path()
path_input <- file.path(path_project, "input")
path_save <- file.path(path_project, "output")

# Central audit log for manual exclusions, replacements, and reporting-basis
# decisions used across manuscript analyses.
path_data_corrections <- file.path(path_input, "data_corrections.csv")
stopifnot(file.exists(path_data_corrections))
data_corrections <- read.csv(
  path_data_corrections,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)
stopifnot(
  !anyDuplicated(data_corrections$correction_id),
  all(!is.na(data_corrections$correction_id))
)

if (isTRUE(export_outputs) && !dir.exists(path_save)) {
  dir.create(path_save, recursive = TRUE)
}

export_docx <- function(...) {
  if (isTRUE(export_outputs)) {
    return(save_as_docx(...))
  }

  invisible(NULL)
}

figure_specs <- list(
  square = list(width = 7, height = 7),
  wide = list(width = 10, height = 6),
  wide_tall = list(width = 10, height = 7),
  double_wide = list(width = 11, height = 7),
  double_tall = list(width = 11, height = 8),
  extra_wide = list(width = 13, height = 7),
  tall = list(width = 9, height = 11)
)

export_figure <- function(filename, plot, size = "wide_tall", width = NULL,
                          height = NULL, dpi = 300, path = path_save) {
  if (!isTRUE(export_outputs)) {
    return(invisible(NULL))
  }

  if (!size %in% names(figure_specs)) {
    stop("Unknown figure size: ", size, call. = FALSE)
  }

  spec <- figure_specs[[size]]
  if (is.null(width)) width <- spec$width
  if (is.null(height)) height <- spec$height

  ggsave(
    path = path,
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
}

# Read dated WHO API snapshots by default. Set refresh_api <- TRUE only when
# refreshing the raw API inputs for a new analysis snapshot.
read_who_api_json <- function(url, cache_file) {
  if (file.exists(cache_file) && !isTRUE(refresh_api)) {
    message("Reading cached WHO API data: ", cache_file)
    return(fromJSON(cache_file, flatten = TRUE))
  }

  if (!isTRUE(refresh_api)) {
    stop(
      "Cached WHO API file not found: ", cache_file, "\n",
      "Use the committed cache file, or set refresh_api <- TRUE ",
      "to fetch a live copy and overwrite the cache.",
      call. = FALSE
    )
  }

  message("Fetching live WHO API data: ", url)
  response <- GET(
    url,
    add_headers(`Cache-Control` = "no-cache"),
    config = config(fresh_connect = TRUE)
  )
  stop_for_status(response)

  response_text <- content(response, "text", encoding = "UTF-8")
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(response_text, cache_file, useBytes = TRUE)

  fromJSON(response_text, flatten = TRUE)
}

# Theme-related settings
theme_master <- theme_test() +
  theme(
    plot.title = element_text(face = "bold", vjust = 3, size = 14),
    axis.title.x = element_text(vjust = -2, size = 12),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.title.y = element_text(vjust = 4, size = 12),
    axis.text.y = element_text(size = 10, color = "black"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )

levels_standard <- c("HIC", "UMIC", "LMIC", "LIC",
                     "AFR", "AMR", "EMR", "EUR", "SEAR", "WPR",
                     "AMC", "Self-financing", "Non-COVAX", "World")

levels_income <- c("HIC", "UMIC", "LMIC", "LIC")

levels_region <- c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")

levels_groups_short <- c("Total population", "Older adults", 
                         "Health and care workers")

levels_indicator <- c("At least one dose", "Complete primary series",
                      "At least one booster dose")

# Color-related settings
source(file.path(path_project, "a_design specifications.R"))

palette_3 <- c(color_who_cat_base_1, color_who_cat_base_2,
                   color_who_cat_base_3)

palette_4 <- c(color_who_cat_base_1, color_who_cat_base_2, 
                    color_who_cat_base_3, color_who_cat_base_4)

palette_5 <- c(color_who_cat_base_1, color_who_cat_base_2,
               color_who_cat_base_3, color_who_cat_base_4,
               color_who_cat_base_5)

palette_region <- c(color_who_afr, color_who_amr, color_who_emr, color_who_eur,
                    color_who_sear, color_who_wpr)

pal_dose <- setNames(palette_3, levels_indicator)
pal_dose_compare <- c("Complete primary series" = color_who_seq_1,
                      "At least one booster dose" = color_who_cat_base_1)
pal_income <- setNames(palette_4, levels_income)
pal_income_long <- setNames(palette_4,
                            c("High income", "Upper middle income",
                              "Lower middle income", "Low income"))
pal_region <- setNames(palette_region, levels_region)
pal_covax <- c("AMC" = color_who_cat_base_1,
               "Self-financing" = color_who_cat_base_2,
               "Non-COVAX" = color_who_cat_base_3,
               "World" = color_who_seq_1)
pal_income_world <- c(pal_income, "World" = color_who_seq_1)
pal_region_world <- c(pal_region, "World" = color_who_seq_1)
pal_policy <- palette_4
pal_reporting <- c("Complete primary series" = color_who_diverg_1,
                   "Booster" = color_who_diverg_5)
pal_reporting_fill <- c("Complete primary series" = color_who_diverg_2,
                        "Booster" = color_who_diverg_4)
pal_dvr_all <- c("Global" = color_who_seq_1,
                 pal_region,
                 pal_income)

label_axis_coverage <- "Vaccination coverage (% of population)"
label_axis_admin_per100 <- "Doses administered (per 100 population)"
label_axis_income <- "Income group"
label_axis_date <- "Date"
label_axis_member_states_n <- "Number of WHO Member States"
label_axis_member_states_pct <- "Percentage of WHO Member States"
label_legend_income <- "Income group"
label_source_reporting <- paste(
  "Data source: WHO-UNICEF electronic Joint Reporting Form COVID-19 module",
  "and WHO regional reporting systems"
)

panel_tag_theme <- theme(
  plot.tag = element_text(face = "bold", size = 10, color = "black")
)


# Extract data ------------------------------------------------------------

# Load entity & population details
raw_entity_details <- data.frame(
  read.csv(file.path(path_input, "data_export_WIISE_REF_COUNTRIES.csv")))

raw_adhoc <- data.frame(
  read_excel(file.path(path_input, "base_adhoc.xlsx"), 
             sheet = "data"))

raw_population <- data.frame(
  read.csv(file.path(path_input, "data_export_WIISE_MT_REF_POPULATIONS.csv")))

# Load 2021-2023 daily uptake data for daily vaccination rate analysis
raw_data_acute_time_daily <- data.frame(
  read_excel(file.path(path_input, "output_master_test.xlsx"),
             sheet = "1_adm_all_long"))

# Load 2024 policy data
raw_policy_wide <- data.frame(
  read_excel(file.path(path_input, "output_new.xlsx"),
             sheet = "policy_country_wide"))

# The 2024 uptake analysis is reconstructed from frozen source extracts in
# 7_routine coverage analysis.R. output_new.xlsx is retained here only as the
# source of the 2024 policy data above.

# Load uncleaned risk group uptake data
raw_target_json <- read_who_api_json(
  url = "https://xmart-api-public.who.int/WIISE/MT_COV_UPTAKE_TARGETGROUP_LONG",
  cache_file = file.path(path_input,
                         "2026-06-12_MT_COV_UPTAKE_TARGETGROUP_LONG.json")
)
base_target <- as.data.frame(raw_target_json)
names(base_target) <- substring(names(base_target), 7)

# Load uncleaned total pop. uptake data
raw_weekly_json <- read_who_api_json(
  url = "https://xmart-api-public.who.int/WIISE/COV_UPTAKE_WEEKLY",
  cache_file = file.path(path_input,
                         "2026-06-12_COV_UPTAKE_WEEKLY.json")
)
base_total <- as.data.frame(raw_weekly_json)
names(base_total) <- substring(names(base_total), 7)


# Load analyses -----------------------------------------------------------

source(file.path(path_project, "1_entity_pop.R"))
source(file.path(path_project, "2_data cleaning.R"))
source(file.path(path_project, "3_static values.R"))
source(file.path(path_project, "4_acute coverage analysis.R"))
source(file.path(path_project, "5_acute daily vaccination rate analysis.R"))
source(file.path(path_project, "6_routine policy analysis.R"))
source(file.path(path_project, "7_routine coverage analysis.R"))
source(file.path(path_project, "8_reporting analysis.R"))



# Create figures and tables ----------------------------------------------

source(file.path(path_project, "9_figures.R"))
source(file.path(path_project, "10_tables.R"))


# End ---------------------------------------------------------------------
