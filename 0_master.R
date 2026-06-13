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

export_outputs <- TRUE

if (isTRUE(export_outputs) && !dir.exists(path_save)) {
  dir.create(path_save, recursive = TRUE)
}

export_plot <- function(...) {
  if (isTRUE(export_outputs)) {
    return(ggsave(...))
  }

  invisible(NULL)
}

export_docx <- function(...) {
  if (isTRUE(export_outputs)) {
    return(save_as_docx(...))
  }

  invisible(NULL)
}

# Read dated WHO API snapshots by default. Set COVID_VAX_USE_LIVE_API=true
# only when refreshing the raw API inputs for a new analysis snapshot.
read_who_api_json <- function(url, cache_file) {
  use_live_api <- identical(tolower(Sys.getenv("COVID_VAX_USE_LIVE_API")), "true")

  if (file.exists(cache_file) && !use_live_api) {
    message("Reading cached WHO API data: ", cache_file)
    return(fromJSON(cache_file, flatten = TRUE))
  }

  if (!use_live_api) {
    stop(
      "Cached WHO API file not found: ", cache_file, "\n",
      "Use the committed cache file, or set COVID_VAX_USE_LIVE_API=true ",
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


# Extract data ------------------------------------------------------------

# Load entity & population details
raw_entity_details <- data.frame(
  read.csv(file.path(path_input, "data_export_WIISE_REF_COUNTRIES.csv")))

raw_adhoc <- data.frame(
  read_excel(file.path(path_input, "base_adhoc.xlsx"), 
             sheet = "data"))

raw_population <- data.frame(
  read.csv(file.path(path_input, "data_export_WIISE_MT_REF_POPULATIONS.csv")))

# Load 2021-2023 uptake data
raw_data_acute_time <- data.frame(
  read_excel(file.path(path_input, "output_master_test.xlsx"),
             sheet = "1_stock")) 

raw_data_acute_time_daily <- data.frame(
  read_excel(file.path(path_input, "output_master_test.xlsx"),
             sheet = "1_adm_all_long"))

# Load 2024 policy data
raw_policy_wide <- data.frame(
  read_excel(file.path(path_input, "output_new.xlsx"),
             sheet = "policy_country_wide"))

# Load 2024 uptake data
raw_data_post_final <- data.frame(
  read_excel(file.path(path_input, "output_new.xlsx"),
             sheet = "uptake_grouping_wide_red"))

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
