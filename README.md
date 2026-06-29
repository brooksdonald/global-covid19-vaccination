# COVID-19 vaccination analysis

This repository contains the R code and input files used to generate the tables
and figures for the manuscript:

> Global COVID-19 vaccination uptake across the emergency and early 
> post-emergency periods, 2021-2024

The analysis summarizes COVID-19 vaccination coverage in the total population,
older adults, and health and care workers, with stratification by income group,
WHO region, COVAX Facility participation category, and policy category.

## Repository Structure

- `0_master.R`: Main analysis script. Run this file to reproduce outputs.
- `1_entity_pop.R`: Country, grouping, and population preparation.
- `2_data cleaning.R`: Administrative data cleaning and interpolation.
- `3_static values.R`: Static summary values used in the analysis.
- `4_acute coverage analysis.R`: 2021-2023 coverage analysis.
- `5_acute daily vaccination rate analysis.R`: Daily vaccination rate analysis.
- `6_routine policy analysis.R`: 2024 vaccination policy analysis.
- `7_routine coverage analysis.R`: 2024 routine coverage analysis.
- `8_reporting analysis.R`: Reporting completeness analysis.
- `9_figures.R`: Figure generation and export.
- `10_tables.R`: Table generation and export.
- `11_cleaned output exports.R`: Cleaned analytic CSV exports for review and reuse.
- `clean_monotone.R`: Helper for monotonicity cleaning.
- `clean_interpolate.R`: Helper for interpolation from vaccine introduction.
- `a_design specifications.R`: Figure colors and design settings.
- `renv.lock`: Locked R package environment used for reproducibility.
- `input/`: Input data files required by the analysis.
- `output/`: Generated figures and tables.

## Data Sources

The analysis uses local input files, including dated snapshots of WHO API
extracts.

Local input files:

- `input/data_export_WIISE_REF_COUNTRIES.csv`: WHO reference country/entity metadata.
- `input/data_export_WIISE_MT_REF_POPULATIONS.csv`: Population estimates by country, year, and age group.
- `input/base_adhoc.xlsx`: Manually curated country-level fields, including older adult policy age thresholds, introduction dates, and health and care worker denominator information.
- `input/data_corrections.csv`: Central audit log for manual exclusions, replacements, reporting-basis decisions, and documented set-missing corrections.
- `input/2024_uptake_annual_source.csv`: Frozen annual source extract used to reconstruct 2024 priority-group uptake.
- `input/2024_uptake_quarterly_source.csv`: Frozen quarterly source extract used to reconstruct 2024 priority-group uptake.
- `input/2024_uptake_source_manifest.csv`: Upstream filenames, hashes, timestamps, and extraction notes for the 2024 uptake extracts.
- `input/output_new.xlsx`: 2024 policy data used for routine-period analyses.
- `input/2026-06-12_MT_COV_UPTAKE_TARGETGROUP_LONG.json`: Cached WHO API extract for target-group uptake data.
- `input/2026-06-12_COV_UPTAKE_WEEKLY.json`: Cached WHO API extract for weekly total-population uptake data.

The cached JSON files were downloaded from:

- `https://xmart-api-public.who.int/WIISE/MT_COV_UPTAKE_TARGETGROUP_LONG`
- `https://xmart-api-public.who.int/WIISE/COV_UPTAKE_WEEKLY`

By default, `0_master.R` reads the cached JSON files rather than calling the live
API. Total-population monthly administrative uptake values are prepared from
`COV_UPTAKE_WEEKLY` in `2_data cleaning.R` by selecting the last available
weekly report in each country-month and assigning it to the first day of that
month. Target-group monthly administrative uptake values for older adults and
health and care workers are prepared from `MT_COV_UPTAKE_TARGETGROUP_LONG` in
the same script. This mirrors the month representation used in CVAP's coverage
workflow, where monthly periods are stored as first-of-month dates for R
processing.

To refresh these snapshots from the live API, set `refresh_api <- TRUE` near the
top of `0_master.R` before running the script. Refreshing the API snapshots may
change the analysis results if the upstream data have changed.

## Software Requirements

The analysis was checked locally with:

- R version 4.5.1

This repository uses `renv` to restore the package versions recorded in
`renv.lock`. From the project root, run:

```r
install.packages("renv")
renv::restore()
```

If `renv` is already installed, only `renv::restore()` is required.

The project `.Rprofile` uses a project-local renv sandbox (`renv/sandbox`) to
avoid lock-acquisition hangs observed with the shared user-cache sandbox on
fresh macOS copies. The analysis uses the project-local renv library and the
package versions recorded in `renv.lock`.

## Running the Analysis

From R:

```r
source("0_master.R")
```

From a terminal:

```bash
cd /path/to/covid_vaccination
Rscript 0_master.R
```

The script determines the project directory from the location of `0_master.R`,
so it can also be sourced from the parent directory:

```r
source("covid_vaccination/0_master.R")
```

The script reads inputs from `input/` and writes figures and tables to
`output/`. The `output/` directory is created automatically if it does not
already exist.

To run the analysis without exporting figures or tables, set
`export_outputs <- FALSE` near the top of `0_master.R`.

## Expected Runtime

On a local laptop with required packages installed, the analysis is expected to
run in approximately 5-15 minutes using the cached WHO API snapshots. Runtime may
vary based on the time required to render figures and Word tables. Live API
refreshes may take longer and can change results if the upstream data have
changed.

## Generated Outputs

Primary manuscript figures and tables:

- `output/fig_1_cov_end2023_inc.jpg`
- `output/fig_2_cov_cps_boost_priority_end2023.jpg`
- `output/fig_3_admin_time_all.jpg`
- `output/fig_4_cov_time_global_bygroup.jpg`
- `output/fig_5_dvr_summary.jpg`
- `output/fig_6_policy_2024_all.jpg`
- `output/tab_1_end-2023_coverage.docx`
- `output/tab_2_2024_uptake.docx`

Supplementary and additional outputs:

- `output/fig_supp_1_reporting.jpg`
- `output/fig_supp_2_cov_cps_priority_time.jpg`
- `output/fig_supp_3_cov_time_weight_all.jpg`
- `output/fig_supp_4_cov_mediqr_all.jpg`
- `output/fig_supp_5_dvr_time_increg.jpg`
- `output/tab_supp_3_dvr_max.docx`
- `output/log_data_corrections.csv`
- `output/data_emergency_monthly_coverage.csv`
- `output/data_dvr_daily.csv`
- `output/data_2024_uptake.csv`
- `output/data_2024_policy.csv`

## Reproducibility Notes

- The working directory should not need to be set manually.
- The analysis uses cached WHO API snapshots by default and does not require
  internet access unless `refresh_api <- TRUE` is set.
- Total-population, target-group, and daily vaccination rate analyses are
  reconstructed from committed WHO API snapshots and source extracts rather than
  from pre-cleaned workbook exports.
- Cleaned analytic CSV exports are written from the same in-memory objects used
  to generate the manuscript figures and tables.
- The 2024 priority-group uptake analysis is reconstructed from the committed
  annual and quarterly source extracts. The upstream source files and hashes are
  recorded in `input/2024_uptake_source_manifest.csv`.
- Workbook inputs required by the analysis are limited to active policy/metadata
  inputs (`base_adhoc.xlsx`, `output_new.xlsx`).
- Administrative uptake data cleaning is implemented locally in this repository
  to make the exact analytic workflow reproducible for this manuscript. The
  cleaning approach is adapted from the WHO COVID-19 Vaccine Analysis Pipeline
  (CVAP), with a separate methods paper on the CVAP cleaning algorithm planned.
- The exact package environment is recorded in `renv.lock`; run
  `renv::restore()` before reproducing the outputs.
