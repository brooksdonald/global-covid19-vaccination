# COVID-19 vaccination analysis

This repository contains the R code and input files used to generate the tables
and figures for the manuscript:

> Global COVID-19 vaccination uptake across the emergency and early 
> post-emergency periods, 2021-2024

The analysis summarizes COVID-19 vaccination coverage in the total population,
older adults, and health and care workers, with stratification by income group,
WHO region, COVAX participation category, and policy category.

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
- `input/output_master_test.xlsx`: 2021-2023 COVID-19 vaccination administrative uptake data used for acute-period analyses.
- `input/output_new.xlsx`: 2024 policy and uptake data used for routine-period analyses.
- `input/output_master.xlsx`: Earlier/archival workbook retained in the repository but not currently read by `0_master.R`.
- `input/2026-06-12_MT_COV_UPTAKE_TARGETGROUP_LONG.json`: Cached WHO API extract for target-group uptake data.
- `input/2026-06-12_COV_UPTAKE_WEEKLY.json`: Cached WHO API extract for weekly total-population uptake data.

The cached JSON files were downloaded from:

- `https://xmart-api-public.who.int/WIISE/MT_COV_UPTAKE_TARGETGROUP_LONG`
- `https://xmart-api-public.who.int/WIISE/COV_UPTAKE_WEEKLY`

By default, `0_master.R` reads the cached JSON files rather than calling the live
API. To refresh these snapshots from the live API, set the environment variable
`COVID_VAX_USE_LIVE_API=true` before running the script. Refreshing the API
snapshots may change the analysis results if the upstream data have changed.

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

To force a live WHO API refresh instead of using the cached JSON files:

```bash
COVID_VAX_USE_LIVE_API=true Rscript 0_master.R
```

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
- `output/fig_6_dvr_summary.jpg`
- `output/fig_7_policy_2024_all.jpg`
- `output/Table_1_end-2023_coverage.docx`
- `output/Table_2_dvr_max.docx`
- `output/Table_3_2024 coverage.docx`

Supplementary and additional outputs:

- `output/fig_4_cov_time_world.jpg`
- `output/fig_5_cov_time_mediqr.jpg`
- `output/fig_rep_disag_time.jpg`
- `output/fig_report_cumul_inc.jpg`
- `output/fig_supp_4_dvr_time_increg.jpg`
- `output/fig_supp_5_cov_time_weight_all.jpg`
- `output/fig_supp_6_cov_mediqr_all.jpg`
- `output/fig_supp_7_policy_revaccint.jpg`
- `output/fig_supp_x_cov_cps_tot_vs_priority_2021_2023.jpg`
- `output/NU_fig_risk_total_comp.jpg`

## Reproducibility Notes

- The working directory should not need to be set manually.
- The analysis uses cached WHO API snapshots by default and does not require
  internet access unless `COVID_VAX_USE_LIVE_API=true` is set.
- The exact package environment is recorded in `renv.lock`; run
  `renv::restore()` before reproducing the outputs.
- Before public release, remove local/session artifacts such as `.RData`,
  `.Rhistory`, `.DS_Store`, and scratch scripts that are not part of the
  manuscript workflow.
