# COVID-19 vaccination analysis

This repository contains the R code and input files used to generate the tables
and figures for the manuscript:

> Global COVID-19 vaccination uptake across the emergency and early 
> post-emergency periods, 2021-2024

The analysis summarizes COVID-19 vaccination coverage in the total population,
older adults, and health and care workers, with stratification, where relevant,
by income group, WHO region, COVAX Facility participation category, and
vaccination policy category.

## Repository Structure

- `0_master.R`: Main analysis script. Run this file to reproduce outputs.
- `1_entity_pop.R`: Country, grouping, and population preparation.
- `2_data_cleaning.R`: Administrative data cleaning and interpolation.
- `3_static_values.R`: Static summary values used in the analysis.
- `4_emergency_coverage_analysis.R`: 2021-2023 coverage analysis.
- `5_emergency_dvr_analysis.R`: Daily vaccination rate analysis.
- `6_postemergency_policy_analysis.R`: 2024 vaccination policy analysis.
- `7_postemergency_uptake_analysis.R`: 2024 routine coverage analysis.
- `8_reporting_analysis.R`: Reporting completeness analysis.
- `9_export_figures.R`: Figure generation and export.
- `10_export_tables.R`: Table generation and export.
- `11_export_data.R`: Cleaned analytic CSV exports for review and reuse.
- `helper_clean_monotone.R`: Helper for monotonicity cleaning.
- `helper_clean_interpolate.R`: Helper for interpolation from vaccine introduction.
- `helper_design_specifications.R`: Figure colors and design settings.
- `renv.lock`: Locked R package environment used for reproducibility.
- `LICENSE`: MIT license for the analysis code.
- `DATA_TERMS.md`: Data reuse notice for input files and derived outputs.
- `input/`: Input data files required by the analysis.
- `output/`: Generated figures and tables.

## Data Sources

The analysis uses local input files, including cached snapshots of WHO API
extracts.

Local input files:

- `input/data_REF_COUNTRIES.json`: Cached WHO API extract for the `REF_COUNTRIES` fields used for country/entity metadata, WHO region, income group, WHO legal status, and COVAX participation category.
- `input/base_adhoc.csv`: Manually curated country-level fields used by the analysis, including older adult policy age thresholds, introduction dates, health and care worker denominator information, and older-adult threshold provenance.
- `input/data_corrections.csv`: Central audit log for manual exclusions, replacements, reporting-basis decisions, and documented set-missing corrections.
- `input/data_MT_REF_POPULATIONS.json`: Cached WHO API extract for population denominators, filtered to 2021 and 2024 both-sex population estimates.
- `input/data_AD_INDICATORS.json`: Cached WHO API extract for the `AD_INDICATORS` records used in the 2024 routine vaccination policy analysis, filtered to the 2024 COVID-19 vaccination policy indicators.
- `input/data_AD_COVERAGES.json`: Cached WHO API extract for the `AD_COVERAGES` records used in the 2024 routine priority-group uptake analysis, filtered to administrative COVID-19 coverage records for 2024.
- `input/data_COV_UPTAKE.json`: Cached WHO API extract for the `COV_UPTAKE` columns and periods used in the 2024 routine priority-group uptake analysis.
- `input/data_MT_COV_UPTAKE_TARGETGROUP_LONG.json`: Cached WHO API extract for target-group uptake data.
- `input/data_COV_UPTAKE_WEEKLY.json`: Cached WHO API extract for weekly total-population uptake data.
- `input/api_csv/*.csv`: Reviewer-friendly CSV mirrors of the cached WHO API
  JSON snapshots. These files contain the flattened OData `value` tables and
  are named to match the XMart API table names and are not used as analysis
  inputs.

The cached WHO API snapshot files correspond to:

- `https://xmart-api-public.who.int/WIISE/REF_COUNTRIES`
- `https://xmart-api-public.who.int/WIISE/MT_REF_POPULATIONS`
- `https://xmart-api-public.who.int/WIISE/AD_INDICATORS`
- `https://xmart-api-public.who.int/WIISE/AD_COVERAGES`
- `https://xmart-api-public.who.int/WIISE/COV_UPTAKE`
- `https://xmart-api-public.who.int/WIISE/MT_COV_UPTAKE_TARGETGROUP_LONG`
- `https://xmart-api-public.who.int/WIISE/COV_UPTAKE_WEEKLY`

By default, `0_master.R` reads the cached JSON files rather than calling the live
API, and writes flattened CSV mirrors to `input/api_csv/` for inspection.
Country and grouping metadata are prepared from the scoped `REF_COUNTRIES`
API snapshot in `1_entity_pop.R` and `8_reporting_analysis.R`; the stored
snapshot includes only the fields used by this analysis. Population denominators
are prepared from the filtered `MT_REF_POPULATIONS`
API snapshot in `1_entity_pop.R`; the stored snapshot is filtered to
`YEAR == 2021` or `YEAR == 2024`, and `GENDER == "BOTH"`.
Total-population monthly administrative uptake values are prepared from
`COV_UPTAKE_WEEKLY` in `2_data_cleaning.R` by selecting the last available
weekly report in each country-month and assigning it to the first day of that
month. Target-group monthly administrative uptake values for older adults and
health and care workers are prepared from `MT_COV_UPTAKE_TARGETGROUP_LONG` in
the same script. This mirrors the month representation used in CVAP's coverage
workflow, where monthly periods are stored as first-of-month dates for R
processing. Routine 2024 vaccination policy is prepared from the filtered
`AD_INDICATORS` API snapshot in `6_postemergency_policy_analysis.R`; the stored
snapshot is filtered to `YEAR == 2024` and `INDCODE` values
`POLICY_VACCINATION`, `POLICY_REVACCINATION`, `POLICY_VACCINATION_COM`, and
`POLICY_INTERVAL`. Routine 2024 priority-group uptake is prepared from filtered
`AD_COVERAGES` and `COV_UPTAKE` API snapshots in
`7_postemergency_uptake_analysis.R`; the stored `AD_COVERAGES` snapshot is filtered
to `COVERAGE_CATEGORY == "ADMIN"`, `VACCINECODE == "COVID19"`, and
`YEAR == 2024`.

To refresh these snapshots from the live API, set `refresh_api <- TRUE` near the
top of `0_master.R` before running the script. Refreshing the API snapshots may
change the analysis results if the upstream data have changed.

## License and Data Reuse

The MIT license in `LICENSE` applies to the analysis code in this repository.
The input data files and derived analytic outputs are included to support
transparent review and reproducibility of the manuscript, but they are derived
from aggregate country-reported COVID-19 vaccination data submitted to WHO and
from other referenced data sources. The MIT license should not be interpreted as
changing any data-use restrictions, attribution requirements, or terms that
apply to those underlying data sources.

Additional details are provided in `DATA_TERMS.md`.

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
cd /path/to/global-covid19-vaccination
Rscript 0_master.R
```

The script determines the project directory from the location of `0_master.R`,
so it can also be sourced from the parent directory:

```r
source("global-covid19-vaccination/0_master.R")
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
- `output/data_emergency_coverage.csv`
- `output/data_emergency_dvr.csv`
- `output/data_postemergency_uptake.csv`
- `output/data_postemergency_policy.csv`

## Reproducibility Notes

- The working directory should not need to be set manually.
- The analysis uses cached WHO API snapshots by default and does not require
  internet access unless `refresh_api <- TRUE` is set.
- Country and grouping metadata use a committed scoped
  `REF_COUNTRIES` API snapshot.
- Population denominators use a committed filtered
  `MT_REF_POPULATIONS` API snapshot.
- Total-population, target-group, and daily vaccination rate analyses are
  prepared from committed WHO API snapshots.
- Cleaned analytic CSV exports are written from the same in-memory objects used
  to generate the manuscript figures and tables.
- The 2024 policy and priority-group uptake analyses use committed filtered
  `AD_INDICATORS`, `AD_COVERAGES`, and `COV_UPTAKE` API snapshots.
- Active input files are listed in the Data Sources section above.
- Administrative uptake data cleaning is implemented locally in this repository
  to make the exact analytic workflow reproducible for this manuscript. The
  cleaning approach is adapted from the WHO COVID-19 Vaccine Analysis Pipeline
  (CVAP), with a separate methods paper on the CVAP cleaning algorithm planned.
- The exact package environment is recorded in `renv.lock`; run
  `renv::restore()` before reproducing the outputs.
