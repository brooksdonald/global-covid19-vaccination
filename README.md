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

- `0_master.R`: Main analysis script, input data loading and export functions.
  Run this file to reproduce outputs.
- `1_entity_pop.R`: Country, grouping, and population preparation.
- `2_data_cleaning.R`: Preparation, treatment and reconstruction of total population
  and priority group uptake data.
- `3_static_values.R`: Country membership and fixed population denominators for aggregates.
- `4_emergency_coverage_analysis.R`: 2021-2023 coverage analysis.
- `5_emergency_dvr_analysis.R`: Daily vaccination rate analysis.
- `6_postemergency_policy_analysis.R`: 2024 vaccination policy analysis.
- `7_postemergency_uptake_analysis.R`: 2024 post-emergency uptake analysis.
  Includes end-of-year uptake and a sensitivity analysis adding quarterly-only reports.
- `8_reporting_analysis.R`: Reporting completeness analysis.
- `9_sensitivity_analyses.R`: HCW adjustment, capping and matched-country comparisons.
- `9_calculation_checks.R`: Checks of reported values and reconstructed estimates.
- `10_export_figures.R`: Figure generation and export.
- `11_export_tables.R`: Table generation and export.
- `12_export_data.R`: Checks and exports of analytical data and calculation audits.
- `helper_clean_monotone.R`: Source preparation, negative and zero exclusions,
  and contextual exclusion for total population and priority groups.
- `helper_clean_interpolate.R`: Monthly priority group and dated total population
  reconstruction, including month end estimates and the cumulative curve for daily rates.
- `helper_design_specifications.R`: Figure colors and design settings.
- `renv.lock`: Locked R package environment used for reproducibility.
- `LICENSE`: MIT license for the analysis code.
- `DATA_TERMS.md`: Data reuse notice for input files and derived outputs.
- `input/`: Input data files required by the analysis.
- `output/`: Generated figures, tables and analytical CSVs.
- `output/audit/`: Optional source, treatment, reconstruction and validation logs.

## Data Sources

The analysis uses local input files, including cached snapshots of WHO API
extracts.

Local input files:

- `input/260630_data_REF_COUNTRIES.json`: Cached WHO API extract for the
  `REF_COUNTRIES` fields used for country/entity metadata, WHO region, income
  group, WHO legal status, and COVAX participation category.
- `input/base_adhoc.csv`: Manually curated country-level fields used by the
  analysis, including older adult policy age thresholds, introduction dates,
  health and care worker denominator information, and older adult threshold
  provenance.
- `input/data_corrections.csv`: Central audit log for manual exclusions,
  replacements, reporting-basis decisions, and documented set-missing
  corrections.
- `input/260630_data_MT_REF_POPULATIONS.json`: Cached WHO API extract for
  population denominators, filtered to 2021 and 2024 both-sex population
  estimates.
- `input/260630_data_AD_INDICATORS.json`: Cached WHO API extract for the
  `AD_INDICATORS` records used in the 2024 post-emergency vaccination policy
  analysis, filtered to the 2024 COVID-19 vaccination policy indicators.
- `input/260630_data_AD_COVERAGES.json`: Cached WHO API extract for the
  `AD_COVERAGES` records used in the 2024 post-emergency priority group uptake
  analysis, filtered to administrative COVID-19 coverage records for 2024.
- `input/260630_data_COV_UPTAKE.json`: Cached WHO API extract for the
  `COV_UPTAKE` columns and periods used in the 2024 post-emergency priority
  group uptake analysis.
- `input/260630_data_MT_COV_UPTAKE_TARGETGROUP_LONG.json`: Cached WHO API
  extract for 2021-2023 emergency target group uptake data.
- `input/260630_data_COV_UPTAKE_WEEKLY.json`: Cached WHO API extract for
  2021-2023 emergency weekly total population uptake data.
- `input/api_csv/260630_*.csv`: CSV copies of the cached WHO API JSON extracts
  for inspection. These contain the OData `value` tables and are named to match
  the XMart API tables. Analyses read the JSON files.

The cached WHO API snapshot files correspond to:

- `https://xmart-api-public.who.int/WIISE/REF_COUNTRIES`
- `https://xmart-api-public.who.int/WIISE/MT_REF_POPULATIONS`
- `https://xmart-api-public.who.int/WIISE/AD_INDICATORS`
- `https://xmart-api-public.who.int/WIISE/AD_COVERAGES`
- `https://xmart-api-public.who.int/WIISE/COV_UPTAKE`
- `https://xmart-api-public.who.int/WIISE/MT_COV_UPTAKE_TARGETGROUP_LONG`
- `https://xmart-api-public.who.int/WIISE/COV_UPTAKE_WEEKLY`

By default, `0_master.R` reads the cached JSON files rather than calling the live
API, and writes flattened CSV mirrors to `input/api_csv/` for inspection when
`export_outputs` is enabled. Cached WHO API snapshot filenames are prefixed with
the refresh date in `YYMMDD` format;
the committed snapshot prefix `260630` corresponds to a 2026-06-30 refresh.

Country, grouping, and denominator inputs:

- Country and grouping metadata are prepared from the scoped `REF_COUNTRIES`
  API snapshot in `1_entity_pop.R` and `8_reporting_analysis.R`. The stored
  snapshot includes only the fields used by this analysis.
- Population denominators are prepared from the filtered `MT_REF_POPULATIONS`
  API snapshot in `1_entity_pop.R`. The stored snapshot is filtered to
  `YEAR == 2021` or `YEAR == 2024`, and `GENDER == "BOTH"`.

Emergency period uptake inputs, 2021-2023:

- Total population estimates are prepared from all dated `COV_UPTAKE_WEEKLY`
  reports in `2_data_cleaning.R`. Treatment and reconstruction precede
  selection of estimates for the final calendar day of each month.
- Total population reporting completeness counts any non-missing, nonzero
  report for each indicator within a month, before anomaly treatment, in
  `8_reporting_analysis.R`.
- Target group monthly administrative uptake values for older adults and health
  and care workers are prepared from `MT_COV_UPTAKE_TARGETGROUP_LONG` in
  `2_data_cleaning.R`.
- Monthly records use the first day as the month identifier. The total
  population audit also records `target_date`, the final calendar day used
  for estimation.

Post-emergency inputs, 2024:

- Vaccination policy is prepared from the filtered `AD_INDICATORS` API snapshot
  in `6_postemergency_policy_analysis.R`. The stored snapshot is filtered to
  `YEAR == 2024` and `INDCODE` values `POLICY_VACCINATION`,
  `POLICY_REVACCINATION`, `POLICY_VACCINATION_COM`, and `POLICY_INTERVAL`.
- Priority group uptake is prepared from filtered `AD_COVERAGES` and
  `COV_UPTAKE` API snapshots in `7_postemergency_uptake_analysis.R`. The stored
  `AD_COVERAGES` snapshot is filtered to `COVERAGE_CATEGORY == "ADMIN"`,
  `VACCINECODE == "COVID19"`, and `YEAR == 2024`.

To refresh these snapshots from the live API, set `refresh_api <- TRUE` near the
top of `0_master.R` before running the script. Refreshing the API snapshots may
change the analysis results if the upstream data have changed.

## Retrospective Estimation

For total population uptake in 2021–2023, agreeing values for the same country,
indicator and date contribute one observation; conflicting values contribute
no reconstruction anchor. Priority group rows are selected by reporting month
and require a non-missing, non-zero CPS count; the row with the largest CPS
count supplies all three indicators. Equal values for the same country,
priority group, indicator and date are collapsed after report selection.
Monthly priority group dates identify
the reporting month and are treated as observations at its end.

All three populations use the same count treatment and dated reconstruction
functions. Negative counts and zeros following an earlier positive value are
excluded. Contextual exclusion then addresses downward
changes until retained observations are nondecreasing. Indicators are treated
independently. Repetitions, rapid increases and counts exceeding the population
denominator are retained.

Retained reports are joined by linear interpolation using elapsed calendar days.
Early interpolation starts at zero on the documented programme introduction date
for total doses and at least one dose, and two calendar months later for CPS.
Reports take precedence over assumed introduction dates. Total population
booster series have no early reconstruction. Early reconstruction of other
total population indicators requires a known introduction date. The final
retained count is carried forward through December 2023, without a maximum gap duration.
Reconstructed counts are rounded before calculating coverage; retained reported
values remain unchanged before the final coverage cap. Coverage for indicators
counting persons is capped at 100%; total doses are not capped.

The total population estimation cohort comprises 194 WHO Member States plus
Kosovo and occupied Palestinian territory, excluding Eritrea and the Democratic
People's Republic of Korea, leaving 194 countries. Country membership for
reporting and aggregate estimates is defined separately in
`8_reporting_analysis.R` and `3_static_values.R`.

Documented priority group corrections precede the common treatment sequence.
Counts are reconstructed at month end using elapsed days, including before
the first retained at-least-one-dose or CPS report when a valid programme zero
date is available. For older adult and HCW boosters, the first month end with
a strictly positive total population booster estimate supplies an assumed
zero starting point. Counts are interpolated from that point to the first
retained group-specific booster report only when the proxy precedes it. Zero
total population estimates do not define the proxy. Earlier months, and
leading gaps without a positive total population estimate, remain missing.
Retained reports take precedence, including valid zero reports. Reconstructed
counts are rounded, and retained reports are preserved. Older adult counts are capped
after reconstruction. HCW counts receive the adjustment, using reconstructed
total population coverage, before their final cap. Coverage is then calculated
from the capped counts and the corresponding population denominators.

Daily vaccination rates use the same treated total dose observations and dated
reconstruction as monthly uptake. Rates are summarized as trailing 28 day
means and stop at the final retained report. The first daily rate is missing
where no earlier introduction anchor is available. Daily rate estimates also
include reporting territories outside the monthly coverage cohort. Aggregate
rates use the fixed total population CPS reporting denominators for each grouping.

### Calculation records

The following intermediate datasets are available after running `0_master.R`:

- `retro_preparation$mapping` links dated total population inputs to their
  country, indicator and date, including duplicates and conflicting values.
- `retro_audit` records the reported value and the value after each applied
  treatment: negative count exclusion, zero exclusion and contextual exclusion.
- `retro_monthly` records the retained reports supporting each month end estimate,
  interpolation weights, rounding and coverage capping.
- `priority_audit` records older adult and HCW counts after manual corrections,
  negative-count exclusion, zero exclusion and contextual exclusion.
- `target_report_values` records the selected priority-group values and identifies
  identical dated values collapsed before count treatment.
- `priority_decisions` records the priority group contextual exclusions.
- `priority_monthly` records reconstructed priority group counts and their
  supporting observations, including the booster zero-date proxy, before the
  final population caps and HCW adjustment.
- `country_groupings` lists the countries included in each aggregate.
- `reporting_population_country` lists countries with an end-2023 estimate for
  each indicator. `population_reporting` sums their populations for coverage
  and cumulative doses per 100 population. Reporting participation percentages
  use the country counts in `population_all`.
- `data_cov_time_counts` contains the country uptake counts used for aggregation, and
  `data_cov_time_all` contains their sums, denominators and calculated coverage.

Figures are prepared in `10_export_figures.R`; tables are calculated and
formatted in `11_export_tables.R`.

## HCW Adjustment Sensitivity in 2021-2023

`9_sensitivity_analyses.R` compares HCW estimates with and without the
post-reconstruction adjustment for the difference between the HCW population
and vaccination target. Both scenarios retain the same source selection,
cleaning, interpolation and population denominators. The adjusted scenario
adds the HCW adjustment to the uncapped reconstructed count and then applies
the final population cap. The unadjusted scenario independently caps the
reconstructed count without an adjustment. Coverage is calculated from the
final count in each scenario. The adjusted scenario reproduces the primary
analysis. Differences are adjusted minus unadjusted, in percentage points.

The aggregate comparison retains the fixed reporting denominator used in the
primary analysis. A separate comparison includes the same countries under both
scenarios, with HCW and total population estimates available at that month and
indicator. It reports population-weighted coverage, within-country coverage
gaps and ratios, and counts of countries moving from below to above total
population coverage. Ratios are undefined where total population coverage is
zero. These comparisons are descriptive: total population coverage is an input
to the HCW adjustment.

The adjustment is applied only when the HCW target is below the population
estimate and corresponding total population coverage is available. When no
adjustment applies, the HCW count is retained. If a positive target gap exists
but total population coverage is missing, the HCW count is retained without
an addition and flagged as `Adjustment unavailable`. A missing HCW count stays
missing. All resulting counts remain capped at the HCW population denominator.

The country audit records adjustment status, uncapped reconstructed counts
and both final estimates. For the missing-input comparison,
`previous_adjusted_count` adds the target gap multiplied by total population
coverage to the reconstructed HCW count and applies the HCW denominator cap.
`restored_estimate` identifies counts retained in the analysis when this
calculation gives a missing value. The country file distinguishes unavailable
adjustments from missing HCW estimates. The primary coverage CSV
also includes `hcw_adjustment_status`; `hcw_denominator_adjustment_applied`
reflects the adjustment for that specific HCW indicator and month. It is not
populated for other population groups.

Fallback months have no estimated addition for HCWs outside the vaccination
target and may therefore understate coverage relative to fully adjusted
estimates. The matched-country comparison requires total population coverage
and does not include these fallback months.

Capping is counted separately for final estimates without and with the HCW
adjustment. Both scenarios start from the same uncapped reconstructed counts.
An estimate is counted as capped only when the count before its final cap
exceeds the HCW denominator; exactly 100% coverage is not itself evidence of
capping. Counts refer to country-month-indicator estimates, and the scenario
counts overlap. The country audit separately flags reported counts above the
denominator as `reported_above_denominator`; these reports are not capped
before cleaning or reconstruction.

Outputs:

- `output/data_emergency_hcw_adjustment_country.csv`: Country-month-indicator
  audit with targets, denominators, unadjusted and adjusted counts, capping,
  adjustment status, restored estimates, and HCW–total population comparisons.
- [Figure S6](output/fig_supp_6_hcw_adjustment.pdf) shows year-end HCW coverage
  with and without adjustment in 2021–2023 and the HCW–total population coverage
  gap at the end of 2023.
- `output/tab_supp_5_capping.docx` gives the percentage of estimates capped and
  the percentage of countries with at least one capped estimate. For 2021–2023,
  monthly estimates cover the total population, older adults and HCWs, with HCWs
  shown without and with adjustment.
  For 2024, the table uses one annual estimate per Member State reporting
  through the end of 2024 for older adults or HCWs (`output/data_postemergency_uptake.csv`).

The HCW scenario summaries include global, WHO region, income and COVAX groupings.
Global summaries use WHO Member States. Country audits also retain the other
entities included in the primary analytical dataset; select WHO Member States
when reproducing global counts. Retained population denominators are not
re-estimated in these analyses.

## Processing Sequence and Effects in 2021-2023

The objective is to construct harmonized retrospective cumulative vaccination
series for descriptive uptake analyses, preserving retained observations and
reconstructing month-end counts. The processing summaries in
`9_sensitivity_analyses.R` quantify the implemented sequence without comparing
alternative cleaning or reconstruction methods.

The source summaries distinguish same-date preparation, priority-group report
selection, documented corrections, negative-count exclusions, zero exclusions
and downward-step treatment. Submitted values, report rows and prepared
country-date-indicator observations are separate units. Source exclusions are
assigned to the first stage that removes an observation. The priority-group
selection data retain rejected rows and their dose-specific counts, so
rows containing positive first-dose or booster uptake can be distinguished
from rows containing only zeros or missing values.

Monthly records distinguish retained month-end observations, internal
interpolation, early interpolation and carry-forward. Rounding counts only
numerical changes. The HCW adjustment frequency in this processing summary
counts positive additions before capping; this differs from the number of
final estimates increased in the adjusted-versus-unadjusted sensitivity.
Final capping counts reductions after any HCW addition. Countries are counted
once within each step; later processing steps can affect the same monthly
estimate and their frequencies must not be summed.

Year-end summaries retain the primary fixed reporting denominators and show
estimate origins, the age of carried-forward reports and coverage before adjustment and capping, after HCW
adjustment, and after the final cap. Differences are final minus reconstructed
coverage in percentage points. These differences quantify the final count
operations; they are not a comparison with an unprocessed monthly series.
Processing summaries are restricted to WHO Member States.

Outputs:

- `output/tab_supp_3_processing_sequence.docx` lists the processing rules in order.
  For each population group, it gives the number of affected units with the
  number of distinct countries in parentheses.
- `output/tab_supp_6_processing_year_end.docx` shows how many countries had an
  end-2023 estimate carried forward out of all countries with an estimate for
  each population group and indicator. It covers at least one dose, complete
  primary series and booster doses for all three groups, and total doses
  administered for the total population. Days since the last retained report
  are summarized as the median (25th–75th percentiles) among countries whose
  estimate was carried forward. The table displays whole days.

## Reporting Completeness in 2021–2023

Reporting participation is assessed from submitted values before anomaly
exclusion, interpolation, carry-forward, HCW adjustment or capping. A qualifying
report contains a non-missing, nonzero count for the relevant indicator.

For the total population, a Member State qualifies for an indicator in a month
if any dated report in that month qualifies. Multiple reports count once per
country-month-indicator; CPS and booster values may occur in different weekly
records within the same month.

Older-adult and HCW reporting retain the coverage source-selection rule:
records require non-missing, nonzero CPS, and the row with the largest CPS
count supplies all indicators. Reporting summaries select one row when maximum
CPS values tie. Booster fields in otherwise ineligible or unselected source
rows do not establish booster reporting participation.

Monthly CPS-only, booster-only and joint reporting categories are calculated
from country-level reporting flags. Cumulative participation counts countries
that have had at least one qualifying report by each month. Once established,
reporting history remains present despite subsequent zeros or missing values;
no replacement of country-specific vaccination values is needed to construct
these counts. Monthly and cumulative summaries use the same qualifying reports.

Income-group percentages use all Member States assigned to each group as the
denominator, with the same country classification in numerator and denominator.
Supplementary Figure S1 distinguishes monthly reporting categories (bars) from
cumulative CPS and booster reporting (lines). Dates identify reporting periods.

## Reporting Completeness in 2024

Table 2 uses end-of-year reports for calendar-year 2024 from eJRF and TESSy.
Annual TESSy reports are stored in `COV_UPTAKE` at `YEAR = 2025` and
`REPORTING_PERIOD = 1`. The sensitivity analysis adds the latest cumulative
quarterly value for countries without an annual report for the population group.

For Belgium, Norway, Slovenia and Sweden, the selected TESSy report covers
persons aged 60 years and older, so the 2024 uptake analysis uses the population
aged 60 years and older. Slovenia reported separate counts for ages 60–69,
70–79 and 80 years and older.

Reporting completeness is assessed separately for older adults and health and
care workers, globally and by WHO region, income group and vaccination policy.
Reported zeros count as reports. For each analysis, uptake is the sum of capped
vaccinated counts divided by the population of the contributing countries.
Population represented uses the same all-country denominator in both analyses.
When a group has no end-of-year reporters, Table 2 shows zero countries,
zero population represented and a dash for uptake.

Supplementary Table S4 (`output/tab_supp_4_2024_reporting_completeness.docx`)
compares global reporting counts, population represented and uptake among
end-of-year reporters and after including quarterly-only reporters.

- `output/data_postemergency_uptake.csv` contains the primary country and grouped
  estimates. Country rows retain the latest reporting period even when annual
  uptake is missing.
- `output/data_postemergency_quarterly_carry_forward.csv` gives the latest
  reporting period and contribution to the combined global estimate for each
  quarterly-only reporting country and population group.

Uptake and shares are proportions. Columns ending in `_pp` are in percentage
points. Each country's contribution is 100 times its capped vaccinated count
divided by the combined reporting population. Differences in Supplementary
Table S4 are calculated by subtracting the end-of-year estimate from the
corresponding estimate after quarterly-only reporters are included.
The two estimates cover different countries. Vaccinations after a country's
last quarterly report and uptake in non-reporting countries are unknown.

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

The project `.Rprofile` uses a local renv library and sandbox (`renv/sandbox`)
with the package versions recorded in `renv.lock`.

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

The script reads inputs from `input/` and writes figures, tables and seven
analytical CSVs to `output/`. The script creates the output directory if needed.

To run the analysis without writing outputs, set
`export_outputs <- FALSE` near the top of `0_master.R`.

## Expected Runtime

On a local laptop with required packages installed, the analysis is expected to
run in approximately 5-15 minutes using the cached WHO API snapshots. Runtime may
vary based on the time required to render figures and Word tables. Live API
refreshes may take longer and can change results if the upstream data have
changed.

## Generated Outputs

Primary manuscript figures and tables:

- `output/fig_1_end2023_coverage.jpg`
- `output/fig_2_emergency_coverage_time.jpg`
- `output/fig_3_dvr_summary.jpg`
- `output/fig_4_policy_2024_all.jpg`
- `output/tab_1_end-2023_coverage.docx`
- `output/tab_2_2024_uptake.docx`
- `output/tab_3_dvr_max.docx`

Supplementary and additional outputs:

- `output/fig_supp_1_reporting.jpg`
- `output/fig_supp_2_cov_cps_priority_time.jpg`
- `output/fig_supp_3_cov_time_weight_all.jpg`
- `output/fig_supp_4_cov_mediqr_all.jpg`
- `output/fig_supp_5_dvr_time_increg.jpg`
- `output/fig_supp_6_hcw_adjustment.jpg` and `output/fig_supp_6_hcw_adjustment.pdf`: HCW coverage with and without adjustment and end-2023 comparisons with total-population coverage.

Supplementary tables are exported on A4 portrait pages.

- `output/tab_supp_3_processing_sequence.docx`
- `output/tab_supp_4_2024_reporting_completeness.docx`
- `output/tab_supp_5_capping.docx`
- `output/tab_supp_6_processing_year_end.docx`

Data exports:

- `output/log_data_corrections.csv`
- `output/data_emergency_coverage.csv`
- `output/data_emergency_hcw_adjustment_country.csv`
- `output/data_emergency_dvr.csv`
- `output/data_postemergency_uptake.csv`
- `output/data_postemergency_quarterly_carry_forward.csv`
- `output/data_postemergency_policy.csv`

For detailed calculation records, set `export_audit <- TRUE` and leave
`export_outputs <- TRUE` in `0_master.R`. This writes method settings, study
population, source preparation, treatment decisions, monthly estimates, daily
total doses, source preservation counts, validation results and R session
information to `output/audit/`. Calculation checks run with either setting.

## Reproducibility Notes

- The analysis uses cached WHO API snapshots by default and does not require
  internet access unless `refresh_api <- TRUE` is set.
- Country and grouping metadata use a committed scoped
  `REF_COUNTRIES` API snapshot.
- Population denominators use a committed filtered
  `MT_REF_POPULATIONS` API snapshot.
- Total population, target group, and daily vaccination rate analyses are
  prepared from committed WHO API snapshots.
- Cleaned analytic CSV exports are written from the same in-memory objects used
  to generate the manuscript figures and tables.
- The 2024 post-emergency policy and priority group uptake analyses use
  committed filtered `AD_INDICATORS`, `AD_COVERAGES`, and `COV_UPTAKE` API
  snapshots.
- Active input files are listed in the Data Sources section above.
- Administrative uptake data cleaning is implemented locally in this repository
  to make the exact analytic workflow reproducible for this manuscript. The
  cleaning approach is adapted from the WHO COVID-19 Vaccine Analysis Pipeline
  (CVAP).
- The exact package environment is recorded in `renv.lock`; run
  `renv::restore()` before reproducing the outputs.
