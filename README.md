# The Invisible Energy Leak
Electricity Consumption Analytics and Anomaly Detection (R + Tableau)
UCI "Individual Household Electric Power Consumption" (one French household, Dec 2006 - Nov 2010).

## Quick start (RStudio)
1. Install packages once:
   `install.packages(c("data.table", "lubridate", "ggplot2", "scales", "corrplot"))`
2. Put `household_power_consumption.txt` in `data/raw/` (or run `source("R/00_download_data.R")`).
3. Set the working directory to this project folder (`getwd()` must show it).
4. Run everything: `source("R/run_all.R")`   (about 1-3 minutes on the full file)

`R/run_all.R` is the single script: import and clean -> features -> descriptive stats -> hourly/daily/weekly/monthly
aggregation -> peaks -> correlation -> anomaly detection (global vs time-conditioned IQR) -> sub-metering ->
Tableau exports -> figures -> findings report -> HTML preview -> validation.

## What you get
| Folder | Contents |
|---|---|
| `data/processed/` | Tableau-ready CSVs (see `tableau/tableau_dashboard_guide.md`) |
| `outputs/figures/` | 12 PNG charts |
| `outputs/tables/` | cleaning log, missingness, descriptive stats, **validation_report.csv** |
| `outputs/reports/` | `findings_and_recommendations.md`, `dashboard_preview.html`, `session_info.txt` |

Check `outputs/tables/validation_report.csv`: every row should say TRUE. The console prints "Validation: n/n checks passed".

## Tableau
R cannot create a Tableau workbook. Follow `tableau/tableau_dashboard_guide.md` (data sources, calculated fields,
five dashboards, R-vs-Tableau cross-check, QA checklist).

## Key method choices
- Missing values ('?') stay NA; never zero-filled. Coverage columns show how complete each period is.
- Power (kW) and energy (kWh) are kept separate: minute energy = kW / 60. Daily energy estimate = mean kW x 24 h on days with >= 90% coverage.
- Anomaly = Tukey IQR fences (k = 1.5), applied globally and within comparable hour-of-week groups (and season x hour-of-week). A flag means "unusual - investigate", never "proven waste".
- Sub-metering covers only three circuits; the "remainder" is derived.

## Testing without the real data
`Rscript tests/make_synthetic_dataset.R /tmp/synth.txt 450` then `Rscript R/run_all.R /tmp/synth.txt /tmp/out`
(synthetic data is for testing the pipeline only; never report its numbers).

## Verification status
- The full pipeline passes 37/37 validation checks on synthetic data in the build environment (R 4.3.3).
- It has not been run on the real UCI file by the author; the first run on your machine is the real test.
- Older stage scripts (`00_*`, `01_*`) are kept for reference; `run_all.R` does not depend on them.
