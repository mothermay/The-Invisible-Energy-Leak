# Tableau Dashboard Guide — THE INVISIBLE ENERGY LEAK
*Discovering Hidden Patterns in Household Electricity Consumption*

R cannot generate a Tableau workbook. `R/run_all.R` produces every dataset and number; you assemble the
workbook in Tableau Desktop or Tableau Public by following this guide (about 1–2 hours).
`outputs/reports/dashboard_preview.html` is a static preview of the intended result.

## 1. Connect the data (Data > New Data Source > Text file)
All files are in `data/processed/`. **Do not use physical joins** (they can multiply rows). Add the files as
separate tables in one data source using **Relationships** (the "noodle"), which keep each file at its own granularity.

| File | One row per | Relationship key | Used by |
|---|---|---|---|
| `kpi_summary.csv` | KPI | none (standalone) | D1 KPI cards, D5 |
| `daily_summary.csv` | calendar day | `date` | D1, D2, D3 |
| `weekly_summary.csv` | week (Mon start) | none | optional trend |
| `monthly_summary.csv` | calendar month | `year_month` | D1, D2 |
| `hourly_summary.csv` | day_type × hour of day | none | D2 |
| `hour_weekday_heatmap.csv` | weekday × hour | none | D2 |
| `seasonal_summary.csv` | season × day_type | none | D2 |
| `hourly_series.csv` | clock hour (with flags) | `date` → daily_summary.date; `year_month` → monthly_summary | D3 |
| `anomaly_records.csv` | flagged hour only | none | D3 table / tooltips |
| `anomaly_summary.csv` | level × method × group | none (filter `level`, `method`, `group_type`) | D3 |
| `anomaly_thresholds.csv` | level × method × group | none | D3 threshold note |
| `submetering_summary.csv` | month × category | `year_month` | D4 |
| `submetering_hourly.csv` | day_type × hour × category | none | D4 |
| `correlation_matrix.csv` | variable pair | none | D4 |
| `data_quality_report.csv` | metric | none | D5 |

Set data types: `date` = Date, `hour_start` = Date & Time, `year_month` = String, `flag_*` = Number (whole),
`is_partial_month` = Boolean. Blank cells are true NULLs (missing data was never zero-filled).

## 2. Calculated fields
```
// hourly_summary / hour_weekday_heatmap: correct (weighted) mean power; never AVG of means
Mean Power kW        = SUM([sum_active_power_kw]) / SUM([n_valid_minutes])
Weekday Mean kW      = SUM(IF [day_type]="Weekday" THEN [sum_active_power_kw] END) / SUM(IF [day_type]="Weekday" THEN [n_valid_minutes] END)
Weekend Mean kW      = SUM(IF [day_type]="Weekend" THEN [sum_active_power_kw] END) / SUM(IF [day_type]="Weekend" THEN [n_valid_minutes] END)
Weekend vs Weekday % = IF [Weekday Mean kW]=0 OR ISNULL([Weekday Mean kW]) THEN NULL ELSE ([Weekend Mean kW]-[Weekday Mean kW])/[Weekday Mean kW]*100 END
// (on hourly_summary, filter day_type <> "All" for weekday/weekend charts; use "All" for the overall line)

// hourly_series (anomalies)
Flag Label           = IF [flag_hour_of_week]=1 THEN "Flagged" ELSEIF [flag_hour_of_week]=0 THEN "Normal" END
Flagged Hours        = SUM([flag_hour_of_week])
Evaluated Hours      = COUNT([flag_hour_of_week])          // counts non-null only
Anomaly %            = IF [Evaluated Hours]=0 THEN NULL ELSE [Flagged Hours]/[Evaluated Hours] END   // format as %

// generic safe % change (monthly_summary already contains pct_change_vs_prev_month / _year)
Pct Change           = IF ISNULL([Prev]) OR [Prev]=0 THEN NULL ELSE ([Current]-[Prev])/[Prev]*100 END

// submetering_summary: share across categories (do NOT SUM pct_of_active_energy or active_energy_kwh_total; they repeat per category)
Category Share       = SUM([energy_kwh]) / TOTAL(SUM([energy_kwh]))     // table calc, compute using Category
```
Energy vs power: `energy_kwh_*` columns are kWh; `*_power_kw` are kW. Never plot them on one axis.

## 3. Dashboards (1400 × 900, Automatic size; palette navy `#1F3A5F`, teal `#2A9D8F`, amber `#E9A23B`, red `#C8553D`, grey)

### D1 Energy Overview
- **Question:** How much is consumed, and how has it moved over time?
- **KPI cards** (`kpi_summary`, text sheets filtered by `metric`): total_records_analyzed, average_active_power, peak_active_power, avg_daily_energy, anomalous_hours_hour_of_week_iqr.
- **Trend:** `daily_summary`; Columns `date` (continuous), Rows `energy_kwh_est_full_day` as faint circles, plus `roll7_energy_kwh` (amber) and `roll30_energy_kwh` (navy) lines (same axis, kWh/day).
- **Filter:** `date` range slider applied to the whole dashboard (Apply to Worksheets > All Using This Data Source).
- **Tooltip:** date, kWh/day, coverage_pct. **Sort:** chronological.
- **Decision support:** shows baseline level and long-term drift; low-coverage days are blank, not zero.

### D2 When Does Demand Peak?
- **Hour-of-day line:** `hourly_summary`, Columns `hour`, Rows `Mean Power kW`, colour `day_type` (Weekday navy, Weekend amber; exclude "All").
- **Heatmap:** `hour_weekday_heatmap`, Columns `hour`, Rows `dow_label` (sort Mon→Sun manually), colour `Mean Power kW` (sequential light→red), square marks.
- **Weekday vs weekend bar:** `Weekday Mean kW` and `Weekend Mean kW`; show `Weekend vs Weekday %` as a caption.
- **Monthly/seasonal:** `monthly_summary` bars of `avg_daily_energy_kwh` by `year_month`, colour grey where `is_partial_month` = TRUE; `seasonal_summary` box or bar by season.
- **Filters:** year, season, day_type. **Interpretation:** hours/slots with persistently high demand are candidates for load-shifting review.

### D3 The Anomaly Detective
- **Time series:** `hourly_series`, Columns `hour_start`, Rows `mean_active_power_kw`; grey line, red circles where `Flag Label`="Flagged".
- **Normal vs flagged:** `Flag Label` count (bar or donut); **by month:** `Anomaly %` by `year_month`; **by hour:** `Anomaly %` by `hour`.
- **Method comparison:** `anomaly_summary` filtered `level`="hourly", `group_type`="overall", bars of `pct_flagged` by `method`.
- **Threshold note (required text box):** "Flag = hourly mean outside Q1 − 1.5·IQR or Q3 + 1.5·IQR of comparable hours (same hour of week). Groups with fewer than 30 hours are not evaluated. A flag marks an unusual period to investigate; it is not proof of waste or a fault."
- **Tooltips:** hour_start, mean kW, `expected_upper_kw`, `severity_ratio`. **Sort:** `anomaly_records` by `severity_ratio` descending.
- **Filters:** date range, hour, season, `method` flag choice.

### D4 Where Does the Energy Go?
- **Category comparison:** `submetering_summary`, Columns `year_month`, Rows `Category Share` (100% stacked), colour `category`.
- **Hour pattern:** `submetering_hourly`, Columns `hour`, Rows `mean_wh_per_minute`, colour `category`, filter `day_type`.
- **Distribution/relationships:** daily scatter from `daily_summary` (e.g. `mean_active_power_kw` vs `max_active_power_kw`) as needed.
- **Correlation heatmap:** `correlation_matrix`, Columns `variable_1`, Rows `variable_2`, colour `pearson_r` (diverging red–white–navy, fixed range −1…1), label `pearson_r`.
- **Caption (required):** "The three sub-meters cover only part of the household; the 'remainder' is derived (total active energy minus sub-meters). Correlation does not imply causation."

### D5 Insights and Recommendations
- Text boxes copied from `outputs/reports/findings_and_recommendations.md` (all numbers are computed by R) plus `data_quality_report` as a table (missing %, longest missing run, rows removed).
- Include the limitations section verbatim. Do not add "energy saved" figures: no baseline/comparison method is defined.

## 4. Cross-check R vs Tableau (do this before presenting)
| Tableau (build a check sheet) | Must equal (R) |
|---|---|
| `SUM(sum_active_power_kw)/SUM(n_valid_minutes)` on hourly_summary, day_type="All" | `kpi_summary`: average_active_power |
| `SUM(n_valid_minutes)` on hourly_summary, day_type="All" | `kpi_summary`: valid_active_power_records |
| `SUM(energy_kwh_observed)` on daily_summary vs monthly_summary | identical totals |
| `MAX(max_active_power_kw)` on daily_summary | `kpi_summary`: peak_active_power |
| `SUM(flag_hour_of_week)` on hourly_series | `kpi_summary`: anomalous_hours_hour_of_week_iqr |
| `Anomaly %` overall | `anomaly_summary` (hourly, hour_of_week, overall) `pct_flagged` |

## 5. QA checklist
- [ ] Every chart title states measure and unit (kW vs kWh); axes start at zero for bars.
- [ ] Heatmap and hour charts use weighted `Mean Power kW`, not AVG of averages.
- [ ] No physical joins; row counts per table match the CSVs.
- [ ] Partial/low-coverage months are visibly marked; no gaps drawn as zero.
- [ ] Threshold explanation and sub-metering caveat are visible on D3 and D4.
- [ ] Filters behave on all sheets of each dashboard; tooltips are readable.
- [ ] Cross-check table above passes.
