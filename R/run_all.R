# =============================================================================
#  THE INVISIBLE ENERGY LEAK -- complete analysis pipeline in ONE script
#  Electricity Consumption Analytics and Anomaly Detection
# =============================================================================
#  HOW TO RUN (RStudio): open the project folder as working directory, then
#      source("R/run_all.R")
#  or from a terminal in the project folder:
#      Rscript R/run_all.R
#  Optional arguments:  Rscript R/run_all.R <raw_file> <output_root>
#
#  INPUT : data/raw/household_power_consumption.txt  (official UCI file, never modified)
#  OUTPUT: data/processed/*.csv      Tableau-ready exports
#          outputs/tables/*.csv      quality, descriptive and validation tables
#          outputs/figures/*.png     publication-quality charts
#          outputs/reports/          findings_and_recommendations.md, dashboard_preview.html
#
#  SECTIONS
#   0 Setup   1 Import & clean   2 Features   3 Descriptive stats   4 Aggregations
#   5 Peaks   6 Correlation      7 Anomalies  8 Sub-metering        9 Exports
#  10 Figures 11 Findings/HTML   12 Validation
#
#  Units: Global_active_power kW | Voltage V | Global_intensity A |
#         Sub_metering_x = Wh of active energy per minute.
#  Power (kW) and energy (kWh) are never mixed: minute energy_kwh = kW / 60.
# =============================================================================

# ---- 0. SETUP ---------------------------------------------------------------
t_start <- Sys.time()
args     <- commandArgs(trailingOnly = TRUE)
RAW_PATH <- if (length(args) >= 1) args[1] else file.path("data", "raw", "household_power_consumption.txt")
OUT_ROOT <- if (length(args) >= 2) args[2] else "."

IQR_K               <- 1.5     # Tukey fence multiplier
MIN_DAILY_COVERAGE  <- 0.90    # share of the 1440 minutes needed to trust a daily value
MIN_HOURLY_VALID    <- 45L     # valid minutes (of 60) needed to trust an hourly mean
MIN_N_MINUTE_GROUP  <- 100L    # min observations per group for minute-level IQR bounds
MIN_N_HOUR_GROUP    <- 30L     # min observations per group for hour-level IQR bounds
SPEARMAN_SAMPLE     <- 200000L # rows sampled for the Spearman matrix (speed)
EXPORT_MINUTE_CSV   <- FALSE   # TRUE writes a ~150 MB minute-level CSV (not needed for Tableau)

needed <- c("data.table", "lubridate", "ggplot2", "scales", "corrplot")
missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs))
  stop("Missing packages. Run:  install.packages(c(", paste0('"', missing_pkgs, '"', collapse = ", "), "))")
suppressPackageStartupMessages({ library(data.table); library(lubridate)
  library(ggplot2); library(scales); library(corrplot) })

if (!file.exists(RAW_PATH))
  stop("Raw file not found: ", RAW_PATH, "\nPlace household_power_consumption.txt in data/raw/ ",
       "(see README) and make sure the working directory is the project root: ", getwd())

P        <- function(...) file.path(OUT_ROOT, ...)
DIR_PROC <- P("data", "processed"); DIR_FIG <- P("outputs", "figures")
DIR_TAB  <- P("outputs", "tables"); DIR_REP <- P("outputs", "reports")
for (d in c(DIR_PROC, DIR_FIG, DIR_TAB, DIR_REP)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

say <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
say("Pipeline started. Raw file: ", RAW_PATH)

NUM_COLS <- c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity",
              "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
UNITS <- c(Global_active_power = "kW", Global_reactive_power = "kW (reactive, as labelled by UCI)",
           Voltage = "V", Global_intensity = "A", Sub_metering_1 = "Wh/min",
           Sub_metering_2 = "Wh/min", Sub_metering_3 = "Wh/min", remainder_wh = "Wh/min")
DOW_LABELS <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
SEASONS    <- c("Winter", "Spring", "Summer", "Autumn")
SUB_LABELS <- c(Sub_metering_1 = "Kitchen", Sub_metering_2 = "Laundry",
                Sub_metering_3 = "Water heater & air-conditioner",
                remainder_wh   = "Not sub-metered (remainder)")
COL <- c(navy = "#1F3A5F", teal = "#2A9D8F", amber = "#E9A23B", red = "#C8553D",
         grey = "#8D99AE", light = "#EDF2F4")

# ---- helper functions (unit-tested in section 12) ----------------------------
pct_change <- function(new, old) {                       # ((new-old)/old)*100, safe
  out <- (new - old) / old * 100
  out[is.na(new) | is.na(old) | old == 0] <- NA_real_
  out
}
season_of <- function(m) {                               # meteorological seasons (N. hemisphere)
  fcase(m %in% c(12L, 1L, 2L), "Winter", m %in% 3:5, "Spring", m %in% 6:8, "Summer", default = "Autumn")
}
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
fmt <- function(x, d = 2) ifelse(is.na(x), "n/a", formatC(x, format = "f", digits = d, big.mark = ","))

add_time_features <- function(DT, tcol) {
  tt <- DT[[tcol]]
  set(DT, j = "date",         value = as.Date(tt))
  set(DT, j = "year",         value = as.integer(year(tt)))
  set(DT, j = "month",        value = as.integer(month(tt)))
  set(DT, j = "year_month",   value = sprintf("%04d-%02d", year(tt), month(tt)))
  set(DT, j = "month_name",   value = month.name[month(tt)])
  set(DT, j = "hour",         value = as.integer(hour(tt)))
  set(DT, j = "dow",          value = as.integer(wday(tt, week_start = 1)))   # 1 = Mon ... 7 = Sun
  set(DT, j = "dow_label",    value = DOW_LABELS[DT$dow])
  set(DT, j = "day_type",     value = fifelse(DT$dow >= 6L, "Weekend", "Weekday"))
  set(DT, j = "season",       value = season_of(month(tt)))
  set(DT, j = "hour_of_week", value = (DT$dow - 1L) * 24L + DT$hour)          # 0..167
  invisible(DT)
}

iqr_table <- function(DT, value, by, min_n, k = IQR_K) {  # Tukey IQR bounds per group
  b <- DT[!is.na(DT[[value]]), {
    q <- quantile(get(value), c(0.25, 0.75), names = FALSE)
    .(n = .N, q1 = q[1], q3 = q[2])
  }, by = by]
  b[, iqr := q3 - q1]
  b[, `:=`(lower = q1 - k * iqr, upper = q3 + k * iqr)]
  b[n < min_n, `:=`(lower = NA_real_, upper = NA_real_)]  # too few observations -> not evaluated
  b[]
}
attach_bounds <- function(DT, b, by) b[DT, on = by, .(lower, upper)]

# ---- logging / quality-report helpers ------------------------------------------------
clean_log <- data.table(step = character(), description = character(), rows_before = integer(),
                        rows_after = integer(), rows_changed = integer())
log_step <- function(step, description, before, after, changed = before - after) {
  clean_log <<- rbindlist(list(clean_log, data.table(step = step, description = description,
                                                     rows_before = as.integer(before), rows_after = as.integer(after), rows_changed = as.integer(changed))))
  say(sprintf("[%s] %s | before=%s after=%s", step, description,
              format(before, big.mark = ","), format(after, big.mark = ",")))
}
quality <- list()
add_q <- function(section, metric, value, note = "")
  quality[[length(quality) + 1L]] <<- data.table(section = section, metric = metric,
                                                 value = as.character(value), note = note)
write_csv <- function(DT, path) {
  fwrite(DT, path, dateTimeAs = "write.csv", na = "")
  if (!file.exists(path)) stop("Failed to write ", path)
  invisible(path)
}

# =============================================================================
# 1. IMPORT & CLEAN  (raw file is only read; NAs are kept, never zero-filled)
# =============================================================================
say("1. Importing and cleaning ...")
dt <- fread(RAW_PATH, sep = ";", header = TRUE, colClasses = "character", na.strings = c("?", ""))
expected_cols <- c("Date", "Time", NUM_COLS)
if (length(setdiff(expected_cols, names(dt))))
  stop("Missing columns: ", paste(setdiff(expected_cols, names(dt)), collapse = ", "))
dt <- dt[, expected_cols, with = FALSE]
n_raw <- nrow(dt)
log_step("01_import", "Rows read from raw file", n_raw, n_raw, 0L)
add_q("volume", "raw_rows", n_raw, "Rows in raw file (excluding header)")

na_before <- sum(unlist(dt[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = NUM_COLS]))
for (col in NUM_COLS) set(dt, j = col, value = suppressWarnings(as.numeric(dt[[col]])))
na_after  <- sum(unlist(dt[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = NUM_COLS]))
if (na_after != na_before) stop(na_after - na_before, " non-numeric value(s) (other than '?') found.")
log_step("02_numeric", "Converted 7 measurement columns to numeric (no new NAs)", n_raw, n_raw, 0L)

dt[, timestamp := dmy_hms(paste(Date, Time), tz = "UTC", quiet = TRUE)]  # naive local clock time
dt[, c("Date", "Time") := NULL]
bad_ts <- is.na(dt$timestamp); n0 <- nrow(dt); dt <- dt[!bad_ts]
log_step("03_timestamp", "Removed rows with unparseable timestamp", n0, nrow(dt))
add_q("timestamps", "unparseable_timestamps", sum(bad_ts), "Rows removed")

was_unsorted <- is.unsorted(dt$timestamp)
if (was_unsorted) setorder(dt, timestamp)
log_step("04_order", paste("Sorted by timestamp (was unsorted:", was_unsorted, ")"), nrow(dt), nrow(dt), 0L)
add_q("timestamps", "file_was_unsorted", was_unsorted)

dup_idx    <- duplicated(dt$timestamp)
dup_any    <- dup_idx | duplicated(dt$timestamp, fromLast = TRUE)
n_conflict <- if (any(dup_any)) dt[dup_any, .(k = uniqueN(.SD)), by = timestamp, .SDcols = NUM_COLS][k > 1, .N] else 0L
n0 <- nrow(dt); dt <- dt[!dup_idx]
log_step("05_duplicates", "Removed repeated timestamps (first kept)", n0, nrow(dt))
add_q("duplicates", "duplicate_rows_removed", sum(dup_idx))
add_q("duplicates", "duplicate_timestamps_with_conflicting_values", n_conflict)

secs <- diff(as.numeric(dt$timestamp))
interval_tab <- as.data.table(table(interval_seconds = secs))
interval_tab[, interval_seconds := as.numeric(as.character(interval_seconds))]
setnames(interval_tab, "N", "n_intervals")
gap_n <- sum(secs > 60); gap_minutes <- sum(secs[secs > 60] / 60 - 1)
expected_rows <- as.numeric(difftime(max(dt$timestamp), min(dt$timestamp), units = "mins")) + 1
add_q("time_coverage", "first_timestamp", format(min(dt$timestamp), "%Y-%m-%d %H:%M:%S"))
add_q("time_coverage", "last_timestamp",  format(max(dt$timestamp), "%Y-%m-%d %H:%M:%S"))
add_q("time_coverage", "expected_rows_if_complete_minute_grid", expected_rows)
add_q("time_coverage", "rows_present", nrow(dt))
add_q("time_coverage", "gaps_longer_than_1_minute", gap_n, "Places where consecutive rows are >60 s apart")
add_q("time_coverage", "minutes_missing_from_gaps", gap_minutes, "Minutes with no row at all (distinct from '?' rows)")
add_q("time_coverage", "intervals_not_multiple_of_60s", sum(secs %% 60 != 0))
log_step("06_intervals", "Checked sampling interval (gaps reported, not filled)", nrow(dt), nrow(dt), 0L)
rm(secs)

impossible <- list(Global_active_power = function(x) x < 0, Global_reactive_power = function(x) x < 0,
                   Voltage = function(x) x <= 0, Global_intensity = function(x) x < 0,
                   Sub_metering_1 = function(x) x < 0, Sub_metering_2 = function(x) x < 0,
                   Sub_metering_3 = function(x) x < 0)
for (col in names(impossible)) {
  bad <- which(impossible[[col]](dt[[col]]))
  add_q("impossible_values", col, length(bad), "Negative values (voltage <= 0) set to NA")
  if (length(bad)) set(dt, i = bad, j = col, value = NA_real_)
}
log_step("07_impossible", "Impossible values set to NA (rows retained)", nrow(dt), nrow(dt), 0L)

# ---- missingness analysis ------------------------------------------------------------
add_time_features(dt, "timestamp")
dt[, valid := !is.na(Global_active_power)]
miss_var <- data.table(variable = NUM_COLS, n_missing = sapply(NUM_COLS, function(v) sum(is.na(dt[[v]]))))
miss_var[, `:=`(n_rows = nrow(dt), pct_missing = round(100 * n_missing / nrow(dt), 4))]
na_count_row <- rowSums(is.na(dt[, NUM_COLS, with = FALSE]))
add_q("missingness", "rows_all_7_measurements_missing", sum(na_count_row == 7))
add_q("missingness", "rows_partially_missing", sum(na_count_row > 0 & na_count_row < 7))
add_q("missingness", "rows_fully_observed", sum(na_count_row == 0))
add_q("missingness", "pct_rows_missing_active_power", round(100 * mean(!dt$valid), 4))
rm(na_count_row)
miss_month <- dt[, .(rows = .N, missing_global_active_power = sum(!valid)), by = year_month]
miss_month[, pct_missing := round(100 * missing_global_active_power / rows, 3)]
setnames(miss_month, "year_month", "year_month")
r <- rle(!dt$valid); ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
runs <- data.table(start = dt$timestamp[starts], end = dt$timestamp[ends], rows_in_run = r$lengths)[r$values]
setorder(runs, -rows_in_run)
longest_run <- if (nrow(runs)) runs$rows_in_run[1] else 0L
add_q("missingness", "number_of_missing_runs", nrow(runs), "Consecutive stretches where Global_active_power is NA")
add_q("missingness", "longest_missing_run_rows", longest_run)
add_q("missingness", "handling_decision", "No imputation",
      "NA kept; aggregates use na.rm=TRUE plus coverage columns; missing data is never zero-filled")
rm(r, ends, starts)

expected_final <- n_raw - sum(bad_ts) - sum(dup_idx)
stopifnot(!anyNA(dt$timestamp), !anyDuplicated(dt$timestamp), !is.unsorted(dt$timestamp))
if (nrow(dt) != expected_final) stop("Row reconciliation failed: ", nrow(dt), " vs ", expected_final)
add_q("volume", "rows_after_cleaning", nrow(dt), "= raw rows - unparseable timestamps - duplicates")
add_q("volume", "pct_rows_removed", round(100 * (n_raw - nrow(dt)) / n_raw, 5))
log_step("08_final", "Final cleaned base dataset (NAs retained)", n_raw, nrow(dt))

# =============================================================================
# 2. FEATURE ENGINEERING  (power in kW; energy in kWh; sub-meters in Wh)
# =============================================================================
say("2. Feature engineering ...")
dt[, hour_start    := as.POSIXct(floor(as.numeric(timestamp) / 3600) * 3600, origin = "1970-01-01", tz = "UTC")]
dt[, energy_kwh    := Global_active_power / 60]                 # one reading = one minute
dt[, remainder_wh  := Global_active_power * 1000 / 60 - (Sub_metering_1 + Sub_metering_2 + Sub_metering_3)]
dt[, grp_all       := 1L]
n_negative_remainder <- sum(dt$remainder_wh < 0, na.rm = TRUE)
add_q("sub_metering", "minutes_with_negative_remainder", n_negative_remainder,
      "Sub-meter sum exceeded total active energy (measurement noise); kept as-is, not clipped")
saveRDS(dt[, c("timestamp", NUM_COLS), with = FALSE], file.path(DIR_PROC, "cleaned_minute_base.rds"))

# =============================================================================
# 3. DESCRIPTIVE STATISTICS
# =============================================================================
say("3. Descriptive statistics ...")
desc_vars <- c(NUM_COLS, "remainder_wh")
desc <- rbindlist(lapply(desc_vars, function(v) {
  x <- dt[[v]]; q <- quantile(x, c(.25, .5, .75), na.rm = TRUE, names = FALSE)
  data.table(variable = v, unit = UNITS[[v]], n_valid = sum(!is.na(x)), n_missing = sum(is.na(x)),
             mean = mean(x, na.rm = TRUE), median = q[2], sd = sd(x, na.rm = TRUE),
             min = min(x, na.rm = TRUE), q1 = q[1], q3 = q[3], max = max(x, na.rm = TRUE), iqr = q[3] - q[1])
}))

# =============================================================================
# 4. AGGREGATIONS  (weighted-mean friendly: sums and counts are kept)
# =============================================================================
say("4. Aggregating hourly / daily / weekly / monthly ...")
prof <- function(DT, by) DT[valid == TRUE, .(
  n_valid_minutes = .N, sum_active_power_kw = sum(Global_active_power),
  mean_active_power_kw = mean(Global_active_power), median_active_power_kw = median(Global_active_power),
  p95_active_power_kw = quantile(Global_active_power, 0.95, names = FALSE),
  max_active_power_kw = max(Global_active_power), sd_active_power_kw = sd(Global_active_power)), by = by]
hourly_summary <- rbind(prof(dt, c("day_type", "hour")), prof(dt, "hour")[, day_type := "All"], use.names = TRUE)
setcolorder(hourly_summary, c("day_type", "hour")); setorder(hourly_summary, day_type, hour)

heatmap <- dt[valid == TRUE, .(n_valid_minutes = .N, sum_active_power_kw = sum(Global_active_power),
                               mean_active_power_kw = mean(Global_active_power)), by = .(dow, dow_label, hour)]
setorder(heatmap, dow, hour)

# daily table on a COMPLETE calendar so rolling windows and gaps are explicit
daily_obs <- dt[, .(n_valid_minutes = sum(valid),
                    mean_active_power_kw = if (any(valid)) mean(Global_active_power, na.rm = TRUE) else NA_real_,
                    max_active_power_kw = safe_max(Global_active_power),
                    energy_kwh_observed = sum(energy_kwh, na.rm = TRUE)), by = date]
daily <- merge(data.table(date = seq(min(dt$date), max(dt$date), by = "day")), daily_obs, by = "date", all.x = TRUE)
daily[is.na(n_valid_minutes), `:=`(n_valid_minutes = 0L, energy_kwh_observed = 0)]
daily[, coverage_pct := round(100 * n_valid_minutes / 1440, 2)]
# estimated full-day energy = mean power x 24 h, only when coverage is adequate
daily[, energy_kwh_est_full_day := fifelse(n_valid_minutes / 1440 >= MIN_DAILY_COVERAGE, mean_active_power_kw * 24, NA_real_)]
daily[, tt := as.POSIXct(date)]; add_time_features(daily, "tt"); daily[, tt := NULL]
setorder(daily, date)
roll <- function(x, n) { r <- frollmean(x, n, align = "center", na.rm = TRUE); r[is.nan(r)] <- NA_real_; r }
daily[, `:=`(roll7_energy_kwh = roll(energy_kwh_est_full_day, 7L), roll30_energy_kwh = roll(energy_kwh_est_full_day, 30L))]

weekly <- daily[, .(n_valid_minutes = sum(n_valid_minutes), energy_kwh_observed = sum(energy_kwh_observed),
                    peak_active_power_kw = safe_max(max_active_power_kw)),
                by = .(week_start = date - (dow - 1L))]
weekly[, `:=`(coverage_pct = round(100 * n_valid_minutes / (7 * 1440), 2),
              mean_active_power_kw = fifelse(n_valid_minutes > 0, energy_kwh_observed * 60 / n_valid_minutes, NA_real_))]
setorder(weekly, week_start)

monthly <- daily[, .(n_days_with_data = sum(n_valid_minutes > 0), n_valid_minutes = sum(n_valid_minutes),
                     energy_kwh_observed = sum(energy_kwh_observed), peak_active_power_kw = safe_max(max_active_power_kw),
                     n_days_used_for_avg = sum(!is.na(energy_kwh_est_full_day)),
                     avg_daily_energy_kwh = mean(energy_kwh_est_full_day, na.rm = TRUE)), by = .(year, month)]
monthly[is.nan(avg_daily_energy_kwh), avg_daily_energy_kwh := NA_real_]
setorder(monthly, year, month)
monthly[, `:=`(year_month = sprintf("%04d-%02d", year, month), month_name = month.name[month], season = season_of(month),
               days_in_month = days_in_month(as.Date(sprintf("%d-%02d-01", year, month))))]
monthly[, `:=`(coverage_pct = round(100 * n_valid_minutes / (days_in_month * 1440), 2),
               mean_active_power_kw = fifelse(n_valid_minutes > 0, energy_kwh_observed * 60 / n_valid_minutes, NA_real_))]
monthly[, is_partial_month := coverage_pct < 100 * MIN_DAILY_COVERAGE]
ok <- !monthly$is_partial_month
monthly[, `:=`(pct_change_vs_prev_month = fifelse(ok & shift(ok, 1), pct_change(avg_daily_energy_kwh, shift(avg_daily_energy_kwh, 1)), NA_real_),
               pct_change_vs_prev_year  = fifelse(ok & shift(ok, 12), pct_change(avg_daily_energy_kwh, shift(avg_daily_energy_kwh, 12)), NA_real_))]
rm(ok)

seasonal <- daily[!is.na(energy_kwh_est_full_day), .(n_days = .N, mean_daily_energy_kwh = mean(energy_kwh_est_full_day),
                                                     median_daily_energy_kwh = median(energy_kwh_est_full_day), sd_daily_energy_kwh = sd(energy_kwh_est_full_day),
                                                     mean_active_power_kw = mean(mean_active_power_kw)), by = .(season, day_type)]
setorder(seasonal, season, day_type)

wk <- dt[valid == TRUE, .(mean_kw = mean(Global_active_power)), by = day_type]
mean_weekday <- wk[day_type == "Weekday", mean_kw]; mean_weekend <- wk[day_type == "Weekend", mean_kw]
dwk <- daily[!is.na(energy_kwh_est_full_day), .(e = mean(energy_kwh_est_full_day)), by = day_type]
de_weekday <- dwk[day_type == "Weekday", e]; de_weekend <- dwk[day_type == "Weekend", e]

# =============================================================================
# 5. PEAK DEMAND
# =============================================================================
say("5. Peak demand ...")
overall_mean_kw <- mean(dt$Global_active_power, na.rm = TRUE)
peak_periods <- copy(heatmap)[order(-mean_active_power_kw)][1:10]
peak_periods[, `:=`(rank = 1:10, ratio_to_overall_mean = mean_active_power_kw / overall_mean_kw)]
setcolorder(peak_periods, c("rank", "dow_label", "hour"))
pk_i <- which.max(dt$Global_active_power)
peak_minute_kw <- dt$Global_active_power[pk_i]; peak_minute_ts <- dt$timestamp[pk_i]
hr_all <- hourly_summary[day_type == "All"]
peak_hour   <- hr_all[which.max(mean_active_power_kw), hour]
trough_hour <- hr_all[which.min(mean_active_power_kw), hour]

# =============================================================================
# 6. CORRELATION  (Pearson on all rows; Spearman on a seeded sample)
# =============================================================================
say("6. Correlation ...")
cor_vars <- c(NUM_COLS, "remainder_wh")
pear <- cor(dt[, cor_vars, with = FALSE], use = "pairwise.complete.obs", method = "pearson")
set.seed(42)
cc <- which(complete.cases(dt[, cor_vars, with = FALSE]))
cc <- if (length(cc) > SPEARMAN_SAMPLE) sample(cc, SPEARMAN_SAMPLE) else cc
spear <- cor(dt[cc, cor_vars, with = FALSE], method = "spearman")
cor_long <- as.data.table(as.table(pear))
setnames(cor_long, c("variable_1", "variable_2", "pearson_r"))
cor_long[, spearman_rho := as.vector(spear)]
cor_long[, n_complete_rows := sum(complete.cases(dt[, cor_vars, with = FALSE]))]
rm(cc)

# =============================================================================
# 7. ANOMALY DETECTION  (Tukey IQR: global vs time-conditioned thresholds)
#    A flag means "unusual versus comparable periods" -- NOT confirmed waste.
# =============================================================================
say("7. Anomaly detection ...")
hs <- dt[, .(n_valid_minutes = sum(valid), mean_active_power_kw = mean(Global_active_power, na.rm = TRUE)), by = hour_start]
hs[is.nan(mean_active_power_kw) | n_valid_minutes < MIN_HOURLY_VALID, mean_active_power_kw := NA_real_]
setorder(hs, hour_start)
add_time_features(hs, "hour_start"); hs[, grp_all := 1L]

METHODS <- list(global = "grp_all", hour_of_week = "hour_of_week", season_hour_of_week = c("season", "hour_of_week"))
thr <- list()
for (lvl in c("minute", "hourly")) {
  DT <- if (lvl == "minute") dt else hs
  vcol <- if (lvl == "minute") "Global_active_power" else "mean_active_power_kw"
  min_n <- if (lvl == "minute") MIN_N_MINUTE_GROUP else MIN_N_HOUR_GROUP
  for (m in names(METHODS)) {
    by <- METHODS[[m]]
    b  <- iqr_table(DT, vcol, by, min_n)
    bnd <- attach_bounds(DT, b, by)
    set(DT, j = paste0("hi_", m), value = DT[[vcol]] > bnd$upper)
    set(DT, j = paste0("lo_", m), value = DT[[vcol]] < bnd$lower)
    b[, `:=`(level = lvl, method = m)]
    thr[[paste(lvl, m)]] <- b
  }
}
thresholds <- rbindlist(thr, fill = TRUE)
thresholds[, grp_all := NULL]
setcolorder(thresholds, c("level", "method"))

anom_summary <- rbindlist(lapply(c("minute", "hourly"), function(lvl) {
  DT <- if (lvl == "minute") dt else hs
  rbindlist(lapply(names(METHODS), function(m) {
    hi <- paste0("hi_", m); lo <- paste0("lo_", m)
    groups <- list(overall = NULL, year_month = "year_month", hour = "hour", weekday = "dow_label", season = "season")
    out <- rbindlist(lapply(names(groups), function(gt) {
      bycol <- groups[[gt]]
      r <- if (is.null(bycol)) DT[, .(n_evaluated = sum(!is.na(get(hi))), n_high = sum(get(hi), na.rm = TRUE), n_low = sum(get(lo), na.rm = TRUE))]
      else DT[, .(n_evaluated = sum(!is.na(get(hi))), n_high = sum(get(hi), na.rm = TRUE), n_low = sum(get(lo), na.rm = TRUE)), by = bycol]
      if (is.null(bycol)) r[, group_value := "all"] else { setnames(r, bycol, "group_value"); r[, group_value := as.character(group_value)] }
      r[, group_type := gt]
    }), use.names = TRUE)
    out[, `:=`(level = lvl, method = m, n_flagged = n_high + n_low)]
    out[, pct_flagged := fifelse(n_evaluated > 0, 100 * n_flagged / n_evaluated, NA_real_)]
    out
  }))
}))
setcolorder(anom_summary, c("level", "method", "group_type", "group_value", "n_evaluated", "n_high", "n_low", "n_flagged", "pct_flagged"))

# hourly series with flags (Tableau time series) and flagged-only records
hs[, `:=`(flag_global = as.integer(hi_global | lo_global),
          flag_hour_of_week = as.integer(hi_hour_of_week | lo_hour_of_week),
          flag_season_hour_of_week = as.integer(hi_season_hour_of_week | lo_season_hour_of_week))]
b_how <- thresholds[level == "hourly" & method == "hour_of_week", .(hour_of_week, expected_upper_kw = upper, expected_lower_kw = lower)]
hs <- merge(hs, b_how, by = "hour_of_week", all.x = TRUE, sort = FALSE); setorder(hs, hour_start)
hs[, severity_ratio := mean_active_power_kw / expected_upper_kw]
hs[, direction := fifelse(hi_hour_of_week %in% TRUE, "High", fifelse(lo_hour_of_week %in% TRUE, "Low", NA_character_))]
anom_hours_day <- hs[flag_hour_of_week %in% 1L, .(anomaly_hours = .N), by = date]
daily <- merge(daily, anom_hours_day, by = "date", all.x = TRUE); setorder(daily, date)
daily[is.na(anomaly_hours), anomaly_hours := 0L]

# =============================================================================
# 8. SUB-METERING  (only rows where total + all 3 sub-meters are valid)
# =============================================================================
say("8. Sub-metering ...")
sm <- dt[!is.na(remainder_wh)]
sub_month <- sm[, .(n_valid_minutes = .N, active_energy_kwh_total = sum(energy_kwh),
                    Sub_metering_1 = sum(Sub_metering_1) / 1000, Sub_metering_2 = sum(Sub_metering_2) / 1000,
                    Sub_metering_3 = sum(Sub_metering_3) / 1000, remainder_wh = sum(remainder_wh) / 1000), by = year_month]
sub_summary <- melt(sub_month, id.vars = c("year_month", "n_valid_minutes", "active_energy_kwh_total"),
                    variable.name = "category_id", value.name = "energy_kwh")
sub_summary[, `:=`(category = SUB_LABELS[as.character(category_id)],
                   pct_of_active_energy = 100 * energy_kwh / active_energy_kwh_total)]
sub_summary[, category_id := fifelse(category_id == "remainder_wh", "Remainder", as.character(category_id))]
setorder(sub_summary, year_month, category_id)
sub_hour_wide <- sm[, .(n_valid_minutes = .N, Sub_metering_1 = sum(Sub_metering_1) / 1000, Sub_metering_2 = sum(Sub_metering_2) / 1000,
                        Sub_metering_3 = sum(Sub_metering_3) / 1000, remainder_wh = sum(remainder_wh) / 1000), by = .(day_type, hour)]
sub_hourly <- melt(sub_hour_wide, id.vars = c("day_type", "hour", "n_valid_minutes"), variable.name = "category_id", value.name = "energy_kwh")
sub_hourly[, `:=`(category = SUB_LABELS[as.character(category_id)], mean_wh_per_minute = energy_kwh * 1000 / n_valid_minutes)]
sub_hourly[, category_id := fifelse(category_id == "remainder_wh", "Remainder", as.character(category_id))]
setorder(sub_hourly, day_type, hour, category_id)
sub_overall <- sub_summary[, .(energy_kwh = sum(energy_kwh)), by = category][, pct := 100 * energy_kwh / sum(energy_kwh)]
rm(sm)

# =============================================================================
# 9. KPIs AND TABLEAU EXPORTS
# =============================================================================
say("9. Writing exports ...")
hs_flag_n   <- sum(hs$flag_hour_of_week, na.rm = TRUE); hs_eval_n <- sum(!is.na(hs$flag_hour_of_week))
kpi <- rbindlist(list(
  data.table(metric = "total_records_analyzed", value = nrow(dt), unit = "rows", note = "Rows after cleaning (minute observations)"),
  data.table(metric = "valid_active_power_records", value = sum(dt$valid), unit = "rows", note = "Rows with Global_active_power observed"),
  data.table(metric = "pct_records_missing", value = 100 * mean(!dt$valid), unit = "%", note = "Share of rows with missing measurements"),
  data.table(metric = "average_active_power", value = overall_mean_kw, unit = "kW", note = "Mean of valid minute readings"),
  data.table(metric = "peak_active_power", value = peak_minute_kw, unit = "kW", note = paste("Highest minute reading at", format(peak_minute_ts, "%Y-%m-%d %H:%M"))),
  data.table(metric = "avg_daily_energy", value = mean(daily$energy_kwh_est_full_day, na.rm = TRUE), unit = "kWh/day", note = paste0("Days with >=", 100 * MIN_DAILY_COVERAGE, "% coverage; mean power x 24 h")),
  data.table(metric = "weekday_mean_power", value = mean_weekday, unit = "kW", note = "Mean of valid minutes Mon-Fri"),
  data.table(metric = "weekend_mean_power", value = mean_weekend, unit = "kW", note = "Mean of valid minutes Sat-Sun"),
  data.table(metric = "weekend_vs_weekday_pct_change", value = pct_change(mean_weekend, mean_weekday), unit = "%", note = "((weekend - weekday)/weekday)*100"),
  data.table(metric = "peak_hour_of_day", value = peak_hour, unit = "hour", note = "Hour with highest mean active power (all days)"),
  data.table(metric = "anomalous_hours_hour_of_week_iqr", value = hs_flag_n, unit = "hours", note = "Hourly means outside hour-of-week IQR fences"),
  data.table(metric = "anomalous_hours_pct", value = 100 * hs_flag_n / hs_eval_n, unit = "%", note = "Flagged / evaluated hours"),
  data.table(metric = "correlation_active_power_vs_intensity", value = pear["Global_active_power", "Global_intensity"], unit = "r", note = "Pearson")
))

exports <- list(
  daily_summary = daily[, .(date, year, month, month_name, dow_label, day_type, season, n_valid_minutes, coverage_pct,
                            mean_active_power_kw, max_active_power_kw, energy_kwh_observed, energy_kwh_est_full_day,
                            roll7_energy_kwh, roll30_energy_kwh, anomaly_hours)],
  weekly_summary = weekly,
  monthly_summary = monthly[, .(year_month, year, month, month_name, season, days_in_month, n_days_with_data, n_valid_minutes,
                                coverage_pct, is_partial_month, mean_active_power_kw, peak_active_power_kw, energy_kwh_observed,
                                n_days_used_for_avg, avg_daily_energy_kwh, pct_change_vs_prev_month, pct_change_vs_prev_year)],
  hourly_summary = hourly_summary,
  hour_weekday_heatmap = heatmap,
  seasonal_summary = seasonal,
  peak_periods = peak_periods,
  hourly_series = hs[, .(hour_start, date, year_month, season, dow_label, hour, hour_of_week, n_valid_minutes, mean_active_power_kw,
                         expected_upper_kw, expected_lower_kw, severity_ratio, direction, flag_global, flag_hour_of_week, flag_season_hour_of_week)],
  anomaly_records = hs[flag_hour_of_week %in% 1L, .(hour_start, date, year_month, season, dow_label, hour, mean_active_power_kw,
                                                    expected_upper_kw, severity_ratio, direction, flag_global, flag_season_hour_of_week)],
  anomaly_summary = anom_summary,
  anomaly_thresholds = thresholds,
  correlation_matrix = cor_long,
  submetering_summary = sub_summary[, .(year_month, category_id, category, n_valid_minutes, energy_kwh, active_energy_kwh_total, pct_of_active_energy)],
  submetering_hourly = sub_hourly[, .(day_type, hour, category_id, category, n_valid_minutes, energy_kwh, mean_wh_per_minute)],
  kpi_summary = kpi,
  data_quality_report = rbindlist(quality)
)
for (nm in names(exports)) write_csv(exports[[nm]], file.path(DIR_PROC, paste0(nm, ".csv")))
if (EXPORT_MINUTE_CSV) write_csv(dt[, c("timestamp", NUM_COLS), with = FALSE], file.path(DIR_PROC, "cleaned_minute_data.csv"))

write_csv(clean_log,    file.path(DIR_TAB, "cleaning_log.csv"))
write_csv(miss_var,     file.path(DIR_TAB, "missingness_by_variable.csv"))
write_csv(miss_month,   file.path(DIR_TAB, "missingness_by_month.csv"))
write_csv(head(runs, 25), file.path(DIR_TAB, "missing_runs_top25.csv"))
write_csv(interval_tab, file.path(DIR_TAB, "interval_distribution.csv"))
write_csv(desc,         file.path(DIR_TAB, "descriptive_statistics.csv"))

# =============================================================================
# 10. FIGURES
# =============================================================================
say("10. Figures ...")
fig_fail <- character()
th <- theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold", colour = COL[["navy"]]),
                                            plot.subtitle = element_text(colour = "grey35"), panel.grid.minor = element_blank(), legend.position = "bottom")
save_fig <- function(p, name, w = 10, h = 5.5) {
  ok <- tryCatch({ ggsave(file.path(DIR_FIG, name), p, width = w, height = h, dpi = 150, bg = "white"); TRUE },
                 error = function(e) { fig_fail <<- c(fig_fail, paste(name, conditionMessage(e))); FALSE })
  invisible(ok)
}
dt_cols <- c(Weekday = COL[["navy"]], Weekend = COL[["amber"]])
sub_cols <- c("Kitchen" = COL[["teal"]], "Laundry" = COL[["amber"]],
              "Water heater & air-conditioner" = COL[["red"]], "Not sub-metered (remainder)" = COL[["grey"]])

save_fig(ggplot(hourly_summary[day_type != "All"], aes(hour, mean_active_power_kw, colour = day_type)) +
           geom_line(linewidth = 1.1) + geom_point(size = 1.6) + scale_colour_manual(values = dt_cols, name = NULL) +
           scale_x_continuous(breaks = 0:23) + labs(title = "Average active power by hour of day", subtitle = "Mean of valid minute readings (kW)",
                                                    x = "Hour of day", y = "Mean active power (kW)") + th, "fig01_hourly_profile.png")

hm <- copy(heatmap)[, dow_label := factor(dow_label, levels = rev(DOW_LABELS))]
save_fig(ggplot(hm, aes(hour, dow_label, fill = mean_active_power_kw)) + geom_tile(colour = "white") +
           scale_fill_gradient(low = COL[["light"]], high = COL[["red"]], name = "kW") + scale_x_continuous(breaks = 0:23, expand = c(0, 0)) +
           labs(title = "When does demand peak? Hour x weekday", subtitle = "Mean active power (kW)", x = "Hour of day", y = NULL) + th, "fig02_heatmap_hour_weekday.png")

mp <- copy(monthly)[, month_start := as.Date(sprintf("%d-%02d-01", year, month))]
save_fig(ggplot(mp, aes(month_start, avg_daily_energy_kwh, fill = is_partial_month)) + geom_col(na.rm = TRUE) +
           scale_fill_manual(values = c("FALSE" = COL[["teal"]], "TRUE" = "#BDBDBD"), labels = c("Adequate coverage", "Partial / low coverage"), name = NULL) +
           labs(title = "Average daily energy by month", subtitle = "Mean of days with adequate coverage (kWh/day); grey = low-coverage month",
                x = NULL, y = "kWh per day") + th, "fig03_monthly_energy.png")

save_fig(ggplot(daily, aes(date)) +
           geom_line(aes(y = roll7_energy_kwh, colour = "7-day"), linewidth = .45, alpha = .7, na.rm = TRUE) +
           geom_line(aes(y = roll30_energy_kwh, colour = "30-day"), linewidth = 1.2, na.rm = TRUE) +
           scale_colour_manual(values = c("7-day" = COL[["amber"]], "30-day" = COL[["navy"]]), breaks = c("30-day", "7-day"), name = NULL) +
           scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, .05))) +
           labs(title = "Daily energy with rolling averages", subtitle = "Rolling means of estimated full-day energy (kWh per day)", x = NULL, y = "kWh per day") + th,
         "fig04_daily_energy_rolling.png")

sb <- daily[!is.na(energy_kwh_est_full_day)][, season := factor(season, levels = SEASONS)]
save_fig(ggplot(sb, aes(season, energy_kwh_est_full_day, fill = day_type)) + geom_boxplot(outlier.alpha = .3) +
           scale_fill_manual(values = dt_cols, name = NULL) + labs(title = "Daily energy by season and day type", x = NULL, y = "kWh per day") + th, "fig05_season_boxplot.png")

tryCatch({ png(file.path(DIR_FIG, "fig06_correlation_heatmap.png"), width = 1700, height = 1500, res = 200)
  corrplot(pear, method = "color", type = "upper", addCoef.col = "black", number.cex = .65, tl.col = "black", tl.srt = 45,
           col = colorRampPalette(c(COL[["red"]], "white", COL[["navy"]]))(200), title = "Pearson correlation", mar = c(0, 0, 2, 0))
  dev.off() }, error = function(e) { try(dev.off(), silent = TRUE); fig_fail <<- c(fig_fail, paste("fig06", conditionMessage(e))) })

sp <- copy(sub_summary)[, `:=`(month_start = as.Date(paste0(year_month, "-01")), category = factor(category, levels = names(sub_cols)))]
save_fig(ggplot(sp, aes(month_start, energy_kwh, fill = category)) + geom_col(position = "fill", width = 28) +
           scale_fill_manual(values = sub_cols, name = NULL) + scale_y_continuous(labels = percent) +
           labs(title = "Where does the energy go?", subtitle = "Share of active energy by sub-metering category (sub-meters do not cover the whole house)", x = NULL, y = "Share") + th, "fig07_submetering_share.png")

hs_flag <- hs[flag_hour_of_week %in% 1L]
save_fig(ggplot(hs, aes(hour_start, mean_active_power_kw)) + geom_line(colour = "#C9D1DB", linewidth = .15, na.rm = TRUE) +
           geom_point(data = hs_flag, colour = COL[["red"]], size = .5, alpha = .7) +
           labs(title = "Hourly consumption with flagged hours", subtitle = "Red = outside the hour-of-week IQR fences (a statistical flag, not proof of waste)", x = NULL, y = "Hourly mean active power (kW)") + th, "fig08_anomaly_timeline.png")

am <- anom_summary[level == "hourly" & method == "hour_of_week" & group_type == "year_month"][, month_start := as.Date(paste0(group_value, "-01"))]
save_fig(ggplot(am, aes(month_start, pct_flagged)) + geom_col(fill = COL[["red"]], na.rm = TRUE) +
           labs(title = "Share of hours flagged by month", subtitle = "Hour-of-week IQR method, hourly means", x = NULL, y = "% of evaluated hours") + th, "fig09_anomalies_by_month.png")

ah <- anom_summary[level == "hourly" & method %in% c("global", "hour_of_week") & group_type == "hour"][, hour := as.integer(group_value)]
save_fig(ggplot(ah, aes(hour, pct_flagged, colour = method)) + geom_line(linewidth = 1.1) + geom_point() +
           scale_colour_manual(values = c(global = COL[["grey"]], hour_of_week = COL[["red"]]), labels = c("Global threshold", "Hour-of-week threshold"), name = NULL) +
           scale_x_continuous(breaks = 0:23) + labs(title = "Flagged hours by hour of day", subtitle = "A single global threshold concentrates flags in naturally busy hours", x = "Hour of day", y = "% of hours flagged") + th,
         "fig10_anomalies_by_hour.png")

th_how <- thresholds[level == "hourly" & method == "hour_of_week"]
th_glob <- thresholds[level == "hourly" & method == "global", upper]
med_how <- hs[!is.na(mean_active_power_kw), .(med = median(mean_active_power_kw)), by = hour_of_week]
save_fig(ggplot() + geom_step(data = th_how, aes(hour_of_week, upper, colour = "Hour-of-week upper fence"), linewidth = .9, na.rm = TRUE) +
           geom_line(data = med_how, aes(hour_of_week, med, colour = "Median hourly mean"), linewidth = .8) +
           geom_hline(aes(yintercept = th_glob, colour = "Global upper fence"), linetype = 2, linewidth = .9) +
           scale_colour_manual(values = c("Hour-of-week upper fence" = COL[["red"]], "Median hourly mean" = COL[["navy"]], "Global upper fence" = COL[["grey"]]), name = NULL) +
           scale_x_continuous(breaks = seq(0, 168, 24), labels = c(DOW_LABELS, "")) +
           labs(title = "Global vs time-conditioned anomaly thresholds", subtitle = "Hourly mean active power (kW), Monday 00:00 to Sunday 23:00", x = NULL, y = "kW") + th, "fig11_threshold_comparison.png")

mm <- copy(miss_month)[, month_start := as.Date(paste0(year_month, "-01"))]
save_fig(ggplot(mm, aes(month_start, pct_missing)) + geom_col(fill = COL[["amber"]]) +
           labs(title = "Missing measurements by month", subtitle = "% of rows with Global_active_power missing", x = NULL, y = "% missing") + th, "fig12_missingness_by_month.png")
if (length(fig_fail)) warning("Some figures failed: ", paste(fig_fail, collapse = " | "))

# =============================================================================
# 11. FINDINGS REPORT AND STATIC DASHBOARD PREVIEW (all numbers computed above)
# =============================================================================
say("11. Findings and dashboard preview ...")
mh <- function(h) sprintf("%02d:00", as.integer(h))
mo <- monthly[!is_partial_month & !is.na(avg_daily_energy_kwh), .(avg = mean(avg_daily_energy_kwh)), by = .(month, month_name)][order(-avg)]
se <- seasonal[, .(avg = mean(mean_daily_energy_kwh)), by = season][order(-avg)]
cr <- cor_long[variable_1 < variable_2][order(-abs(pearson_r))][1:5]
ag <- anom_summary[level == "hourly" & group_type == "overall", .(method, n_flagged, pct_flagged)]
am_top <- anom_summary[level == "hourly" & method == "hour_of_week" & group_type == "year_month" & n_evaluated >= 300][order(-pct_flagged)][1:3]
ah_top <- anom_summary[level == "hourly" & method == "hour_of_week" & group_type == "hour"][order(-pct_flagged)][1:3]
pk <- peak_periods[1:3]
pm <- function(m) ag[method == m, pct_flagged]
pct_txt <- function(x) ifelse(is.na(x), "not evaluated (groups too small)", paste0(fmt(x), "%"))
miss_pct <- 100 * mean(!dt$valid)
first_day <- format(min(dt$date)); last_day <- format(max(dt$date))
sub_pct <- function(cat) sub_overall[category == cat, pct]

find_lines <- c(
  sprintf("1. **Coverage.** %s minute records from %s to %s; %s%% of rows have no measurement (missing ('?') rows were left as NA, never zero-filled). Longest missing stretch: %s consecutive rows (about %s hours).",
          format(nrow(dt), big.mark = ","), first_day, last_day, fmt(miss_pct), format(longest_run, big.mark = ","), fmt(longest_run / 60, 1)),
  sprintf("2. **Daily rhythm.** Mean active power is highest at %s (%s kW) and lowest at %s (%s kW); the all-day mean is %s kW.",
          mh(peak_hour), fmt(hr_all[hour == peak_hour, mean_active_power_kw]), mh(trough_hour), fmt(hr_all[hour == trough_hour, mean_active_power_kw]), fmt(overall_mean_kw)),
  sprintf("3. **Highest-demand hour-of-week slots (mean kW):** %s.", paste(sprintf("%s %s (%s)", pk$dow_label, mh(pk$hour), fmt(pk$mean_active_power_kw)), collapse = "; ")),
  sprintf("4. **Weekday vs weekend.** Mean power is %s kW on weekdays and %s kW on weekends (%s%% change, weekend relative to weekday). Average estimated daily energy: %s vs %s kWh/day.",
          fmt(mean_weekday), fmt(mean_weekend), fmt(pct_change(mean_weekend, mean_weekday), 1), fmt(de_weekday, 1), fmt(de_weekend, 1)),
  sprintf("5. **Seasonality** (days with adequate coverage). Average daily energy by season: %s.", paste(sprintf("%s %s kWh/day", se$season, fmt(se$avg, 1)), collapse = ", ")),
  sprintf("6. **Calendar months** ranked by average daily energy (complete months only): highest %s (%s kWh/day), lowest %s (%s kWh/day).",
          mo$month_name[1], fmt(mo$avg[1], 1), mo$month_name[nrow(mo)], fmt(mo$avg[nrow(mo)], 1)),
  sprintf("7. **Strongest correlations (Pearson r):** %s. Correlation does not imply causation; power, current and voltage are physically linked by construction.",
          paste(sprintf("%s ~ %s = %s", cr$variable_1, cr$variable_2, fmt(cr$pearson_r, 2)), collapse = "; ")),
  sprintf("8. **Sub-metering share of active energy:** %s. The remainder is energy not captured by the three sub-meters, so the sub-meters do NOT account for the whole household.",
          paste(sprintf("%s %s%%", sub_overall$category, fmt(sub_overall$pct, 1)), collapse = "; ")),
  sprintf("9. **Anomalies (hourly means).** Share of hours flagged: global IQR %s, hour-of-week IQR %s, season x hour-of-week IQR %s. Time-conditioned thresholds ask 'unusual for this hour of the week', which a single global threshold cannot.",
          pct_txt(pm("global")), pct_txt(pm("hour_of_week")), pct_txt(pm("season_hour_of_week"))),
  sprintf("10. **Where flags concentrate (hour-of-week method).** Months with the highest flagged share: %s. Hours of day: %s.",
          paste(sprintf("%s (%s%%)", am_top$group_value, fmt(am_top$pct_flagged, 1)), collapse = ", "),
          paste(sprintf("%s (%s%%)", mh(as.integer(ah_top$group_value)), fmt(ah_top$pct_flagged, 1)), collapse = ", ")))

rec_lines <- c(
  sprintf("- **Review loads running in the peak window.** Demand peaks around %s; check whether flexible loads (e.g. water heating, laundry, dishwashing) could be shifted outside the highest-demand slots listed in finding 3.", mh(peak_hour)),
  sprintf("- **Investigate the unmetered remainder.** %s%% of measured active energy is outside the three sub-meters; an audit of what runs on that circuit would explain the largest unknown.", fmt(sub_pct("Not sub-metered (remainder)"), 1)),
  "- **Inspect flagged periods.** Start with the months and hours in finding 10 and the largest severity ratios in anomaly_records.csv; confirm against occupancy, appliance use or weather before concluding anything is wasteful.",
  "- **Compare like with like.** Use the weekday/weekend and seasonal differences (findings 4-5) as baselines when judging future consumption; do not label differences as 'savings' without a defined baseline period.",
  "- **Close the data gaps.** Long missing stretches (finding 1) limit conclusions for those months; add redundancy in metering or logging."
)
lim_lines <- c(
  "- One household, one country, 2006-2010; results do not generalise to other homes or today's appliances.",
  "- A statistical anomaly is an observation unusual relative to comparable periods. It is not confirmed waste, a fault, or a cause.",
  "- Missing rows are treated as missing (not zero); monthly totals for low-coverage months are underestimates, so comparisons use average daily energy from adequate-coverage days.",
  "- Daily energy estimates use mean power x 24 h on days with at least 90% coverage; this assumes the observed minutes represent the whole day.",
  "- Sub-metering covers only three circuits; the remainder is a derived quantity and can be slightly negative from measurement noise.",
  "- IQR fences assume comparable periods have similar distributions; seasonal effects can remain inside the hour-of-week groups (see the season x hour-of-week method)."
)
report <- c("# Findings and Recommendations", "",
            "_All numbers below are computed from the data by `R/run_all.R`; nothing is hand-entered._", "",
            "## Key findings", "", find_lines, "", "## Recommendations (investigation-oriented)", "", rec_lines, "",
            "## Limitations", "", lim_lines, "")
writeLines(report, file.path(DIR_REP, "findings_and_recommendations.md"))

# ---- static dashboard preview: one tab per dashboard, one idea per screen ----------------------
md2html <- function(x) gsub("\\*\\*(.+?)\\*\\*", "<b>\\1</b>", gsub("`", "", x))
fig  <- function(f, cap, alt = cap) paste0('<figure class="fig"><img loading="lazy" src="../figures/', f, '" alt="', alt, '"><figcaption>', cap, '</figcaption></figure>')
kpic <- function(label, val, unit, sub) paste0('<div class="kpi"><div class="kl">', label, '</div><div class="kv">', val, '<span>', unit, '</span></div><div class="ks">', sub, '</div></div>')
panel <- function(id, heading, lede, body, on = FALSE)
  paste0('<section class="panel', if (on) ' on' else '', '" id="', id, '"><h2>', heading, '</h2><p class="lede">', lede, '</p>', body, '</section>')

days_n   <- sum(daily$n_valid_minutes > 0)
we_dir   <- if (isTRUE(pct_change(mean_weekend, mean_weekday) >= 0)) "higher" else "lower"
metered  <- sub_overall[category != "Not sub-metered (remainder)"][which.max(pct)]
kpi_html <- paste0('<div class="kpis">',
                   kpic("Records analysed", format(nrow(dt), big.mark = ","), "", "minute observations"),
                   kpic("Average power", fmt(overall_mean_kw), " kW", "valid readings only"),
                   kpic("Peak power", fmt(peak_minute_kw), " kW", format(peak_minute_ts, "%d %b %Y, %H:%M")),
                   kpic("Daily energy", fmt(kpi[metric == "avg_daily_energy", value], 1), " kWh/day", "days with 90%+ coverage"),
                   kpic("Flagged hours", format(hs_flag_n, big.mark = ","), paste0(" (", fmt(100 * hs_flag_n / hs_eval_n, 1), "%)"), "hour-of-week IQR"), '</div>')

pv_find <- vapply(find_lines, function(x) {
  lead <- sub("\\.$", "", sub("^[0-9]+\\. \\*\\*(.+?)\\*\\*.*$", "\\1", x))
  body <- md2html(sub("^[0-9]+\\. \\*\\*.+?\\*\\* ?", "", x))
  paste0('<div class="find"><div class="fh">', lead, '</div><p>', body, '</p></div>') }, character(1))

p1 <- panel("p1", "Energy overview",
            sprintf("Across %s days the household averaged %s kW, about %s kWh per day. The thick line shows the long-term level; the thin line shows short-term swings.",
                    format(days_n, big.mark = ","), fmt(overall_mean_kw), fmt(kpi[metric == "avg_daily_energy", value], 1)),
            paste0(kpi_html, '<div class="stack">',
                   fig("fig04_daily_energy_rolling.png", "Daily energy (kWh/day) with 7-day and 30-day rolling means. Days with too little data are left blank, never drawn as zero."),
                   fig("fig03_monthly_energy.png", "Average daily energy per month. Grey bars are months with incomplete data, so compare the teal bars."), '</div>'), TRUE)
p2 <- panel("p2", "When does demand peak?",
            sprintf("Demand is highest at %s (%s kW) and lowest at %s (%s kW). Weekend average power is %s%% %s than weekday.",
                    mh(peak_hour), fmt(hr_all[hour == peak_hour, mean_active_power_kw]), mh(trough_hour), fmt(hr_all[hour == trough_hour, mean_active_power_kw]),
                    fmt(abs(pct_change(mean_weekend, mean_weekday)), 1), we_dir),
            paste0('<div class="stack">',
                   fig("fig02_heatmap_hour_weekday.png", "Darker red cells are the busiest hour-of-week slots. Look for the evening band and for how weekends differ."),
                   '<div class="pair">', fig("fig01_hourly_profile.png", "Average power by hour of day, weekday vs weekend."),
                   fig("fig05_season_boxplot.png", "Daily energy by season: winter is the clearest high-use period."), '</div></div>'))
p3 <- panel("p3", "The anomaly detective",
            sprintf("%s of %s evaluated hours (%s%%) are unusual for their hour of the week. A flag marks a period worth investigating, not proof of waste.",
                    format(hs_flag_n, big.mark = ","), format(hs_eval_n, big.mark = ","), fmt(100 * hs_flag_n / hs_eval_n, 1)),
            paste0('<div class="note"><b>How a flag is decided.</b> An hourly average is flagged when it falls outside Q1 &minus; 1.5 &times; IQR or Q3 + 1.5 &times; IQR of comparable hours (same hour of the week). Groups with fewer than 30 hours are not evaluated.</div>',
                   '<div class="stack">', fig("fig08_anomaly_timeline.png", "Hourly average power; red dots are flagged hours."),
                   '<div class="pair">', fig("fig09_anomalies_by_month.png", "Share of hours flagged in each month."),
                   fig("fig10_anomalies_by_hour.png", "Which hours of the day get flagged, global vs hour-of-week threshold."), '</div>',
                   fig("fig11_threshold_comparison.png", "A single global threshold ignores the daily rhythm; the hour-of-week threshold follows it."), '</div>'))
p4 <- panel("p4", "Where does the energy go?",
            sprintf("%s%% of measured active energy is not captured by the three sub-meters. Among the metered circuits, %s is largest at %s%%.",
                    fmt(sub_pct("Not sub-metered (remainder)"), 1), metered$category, fmt(metered$pct, 1)),
            paste0('<div class="pair">', fig("fig07_submetering_share.png", "Monthly share of active energy by category. The sub-meters do not cover the whole house."),
                   fig("fig06_correlation_heatmap.png", "Pearson correlation between electrical measurements. Correlation does not prove causation."), '</div>'))
p5 <- panel("p5", "Insights and recommendations",
            "Every number below is computed from the data. Recommendations point to what to investigate next; none claims energy saved.",
            paste0('<div class="finds">', paste(pv_find, collapse = ""), '</div><h3>Recommendations</h3><ul class="list">', paste0("<li>", md2html(sub("^- ", "", rec_lines)), "</li>", collapse = ""), '</ul>',
                   '<details><summary>Limitations and data quality</summary><ul class="list">', paste0("<li>", md2html(sub("^- ", "", lim_lines)), "</li>", collapse = ""), '</ul>',
                   fig("fig12_missingness_by_month.png", "Missing measurements by month."), '</details>'))

css <- r"---(:root{--bg:#F4F6F8;--ink:#1F2A37;--mut:#5B6B7B;--navy:#1F3A5F;--teal:#2A9D8F;--amber:#E9A23B;--card:#fff}
*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--ink);font:15px/1.55 "Segoe UI",Inter,Arial,sans-serif}
header{background:var(--navy);color:#fff;position:sticky;top:0;z-index:5;box-shadow:0 2px 8px #0003}
.hd{max-width:1200px;margin:0 auto;padding:14px 28px 0}
h1{margin:0;font-size:20px;letter-spacing:.14em}.sub{margin:2px 0 10px;font-size:13px;opacity:.75}
nav{display:flex;gap:4px;flex-wrap:wrap}
.tab{border:0;background:transparent;color:#cfd9e6;font:inherit;font-size:14px;padding:10px 16px;border-radius:8px 8px 0 0;cursor:pointer}
.tab:hover{color:#fff}.tab.on{background:var(--bg);color:var(--navy);font-weight:600}
main{max-width:1200px;margin:0 auto;padding:28px 28px 56px}
.panel{display:none}.panel.on{display:block}
h2{margin:0 0 6px;font-size:26px;color:var(--navy)}h3{margin:32px 0 8px;color:var(--navy)}
.lede{margin:0 0 24px;color:var(--mut);max-width:820px;font-size:16px}
.kpis{display:grid;grid-template-columns:repeat(auto-fit,minmax(190px,1fr));gap:16px;margin-bottom:28px}
.kpi,.fig,.find{background:var(--card);border-radius:12px;box-shadow:0 1px 3px #1f3a5f1a}
.kpi{padding:18px 20px}.kl{font-size:12px;text-transform:uppercase;letter-spacing:.06em;color:var(--mut)}
.kv{font-size:32px;font-weight:700;color:var(--navy);line-height:1.2;margin:4px 0}.kv span{font-size:14px;font-weight:500;color:var(--mut)}
.ks{font-size:12.5px;color:var(--mut)}
.stack{display:grid;gap:24px}.pair{display:grid;grid-template-columns:repeat(auto-fit,minmax(min(420px,100%),1fr));gap:24px}
.fig{margin:0;padding:16px}.fig img{width:100%;height:auto;display:block;border-radius:6px}
.fig figcaption{margin-top:10px;font-size:13.5px;color:var(--mut)}
.note{background:#FFF6E5;border:1px solid #F3D9A4;padding:12px 16px;border-radius:10px;margin-bottom:24px;max-width:900px}
.finds{display:grid;grid-template-columns:repeat(auto-fit,minmax(min(340px,100%),1fr));gap:16px}
.find{padding:16px 18px}.fh{font-weight:700;color:var(--navy);margin-bottom:4px}.find p{margin:0;color:var(--ink);font-size:14px}
.list{padding-left:20px;max-width:900px}.list li{margin:8px 0}
details{margin-top:28px}summary{cursor:pointer;font-weight:600;color:var(--navy);margin-bottom:12px}
footer{max-width:1200px;margin:0 auto;padding:0 28px 32px;color:var(--mut);font-size:12.5px}
@media print{header{position:static}.tab{display:none}.panel{display:block!important;margin-bottom:32px}}
)---"
js <- r"---(const tabs=[...document.querySelectorAll('.tab')],panels=[...document.querySelectorAll('.panel')];
function show(id){if(!document.getElementById(id))id='p1';tabs.forEach(t=>t.classList.toggle('on',t.dataset.p===id));panels.forEach(p=>p.classList.toggle('on',p.id===id));history.replaceState(null,'','#'+id);window.scrollTo(0,0);}
tabs.forEach(t=>t.addEventListener('click',()=>show(t.dataset.p)));show((location.hash||'#p1').slice(1));)---"
nav <- paste0('<button class="tab" data-p="p', 1:5, '">', c("Overview", "Demand peaks", "Anomaly detective", "Where energy goes", "Insights"), '</button>', collapse = "")
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>The Invisible Energy Leak</title><style>', css,
               '</style></head><body><header><div class="hd"><h1>THE INVISIBLE ENERGY LEAK</h1><div class="sub">Discovering hidden patterns in household electricity consumption (static preview from R; the interactive version is built in Tableau)</div><nav>', nav,
               '</nav></div></header><main>', p1, p2, p3, p4, p5, '</main><footer>Source: UCI Individual Household Electric Power Consumption. All figures computed by R/run_all.R.</footer><script>', js, '</script></body></html>')
writeLines(html, file.path(DIR_REP, "dashboard_preview.html"))

# =============================================================================
# 12. VALIDATION  (unit tests of helpers + R-side consistency checks)
# =============================================================================
say("12. Validation ...")
checks <- list()
chk <- function(name, expr) { ok <- tryCatch(isTRUE(expr), error = function(e) FALSE); checks[[length(checks) + 1L]] <<- data.table(test = name, passed = ok) }
rel_eq <- function(a, b) abs(a - b) <= 1e-8 * max(1, abs(a), abs(b))

chk("pct_change(10,5) == 100", pct_change(10, 5) == 100)
chk("pct_change zero denominator -> NA", is.na(pct_change(5, 0)))
chk("pct_change missing -> NA", is.na(pct_change(NA_real_, 5)) && is.na(pct_change(5, NA_real_)))
chk("season mapping Dec/Mar/Jul/Oct", identical(season_of(c(12L, 3L, 7L, 10L)), c("Winter", "Spring", "Summer", "Autumn")))
tmp <- data.table(v = c(1:9, 100), g = 1L); tb <- iqr_table(tmp, "v", "g", 5L)
chk("IQR fences on 1..9,100 (Q1=3.25,Q3=7.75,upper=14.5)", rel_eq(tb$q1, 3.25) && rel_eq(tb$q3, 7.75) && rel_eq(tb$upper, 14.5) && sum(tmp$v > tb$upper) == 1)
chk("IQR bounds withheld when group too small", is.na(iqr_table(tmp, "v", "g", 50L)$upper))
chk("2007-01-01 is Monday (dow=1) and hour_of_week in 0..167",
    { x <- data.table(t = as.POSIXct("2007-01-01 00:00:00", tz = "UTC")); add_time_features(x, "t"); x$dow == 1L && x$hour_of_week == 0L } &&
      all(dt$hour_of_week >= 0L & dt$hour_of_week <= 167L))
chk("timestamps unique, sorted, no NA", !anyDuplicated(dt$timestamp) && !is.unsorted(dt$timestamp) && !anyNA(dt$timestamp))
chk("rows reconcile: raw - removed = final", nrow(dt) == n_raw - sum(bad_ts) - sum(dup_idx))
chk("no negative active power remains", all(dt$Global_active_power >= 0, na.rm = TRUE))
chk("daily valid minutes == minute-level valid count", sum(daily$n_valid_minutes) == sum(dt$valid))
chk("daily energy == sum(kW/60) at minute level", rel_eq(sum(daily$energy_kwh_observed), sum(dt$energy_kwh, na.rm = TRUE)))
chk("monthly energy == daily energy", rel_eq(sum(monthly$energy_kwh_observed), sum(daily$energy_kwh_observed)))
chk("weekly energy == daily energy", rel_eq(sum(weekly$energy_kwh_observed), sum(daily$energy_kwh_observed)))
chk("hourly profile valid minutes == minute-level", sum(hourly_summary[day_type == "All", n_valid_minutes]) == sum(dt$valid))
chk("heatmap valid minutes == minute-level", sum(heatmap$n_valid_minutes) == sum(dt$valid))
chk("hourly series valid minutes == minute-level", sum(hs$n_valid_minutes) == sum(dt$valid))
chk("full-day estimate equals observed when coverage is 100%", {
  f <- daily[coverage_pct == 100]; nrow(f) == 0 || all(abs(f$energy_kwh_est_full_day - f$energy_kwh_observed) < 1e-6) })
chk("no Inf/NaN in exported daily table", !any(sapply(exports$daily_summary[, .(mean_active_power_kw, max_active_power_kw, energy_kwh_observed)], function(x) any(is.infinite(x) | is.nan(x)))))
gl <- thresholds[level == "minute" & method == "global"]
x_all <- dt$Global_active_power[!is.na(dt$Global_active_power)]; qq <- quantile(x_all, c(.25, .75), names = FALSE)
chk("global minute IQR bounds match independent calculation", rel_eq(gl$lower, qq[1] - 1.5 * (qq[2] - qq[1])) && rel_eq(gl$upper, qq[2] + 1.5 * (qq[2] - qq[1])))
chk("global minute flag count matches independent count", sum(dt$hi_global | dt$lo_global, na.rm = TRUE) == sum(x_all < gl$lower | x_all > gl$upper))
chk("anomaly pct within 0..100", all(anom_summary$pct_flagged >= 0 & anom_summary$pct_flagged <= 100, na.rm = TRUE))
chk("sub-metering categories sum to total active energy", rel_eq(sum(sub_summary$energy_kwh), sum(sub_month$active_energy_kwh_total)))
chk("sub-metering shares sum to 100 per month", all(abs(sub_summary[, .(s = sum(pct_of_active_energy)), by = year_month]$s - 100) < 1e-6))
chk("correlation diagonal == 1", all(abs(diag(pear) - 1) < 1e-12))
chk("figures created", length(fig_fail) == 0 && length(list.files(DIR_FIG, pattern = "\\.png$")) >= 12)
req_cols <- list(daily_summary = c("date", "energy_kwh_est_full_day", "coverage_pct"), monthly_summary = c("year_month", "avg_daily_energy_kwh", "is_partial_month"),
                 hourly_summary = c("day_type", "hour", "mean_active_power_kw"), hour_weekday_heatmap = c("dow_label", "hour", "mean_active_power_kw"),
                 hourly_series = c("hour_start", "flag_hour_of_week"), anomaly_records = c("hour_start", "severity_ratio"),
                 anomaly_summary = c("level", "method", "pct_flagged"), correlation_matrix = c("variable_1", "pearson_r"),
                 submetering_summary = c("category", "energy_kwh"), kpi_summary = c("metric", "value"), data_quality_report = c("section", "metric", "value"))
for (nm in names(req_cols)) {
  f <- file.path(DIR_PROC, paste0(nm, ".csv"))
  chk(paste("export exists with expected columns:", nm), file.exists(f) && all(req_cols[[nm]] %in% names(fread(f, nrows = 1))))
}
validation <- rbindlist(checks)
write_csv(validation, file.path(DIR_TAB, "validation_report.csv"))
writeLines(capture.output(sessionInfo()), file.path(DIR_REP, "session_info.txt"))

n_fail <- sum(!validation$passed)
say(sprintf("Validation: %d/%d checks passed.", nrow(validation) - n_fail, nrow(validation)))
if (n_fail) warning("FAILED checks: ", paste(validation[passed == FALSE, test], collapse = " | "))
say(sprintf("DONE in %.1f minutes. Open outputs/reports/dashboard_preview.html and connect Tableau to data/processed/*.csv",
            as.numeric(difftime(Sys.time(), t_start, units = "mins"))))