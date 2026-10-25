# 01_import_and_clean.R -- Stage 2: import, validate, clean (non-destructively) and report data quality.
# Usage (project root):  Rscript R/01_import_and_clean.R [raw_file] [processed_dir] [tables_dir]
#
# Design decisions (documented in docs/methodology.md later):
#  * The raw file is only ever READ. Cleaning happens on an in-memory copy.
#  * Missing values ('?') are KEPT as NA. They are NOT replaced with zero or imputed here.
#    Downstream aggregates use na.rm = TRUE with an explicit completeness (coverage) column.
#  * Timestamps are naive local clock time, stored as UTC to avoid DST artefacts (no zone info in source).
#  * Every step logs rows before/after to cleaning_log.csv.
suppressPackageStartupMessages({ library(data.table); library(lubridate) })

args      <- commandArgs(trailingOnly = TRUE)
in_path   <- if (length(args) >= 1) args[1] else file.path("data", "raw", "household_power_consumption.txt")
proc_dir  <- if (length(args) >= 2) args[2] else file.path("data", "processed")
table_dir <- if (length(args) >= 3) args[3] else file.path("outputs", "tables")
if (!file.exists(in_path)) stop("Raw file not found: ", in_path, "\nRun R/00_download_data.R first.")
dir.create(proc_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

num_cols <- c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity",
              "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
expected_cols <- c("Date", "Time", num_cols)

# ---- logging helpers ---------------------------------------------------------------------------
clean_log <- data.table(step = character(), description = character(),
                        rows_before = integer(), rows_after = integer(), rows_changed = integer())
log_step <- function(step, description, before, after, changed = before - after) {
  clean_log <<- rbindlist(list(clean_log, data.table(step = step, description = description,
                  rows_before = as.integer(before), rows_after = as.integer(after),
                  rows_changed = as.integer(changed))))
  message(sprintf("[%s] %s | before=%s after=%s changed=%s", step, description,
                  format(before, big.mark = ","), format(after, big.mark = ","), format(changed, big.mark = ",")))
}
quality <- list()
add_q <- function(section, metric, value, note = "") {
  quality[[length(quality) + 1]] <<- data.table(section = section, metric = metric,
                                                 value = as.character(value), note = note)
}

# ---- 1. Import (everything as character so nothing is silently coerced) -------------------------
message("Reading ", in_path, " ...")
dt <- fread(in_path, sep = ";", header = TRUE, colClasses = "character", na.strings = c("?", ""))
if (length(setdiff(expected_cols, names(dt)))) stop("Missing columns: ", paste(setdiff(expected_cols, names(dt)), collapse = ", "))
dt <- dt[, ..expected_cols]
n_raw <- nrow(dt)
log_step("01_import", "Rows read from raw file", n_raw, n_raw, 0L)
add_q("volume", "raw_rows", n_raw, "Rows in raw file (excluding header)")

# ---- 2. Numeric conversion with a safety check ---------------------------------------------------
na_before <- sum(unlist(dt[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = num_cols]))
for (col in num_cols) set(dt, j = col, value = suppressWarnings(as.numeric(dt[[col]])))
na_after  <- sum(unlist(dt[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = num_cols]))
if (na_after != na_before) stop(na_after - na_before, " non-numeric value(s) (other than '?') found during conversion.")
log_step("02_numeric", "Converted 7 measurement columns to numeric (no new NAs introduced)", n_raw, n_raw, 0L)

# ---- 3. Timestamp parsing (dd/mm/yyyy HH:MM:SS) ---------------------------------------------------
dt[, timestamp := dmy_hms(paste(Date, Time), tz = "UTC", quiet = TRUE)]
dt[, c("Date", "Time") := NULL]
bad_ts <- is.na(dt$timestamp)
n0 <- nrow(dt); dt <- dt[!bad_ts]
log_step("03_timestamp", "Removed rows with unparseable timestamp", n0, nrow(dt))
add_q("timestamps", "unparseable_timestamps", sum(bad_ts), "Rows removed; none expected in the official file")

# ---- 4. Ordering ------------------------------------------------------------------------------------
was_unsorted <- is.unsorted(dt$timestamp)
if (was_unsorted) setorder(dt, timestamp)
log_step("04_order", paste("Sorted by timestamp (was unsorted:", was_unsorted, ")"), nrow(dt), nrow(dt), 0L)
add_q("timestamps", "file_was_unsorted", was_unsorted)

# ---- 5. Duplicate timestamps ----------------------------------------------------------------------------
dup_idx   <- duplicated(dt$timestamp)
dup_any   <- dup_idx | duplicated(dt$timestamp, fromLast = TRUE)
n_conflict <- if (any(dup_any)) dt[dup_any, .(k = uniqueN(.SD)), by = timestamp, .SDcols = num_cols][k > 1, .N] else 0L
n0 <- nrow(dt); dt <- dt[!dup_idx]
log_step("05_duplicates", "Removed repeated timestamps (first occurrence kept)", n0, nrow(dt))
add_q("duplicates", "duplicate_rows_removed", sum(dup_idx))
add_q("duplicates", "duplicate_timestamps_with_conflicting_values", n_conflict, "If >0, the first occurrence was kept; review")

# ---- 6. Sampling interval and gaps ------------------------------------------------------------------------
secs <- diff(as.numeric(dt$timestamp))
interval_tab <- as.data.table(table(interval_seconds = secs))
interval_tab[, interval_seconds := as.numeric(as.character(interval_seconds))]
setnames(interval_tab, "N", "n_intervals")
gap_n       <- sum(secs > 60)
gap_minutes <- sum(secs[secs > 60] / 60 - 1)
expected_rows <- as.numeric(difftime(max(dt$timestamp), min(dt$timestamp), units = "mins")) + 1
add_q("time_coverage", "first_timestamp", format(min(dt$timestamp), "%Y-%m-%d %H:%M:%S"))
add_q("time_coverage", "last_timestamp",  format(max(dt$timestamp), "%Y-%m-%d %H:%M:%S"))
add_q("time_coverage", "expected_rows_if_complete_minute_grid", expected_rows)
add_q("time_coverage", "rows_present", nrow(dt))
add_q("time_coverage", "gaps_longer_than_1_minute", gap_n, "Count of places where consecutive rows are >60 s apart")
add_q("time_coverage", "minutes_missing_from_gaps", gap_minutes, "Minutes with no row at all (distinct from '?' rows)")
add_q("time_coverage", "intervals_not_multiple_of_60s", sum(secs %% 60 != 0))
log_step("06_intervals", "Checked sampling interval (rows not changed; gaps reported, not filled)", nrow(dt), nrow(dt), 0L)

# ---- 7. Impossible values -> NA (not deleted rows) ---------------------------------------------------------------
impossible <- list(
  Global_active_power   = function(x) x < 0,
  Global_reactive_power = function(x) x < 0,
  Voltage               = function(x) x <= 0,
  Global_intensity      = function(x) x < 0,
  Sub_metering_1        = function(x) x < 0,
  Sub_metering_2        = function(x) x < 0,
  Sub_metering_3        = function(x) x < 0)
for (col in names(impossible)) {
  bad <- which(impossible[[col]](dt[[col]]))
  add_q("impossible_values", col, length(bad), "Values violating physical rule (negative; voltage <= 0) set to NA")
  if (length(bad)) set(dt, i = bad, j = col, value = NA_real_)
}
log_step("07_impossible", "Impossible values set to NA (rows retained)", nrow(dt), nrow(dt), 0L)

# ---- 8. Missingness analysis -------------------------------------------------------------------------------------------
miss_var <- data.table(variable = num_cols,
                       n_missing = sapply(num_cols, function(v) sum(is.na(dt[[v]]))))
miss_var[, `:=`(n_rows = nrow(dt), pct_missing = round(100 * n_missing / nrow(dt), 4))]
na_count_row <- rowSums(is.na(dt[, ..num_cols]))
add_q("missingness", "rows_all_7_measurements_missing", sum(na_count_row == 7))
add_q("missingness", "rows_partially_missing", sum(na_count_row > 0 & na_count_row < 7))
add_q("missingness", "rows_fully_observed", sum(na_count_row == 0))

dt[, ym := format(timestamp, "%Y-%m")]
miss_month <- dt[, .(rows = .N, missing_global_active_power = sum(is.na(Global_active_power))), by = ym]
miss_month[, pct_missing := round(100 * missing_global_active_power / rows, 3)]
setnames(miss_month, "ym", "year_month")
dt[, ym := NULL]

r <- rle(is.na(dt$Global_active_power))
ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
runs <- data.table(start = dt$timestamp[starts], end = dt$timestamp[ends], rows_in_run = r$lengths)[r$values]
setorder(runs, -rows_in_run)
add_q("missingness", "number_of_missing_runs", nrow(runs), "Consecutive stretches where Global_active_power is NA")
add_q("missingness", "longest_missing_run_rows", if (nrow(runs)) runs$rows_in_run[1] else 0L)
add_q("missingness", "handling_decision",
      "No imputation in base data",
      "NA kept; aggregates use na.rm=TRUE plus a coverage column; long runs are reported, never zero-filled")

# ---- 9. Save outputs -------------------------------------------------------------------------------------------------------------
setcolorder(dt, c("timestamp", num_cols))
if (nrow(dt) == 0) stop("No rows left after cleaning.")
stopifnot(!anyNA(dt$timestamp), !anyDuplicated(dt$timestamp), !is.unsorted(dt$timestamp))
expected_final <- n_raw - sum(bad_ts) - sum(dup_idx)
if (nrow(dt) != expected_final) stop("Row reconciliation failed: ", nrow(dt), " vs expected ", expected_final)
add_q("volume", "rows_after_cleaning", nrow(dt), "= raw rows - unparseable timestamps - duplicates")
add_q("volume", "pct_rows_removed", round(100 * (n_raw - nrow(dt)) / n_raw, 5))
log_step("09_final", "Final cleaned base dataset (NAs retained)", n_raw, nrow(dt))

saveRDS(dt, file.path(proc_dir, "cleaned_minute_base.rds"))
fwrite(rbindlist(quality),  file.path(table_dir, "data_quality_report.csv"))
fwrite(clean_log,           file.path(table_dir, "cleaning_log.csv"))
fwrite(miss_var,            file.path(table_dir, "missingness_by_variable.csv"))
fwrite(miss_month,          file.path(table_dir, "missingness_by_month.csv"))
fwrite(head(runs, 25),      file.path(table_dir, "missing_runs_top25.csv"))
fwrite(interval_tab,        file.path(table_dir, "interval_distribution.csv"))

for (f in c("data_quality_report.csv", "cleaning_log.csv", "missingness_by_variable.csv",
            "missingness_by_month.csv", "missing_runs_top25.csv", "interval_distribution.csv"))
  if (!file.exists(file.path(table_dir, f))) stop("Expected output missing: ", f)
if (!file.exists(file.path(proc_dir, "cleaned_minute_base.rds"))) stop("Expected cleaned RDS missing")
message("\nStage 2 complete. Cleaned rows: ", format(nrow(dt), big.mark = ","))
