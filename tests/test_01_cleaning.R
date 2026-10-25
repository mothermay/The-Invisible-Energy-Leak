# Test for 01_import_and_clean.R using a SYNTHETIC fixture with known, planted problems (not real data).
# Run from project root:  Rscript tests/test_01_cleaning.R
suppressPackageStartupMessages(library(data.table))
tmp <- tempfile("clean_test_"); dir.create(tmp)
set.seed(1)
ts <- seq(as.POSIXct("2007-01-01 00:00:00", tz = "UTC"), by = "1 min", length.out = 4320)
mk <- function(ts) data.table(Date = format(ts, "%d/%m/%Y"), Time = format(ts, "%H:%M:%S"),
  Global_active_power = sprintf("%.3f", runif(length(ts), 0.2, 4)), Global_reactive_power = sprintf("%.3f", runif(length(ts), 0, .5)),
  Voltage = sprintf("%.3f", rnorm(length(ts), 240, 2)), Global_intensity = sprintf("%.3f", runif(length(ts), 1, 18)),
  Sub_metering_1 = "0.000", Sub_metering_2 = "1.000", Sub_metering_3 = "5.000")
d <- mk(ts)
d[1000:1089, (3:9) := "?"]                      # 90 fully-missing rows
d[2000, Global_active_power := "-1.000"]        # impossible value
dup <- copy(d[500])                              # duplicate timestamp appended at end -> unsorted file
bad <- copy(d[1]); bad[, Time := "25:99:00"]     # invalid timestamp
d <- d[-(3000:3009)]                             # 10-minute gap (rows absent)
d <- rbind(d, dup, bad)
raw <- file.path(tmp, "raw.txt")
fwrite(d, raw, sep = ";", quote = FALSE)
script <- file.path("R", "01_import_and_clean.R")
if (!file.exists(script)) stop("Cannot find ", script, " -- save the script into the project's R/ folder and run from the project root (getwd() = ", getwd(), ")")
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
out <- suppressWarnings(system2(rscript, shQuote(c(script, raw, file.path(tmp, "proc"), file.path(tmp, "tab"))),
                                stdout = TRUE, stderr = TRUE))
status <- attr(out, "status"); if (is.null(status)) status <- 0L
if (status != 0) { cat(out, sep = "\n"); stop("Cleaning script exited with status ", status) }
q <- fread(file.path(tmp, "tab", "data_quality_report.csv"))
get <- function(m) as.numeric(q[metric == m, value][1])
checks <- c(
  raw_rows              = get("raw_rows") == 4312,
  unparseable_ts        = get("unparseable_timestamps") == 1,
  duplicates            = get("duplicate_rows_removed") == 1,
  final_rows            = get("rows_after_cleaning") == 4310,
  gap_minutes           = get("minutes_missing_from_gaps") == 10,
  impossible_gap        = get("Global_active_power") == 1,
  all_missing_rows      = get("rows_all_7_measurements_missing") == 90,
  longest_run           = get("longest_missing_run_rows") == 90,
  unsorted_detected     = q[metric == "file_was_unsorted", value] == "TRUE",
  no_zero_fill          = !any(readRDS(file.path(tmp, "proc", "cleaned_minute_base.rds"))[1000:1089, Global_active_power] == 0, na.rm = TRUE))
print(checks)
if (!all(checks)) stop("TEST FAILED") else cat("\nALL CLEANING TESTS PASSED\n")
