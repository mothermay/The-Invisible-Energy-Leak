# 00_check_dataset_structure.R -- Stage 1: confirm the raw file matches the documented structure.
# Usage: Rscript R/00_check_dataset_structure.R [path_to_file]
suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
path <- if (length(args) >= 1) args[1] else file.path("data", "raw", "household_power_consumption.txt")
if (!file.exists(path)) stop("Raw file not found: ", path,
  "\nRun R/00_download_data.R or follow the manual download steps in README.md")

expected_cols <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage",
                   "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")

# Read all as character so nothing is silently coerced; '?' marks missing values.
dt <- fread(path, sep = ";", header = TRUE, colClasses = "character", na.strings = c("?", ""))

cat("File:        ", path, "\n")
cat("Rows x cols: ", nrow(dt), "x", ncol(dt), "\n")
cat("Columns:     ", paste(names(dt), collapse = ", "), "\n")

missing_cols <- setdiff(expected_cols, names(dt))
extra_cols   <- setdiff(names(dt), expected_cols)
if (length(missing_cols)) stop("Missing expected columns: ", paste(missing_cols, collapse = ", "))
if (length(extra_cols))   warning("Unexpected extra columns: ", paste(extra_cols, collapse = ", "))

cat("\nFirst rows:\n"); print(head(dt, 5))
cat("\nNA counts per column ('?' or blank):\n")
print(dt[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = expected_cols])

# UCI date format is dd/mm/yyyy; verify rather than assume.
d <- as.Date(dt$Date, format = "%d/%m/%Y")
cat("\nUnparseable dates:", sum(is.na(d) & !is.na(dt$Date)), "\n")
cat("Date range:", format(min(d, na.rm = TRUE)), "to", format(max(d, na.rm = TRUE)), "\n")

for (col in expected_cols[-(1:2)]) {
  bad <- sum(is.na(suppressWarnings(as.numeric(dt[[col]]))) & !is.na(dt[[col]]))
  if (bad > 0) warning(col, ": ", bad, " non-numeric values")
}
cat("\nStage 1 structure check PASSED for:", path, "\n")
