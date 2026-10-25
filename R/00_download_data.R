# 00_download_data.R -- obtain the official UCI dataset into data/raw/ (never modified afterwards)
# Source: https://archive.ics.uci.edu/dataset/235/individual+household+electric+power+consumption
raw_dir  <- file.path("data", "raw")
txt_path <- file.path(raw_dir, "household_power_consumption.txt")
zip_path <- file.path(raw_dir, "household_power_consumption.zip")
zip_url  <- "https://archive.ics.uci.edu/static/public/235/individual+household+electric+power+consumption.zip"

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

if (file.exists(txt_path)) {
  message("Raw file already present: ", txt_path, " (skipping; never overwritten).")
} else {
  options(timeout = 600)
  ok <- tryCatch({ download.file(zip_url, zip_path, mode = "wb", quiet = FALSE); TRUE },
                 error = function(e) { message("Download failed: ", conditionMessage(e)); FALSE },
                 warning = function(w) { message("Download warning: ", conditionMessage(w)); FALSE })
  if (ok) {
    unzip(zip_path, exdir = raw_dir)
    message("Downloaded and extracted to ", raw_dir)
  } else {
    message("\nMANUAL DOWNLOAD REQUIRED:\n",
            " 1. Open https://archive.ics.uci.edu/dataset/235/individual+household+electric+power+consumption\n",
            " 2. Click 'Download', unzip it.\n",
            " 3. Place household_power_consumption.txt in: ", normalizePath(raw_dir, mustWork = FALSE), "\n",
            " 4. Run: Rscript R/00_check_dataset_structure.R")
  }
}
