# requirements.R -- run once:  source("requirements.R")
required <- c("data.table", "dplyr", "lubridate", "ggplot2", "tidyr",
              "readr", "scales", "corrplot")
optional <- c("janitor")   # optional; pipeline uses its own name-cleaning fallback

install_if_missing <- function(pkgs, must = TRUE) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      message("Installing ", p, " ...")
      tryCatch(install.packages(p, repos = "https://cloud.r-project.org"),
               error = function(e) NULL, warning = function(w) NULL)
      if (!requireNamespace(p, quietly = TRUE) && must)
        stop("Required package '", p, "' could not be installed.")
    }
  }
}
install_if_missing(required, must = TRUE)
install_if_missing(optional, must = FALSE)
message("Setup check complete. R version: ", R.version.string)
