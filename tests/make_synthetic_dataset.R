# Generates a SYNTHETIC file in the UCI format for pipeline testing only (NOT real data, never use for findings).
# Usage: Rscript tests/make_synthetic_dataset.R <out_file> [days]
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE); out <- args[1]; days <- if (length(args) >= 2) as.integer(args[2]) else 450L
set.seed(7)
ts <- seq(as.POSIXct("2007-01-01 00:00:00", tz = "UTC"), by = "1 min", length.out = days * 1440L)
h <- as.integer(format(ts, "%H")); wd <- as.integer(format(ts, "%u")); mo <- as.integer(format(ts, "%m"))
base <- 0.4 + 1.2 * exp(-((h - 7.5)^2) / 4) + 1.8 * exp(-((h - 20)^2) / 5)
gap <- base * ifelse(wd >= 6, 1.15, 1) * ifelse(mo %in% c(12, 1, 2), 1.4, ifelse(mo %in% 6:8, 0.8, 1)) * rlnorm(length(ts), 0, 0.35)
# planted multi-hour events (7 days, 10:00-17:59 at 2.5x) so the detector has something to find
ev_days <- sample(unique(as.Date(ts)), 7); ev <- as.Date(ts) %in% ev_days & h >= 10 & h <= 17; gap[ev] <- gap[ev] * 2.5
spk <- sample(length(ts), 400); gap[spk] <- gap[spk] * 3
volt <- 242 - 1.5 * gap + rnorm(length(ts), 0, 1.2)
inten <- gap * 1000 / volt * 1.05
reac <- 0.08 + 0.05 * gap + abs(rnorm(length(ts), 0, 0.03))
s1 <- ifelse(runif(length(ts)) < 0.02, round(runif(length(ts), 5, 40)), 0)
s2 <- ifelse(runif(length(ts)) < 0.03, round(runif(length(ts), 3, 30)), 1)
s3 <- ifelse(h %in% c(6, 7, 22, 23), round(runif(length(ts), 8, 18)), round(runif(length(ts), 0, 2)))
d <- data.table(Date = format(ts, "%d/%m/%Y"), Time = format(ts, "%H:%M:%S"), Global_active_power = sprintf("%.3f", gap),
  Global_reactive_power = sprintf("%.3f", reac), Voltage = sprintf("%.3f", volt), Global_intensity = sprintf("%.3f", inten),
  Sub_metering_1 = sprintf("%.3f", s1), Sub_metering_2 = sprintf("%.3f", s2), Sub_metering_3 = sprintf("%.3f", s3))
for (blk in list(5000:5600, 300000:303000, 500000:500090)) d[blk, 3:9 := "?"]   # missing blocks
fwrite(d, out, sep = ";", quote = FALSE)
cat("Wrote synthetic file:", out, "rows:", nrow(d), "\n")
