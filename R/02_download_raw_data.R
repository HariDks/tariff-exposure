# ============================================================================
# Phase 2 (part 1) — Download raw data sources
#
# Three sources downloaded here:
#   1. USDA ERS 2020 Commuting Zones (CSV, ~200 KB)         -> Phase 4
#   2. BLS QCEW 2024 quarterly singlefile (zip, ~304 MB)    -> Phase 2 (employment)
#   3. BLS LAUS county annual averages 2024 (xlsx, ~230 KB) -> outcome variable
#
# Two sources NOT scripted:
#   - PIIE tariff tracker (Cloudflare-blocked, user downloads in browser)
#   - ACS controls (pulled live via tidycensus in script 02d_pull_acs.R)
#
# Course concept: separation of "download" from "transform".
# Raw downloads land in data/raw/, then a separate script reads + processes them.
# This means we can re-run analysis without re-downloading 300 MB every time.
# ============================================================================

library(here)

raw_dir <- here("data", "raw")

# BLS rejects default R/curl User-Agents (Akamai filtering). Set a custom UA.
ua <- "Mozilla/5.0 (R academic; GIS Harris 2026 tariff exposure project)"

# Helper: download only if the file doesn't already exist.
# This is a beginner-friendly cache — re-running the script is cheap.
download_if_missing <- function(url, dest, headers = NULL) {
  if (file.exists(dest)) {
    cat("[skip]", basename(dest), "already exists (", round(file.info(dest)$size / 1e6, 1), "MB )\n")
    return(invisible(NULL))
  }
  cat("[get ]", basename(dest), "from", url, "\n")
  utils::download.file(
    url      = url,
    destfile = dest,
    headers  = headers,
    mode     = "wb",        # binary mode — required for zips and xlsx
    quiet    = TRUE
  )
  cat("       saved (", round(file.info(dest)$size / 1e6, 1), "MB )\n")
}

# ---- 1. USDA ERS 2020 Commuting Zones --------------------------------------
download_if_missing(
  url  = "https://www.ers.usda.gov/media/6968/2020-commuting-zones.csv",
  dest = file.path(raw_dir, "usda_commuting_zones_2020.csv")
)

# ---- 2. BLS QCEW 2024 quarterly singlefile (all states, all NAICS) ---------
# 304 MB zip. Contains 4 CSVs (one per quarter). We'll use 2024 Q4 = pre-tariff
# baseline (tariffs were announced April 2025, implemented Aug 2025).
download_if_missing(
  url     = "https://data.bls.gov/cew/data/files/2024/csv/2024_qtrly_singlefile.zip",
  dest    = file.path(raw_dir, "bls_qcew_2024_qtrly_singlefile.zip"),
  headers = c("User-Agent" = ua)
)

# ---- 3. BLS LAUS county annual averages 2024 -------------------------------
# Small file with annual averages for every county. We use this as the v1
# outcome variable (avg county unemployment rate 2024).
# For the DiD scaffold (monthly panel) we'll grab the larger time-series file
# in a separate script after v1 works end-to-end.
download_if_missing(
  url     = "https://www.bls.gov/lau/laucnty24.xlsx",
  dest    = file.path(raw_dir, "bls_laus_county_annual_2024.xlsx"),
  headers = c("User-Agent" = ua)
)

cat("\nDone. Contents of data/raw/:\n")
print(file.info(list.files(raw_dir, full.names = TRUE))[, "size", drop = FALSE])
