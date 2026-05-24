# ============================================================================
# Phase 2 — Process QCEW into county × NAICS-2 employment shares
#
# Pre-tariff baseline: 2024 Q4 (tariffs were announced April 2025).
#
# Input:  data/raw/bls_qcew_2024_qtrly_singlefile.zip
# Output: data/processed/county_naics2_employment.parquet
#         (long-format: one row per county-sector)
#
# Course concepts:
#   - Long-format / tidy data (Lecture 3)
#   - Share-weighted aggregation primitives (Lecture 4)
#   - Why we unzip-then-read rather than read-from-zip-every-time (Lecture 8)
# ============================================================================

library(here)
library(dplyr)
library(readr)
library(stringr)

raw_zip <- here("data", "raw", "bls_qcew_2024_qtrly_singlefile.zip")
unzip_dir <- here("data", "raw", "qcew_2024_unzipped")

# ---- 1. Unzip once -----------------------------------------------------
# The zip contains 4 CSVs (one per quarter). We unzip into a sibling folder
# inside data/raw/ so we don't re-extract on every script re-run.
if (!dir.exists(unzip_dir)) {
  dir.create(unzip_dir)
  unzip(raw_zip, exdir = unzip_dir)
}
qcew_files <- list.files(unzip_dir, pattern = "\\.csv$", full.names = TRUE)
cat("QCEW files extracted:\n")
print(basename(qcew_files))

# BLS packs all 4 quarters of 2024 into a single CSV (2024.q1-q4.singlefile.csv).
# We read the whole file and filter on `qtr` at the dplyr level.
qcew_file <- qcew_files[1]
cat("\nReading:", basename(qcew_file), "\n")

# ---- 2. Read only the columns we need ----------------------------------
# QCEW singlefile has ~40 columns; we want 6. Specifying col_select keeps
# memory use low (saves ~70% on a 1 GB CSV).
needed <- c(
  "area_fips",     # 5-digit county FIPS
  "industry_code", # NAICS (digit length varies by agglvl)
  "own_code",      # ownership type (0 = total covered)
  "agglvl_code",   # aggregation level (70 = county total, 71 = county-sector)
  "qtr",
  "year",
  "month3_emplvl"  # employment in last month of quarter (Dec for Q4)
)

qcew_raw <- read_csv(
  qcew_file,
  col_select = all_of(needed),
  show_col_types = FALSE
) |>
  # Keep only Q4 2024 rows — pre-tariff baseline.
  filter(year == 2024, qtr == 4)

cat("\nRaw QCEW rows:", format(nrow(qcew_raw), big.mark = ","), "\n")
cat("Aggregation levels present:\n")
print(qcew_raw |> count(agglvl_code) |> arrange(desc(n)))

# ---- 3. County total employment (denominator) --------------------------
# agglvl_code = 70, own_code = 0 -> one row per county = total covered employment
# (sum across private + federal + state + local).
county_total <- qcew_raw |>
  filter(agglvl_code == 70, own_code == 0) |>
  transmute(
    geoid     = area_fips,
    emp_total = month3_emplvl
  )

cat("\nCounty totals: ", nrow(county_total), "rows\n")

# ---- 4. County × NAICS-2 sector employment (numerator) -----------------
# agglvl_code = 74 = "County, by NAICS Sector — by Ownership". One row per
# (county, NAICS 2-digit sector, ownership type). We SUM across the four
# ownership codes (1 Federal, 2 State, 3 Local, 5 Private) to get total
# sector employment per county.
#
# NAICS 2-digit sector codes include some combined ones:
#   11  Agriculture/Forestry/Fishing/Hunting
#   21  Mining/Quarrying/Oil&Gas
#   22  Utilities
#   23  Construction
#   31-33  Manufacturing (combined)
#   42  Wholesale Trade
#   44-45  Retail Trade (combined)
#   48-49  Transportation & Warehousing (combined)
#   51  Information
#   52  Finance & Insurance
#   53  Real Estate
#   54  Professional/Scientific/Technical Services
#   55  Management of Companies
#   56  Admin & Support / Waste Management
#   61  Educational Services
#   62  Health Care & Social Assistance
#   71  Arts/Entertainment/Recreation
#   72  Accommodation & Food
#   81  Other Services (except Public Admin)
#   92  Public Administration
county_sector <- qcew_raw |>
  filter(agglvl_code == 74) |>
  group_by(area_fips, industry_code) |>
  summarise(emp = sum(month3_emplvl, na.rm = TRUE), .groups = "drop") |>
  transmute(
    geoid        = area_fips,
    naics_sector = industry_code,
    emp
  ) |>
  filter(emp > 0)   # drop zero / suppressed cells

cat("County-sector rows:", nrow(county_sector), "\n")
cat("Distinct NAICS-2 sectors present:\n")
print(sort(unique(county_sector$naics_sector)))

# ---- 5. Compute employment shares --------------------------------------
# share_{ij} = emp_{ij} / emp_total_j gives the "raw" share of county j
# employment in sector i. BUT — BLS suppresses sector counts in small
# counties to protect confidentiality (a sector with very few firms can't
# be reported without identifying them). So the raw shares typically sum to
# LESS than 1 — the missing piece is suppressed cells.
#
# We also drop NAICS "99" (Unclassified) since those don't map to any
# tariff-relevant sector.
#
# Then we RE-NORMALIZE within county so the shift-share weights sum to 1.
# This is standard practice — it treats the suppression-induced gap as
# missing-at-random within the observed industry mix.

county_emp_shares <- county_sector |>
  filter(naics_sector != "99") |>                    # drop Unclassified
  inner_join(county_total, by = "geoid") |>
  mutate(share_raw = emp / emp_total) |>
  group_by(geoid) |>
  mutate(share = share_raw / sum(share_raw)) |>      # re-normalize to sum=1
  ungroup()

# Suppression-induced gap report — how much employment is unobserved at
# sector level? This matters for the methods write-up.
suppression_check <- county_emp_shares |>
  group_by(geoid) |>
  summarise(coverage = sum(share_raw), .groups = "drop") |>
  summarise(
    counties_n              = n(),
    median_coverage         = median(coverage),
    pct_with_coverage_gte80 = mean(coverage >= 0.80) * 100,
    pct_with_coverage_gte95 = mean(coverage >= 0.95) * 100
  )
cat("\nSuppression coverage check (raw shares before normalisation):\n")
print(suppression_check)

# After normalisation, shares sum exactly to 1 by construction:
norm_check <- county_emp_shares |>
  group_by(geoid) |>
  summarise(s = sum(share), .groups = "drop") |>
  summarise(min_sum = min(s), max_sum = max(s))
cat("\nNormalised share-sum check (should all equal 1):\n")
print(norm_check)

# ---- 6. Save -----------------------------------------------------------
out_path <- here("data", "processed", "county_naics2_employment.parquet")
arrow::write_parquet(county_emp_shares, out_path)
cat("\nSaved:", out_path, "(", round(file.info(out_path)$size / 1024, 1), "KB )\n")

cat("\n--- Sample of final table ---\n")
print(head(county_emp_shares))
