# ============================================================================
# Phase 2 (pivot) — Commuting Zones as the primary unit of analysis
#
# Why CZ instead of county:
#   1. BLS suppression: 73% of counties have <95% sector-employment coverage
#      at NAICS-2 in 2024 Q4. Aggregating to ~600 CZs washes most of it out.
#   2. Labor-market integrity: workers commuting across county lines have
#      their exposure mismeasured at county level. CZs are designed to
#      contain whole local labor markets.
#   3. Professor's advice: "Run the analysis at both the county level and
#      the commuting zone level and test whether results hold up across
#      both."
#
# Inputs:
#   data/processed/counties_l48.gpkg
#   data/raw/usda_commuting_zones_2020.csv
#   data/processed/county_naics2_employment.parquet
#
# Outputs:
#   data/processed/cz_l48.gpkg                  (CZ polygons)
#   data/processed/cz_naics2_employment.parquet (CZ-level shares)
#
# Course concepts:
#   - Spatial DISSOLVE via sf::summarise (Lecture 2/3)
#   - tabular vs spatial join (Lecture 2/3)
#   - Aggregation reduces suppression / MAUP (Lecture 7)
# ============================================================================

library(here)
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(arrow)

# ---- 1. Read the USDA county -> CZ crosswalk ---------------------------
cz_xwalk <- read_csv(
  here("data", "raw", "usda_commuting_zones_2020.csv"),
  show_col_types = FALSE
)
cat("CZ crosswalk columns:", paste(names(cz_xwalk), collapse = ", "), "\n")
cat("Rows in crosswalk:", nrow(cz_xwalk), "\n")
cat("Distinct CZs:", n_distinct(cz_xwalk$CZ20), "\n")

# Normalise column names. The USDA file uses different casings/spellings
# across vintages, so we look for the columns by pattern.
fips_col <- names(cz_xwalk)[str_detect(tolower(names(cz_xwalk)), "fips")][1]
cz_col   <- names(cz_xwalk)[str_detect(tolower(names(cz_xwalk)), "^cz")][1]
cz_xwalk <- cz_xwalk |>
  transmute(
    geoid  = str_pad(as.character(.data[[fips_col]]), width = 5, pad = "0"),
    cz20   = as.character(.data[[cz_col]])
  )
cat("\nCleaned crosswalk head:\n")
print(head(cz_xwalk))

# ---- 2. Read county polygons & join CZ ID ------------------------------
counties <- st_read(here("data", "processed", "counties_l48.gpkg"), quiet = TRUE)

# tabular join: counties (sf) <- crosswalk (df) by geoid
counties_with_cz <- counties |>
  left_join(cz_xwalk, by = "geoid")

unmatched <- counties_with_cz |> filter(is.na(cz20))
cat("\nUnmatched counties (no CZ in 2020 vintage):", nrow(unmatched), "\n")
if (nrow(unmatched) > 0) {
  cat("First few unmatched (likely CT planning regions which post-date 2020 CZ):\n")
  print(unmatched |> st_drop_geometry() |> select(geoid, state_name, county_name) |> head(15))
}

# Drop unmatched (will mostly be Connecticut's new planning regions).
# We document this in the methods section as a known limitation.
counties_with_cz <- counties_with_cz |> filter(!is.na(cz20))

# ---- 3. DISSOLVE counties -> CZ polygons -------------------------------
# sf's summarise() unions the geometries of all rows in each group. This
# turns ~3,100 county polygons into ~600 CZ polygons in one statement.
cat("\nDissolving counties -> CZs (this takes ~30 seconds)...\n")
cz_sf <- counties_with_cz |>
  group_by(cz20) |>
  summarise(
    n_counties = n(),
    aland_m2   = sum(aland_m2, na.rm = TRUE),
    .groups    = "drop"
  )

cat("CZ polygons:", nrow(cz_sf), "\n")

# Save CZ polygons
out_gpkg <- here("data", "processed", "cz_l48.gpkg")
st_write(cz_sf, out_gpkg, delete_dsn = TRUE, quiet = TRUE)
cat("Saved CZ polygons to:", out_gpkg, "\n")

# ---- 4. Aggregate county employment up to CZ ---------------------------
county_emp <- read_parquet(here("data", "processed", "county_naics2_employment.parquet"))

# Join CZ ID, then sum employment within (cz, sector) and within cz total.
county_emp_with_cz <- county_emp |>
  inner_join(cz_xwalk, by = "geoid")

# CZ total covered employment = sum of county totals (within unique counties)
cz_total <- county_emp_with_cz |>
  distinct(geoid, emp_total, cz20) |>
  group_by(cz20) |>
  summarise(emp_total = sum(emp_total, na.rm = TRUE), .groups = "drop")

# CZ × sector employment
cz_sector <- county_emp_with_cz |>
  group_by(cz20, naics_sector) |>
  summarise(emp = sum(emp, na.rm = TRUE), .groups = "drop") |>
  filter(emp > 0)

# Compute raw + normalised shares
cz_emp_shares <- cz_sector |>
  inner_join(cz_total, by = "cz20") |>
  mutate(share_raw = emp / emp_total) |>
  group_by(cz20) |>
  mutate(share = share_raw / sum(share_raw)) |>
  ungroup()

# ---- 5. Suppression-coverage check at CZ level -------------------------
# This is THE before/after that justifies the CZ pivot in the report.
cz_coverage <- cz_emp_shares |>
  group_by(cz20) |>
  summarise(coverage = sum(share_raw), .groups = "drop") |>
  summarise(
    cz_n                = n(),
    median_coverage     = median(coverage),
    pct_cov_gte80       = mean(coverage >= 0.80) * 100,
    pct_cov_gte95       = mean(coverage >= 0.95) * 100,
    pct_cov_gte99       = mean(coverage >= 0.99) * 100
  )
cat("\n=== CZ-level suppression coverage ===\n")
print(cz_coverage)
cat("Compare to county-level: median 85.5%, only 26.1% had >=95% coverage.\n")

# ---- 6. Save -----------------------------------------------------------
out_pq <- here("data", "processed", "cz_naics2_employment.parquet")
write_parquet(cz_emp_shares, out_pq)
cat("\nSaved CZ shares to:", out_pq, "\n")

cat("\n--- Sample of CZ shares ---\n")
print(head(cz_emp_shares))
