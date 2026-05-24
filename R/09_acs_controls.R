# ============================================================================
# Phase 5.5 — Pull ACS county data, aggregate to CZ, assemble regression dataset
#
# Outputs:
#   data/processed/cz_analysis_dataset.parquet
#     One row per CZ with: exposure_pp + sector contributions
#     + LISA cluster label + ACS controls + outcome (unemployment rate)
#
# ACS variables (2019-2023 5-year, the most recent stable release):
#   B23025_002 / B23025_005       -> labor force / unemployed
#   B01003_001                    -> total population
#   B15003_001 / B15003_022..025  -> 25+ population / 25+ with BA+
#   B19013_001                    -> median household income
#   B02001_002                    -> white alone
#   B01001 sex-by-age             -> we'll compute 25-64 share from grouped ages
#
# Course concepts:
#   - tidycensus / Census API for ACS data            — Lec 2
#   - county -> CZ aggregation done correctly         — Lec 3
#   - Building a regression-ready analysis dataset    — Lec 3
# ============================================================================

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(arrow)
library(tidycensus)

# ---- 1. Pull ACS county data -------------------------------------------
# We request both the variables we want and the moe (margin of error)
# columns; we ignore MOE in the regression (standard practice) but having
# it available means we could weight by precision if needed.

vars_needed <- c(
  # Labor force / unemployment
  labor_force      = "B23025_002",
  unemployed       = "B23025_005",
  # Population
  total_pop        = "B01003_001",
  # Education (25+ with bachelor's, master's, professional, doctorate)
  pop_25_plus      = "B15003_001",
  ba_only          = "B15003_022",
  ma_only          = "B15003_023",
  prof_degree      = "B15003_024",
  phd              = "B15003_025",
  # Income
  median_hh_income = "B19013_001",
  # Race
  white_alone      = "B02001_002"
)

cat("Calling Census API for ACS 5-year county estimates...\n")
acs_raw <- get_acs(
  geography = "county",
  variables = vars_needed,
  year      = 2023,
  survey    = "acs5",
  output    = "wide",
  cache_table = TRUE
)
cat("County rows returned:", nrow(acs_raw), "\n")

# Drop Estimate/MOE column duplication: keep only estimates (the "E" cols)
acs <- acs_raw |>
  select(GEOID, NAME, ends_with("E")) |>
  rename(geoid = GEOID, name = NAME) |>
  rename_with(~ str_remove(., "E$"), .cols = ends_with("E")) |>
  # B15003 has BA+, MA+, prof, PhD as separate counts — sum to total BA+.
  mutate(college_25plus = ba_only + ma_only + prof_degree + phd) |>
  select(-ba_only, -ma_only, -prof_degree, -phd)

# ---- 2. Sanity: do FIPS line up with our QCEW / CZ crosswalk? ---------
cz_xwalk <- read_csv(
  here("data", "raw", "usda_commuting_zones_2020.csv"),
  show_col_types = FALSE
)
fips_col <- names(cz_xwalk)[str_detect(tolower(names(cz_xwalk)), "fips")][1]
cz_col   <- names(cz_xwalk)[str_detect(tolower(names(cz_xwalk)), "^cz") &
                            !str_detect(tolower(names(cz_xwalk)), "name|contain")][1]
cz_xwalk <- cz_xwalk |>
  transmute(
    geoid = str_pad(as.character(.data[[fips_col]]), width = 5, pad = "0"),
    cz20  = as.character(.data[[cz_col]])
  )

acs_in_cz <- acs |>
  inner_join(cz_xwalk, by = "geoid")
cat("ACS counties matched to a 2020 CZ:", nrow(acs_in_cz),
    "of", nrow(acs), "\n")

# ---- 3. Aggregate county-level ACS up to CZ -----------------------------
# Rates are recomputed from raw counts (correct).
# Median income is population-weighted (compromise — can't recover true
# CZ median from county medians; this is the standard approximation).

cz_acs <- acs_in_cz |>
  group_by(cz20) |>
  summarise(
    # IMPORTANT: compute median_hh_income FIRST, while total_pop still
    # refers to the per-county input vector. Once we summarise total_pop
    # into a scalar below, the input vector is gone.
    median_hh_income = {
      ok <- !is.na(median_hh_income) & !is.na(total_pop) & total_pop > 0
      if (any(ok)) sum(median_hh_income[ok] * total_pop[ok]) / sum(total_pop[ok])
      else NA_real_
    },
    total_pop        = sum(total_pop,        na.rm = TRUE),
    labor_force      = sum(labor_force,      na.rm = TRUE),
    unemployed       = sum(unemployed,       na.rm = TRUE),
    pop_25_plus      = sum(pop_25_plus,      na.rm = TRUE),
    college_25plus   = sum(college_25plus,   na.rm = TRUE),
    white_alone      = sum(white_alone,      na.rm = TRUE),
    n_counties       = n(),
    .groups          = "drop"
  ) |>
  mutate(
    unemp_rate_pct   = 100 * unemployed     / labor_force,
    pct_college      = 100 * college_25plus / pop_25_plus,
    pct_white        = 100 * white_alone    / total_pop
  )

cat("\nCZ-level ACS dataset:\n")
print(cz_acs |>
        summarise(
          n          = n(),
          across(c(unemp_rate_pct, pct_college, pct_white, median_hh_income),
                 list(median = ~median(.x, na.rm = TRUE),
                      p10    = ~quantile(.x, 0.10, na.rm = TRUE),
                      p90    = ~quantile(.x, 0.90, na.rm = TRUE)))
        ))

# ---- 4. Join with exposure + LISA -> final analysis dataset ------------
cz_expo <- read_parquet(here("data", "processed", "cz_exposure_index.parquet"))
cz_lisa <- read_parquet(here("data", "processed", "cz_lisa.parquet"))

cz_dataset <- cz_expo |>
  inner_join(cz_acs,                                       by = "cz20") |>
  inner_join(cz_lisa |> select(cz20, lisa_cluster, lisa_label),
             by = "cz20")

cat("\n=== Final analysis dataset ===\n")
cat("Rows (CZs):", nrow(cz_dataset), "\n")
cat("Columns:", paste(names(cz_dataset), collapse = ", "), "\n")
cat("\nSummary of regression variables:\n")
print(summary(cz_dataset |>
                select(exposure_pp, unemp_rate_pct, pct_college,
                       pct_white, median_hh_income)))

# ---- 5. Save -----------------------------------------------------------
out <- here("data", "processed", "cz_analysis_dataset.parquet")
write_parquet(cz_dataset, out)
cat("\nSaved:", out, "(", round(file.info(out)$size / 1024, 1), "KB )\n")
