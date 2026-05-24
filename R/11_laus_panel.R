# ============================================================================
# Phase 6.5 (part 1) — Build CZ × month unemployment panel from BLS LAUS
#
# Inputs (user manually downloaded in browser to bypass BLS Akamai):
#   data/raw/la.data.64.County.txt   (~332 MB, tab-delimited time series)
#   data/raw/la.area.txt             (area code lookup)
#   data/raw/la.measure              (measure-type lookup, used only for docs)
#   data/raw/la.series.txt           (series-ID lookup)
#
# Output:
#   data/processed/cz_unemployment_panel.parquet
#     One row per (CZ, year-month). Columns: cz20, date, unemp_rate, labor_force, unemployed
#
# LAUS file structure (BLS time-series format):
#   series_id | year | period (M01..M13) | value | footnote_codes
#
# We need:
#   - series_id that encodes (area, measure) — first chars identify the series
#   - filter to county-level areas and measure_code = 03 (unemployment rate)
#     [other measure codes: 01=labor force, 02=employed, 04=unemployed level]
#   - aggregate counties up to CZ by labor-force-weighted average rate
#
# Course concepts:
#   - tabular join via lookup tables (Lec 2/3)
#   - aggregation of rates (Lec 3): weighted mean, not simple mean
#   - panel structure (CZ x month) for DiD (new concept for Phase 6.5)
# ============================================================================

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(arrow)
library(lubridate)

laus_file    <- here("data", "raw", "la.data.64.County.txt")
area_file    <- here("data", "raw", "la.area.txt")
series_file  <- here("data", "raw", "la.series.txt")

stopifnot(file.exists(laus_file))
stopifnot(file.exists(area_file))
stopifnot(file.exists(series_file))

# ---- 1. Read the lookup tables -----------------------------------------
# These are small TSV files. The `area` lookup tells us which area codes
# correspond to which counties. County areas have area_type_code = "F".
area_lookup <- read_tsv(area_file, show_col_types = FALSE) |>
  filter(area_type_code == "F") |>          # F = County
  transmute(
    area_code = area_code,
    geoid     = str_sub(area_code, 3, 7),   # area code: "CN" + 5-digit FIPS
    state_fips = str_sub(area_code, 3, 4),
    area_text = area_text
  )

cat("County areas in LAUS lookup:", nrow(area_lookup), "\n")

# The `series` lookup tells us which series IDs are which (area, measure).
series_lookup <- read_tsv(series_file, show_col_types = FALSE) |>
  transmute(series_id, area_code, measure_code, srd_code, seasonal)

cat("Total series in lookup:", nrow(series_lookup), "\n")

# ---- 2. Read the big LAUS data file ------------------------------------
# 332 MB but mostly numeric so vroom/readr handles it OK. Skip the
# footnote_codes column to save memory.
laus_raw <- read_tsv(
  laus_file,
  col_select = c("series_id", "year", "period", "value"),
  col_types = cols(
    series_id = col_character(),
    year      = col_integer(),
    period    = col_character(),
    value     = col_double()        # forces numeric; "-" or "" become NA
  ),
  show_col_types = FALSE,
  trim_ws = TRUE
)

cat("LAUS rows read: ", format(nrow(laus_raw), big.mark = ","), "\n")

# ---- 3. Filter to county, monthly, NOT seasonally adjusted -------------
# - Drop annual averages (period = "M13").
# - Keep only county-level series via the area lookup.
# - Keep only measure_code in c("03","04","06") for unemp rate, unemp count, labor force.
laus_filtered <- laus_raw |>
  filter(period != "M13") |>
  inner_join(series_lookup, by = "series_id") |>
  inner_join(area_lookup,   by = "area_code") |>
  # 03 = unemployment rate; 04 = unemployment level; 06 = labor force
  filter(measure_code %in% c("03", "04", "06"),
         seasonal == "U")                  # U = not seasonally adjusted

cat("Filtered (county, monthly, NSA, 3 measures):",
    format(nrow(laus_filtered), big.mark = ","), "\n")

# ---- 4. Reshape: wide by measure code, long by date --------------------
measure_names <- c("03" = "unemp_rate",
                   "04" = "unemployed",
                   "06" = "labor_force")

laus_panel <- laus_filtered |>
  mutate(
    metric = measure_names[measure_code],
    month  = as.integer(str_remove(period, "^M")),
    date   = ymd(sprintf("%d-%02d-01", year, month))
  ) |>
  select(geoid, date, metric, value) |>
  pivot_wider(names_from = metric, values_from = value)

cat("County-month panel rows:", format(nrow(laus_panel), big.mark = ","), "\n")
cat("Date range:", as.character(range(laus_panel$date, na.rm = TRUE)), "\n")

# ---- 5. Filter to study window (Jan 2024 -> latest) --------------------
study_start <- ymd("2024-01-01")
laus_panel <- laus_panel |>
  filter(date >= study_start)
cat("Rows after restricting to >= 2024-01:", format(nrow(laus_panel), big.mark=","), "\n")

# ---- 6. Aggregate counties -> CZ ---------------------------------------
# CZ rate = sum(county unemployed) / sum(county labor force), per month.
cz_xwalk <- read_csv(
  here("data", "raw", "usda_commuting_zones_2020.csv"),
  show_col_types = FALSE
)
fips_col <- names(cz_xwalk)[str_detect(tolower(names(cz_xwalk)), "fips")][1]
cz_col   <- names(cz_xwalk)[str_detect(tolower(names(cz_xwalk)), "^cz") &
                            !str_detect(tolower(names(cz_xwalk)), "name|contain")][1]
cz_xwalk <- cz_xwalk |>
  transmute(geoid = str_pad(as.character(.data[[fips_col]]), 5, pad = "0"),
            cz20  = as.character(.data[[cz_col]]))

cz_panel <- laus_panel |>
  inner_join(cz_xwalk, by = "geoid") |>
  group_by(cz20, date) |>
  summarise(
    labor_force = sum(labor_force, na.rm = TRUE),
    unemployed  = sum(unemployed,  na.rm = TRUE),
    unemp_rate  = 100 * unemployed / labor_force,
    n_counties  = n(),
    .groups     = "drop"
  )

cat("CZ-month panel rows:", format(nrow(cz_panel), big.mark = ","), "\n")
cat("Distinct CZs:", n_distinct(cz_panel$cz20), "\n")
cat("Distinct months:", n_distinct(cz_panel$date), "\n")

# ---- 7. Save -----------------------------------------------------------
out <- here("data", "processed", "cz_unemployment_panel.parquet")
write_parquet(cz_panel, out)
cat("\nSaved:", out, "(", round(file.info(out)$size / 1024, 1), "KB )\n")

cat("\n--- Sample ---\n")
print(cz_panel |> filter(cz20 == "1") |> head(10))
