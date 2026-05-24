# ============================================================================
# Phase 2 — Process PIIE tariff data into a NAICS-sector tariff change schedule
#
# Input:  data/raw/replication package-mar 26/raw data/IM Data_JantoMar26_HS6.xlsx
#         (PIIE Working Paper 25-13 replication, Calculated Duties + CIF Imports)
#
# Output: data/processed/naics_tariff_change.parquet
#         Columns: naics_sector, rate_pre, rate_post, delta_rate_pp, n_hs_codes
#
# Method:
#   1. For each HS-6 code: rate = Calculated Duties / CIF Import Value
#      - pre  = sum(Jan-Mar 2025 duties) / sum(Jan-Mar 2025 CIF)
#      - post = sum(Jan-Mar 2026 duties) / sum(Jan-Mar 2026 CIF)
#      - delta = post - pre (in percentage points)
#   2. Map each HS-6 to a NAICS-2 sector via HS-chapter rules:
#        HS 01-14 -> NAICS 11   (Agriculture)
#        HS 15-24 -> NAICS 31-33 (Manufacturing - processed food)
#        HS 25-27 -> NAICS 21   (Mining/extractives)
#        HS 28-99 -> NAICS 31-33 (Manufacturing)
#   3. Within each NAICS sector, aggregate rates as
#        sector_rate = sum(duties) / sum(CIF)
#      (i.e., import-value-weighted; bigger HS lines get more weight)
#
# Citation: Hufbauer, G. C., & Zhang, Y. (2025). PIIE Working Paper 25-13
#           replication data, accessed via PIIE Trump tariff revenue tracker.
# ============================================================================

library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(arrow)

piie_file <- here("data", "raw", "replication package-mar 26",
                  "raw data", "IM Data_JantoMar26_HS6.xlsx")

# ---- 1. Read both sheets ------------------------------------------------
read_piie_sheet <- function(sheet) {
  read_excel(piie_file, sheet = sheet, skip = 2) |>
    rename(metric = `Data Type`, hts = `HTS Number`, desc = Description, year = Year) |>
    # Pivot the month columns to long form so we can sum Jan+Feb+Mar.
    pivot_longer(
      cols      = January:December,
      names_to  = "month",
      values_to = "value"
    ) |>
    mutate(value = as.numeric(value)) |>
    filter(!is.na(year), !is.na(value))
}

cif    <- read_piie_sheet("CIF Import Value")
duties <- read_piie_sheet("Calculated Duties")

cat("CIF rows:    ", format(nrow(cif),    big.mark=","), "\n")
cat("Duties rows: ", format(nrow(duties), big.mark=","), "\n")
cat("HS codes:    ", n_distinct(cif$hts), "\n")

# ---- 2. Q1 sums per HS-6 per year --------------------------------------
q1_months <- c("January", "February", "March")

q1_sums <- function(df, metric_name) {
  df |>
    filter(month %in% q1_months) |>
    group_by(hts, year) |>
    summarise(!!metric_name := sum(value, na.rm = TRUE), .groups = "drop")
}

cif_q1    <- q1_sums(cif, "cif")
duties_q1 <- q1_sums(duties, "duties")

# Combine and reshape so we have pre (2025) and post (2026) side by side.
hs_panel <- cif_q1 |>
  full_join(duties_q1, by = c("hts", "year")) |>
  pivot_wider(names_from = year, values_from = c(cif, duties),
              values_fill = 0) |>
  rename(cif_pre = cif_2025, cif_post = cif_2026,
         duties_pre = duties_2025, duties_post = duties_2026)

# ---- 3. HS-6 effective tariff rates -------------------------------------
hs_rates <- hs_panel |>
  mutate(
    rate_pre  = if_else(cif_pre  > 0, duties_pre  / cif_pre,  NA_real_),
    rate_post = if_else(cif_post > 0, duties_post / cif_post, NA_real_),
    delta_pp  = (rate_post - rate_pre) * 100
  )

cat("\nHS-6 rate summary (effective rate, percentage points):\n")
hs_rates |>
  summarise(
    n             = n(),
    median_pre    = median(rate_pre  * 100, na.rm = TRUE),
    median_post   = median(rate_post * 100, na.rm = TRUE),
    median_delta  = median(delta_pp,        na.rm = TRUE),
    p90_delta     = quantile(delta_pp, 0.90, na.rm = TRUE),
    p99_delta     = quantile(delta_pp, 0.99, na.rm = TRUE)
  ) |> print()

# ---- 4. HS-chapter -> NAICS-2 sector mapping ---------------------------
# This is the standard HS-Section-to-NAICS-Sector correspondence taught in
# trade economics (e.g., Schott 2008 concordance). We map at HS-2 chapter
# resolution to NAICS 2-digit sector.
hs_to_naics <- function(hts6) {
  chap <- as.integer(str_sub(hts6, 1, 2))
  case_when(
    chap >= 1  & chap <= 14 ~ "11",       # Agriculture (raw)
    chap >= 15 & chap <= 24 ~ "31-33",    # Manufacturing (food/bev/tobacco)
    chap >= 25 & chap <= 27 ~ "21",       # Mining (salt, ores, fuels)
    chap >= 28 & chap <= 99 ~ "31-33",    # Manufacturing (everything else)
    TRUE                    ~ NA_character_
  )
}

hs_rates_naics <- hs_rates |>
  mutate(naics_sector = hs_to_naics(hts)) |>
  filter(!is.na(naics_sector))

cat("\nHS codes per NAICS sector:\n")
print(hs_rates_naics |> count(naics_sector))

# ---- 5. NAICS-sector tariff schedule (import-value-weighted) -----------
naics_tariff <- hs_rates_naics |>
  group_by(naics_sector) |>
  summarise(
    n_hs_codes      = n(),
    cif_pre_total   = sum(cif_pre,    na.rm = TRUE),
    cif_post_total  = sum(cif_post,   na.rm = TRUE),
    duties_pre_tot  = sum(duties_pre, na.rm = TRUE),
    duties_post_tot = sum(duties_post,na.rm = TRUE),
    rate_pre        = duties_pre_tot  / cif_pre_total,
    rate_post       = duties_post_tot / cif_post_total,
    delta_pp        = (rate_post - rate_pre) * 100,
    .groups = "drop"
  )

# ---- 6. Build full NAICS-sector schedule (other sectors = 0 change) ----
# QCEW has 20 NAICS-2 sectors. We need a delta_pp for every one, even if 0.
all_naics <- c("11","21","22","23","31-33","42","44-45","48-49","51","52",
               "53","54","55","56","61","62","71","72","81","92")

naics_tariff_full <- tibble(naics_sector = all_naics) |>
  left_join(
    naics_tariff |> select(naics_sector, rate_pre, rate_post, delta_pp, n_hs_codes),
    by = "naics_sector"
  ) |>
  mutate(
    rate_pre   = coalesce(rate_pre,   0),
    rate_post  = coalesce(rate_post,  0),
    delta_pp   = coalesce(delta_pp,   0),
    n_hs_codes = coalesce(n_hs_codes, 0L),
    tariff_relevant = naics_sector %in% c("11", "21", "31-33")
  )

cat("\n=== NAICS-2 tariff schedule ===\n")
print(naics_tariff_full, n = 25)

# ---- 7. Save ------------------------------------------------------------
out <- here("data", "processed", "naics_tariff_change.parquet")
write_parquet(naics_tariff_full, out)
cat("\nSaved:", out, "\n")
