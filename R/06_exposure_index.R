# ============================================================================
# Phase 2 (final) — Build the CZ tariff exposure index
#
# Formula:
#     Exposure_j = sum_i ( share_{ij} * delta_tariff_i )
#
# where j indexes commuting zones, i indexes NAICS-2 sectors.
# share_{ij} = CZ j's employment share in sector i (re-normalized to sum to 1)
# delta_tariff_i = sector i's pre->post change in effective tariff rate (pp)
#
# Interpretation: a CZ with Exposure_j = 4.0 has a labor-force-weighted
# average effective tariff rate increase of 4 percentage points across its
# industry mix.
#
# Inputs:
#   data/processed/cz_naics2_employment.parquet
#   data/processed/naics_tariff_change.parquet
#
# Outputs:
#   data/processed/cz_exposure_index.parquet
#     (CZ x [aggregate exposure + per-sector contribution + dominant sector])
# ============================================================================

library(here)
library(dplyr)
library(tidyr)
library(arrow)

emp    <- read_parquet(here("data", "processed", "cz_naics2_employment.parquet"))
tariff <- read_parquet(here("data", "processed", "naics_tariff_change.parquet"))

# ---- Join shares with tariff changes -----------------------------------
joined <- emp |>
  left_join(tariff |> select(naics_sector, delta_pp, tariff_relevant),
            by = "naics_sector") |>
  mutate(
    delta_pp        = coalesce(delta_pp, 0),
    contribution_pp = share * delta_pp
  )

# ---- Aggregate exposure per CZ -----------------------------------------
cz_exposure <- joined |>
  group_by(cz20) |>
  summarise(
    exposure_pp        = sum(contribution_pp, na.rm = TRUE),
    # sector contributions — useful for the disaggregated map
    expo_ag_pp         = sum(contribution_pp[naics_sector == "11"],    na.rm = TRUE),
    expo_mining_pp     = sum(contribution_pp[naics_sector == "21"],    na.rm = TRUE),
    expo_mfg_pp        = sum(contribution_pp[naics_sector == "31-33"], na.rm = TRUE),
    # which sector dominates this CZ's exposure?
    dominant_sector    = c("Agriculture", "Mining", "Manufacturing")[
                            which.max(c(
                              sum(contribution_pp[naics_sector == "11"],    na.rm = TRUE),
                              sum(contribution_pp[naics_sector == "21"],    na.rm = TRUE),
                              sum(contribution_pp[naics_sector == "31-33"], na.rm = TRUE)
                            ))
                          ],
    # for context
    emp_total          = first(emp_total),
    .groups            = "drop"
  )

cat("=== CZ exposure index summary ===\n")
print(cz_exposure |>
        summarise(
          n         = n(),
          mean      = mean(exposure_pp),
          median    = median(exposure_pp),
          p25       = quantile(exposure_pp, 0.25),
          p75       = quantile(exposure_pp, 0.75),
          p90       = quantile(exposure_pp, 0.90),
          max       = max(exposure_pp)
        ))

cat("\nDistribution of dominant sector across CZs:\n")
print(cz_exposure |> count(dominant_sector, sort = TRUE))

cat("\nTop 10 most exposed CZs:\n")
print(cz_exposure |> arrange(desc(exposure_pp)) |> head(10) |>
        select(cz20, exposure_pp, expo_ag_pp, expo_mfg_pp, dominant_sector, emp_total))

cat("\nLeast exposed 10 CZs:\n")
print(cz_exposure |> arrange(exposure_pp) |> head(10) |>
        select(cz20, exposure_pp, expo_ag_pp, expo_mfg_pp, dominant_sector, emp_total))

# ---- Save --------------------------------------------------------------
out <- here("data", "processed", "cz_exposure_index.parquet")
write_parquet(cz_exposure, out)
cat("\nSaved:", out, "\n")
