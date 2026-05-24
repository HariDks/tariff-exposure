# ============================================================================
# Phase 4 — County-level robustness check
#
# We re-run the entire Phase 6 cross-sectional spatial regression at COUNTY
# level (n ~ 3,100) instead of CZ (n = 564). If we get the same substantive
# answer, that's the professor's "do it at both spatial units" check passing.
# If we don't, that's itself an important finding about the MAUP.
#
# Inputs (all already built):
#   data/processed/counties_l48.gpkg
#   data/processed/county_naics2_employment.parquet
#   data/processed/naics_tariff_change.parquet
#
# Plus we pull ACS at county level (cached, fast).
#
# Outputs:
#   data/processed/county_analysis_dataset.parquet
#   output/tables/07_cz_vs_county_comparison.csv
#   output/figures/07_county_exposure.png
# ============================================================================

library(here)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(arrow)
library(ggplot2)
library(spdep)
library(spatialreg)
library(tidycensus)

# ---- 1. Build county exposure index ------------------------------------
county_emp <- read_parquet(here("data", "processed", "county_naics2_employment.parquet"))
tariff     <- read_parquet(here("data", "processed", "naics_tariff_change.parquet"))

county_expo <- county_emp |>
  left_join(tariff |> select(naics_sector, delta_pp), by = "naics_sector") |>
  mutate(delta_pp = coalesce(delta_pp, 0),
         contribution_pp = share * delta_pp) |>
  group_by(geoid) |>
  summarise(
    exposure_pp     = sum(contribution_pp, na.rm = TRUE),
    expo_ag_pp      = sum(contribution_pp[naics_sector == "11"],    na.rm = TRUE),
    expo_mining_pp  = sum(contribution_pp[naics_sector == "21"],    na.rm = TRUE),
    expo_mfg_pp     = sum(contribution_pp[naics_sector == "31-33"], na.rm = TRUE),
    emp_total       = first(emp_total),
    .groups         = "drop"
  )
cat("County exposure index built for", nrow(county_expo), "counties\n")

# ---- 2. Pull county-level ACS (tidycensus will use cache from before) --
vars_needed <- c(
  labor_force      = "B23025_002",
  unemployed       = "B23025_005",
  total_pop        = "B01003_001",
  pop_25_plus      = "B15003_001",
  ba_only          = "B15003_022",
  ma_only          = "B15003_023",
  prof_degree      = "B15003_024",
  phd              = "B15003_025",
  median_hh_income = "B19013_001",
  white_alone      = "B02001_002"
)

cat("Pulling ACS county data (cached)...\n")
acs <- get_acs(geography = "county", variables = vars_needed,
               year = 2023, survey = "acs5", output = "wide",
               cache_table = TRUE) |>
  rename(geoid = GEOID) |>
  mutate(college_25plus = ba_onlyE + ma_onlyE + prof_degreeE + phdE) |>
  transmute(
    geoid,
    total_pop        = total_popE,
    labor_force      = labor_forceE,
    unemployed       = unemployedE,
    pop_25_plus      = pop_25_plusE,
    college_25plus,
    white_alone      = white_aloneE,
    median_hh_income = median_hh_incomeE
  ) |>
  mutate(
    unemp_rate_pct = 100 * unemployed     / labor_force,
    pct_college    = 100 * college_25plus / pop_25_plus,
    pct_white      = 100 * white_alone    / total_pop,
    log_income     = log(median_hh_income)
  )

# ---- 3. Assemble county analysis dataset -------------------------------
counties <- st_read(here("data", "processed", "counties_l48.gpkg"), quiet = TRUE)

county_data <- counties |>
  inner_join(county_expo, by = "geoid") |>
  inner_join(acs, by = "geoid") |>
  filter(!is.na(unemp_rate_pct), is.finite(unemp_rate_pct), !is.na(log_income)) |>
  arrange(geoid)

cat("Counties entering regression:", nrow(county_data), "\n")

# Save the assembled dataset (drop geometry for parquet)
county_data |>
  st_drop_geometry() |>
  write_parquet(here("data", "processed", "county_analysis_dataset.parquet"))

# ---- 4. Spatial weights, drop islands ----------------------------------
nb <- poly2nb(county_data, queen = TRUE)
no_neighbors <- which(card(nb) == 0)
cat("Island counties to drop:", length(no_neighbors), "\n")
if (length(no_neighbors) > 0) {
  county_data <- county_data[-no_neighbors, ]
  nb <- poly2nb(county_data, queen = TRUE)
}
W <- nb2listw(nb, style = "W", zero.policy = TRUE)
cat("Counties entering spatial regression:", nrow(county_data), "\n\n")

# ---- 5. OLS -> Moran on residuals -> SAR + SEM -> AIC ------------------
reg_formula <- unemp_rate_pct ~ exposure_pp + pct_college + pct_white + log_income

ols <- lm(reg_formula, data = county_data)
cat("=== County OLS ===\n"); print(summary(ols))

moran_ols <- lm.morantest(ols, listw = W, zero.policy = TRUE)
cat("\n=== Moran's I on county OLS residuals ===\n"); print(moran_ols)

sar <- lagsarlm(reg_formula, data = county_data, listw = W, zero.policy = TRUE)
sem <- errorsarlm(reg_formula, data = county_data, listw = W, zero.policy = TRUE)

aic_county <- data.frame(
  model = c("OLS", "SAR", "SEM"),
  AIC   = c(AIC(ols), AIC(sar), AIC(sem))
) |> mutate(delta_AIC = AIC - min(AIC))
cat("\n=== County model AIC comparison ===\n"); print(aic_county)

best_county <- if (AIC(sem) <= AIC(sar)) sem else sar
best_label  <- if (AIC(sem) <= AIC(sar)) "SEM" else "SAR"
cat("\nBest county model:", best_label, "\n")
cat("\n=== County best model coefficients ===\n")
print(summary(best_county))

# ---- 6. Side-by-side comparison: CZ vs county --------------------------
extract_main <- function(mod, term_keep = c("exposure_pp","pct_college","pct_white","log_income")) {
  s <- summary(mod)
  ct <- if (!is.null(s$Coef)) s$Coef else s$coefficients
  ct <- ct[rownames(ct) %in% term_keep, , drop = FALSE]
  data.frame(
    term     = rownames(ct),
    estimate = ct[, "Estimate"],
    std_err  = ct[, 2],
    p_value  = ct[, ncol(ct)]
  )
}

# Re-load Phase-6 (CZ) models for comparison
cz_models <- readRDS(here("data", "processed", "regression_models.rds"))
cz_best   <- cz_models$sem  # SEM won at CZ level

comparison <- bind_rows(
  extract_main(cz_best)         |> mutate(unit = "Commuting Zone (Phase 6, SEM)"),
  extract_main(best_county)     |> mutate(unit = paste0("County (Phase 4, ", best_label, ")"))
) |> as_tibble() |>
  select(unit, term, estimate, std_err, p_value) |>
  arrange(term, unit)

cat("\n=== CZ vs County side-by-side ===\n")
print(comparison, n = 20)
write.csv(comparison, here("output", "tables", "07_cz_vs_county_comparison.csv"),
          row.names = FALSE)

# Spatial parameter (rho for SAR, lambda for SEM)
cat("\n=== Spatial parameters ===\n")
cat("CZ SEM lambda:    ", round(cz_best$lambda, 3), "\n")
if (best_label == "SEM") {
  cat("County SEM lambda:", round(best_county$lambda, 3), "\n")
} else {
  cat("County SAR rho:   ", round(best_county$rho, 3), "\n")
}

# ---- 7. Quick county-level exposure map (for the report) --------------
map_data <- counties |>
  inner_join(county_expo |> select(geoid, exposure_pp), by = "geoid")

county_map <- ggplot(map_data) +
  geom_sf(aes(fill = exposure_pp), colour = NA) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       name = "Exposure\n(pp)") +
  labs(
    title    = "County-level tariff exposure (robustness view)",
    subtitle = "Same exposure index re-computed at county level (n = 3,109)",
    caption  = "Compare visually with CZ-level map output/figures/03b. Patterns should be similar."
  ) +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(size = 7, colour = "grey50"),
        legend.position = c(0.93, 0.30))

ggsave(here("output", "figures", "07_county_exposure.png"),
       county_map, width = 11, height = 7, dpi = 150)
cat("\nSaved: output/figures/07_county_exposure.png\n")
cat("Saved: output/tables/07_cz_vs_county_comparison.csv\n")
