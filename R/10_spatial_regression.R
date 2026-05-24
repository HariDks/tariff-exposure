# ============================================================================
# Phase 6 — Spatial regression: tariff exposure vs unemployment
#
# Pipeline (the Anselin decision rule from Lec 6):
#   1. Fit baseline OLS:  unemp ~ exposure + controls
#   2. Test Moran's I on the OLS residuals
#   3. If significant -> fit Spatial Lag and Spatial Error, pick by AIC
#   4. Report all three models side by side
#
# Honest caveat: the outcome (ACS 2019-2023 unemployment) is from BEFORE the
# 2025 tariffs took effect. So this is a CROSS-SECTIONAL / DESCRIPTIVE
# spatial relationship, not a causal identification. The causal version
# requires post-2025 LAUS monthly data — that's the v2 DiD design.
#
# Outputs:
#   data/processed/regression_models.rds  (saved fitted models)
#   output/tables/06_regression_results.csv
# ============================================================================

library(here)
library(sf)
library(dplyr)
library(arrow)
library(spdep)
library(spatialreg)

# ---- 1. Load polygons + analysis data, build W -------------------------
cz_sf      <- st_read(here("data", "processed", "cz_l48.gpkg"), quiet = TRUE)
cz_data    <- read_parquet(here("data", "processed", "cz_analysis_dataset.parquet"))

cz <- cz_sf |>
  inner_join(cz_data, by = "cz20") |>
  arrange(cz20)

# Build Queen-contiguity neighbors
nb <- poly2nb(cz, queen = TRUE)
no_neighbors <- which(card(nb) == 0)
cat("Islands (no neighbors) to drop:", length(no_neighbors), "\n")

# Drop islands and rebuild neighbors on the trimmed set
if (length(no_neighbors) > 0) {
  cz <- cz[-no_neighbors, ]
  nb <- poly2nb(cz, queen = TRUE)
}
W  <- nb2listw(nb, style = "W", zero.policy = TRUE)
cat("CZs entering regression:", nrow(cz), "\n")

# ---- 2. Build the regression formula -----------------------------------
# Variables (all CZ level):
#   y  = unemp_rate_pct       (ACS 2019-2023 unemployment rate)
#   x1 = exposure_pp          (our shift-share tariff exposure index)
#   x2 = pct_college          (% with bachelor's or higher, age 25+)
#   x3 = pct_white            (% white alone)
#   x4 = log_income           (log median household income — log because
#                              income distributions are heavily right-skewed)
cz <- cz |>
  mutate(log_income = log(median_hh_income))

reg_formula <- unemp_rate_pct ~ exposure_pp + pct_college + pct_white + log_income

# ---- 3. Baseline OLS ---------------------------------------------------
ols <- lm(reg_formula, data = cz)
cat("\n=== OLS baseline ===\n")
print(summary(ols))

# ---- 4. Moran's I on the OLS residuals ---------------------------------
# If residuals are spatially autocorrelated -> OLS standard errors are
# wrong -> we must use a spatial model.
ols_resid_moran <- lm.morantest(ols, listw = W, zero.policy = TRUE)
cat("\n=== Moran's I on OLS residuals ===\n")
print(ols_resid_moran)

# Lagrange Multiplier tests — Anselin's standard guide to picking
# between lag and error specifications.
lm_tests <- lm.LMtests(ols, listw = W,
                       test = c("LMlag", "RLMlag", "LMerr", "RLMerr", "SARMA"),
                       zero.policy = TRUE)
cat("\n=== Lagrange Multiplier tests ===\n")
print(lm_tests)

# ---- 5. Spatial Lag (SAR) model ----------------------------------------
sar <- lagsarlm(reg_formula, data = cz, listw = W, zero.policy = TRUE)
cat("\n=== Spatial Lag model (SAR) ===\n")
print(summary(sar))

# ---- 6. Spatial Error (SEM) model --------------------------------------
sem <- errorsarlm(reg_formula, data = cz, listw = W, zero.policy = TRUE)
cat("\n=== Spatial Error model (SEM) ===\n")
print(summary(sem))

# ---- 7. AIC comparison -------------------------------------------------
aic_tab <- data.frame(
  model = c("OLS", "Spatial Lag (SAR)", "Spatial Error (SEM)"),
  AIC   = c(AIC(ols), AIC(sar), AIC(sem)),
  logLik = c(as.numeric(logLik(ols)), as.numeric(logLik(sar)), as.numeric(logLik(sem)))
) |> mutate(delta_AIC = AIC - min(AIC))
cat("\n=== Model comparison (AIC: lower is better) ===\n")
print(aic_tab)

best_label <- aic_tab$model[which.min(aic_tab$AIC)]
cat("\nBest model by AIC:", best_label, "\n")

# ---- 8. Compact coefficient table for the report -----------------------
extract_coefs <- function(mod, model_name) {
  # spatialreg's sarlm summary stores its coef table under $Coef,
  # not $coefficients. Handle both.
  s <- summary(mod)
  ct <- if (!is.null(s$Coef)) s$Coef else s$coefficients
  data.frame(
    model    = model_name,
    term     = rownames(ct),
    estimate = ct[, "Estimate"],
    std_err  = ct[, 2],          # std error is always 2nd column
    p_value  = ct[, ncol(ct)],   # p-value is always last
    row.names = NULL
  )
}

coef_table <- bind_rows(
  extract_coefs(ols, "OLS"),
  extract_coefs(sar, "SAR"),
  extract_coefs(sem, "SEM")
)
# Add the spatial parameters explicitly
coef_table <- bind_rows(
  coef_table,
  data.frame(model="SAR", term="rho (spatial lag)",
             estimate = sar$rho,
             std_err  = sar$rho.se,
             p_value  = 2 * (1 - pnorm(abs(sar$rho / sar$rho.se)))),
  data.frame(model="SEM", term="lambda (spatial error)",
             estimate = sem$lambda,
             std_err  = sem$lambda.se,
             p_value  = 2 * (1 - pnorm(abs(sem$lambda / sem$lambda.se))))
)

cat("\n=== Coefficient table (compact) ===\n")
print(coef_table, row.names = FALSE)
write.csv(coef_table,
          here("output", "tables", "06_regression_results.csv"),
          row.names = FALSE)

# ---- 9. Save fitted models ---------------------------------------------
saveRDS(list(ols = ols, sar = sar, sem = sem,
             moran_resid = ols_resid_moran, lm_tests = lm_tests,
             aic = aic_tab),
        here("data", "processed", "regression_models.rds"))

cat("\nDone. Saved:\n")
cat("  data/processed/regression_models.rds\n")
cat("  output/tables/06_regression_results.csv\n")
