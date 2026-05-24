# ============================================================================
# Phase 6.5 (robustness) — three quick checks on the DiD result
#
# Baseline (from R/12_did_analysis.R, "Both events" spec):
#   exposure_pp x post_apr =  +0.112  (p = 0.036)
#   exposure_pp x post_aug =  -0.302  (p < 0.001)
#
# This script re-fits the same spec with three modifications:
#   (1) WEIGHTED  by labor force — each worker counts equally instead of
#                 each CZ counting equally
#   (2) ADJUSTED  for evolving demographic confounders (pct_college,
#                 pct_white, log_income) by interacting them with the
#                 post-treatment dummies
#   (3) PLACEBO   pretend tariffs were announced January 2025 (three months
#                 before the actual announcement). Restrict the sample to
#                 pre-real-treatment months only so the real treatment can't
#                 contaminate the placebo. A null result here means our
#                 identification isn't picking up spurious pre-trends.
#
# Output: output/tables/06_did_robustness.csv — side-by-side coefficients
# ============================================================================

library(here)
library(dplyr)
library(arrow)
library(lubridate)
library(fixest)
library(broom)

# ---- 1. Load data + replicate the coverage filter ---------------------
panel <- read_parquet(here("data", "processed", "cz_unemployment_panel.parquet"))
expo  <- read_parquet(here("data", "processed", "cz_exposure_index.parquet"))
ctrl  <- read_parquet(here("data", "processed", "cz_analysis_dataset.parquet")) |>
  select(cz20, pct_college, pct_white, median_hh_income) |>
  mutate(log_income = log(median_hh_income))

df <- panel |>
  inner_join(expo |> select(cz20, exposure_pp), by = "cz20") |>
  inner_join(ctrl, by = "cz20") |>
  filter(!is.na(unemp_rate), is.finite(unemp_rate))

n_cz_total <- n_distinct(df$cz20)
df <- df |>
  group_by(date) |>
  filter(n() >= 0.50 * n_cz_total) |>
  ungroup() |>
  mutate(
    post_apr = as.integer(date >= ymd("2025-04-01")),
    post_aug = as.integer(date >= ymd("2025-08-01"))
  )

cat("Analysis panel:", nrow(df), "obs;", n_distinct(df$cz20), "CZs;",
    n_distinct(df$date), "months\n\n")

# ---- BASELINE (re-fit for side-by-side comparison) ---------------------
baseline <- feols(
  unemp_rate ~ exposure_pp:post_apr + exposure_pp:post_aug | cz20 + date,
  data = df, cluster = ~cz20
)

# ---- 2. WEIGHTED by labor force ---------------------------------------
weighted_did <- feols(
  unemp_rate ~ exposure_pp:post_apr + exposure_pp:post_aug | cz20 + date,
  data    = df,
  weights = ~labor_force,
  cluster = ~cz20
)

# ---- 3. REGRESSION-ADJUSTED with demographic x post controls ----------
# The CZ demographics (pct_college, pct_white, log_income) are time-invariant
# in our data — they only have one value per CZ — so their main effects are
# absorbed by the CZ fixed effects. We add them only as INTERACTIONS with
# the post-treatment dummies, to test whether the exposure x post effect
# survives controlling for differential post-treatment trends by demographics.
adjusted_did <- feols(
  unemp_rate ~ exposure_pp:post_apr + exposure_pp:post_aug +
               pct_college:post_aug + pct_white:post_aug + log_income:post_aug
               | cz20 + date,
  data = df, cluster = ~cz20
)

# ---- 4. PLACEBO with fake January 2025 treatment date -----------------
# Strategy: restrict sample to pre-real-treatment months only
# (Jan 2024 - March 2025), then pretend the treatment was Jan 2025.
# If we estimate a "significant" effect on a date when nothing real happened,
# our DiD is detecting spurious pre-trends, not the true treatment.
placebo_data <- df |>
  filter(date < ymd("2025-04-01")) |>
  mutate(post_placebo = as.integer(date >= ymd("2025-01-01")))

cat("Placebo sample: pre-real-treatment only.",
    nrow(placebo_data), "obs,",
    n_distinct(placebo_data$date), "months,",
    n_distinct(placebo_data$cz20), "CZs\n")
cat("Placebo treatment fraction:",
    round(mean(placebo_data$post_placebo), 2), "\n\n")

placebo_did <- feols(
  unemp_rate ~ exposure_pp:post_placebo | cz20 + date,
  data    = placebo_data,
  cluster = ~cz20
)

# ---- 5. Side-by-side comparison ---------------------------------------
get_did_row <- function(model, term_name, model_label) {
  est <- broom::tidy(model) |>
    filter(term %in% term_name) |>
    mutate(model = model_label) |>
    select(model, term, estimate, std.error, p.value)
  est
}

robust_table <- bind_rows(
  get_did_row(baseline,    c("exposure_pp:post_apr", "exposure_pp:post_aug"),
              "Baseline (from R/12)"),
  get_did_row(weighted_did, c("exposure_pp:post_apr", "exposure_pp:post_aug"),
              "(1) Weighted by labor force"),
  get_did_row(adjusted_did, c("exposure_pp:post_apr", "exposure_pp:post_aug"),
              "(2) Demographic-adjusted"),
  get_did_row(placebo_did,  c("exposure_pp:post_placebo"),
              "(3) Placebo (fake Jan 2025 date, pre-treatment sample only)")
)

cat("=== Robustness table ===\n")
print(robust_table, n = 20)
write.csv(robust_table,
          here("output", "tables", "06_did_robustness.csv"),
          row.names = FALSE)

# ---- 6. Verbal verdict line per check ----------------------------------
b_apr <- coef(baseline)["exposure_pp:post_apr"]
b_aug <- coef(baseline)["exposure_pp:post_aug"]
w_apr <- coef(weighted_did)["exposure_pp:post_apr"]
w_aug <- coef(weighted_did)["exposure_pp:post_aug"]
a_apr <- coef(adjusted_did)["exposure_pp:post_apr"]
a_aug <- coef(adjusted_did)["exposure_pp:post_aug"]
p_pre <- coef(placebo_did)["exposure_pp:post_placebo"]
p_pre_pval <- broom::tidy(placebo_did) |>
  filter(term == "exposure_pp:post_placebo") |> pull(p.value)

cat("\n=== Verbal verdicts ===\n")
cat(sprintf("(1) Weighted: post-April moves %s (%+0.2f -> %+0.2f), post-August moves %s (%+0.2f -> %+0.2f)\n",
            ifelse(sign(b_apr)==sign(w_apr), "same direction", "FLIPS"),
            b_apr, w_apr,
            ifelse(sign(b_aug)==sign(w_aug), "same direction", "FLIPS"),
            b_aug, w_aug))
cat(sprintf("(2) Adjusted: post-April moves %s (%+0.2f -> %+0.2f), post-August moves %s (%+0.2f -> %+0.2f)\n",
            ifelse(sign(b_apr)==sign(a_apr), "same direction", "FLIPS"),
            b_apr, a_apr,
            ifelse(sign(b_aug)==sign(a_aug), "same direction", "FLIPS"),
            b_aug, a_aug))
cat(sprintf("(3) Placebo: exposure x fake-Jan-2025 = %+0.3f (p = %.3f). Goal: not significant.\n",
            p_pre, p_pre_pval))

cat("\nSaved: output/tables/06_did_robustness.csv\n")
