# ============================================================================
# Phase 5 supplement — Bivariate Moran's I
#
# Question: are high-exposure CZs spatially co-located with high-unemployment
# CZs? Tests whether the two variables CO-CLUSTER spatially, beyond what
# standard univariate Moran's I would detect.
#
# Inputs:
#   data/processed/cz_l48.gpkg
#   data/processed/cz_analysis_dataset.parquet  (has both exposure_pp and unemp_rate_pct)
#
# Outputs:
#   output/figures/05c_bivariate_moran.png  (scatterplot + LISA cluster map)
#   output/tables/05c_bivariate_moran.csv   (global I and per-CZ classification)
# ============================================================================

library(here)
library(sf)
library(dplyr)
library(ggplot2)
library(arrow)
library(spdep)
library(patchwork)

# ---- 1. Load + join + build W -----------------------------------------
cz_sf <- st_read(here("data", "processed", "cz_l48.gpkg"), quiet = TRUE)
df    <- read_parquet(here("data", "processed", "cz_analysis_dataset.parquet"))

cz <- cz_sf |>
  inner_join(df |> select(cz20, exposure_pp, unemp_rate_pct, lisa_label),
             by = "cz20") |>
  arrange(cz20)

nb <- poly2nb(cz, queen = TRUE)
W  <- nb2listw(nb, style = "W", zero.policy = TRUE)

# ---- 2. Global bivariate Moran's I + Monte Carlo significance ---------
z_x <- as.numeric(scale(cz$exposure_pp))
z_y <- as.numeric(scale(cz$unemp_rate_pct))
lag_z_y <- as.numeric(lag.listw(W, z_y, zero.policy = TRUE))

I_bv_observed <- mean(z_x * lag_z_y)

# Permutation: shuffle y, recompute I, count how often the permuted I
# exceeds the observed one.
set.seed(42)
n_perm <- 999
I_perm <- replicate(n_perm, {
  z_y_perm   <- sample(z_y)
  lag_perm   <- as.numeric(lag.listw(W, z_y_perm, zero.policy = TRUE))
  mean(z_x * lag_perm)
})
p_value <- mean(abs(I_perm) >= abs(I_bv_observed))

cat("Global bivariate Moran's I (exposure x neighbor-unemployment):",
    round(I_bv_observed, 4), "\n")
cat("p-value (999 Monte Carlo permutations):", round(p_value, 4), "\n\n")

# ---- 3. Local bivariate Moran's I (per-CZ classification) -------------
# Each CZ's local statistic = its own z_x * mean of its neighbors' z_y
local_bv <- z_x * lag_z_y

# Significance per CZ via permutation
local_perm <- matrix(NA, nrow = length(z_x), ncol = n_perm)
for (k in seq_len(n_perm)) {
  z_y_perm <- sample(z_y)
  lag_perm <- as.numeric(lag.listw(W, z_y_perm, zero.policy = TRUE))
  local_perm[, k] <- z_x * lag_perm
}
local_p <- sapply(seq_along(local_bv), function(i) {
  mean(abs(local_perm[i, ]) >= abs(local_bv[i]))
})

# Classify each CZ into HH/LL/HL/LH (using bivariate quadrants)
cz <- cz |>
  mutate(
    bv_quadrant = case_when(
      z_x >= 0 & lag_z_y >= 0 ~ "HH (high expo, high-unemp neighbors)",
      z_x <  0 & lag_z_y <  0 ~ "LL (low expo, low-unemp neighbors)",
      z_x >= 0 & lag_z_y <  0 ~ "HL (high expo, low-unemp neighbors)",
      z_x <  0 & lag_z_y >= 0 ~ "LH (low expo, high-unemp neighbors)"
    ),
    bv_pvalue = local_p,
    bv_label  = if_else(local_p <= 0.05, bv_quadrant, "Not significant")
  )

cat("Bivariate LISA cluster counts (p<=0.05):\n")
print(cz |> st_drop_geometry() |> count(bv_label, sort = TRUE))

# Save tabular results
cz |> st_drop_geometry() |>
  select(cz20, exposure_pp, unemp_rate_pct, bv_quadrant, bv_pvalue, bv_label) |>
  write.csv(here("output", "tables", "05c_bivariate_moran.csv"), row.names = FALSE)

# ---- 4. Scatterplot (bivariate Moran scatter) + map -------------------
scatter_df <- tibble(
  cz20    = cz$cz20,
  z_expo  = z_x,
  lag_unemp = lag_z_y,
  bv_label  = cz$bv_label
)

scatter_fig <- ggplot(scatter_df, aes(z_expo, lag_unemp, colour = bv_label)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_point(alpha = 0.7, size = 1.4) +
  geom_smooth(method = "lm", se = FALSE, colour = "black",
              linewidth = 0.6, formula = y ~ x) +
  scale_colour_manual(values = c(
    "HH (high expo, high-unemp neighbors)" = "#d7191c",
    "LL (low expo, low-unemp neighbors)"   = "#2c7bb6",
    "HL (high expo, low-unemp neighbors)"  = "#fdae61",
    "LH (low expo, high-unemp neighbors)"  = "#abd9e9",
    "Not significant"                       = "grey80"
  ), name = NULL) +
  labs(
    title    = "Bivariate Moran scatter — tariff exposure vs neighbors' unemployment",
    subtitle = sprintf("Global bivariate Moran's I = %.3f, p = %.3f (999 MC permutations)",
                       I_bv_observed, p_value),
    x = "Own tariff exposure (z-score)",
    y = "Mean of neighbors' unemployment rate (z-score)"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "right",
        plot.title    = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(colour = "grey30", size = 9))

map_palette <- c(
  "HH (high expo, high-unemp neighbors)" = "#d7191c",
  "LL (low expo, low-unemp neighbors)"   = "#2c7bb6",
  "HL (high expo, low-unemp neighbors)"  = "#fdae61",
  "LH (low expo, high-unemp neighbors)"  = "#abd9e9",
  "Not significant"                       = "grey90"
)

map_fig <- ggplot(cz) +
  geom_sf(aes(fill = bv_label), colour = "grey60", linewidth = 0.05) +
  scale_fill_manual(values = map_palette, name = "Bivariate cluster") +
  labs(
    title    = "Where exposure spatially co-clusters with neighbors' unemployment",
    subtitle = "Significant CZs (p<=0.05) shown; others grey"
  ) +
  theme_void(base_size = 10) +
  theme(plot.title    = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(colour = "grey30", size = 9),
        legend.position = "bottom")

combo <- scatter_fig / map_fig +
  plot_layout(heights = c(1, 1.2))

ggsave(here("output", "figures", "05c_bivariate_moran.png"),
       combo, width = 11, height = 11, dpi = 150)
cat("\nSaved: output/figures/05c_bivariate_moran.png\n")
cat("Saved: output/tables/05c_bivariate_moran.csv\n")
