# ============================================================================
# Phase 5 — Spatial autocorrelation of CZ tariff exposure
#
# Three things this script does:
#   1. Builds a Queen-contiguity spatial weights matrix W for the L48 CZs.
#   2. Computes GLOBAL Moran's I on the exposure index (+ Monte Carlo p).
#      Same for each sector (Ag, Mfg) for the sector-disaggregated story.
#   3. Computes LOCAL Moran's I (LISA), classifies each CZ as HH/LL/HL/LH,
#      and produces the canonical LISA cluster map.
#
# Outputs:
#   data/processed/cz_lisa.parquet            (LISA classification per CZ)
#   output/figures/05a_moran_scatter.png      (Moran scatterplot — classic viz)
#   output/figures/05b_lisa_clusters.png      (LISA cluster map)
#   output/tables/05_moran_global.csv         (table of global Moran's I results)
#
# Course concepts:
#   - Spatial weights (Queen contiguity, row-standardization) — Lec 6
#   - Global Moran's I + Monte Carlo significance               — Lec 6
#   - LISA HH/LL/HL/LH classification                           — Lec 6
# ============================================================================

library(here)
library(sf)
library(dplyr)
library(ggplot2)
library(arrow)
library(spdep)
library(rgeoda)

# ---- 1. Load CZ polygons + exposure, INNER-join to drop ghost CZs ------
# The exposure parquet has 598 CZs (full USDA universe); the polygon file
# has 564 (L48 only). Use an inner join to keep only CZs that have both
# a polygon AND an exposure value — required for any spatial statistic.
cz_sf   <- st_read(here("data", "processed", "cz_l48.gpkg"), quiet = TRUE)
cz_expo <- read_parquet(here("data", "processed", "cz_exposure_index.parquet"))

cz <- cz_sf |>
  inner_join(cz_expo, by = "cz20") |>
  arrange(cz20)  # consistent row order matters for W

cat("CZs ready for spatial analysis:", nrow(cz), "\n")

# ---- 2. Build the Queen-contiguity spatial weights matrix --------------
# poly2nb finds neighbors. queen = TRUE means share-edge-or-corner.
# zero.policy = TRUE lets us tolerate any CZ that ended up with no
# neighbors (rare — only islands).
nb_queen <- poly2nb(cz, queen = TRUE)

# Summary: how connected is the network?
cat("\nNeighbor-network summary:\n")
print(summary(nb_queen))

# Row-standardize: each row of W sums to 1. This means a CZ with 12
# neighbors gives each one weight 1/12, while a CZ with 4 neighbors
# gives each one weight 1/4 — equal TOTAL influence per CZ.
W_queen <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)

# ---- 3. Global Moran's I --------------------------------------------
# moran.mc runs the Monte Carlo permutation test.
# nsim = 999 is the convention from the lecture slides.
set.seed(42)

run_moran <- function(values, label) {
  m <- moran.mc(values, listw = W_queen, nsim = 999, zero.policy = TRUE)
  data.frame(
    variable     = label,
    morans_i     = unname(m$statistic),
    p_value      = m$p.value,
    n_simulations = m$parameter
  )
}

global_morans <- bind_rows(
  run_moran(cz$exposure_pp,    "Aggregate exposure"),
  run_moran(cz$expo_mfg_pp,    "Manufacturing contribution"),
  run_moran(cz$expo_ag_pp,     "Agriculture contribution"),
  run_moran(cz$expo_mining_pp, "Mining contribution")
)
cat("\n=== Global Moran's I (Queen W, 999 permutations) ===\n")
print(global_morans, row.names = FALSE)
write.csv(global_morans,
          here("output", "tables", "05_moran_global.csv"),
          row.names = FALSE)

# ---- 4. Moran scatterplot (the classic Lec 6 visual) -----------------
# X-axis: own value (standardised). Y-axis: spatially-lagged value (mean
# of neighbors' values, also standardised). Slope = Moran's I.
z_expo <- as.numeric(scale(cz$exposure_pp))
z_lag  <- as.numeric(lag.listw(W_queen, z_expo, zero.policy = TRUE))

scatter_df <- tibble(
  cz20      = cz$cz20,
  z_expo    = z_expo,
  z_lag     = z_lag,
  quadrant  = case_when(
    z_expo >= 0 & z_lag >= 0 ~ "HH (hot spot)",
    z_expo <  0 & z_lag <  0 ~ "LL (cold spot)",
    z_expo >= 0 & z_lag <  0 ~ "HL (positive outlier)",
    z_expo <  0 & z_lag >= 0 ~ "LH (negative outlier)"
  )
)

scatter_fig <- ggplot(scatter_df, aes(z_expo, z_lag, colour = quadrant)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_point(alpha = 0.7, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "black",
              linewidth = 0.6, formula = y ~ x) +
  scale_colour_manual(values = c(
    "HH (hot spot)"          = "#d7191c",
    "LL (cold spot)"         = "#2c7bb6",
    "HL (positive outlier)"  = "#fdae61",
    "LH (negative outlier)"  = "#abd9e9"
  ), name = NULL) +
  labs(
    title    = "Moran scatterplot — CZ tariff exposure clusters spatially",
    subtitle = sprintf("Slope = global Moran's I = %.3f  (p = %.3f, 999 MC permutations)",
                       global_morans$morans_i[1], global_morans$p_value[1]),
    x = "Own exposure (z-score)",
    y = "Mean of neighbors' exposure (z-score, spatially lagged)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("output", "figures", "05a_moran_scatter.png"),
       scatter_fig, width = 8, height = 6, dpi = 150)
cat("\nSaved: output/figures/05a_moran_scatter.png\n")

# ---- 5. LISA (Local Moran's I) ----------------------------------------
# rgeoda::local_moran matches GeoDa's implementation exactly (the tool
# the prof demonstrated in lecture). 999 permutations, p = 0.05.
W_rgeoda <- queen_weights(cz)
lisa <- local_moran(W_rgeoda, cz["exposure_pp"], permutations = 999, seed = 42)

cz <- cz |>
  mutate(
    lisa_cluster  = lisa_clusters(lisa),  # 0=ns, 1=HH, 2=LL, 3=LH, 4=HL
    lisa_p        = lisa_pvalues(lisa),
    lisa_label    = case_when(
      lisa_p > 0.05    ~ "Not significant",
      lisa_cluster == 1 ~ "HH (hot spot)",
      lisa_cluster == 2 ~ "LL (cold spot)",
      lisa_cluster == 3 ~ "LH (negative outlier)",
      lisa_cluster == 4 ~ "HL (positive outlier)",
      TRUE              ~ "Not significant"
    )
  )

cat("\nLISA cluster counts (p < 0.05):\n")
print(cz |> st_drop_geometry() |> count(lisa_label, sort = TRUE))

# Save LISA results
cz_lisa_out <- cz |>
  st_drop_geometry() |>
  select(cz20, exposure_pp, lisa_cluster, lisa_p, lisa_label)
write_parquet(cz_lisa_out, here("data", "processed", "cz_lisa.parquet"))

# ---- 6. LISA cluster map ----------------------------------------------
lisa_colors <- c(
  "Not significant"        = "grey90",
  "HH (hot spot)"          = "#d7191c",
  "LL (cold spot)"         = "#2c7bb6",
  "HL (positive outlier)"  = "#fdae61",
  "LH (negative outlier)"  = "#abd9e9"
)

lisa_fig <- ggplot(cz) +
  geom_sf(aes(fill = lisa_label), colour = "grey60", linewidth = 0.05) +
  scale_fill_manual(values = lisa_colors, name = "LISA cluster") +
  labs(
    title    = "Spatial clusters of tariff exposure (LISA, p < 0.05)",
    subtitle = sprintf("Global Moran's I = %.3f, p = %.3f  |  %d significant clusters",
                       global_morans$morans_i[1], global_morans$p_value[1],
                       sum(cz$lisa_p <= 0.05)),
    caption  = "Hot spots = high-exposure CZs near other high-exposure CZs.\nCold spots = low-exposure CZs near other low-exposure CZs.\nOutliers = a CZ whose exposure differs sharply from its neighbors."
  ) +
  theme_void(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(colour = "grey30", size = 10),
        plot.caption  = element_text(size = 8, colour = "grey50"),
        legend.position = c(0.93, 0.30))

ggsave(here("output", "figures", "05b_lisa_clusters.png"),
       lisa_fig, width = 11, height = 7, dpi = 150)
cat("Saved: output/figures/05b_lisa_clusters.png\n")
