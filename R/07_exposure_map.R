# ============================================================================
# Phase 3 — First descriptive map of CZ tariff exposure
#
# Three deliverables:
#   1. Classification comparison: same data, three classification methods
#      side-by-side. Pedagogical — shows how the choice changes the story.
#   2. Main exposure choropleth (Jenks natural breaks, 6 classes).
#   3. Dominant-sector map (qualitative palette).
#
# Course concepts:
#   - Choropleth classification (equal interval, quantile, Jenks) — Lec 2
#   - Sequential vs qualitative color palettes (ColorBrewer)        — Lec 2
#   - sf + ggplot2 composition                                       — Lec 1, 2
# ============================================================================

library(here)
library(sf)
library(dplyr)
library(ggplot2)
library(arrow)
library(classInt)   # for Jenks natural breaks
library(RColorBrewer)
library(patchwork)  # combine ggplots into multi-panel figures

# ---- 1. Load CZ polygons + exposure index, join them -------------------
cz_sf      <- st_read(here("data", "processed", "cz_l48.gpkg"), quiet = TRUE)
cz_expo    <- read_parquet(here("data", "processed", "cz_exposure_index.parquet"))

cz_map <- cz_sf |>
  left_join(cz_expo, by = "cz20") |>
  # Some CZs in the polygons don't have exposure (territories that slipped
  # through). Drop them rather than show grey holes.
  filter(!is.na(exposure_pp))

cat("CZs joined and ready to map:", nrow(cz_map), "\n")
cat("Exposure range:", round(range(cz_map$exposure_pp), 2), "pp\n")

# ---- 2. Classification comparison panel --------------------------------
# Same variable, same color ramp, three different break methods.
# This panel goes in the methods section of the report to justify our
# eventual choice of classification.

make_classed <- function(values, method, n = 6) {
  brks <- classIntervals(values, n = n, style = method)$brks
  # nudge boundaries slightly so cut() includes the extremes
  brks[1] <- brks[1] - 1e-9
  brks[length(brks)] <- brks[length(brks)] + 1e-9
  cut(values, breaks = brks, include.lowest = TRUE, dig.lab = 2)
}

cz_map_classed <- cz_map |>
  mutate(
    bin_equal    = make_classed(exposure_pp, "equal"),
    bin_quantile = make_classed(exposure_pp, "quantile"),
    bin_jenks    = make_classed(exposure_pp, "jenks")
  )

reds <- brewer.pal(6, "YlOrRd")

plot_one <- function(bin_col, title) {
  ggplot(cz_map_classed) +
    geom_sf(aes(fill = .data[[bin_col]]), colour = "grey75", linewidth = 0.05) +
    scale_fill_manual(values = reds, name = "Exposure (pp)",
                      drop = FALSE, na.value = "grey90") +
    labs(title = title) +
    theme_void(base_size = 9) +
    theme(legend.position = "right",
          plot.title = element_text(size = 10, face = "bold"))
}

p_equal    <- plot_one("bin_equal",    "Equal interval")
p_quantile <- plot_one("bin_quantile", "Quantile")
p_jenks    <- plot_one("bin_jenks",    "Jenks natural breaks")

compare_fig <- (p_equal | p_quantile | p_jenks) +
  plot_annotation(
    title    = "Same data, three classifications: how the choice changes the story",
    subtitle = "Commuting zone tariff exposure index, 2025 Q1 vs 2026 Q1",
    caption  = "Sources: BLS QCEW 2024 Q4; USITC HS-6 duties + CIF via PIIE WP25-13. Geometry: USDA 2020 CZs from US Census TIGER.",
    theme    = theme(plot.caption = element_text(size = 7, colour = "grey50"))
  )

ggsave(here("output", "figures", "03a_classification_comparison.png"),
       compare_fig, width = 13, height = 5, dpi = 150)
cat("Saved: output/figures/03a_classification_comparison.png\n")

# ---- 3. Main exposure choropleth ---------------------------------------
# Jenks chosen because (a) our data has a long right tail (a few very
# high-exposure CZs) so equal interval would compress the bottom 90%; and
# (b) quantile would arbitrarily split the many near-zero CZs into separate
# colors that aren't substantively different.

main_fig <- ggplot(cz_map_classed) +
  geom_sf(aes(fill = bin_jenks), colour = "grey60", linewidth = 0.05) +
  scale_fill_manual(values = reds,
                    name = "Tariff exposure\n(percentage points)",
                    drop = FALSE) +
  labs(
    title    = "Geography of tariff exposure across U.S. commuting zones",
    subtitle = "Employment-share-weighted change in effective tariff rate, 2025 Q1 → 2026 Q1",
    caption  = "Exposure_j = Σ_i (share_ij × Δtariff_i). Sources: BLS QCEW 2024 Q4; USITC duties+CIF via PIIE WP25-13."
  ) +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(colour = "grey30", size = 10),
        plot.caption = element_text(size = 7, colour = "grey50"),
        legend.position = c(0.93, 0.30))

ggsave(here("output", "figures", "03b_main_exposure.png"),
       main_fig, width = 11, height = 7, dpi = 150)
cat("Saved: output/figures/03b_main_exposure.png\n")

# ---- 4. Dominant-sector map (qualitative palette) ----------------------
# Which sector drives each CZ's exposure? Most CZs are dominated by
# manufacturing, but the geographic distribution of the agriculture-dominated
# CZs is interesting on its own.

qual_palette <- c("Agriculture"   = "#1b9e77",
                  "Manufacturing" = "#d95f02",
                  "Mining"        = "#7570b3")

sector_fig <- ggplot(cz_map) +
  geom_sf(aes(fill = dominant_sector), colour = "grey60", linewidth = 0.05) +
  scale_fill_manual(values = qual_palette, name = "Dominant\nsector") +
  labs(
    title    = "Which sector drives tariff exposure in each commuting zone?",
    subtitle = "CZ-level sector with the largest contribution to total exposure",
    caption  = "Dominant sector = argmax of agriculture / manufacturing / mining exposure contributions."
  ) +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(colour = "grey30", size = 10),
        plot.caption = element_text(size = 7, colour = "grey50"),
        legend.position = c(0.93, 0.30))

ggsave(here("output", "figures", "03c_dominant_sector.png"),
       sector_fig, width = 11, height = 7, dpi = 150)
cat("Saved: output/figures/03c_dominant_sector.png\n")
