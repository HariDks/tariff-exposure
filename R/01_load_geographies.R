# ============================================================================
# Phase 1 — Load US county geometries
#
# Purpose: download county polygons, restrict to contiguous 48 + DC, reproject
#          to Albers Equal-Area Conic, save as a GeoPackage for downstream use.
#
# Course concepts used:
#   - sf simple features (Lecture 1)
#   - FIPS codes (Lecture 2)
#   - CRS / equal-area projection (Lecture 1)
#   - tigris for TIGER/Line shapefiles (Lecture 2)
# ============================================================================

library(here)
library(sf)
library(tigris)
library(dplyr)
library(ggplot2)

# Cache tigris downloads so we don't re-hit Census servers every run.
options(tigris_use_cache = TRUE)

# ---- 1. Download county polygons -------------------------------------------
# cb = TRUE  -> cartographic boundary file (1:500k generalized, smaller, faster,
#               equivalent for national-scale analysis).
# year = 2023 -> pin to a specific vintage. Reproducibility means freezing every
#                input, including geometry vintage.
# resolution = "500k" -> default for cb files.

counties_raw <- counties(cb = TRUE, year = 2023, resolution = "500k")

cat("Counties downloaded:", nrow(counties_raw), "\n")
cat("Native CRS:", st_crs(counties_raw)$Name, "\n")

# ---- 2. Restrict to contiguous 48 + DC -------------------------------------
# State FIPS codes we DROP:
#   02 Alaska, 15 Hawaii, 60 American Samoa, 66 Guam,
#   69 Northern Mariana Is., 72 Puerto Rico, 78 US Virgin Is.
#
# Why drop them?
#   - USDA commuting zones (our Phase 4 alternative spatial unit) are defined
#     only for the lower 48.
#   - AK/HI distort national choropleths.
#   - The territories are not covered consistently by BLS QCEW/LAUS.

non_contig <- c("02", "15", "60", "66", "69", "72", "78")
counties_l48 <- counties_raw |>
  filter(!STATEFP %in% non_contig)

cat("Counties after filter:", nrow(counties_l48), "\n")

# ---- 3. Reproject to Albers Equal-Area Conic (EPSG:5070) -------------------
# EPSG:5070 = NAD83 / Conus Albers.
# Equal-area projection is required because we will later compute:
#   - area-weighted aggregations,
#   - employment density,
#   - neighborhood relationships that depend on planar distance.
# A geographic CRS (lat/lon) would give wrong answers for all three.

counties_l48 <- counties_l48 |>
  st_transform(5070) |>
  st_make_valid()                 # repair any topology errors from simplification

cat("CRS after reprojection:", st_crs(counties_l48)$Name, "\n")

# ---- 4. Slim to the columns we'll actually use -----------------------------
# Keep FIPS (GEOID), state/county names, ALAND (land area, sq m) and geometry.
# Discarding the rest reduces file size and keeps downstream joins clean.

counties_l48 <- counties_l48 |>
  transmute(
    geoid       = GEOID,           # 5-digit county FIPS (state + county)
    state_fips  = STATEFP,
    state_name  = STATE_NAME,
    county_name = NAME,
    aland_m2    = ALAND,           # land area in square meters
    geometry
  )

# ---- 5. Save to data/processed/ --------------------------------------------
# GeoPackage is the modern replacement for shapefile: single file, no 10-char
# column name limit, full geometry/CRS metadata preserved.

out_path <- here("data", "processed", "counties_l48.gpkg")
st_write(counties_l48, out_path, delete_dsn = TRUE, quiet = TRUE)
cat("Saved to:", out_path, "\n")

# ---- 6. Sanity-check map ---------------------------------------------------
fig <- ggplot(counties_l48) +
  geom_sf(fill = "grey95", colour = "grey40", linewidth = 0.05) +
  labs(
    title    = "U.S. counties, contiguous 48 + DC",
    subtitle = "Reprojected to Albers Equal-Area Conic (EPSG:5070)",
    caption  = paste0("Source: US Census TIGER (cartographic boundary, 2023). n = ", nrow(counties_l48), " counties")
  ) +
  theme_void() +
  theme(plot.caption = element_text(size = 7, colour = "grey50"))

ggsave(
  filename = here("output", "figures", "01_counties_basemap.png"),
  plot     = fig,
  width    = 9, height = 6, dpi = 150
)
cat("Figure saved to: output/figures/01_counties_basemap.png\n")
