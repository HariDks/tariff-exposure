# ============================================================================
# Phase 7 — Interactive Leaflet map (for the report + presentation demo)
#
# Output: output/figures/tariff_exposure_interactive.html
#         (standalone HTML; double-click to open in a browser)
#
# Features:
#   - 4 toggleable layers: aggregate exposure, manufacturing contribution,
#     agriculture contribution, LISA cluster classification
#   - Click any CZ for popup with its name, all exposure values, dominant
#     sector, and total covered employment
#   - Hover for short label (CZ name + aggregate exposure)
#   - Carto Positron basemap (clean grey, good for choropleths)
#
# Course concepts:
#   - Reprojection from Albers (EPSG:5070) -> WGS84 (EPSG:4326) for Leaflet
#   - Leaflet layer model (tile layer + polygon layers + controls)  — Lec 5
#   - colorNumeric / colorFactor palette helpers                     — Lec 5
# ============================================================================

library(here)
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(arrow)
library(leaflet)
library(htmlwidgets)

# ---- 1. Load polygons + analysis data ---------------------------------
cz_sf   <- st_read(here("data", "processed", "cz_l48.gpkg"), quiet = TRUE)
cz_expo <- read_parquet(here("data", "processed", "cz_exposure_index.parquet"))
cz_lisa <- read_parquet(here("data", "processed", "cz_lisa.parquet"))

# Pull a human-readable CZ name from the USDA file
cz_xwalk <- read_csv(
  here("data", "raw", "usda_commuting_zones_2020.csv"),
  show_col_types = FALSE
)
fips_col <- names(cz_xwalk)[str_detect(tolower(names(cz_xwalk)), "fips")][1]
cz_col   <- names(cz_xwalk)[str_detect(tolower(names(cz_xwalk)), "^cz") &
                            !str_detect(tolower(names(cz_xwalk)), "name|contain")][1]
name_col <- names(cz_xwalk)[str_detect(tolower(names(cz_xwalk)), "cz.*name|czname")][1]

cz_names <- cz_xwalk |>
  transmute(
    cz20    = as.character(.data[[cz_col]]),
    county  = CountyName,
    state   = StateName,
    cz_name = .data[[name_col]]
  ) |>
  group_by(cz20) |>
  summarise(
    cz_name    = first(cz_name),
    states     = paste(unique(state), collapse = ", "),
    n_counties = n_distinct(county),
    .groups    = "drop"
  )

# Combine everything
cz <- cz_sf |>
  inner_join(cz_expo, by = "cz20") |>
  inner_join(cz_lisa |> select(cz20, lisa_label), by = "cz20") |>
  left_join(cz_names,                              by = "cz20")

# ---- 2. Reproject for Leaflet -----------------------------------------
# Leaflet expects WGS84 lat/lon (EPSG:4326). Our analysis polygons are in
# Albers (EPSG:5070). Reproject just for the map.
cz_ll <- st_transform(cz, 4326)

# Simplify geometry slightly for snappier browser performance. Tolerance is
# in degrees here; 0.005 deg ~ 500m, invisible at national zoom but
# halves file size.
cz_ll <- st_simplify(cz_ll, dTolerance = 0.005, preserveTopology = TRUE)

cat("Polygons ready for Leaflet:", nrow(cz_ll), "\n")

# ---- 3. Color palettes ------------------------------------------------
# Sequential reds for the three exposure variables
pal_total <- colorNumeric("YlOrRd", domain = cz_ll$exposure_pp,    na.color = "grey90")
pal_mfg   <- colorNumeric("YlOrRd", domain = cz_ll$expo_mfg_pp,    na.color = "grey90")
pal_ag    <- colorNumeric("YlGn",   domain = cz_ll$expo_ag_pp,     na.color = "grey90")

# Qualitative palette for LISA clusters
lisa_levels <- c("HH (hot spot)", "LL (cold spot)",
                 "HL (positive outlier)", "LH (negative outlier)",
                 "Not significant")
lisa_palette <- c("#d7191c", "#2c7bb6", "#fdae61", "#abd9e9", "grey85")
pal_lisa <- colorFactor(palette = lisa_palette,
                        levels  = lisa_levels,
                        na.color = "grey85")
cz_ll <- cz_ll |>
  mutate(lisa_label = factor(lisa_label, levels = lisa_levels))

# ---- 4. Build click-popup HTML for every CZ ---------------------------
# Each CZ gets a rich HTML popup combining name, all sector values, and
# employment. Built once and re-used across layers.
popups <- sprintf(
  paste0(
    "<div style='font-family:Helvetica,Arial,sans-serif;font-size:12px;",
    "max-width:280px;line-height:1.45;'>",
    "<strong style='font-size:13px'>%s</strong><br>",
    "<span style='color:#555'>%s &mdash; %d counties</span><hr style='margin:6px 0'>",
    "<b>Aggregate exposure:</b> %+0.2f pp<br>",
    "&nbsp;&nbsp;Manufacturing: %+0.2f pp<br>",
    "&nbsp;&nbsp;Agriculture: %+0.2f pp<br>",
    "&nbsp;&nbsp;Mining: %+0.2f pp<br>",
    "<b>Dominant sector:</b> %s<br>",
    "<b>LISA cluster:</b> %s<br>",
    "<b>Total covered employment:</b> %s",
    "</div>"
  ),
  ifelse(is.na(cz_ll$cz_name), paste0("CZ ", cz_ll$cz20), cz_ll$cz_name),
  ifelse(is.na(cz_ll$states), "", cz_ll$states),
  ifelse(is.na(cz_ll$n_counties), 0L, cz_ll$n_counties),
  cz_ll$exposure_pp,
  cz_ll$expo_mfg_pp,
  cz_ll$expo_ag_pp,
  cz_ll$expo_mining_pp,
  cz_ll$dominant_sector,
  as.character(cz_ll$lisa_label),
  format(round(cz_ll$emp_total), big.mark = ",")
)

labels <- sprintf("<b>%s</b><br>Aggregate exposure: %+0.2f pp",
                  ifelse(is.na(cz_ll$cz_name), paste0("CZ ", cz_ll$cz20), cz_ll$cz_name),
                  cz_ll$exposure_pp) |> lapply(htmltools::HTML)

# ---- 5. Build the Leaflet map -----------------------------------------
m <- leaflet(cz_ll, options = leafletOptions(minZoom = 3, maxZoom = 10)) |>
  setView(lng = -96, lat = 39, zoom = 4) |>
  addProviderTiles(providers$CartoDB.Positron, group = "Basemap")

# Helper to add a choropleth layer with a popup and label
add_choropleth <- function(map, var, palette, group_name, layer_id) {
  addPolygons(
    map,
    fillColor   = palette(cz_ll[[var]]),
    fillOpacity = 0.75,
    color       = "white",
    weight      = 0.4,
    label       = labels,
    popup       = popups,
    group       = group_name,
    layerId     = paste0(layer_id, "_", cz_ll$cz20),
    highlightOptions = highlightOptions(
      weight = 2, color = "#333", fillOpacity = 0.9, bringToFront = TRUE
    )
  )
}

m <- m |>
  add_choropleth("exposure_pp", pal_total, "Aggregate exposure", "agg") |>
  add_choropleth("expo_mfg_pp", pal_mfg,   "Manufacturing only", "mfg") |>
  add_choropleth("expo_ag_pp",  pal_ag,    "Agriculture only",   "ag")  |>
  addPolygons(
    fillColor   = ~pal_lisa(lisa_label),
    fillOpacity = 0.75,
    color       = "white",
    weight      = 0.4,
    label       = labels,
    popup       = popups,
    group       = "LISA clusters",
    layerId     = paste0("lisa_", cz_ll$cz20)
  )

# Legend (only one shown — we use the aggregate one as the default)
m <- m |>
  addLegend(
    pal      = pal_total,
    values   = cz_ll$exposure_pp,
    title    = "Aggregate<br>exposure (pp)",
    position = "bottomright",
    group    = "Aggregate exposure"
  ) |>
  addLegend(
    pal      = pal_mfg,
    values   = cz_ll$expo_mfg_pp,
    title    = "Manufacturing<br>contribution (pp)",
    position = "bottomright",
    group    = "Manufacturing only"
  ) |>
  addLegend(
    pal      = pal_ag,
    values   = cz_ll$expo_ag_pp,
    title    = "Agriculture<br>contribution (pp)",
    position = "bottomright",
    group    = "Agriculture only"
  ) |>
  addLegend(
    colors   = lisa_palette,
    labels   = lisa_levels,
    title    = "LISA cluster",
    position = "bottomright",
    group    = "LISA clusters"
  )

# Layer-toggle control — exclusive choice (radio buttons) for the choropleths
m <- m |>
  addLayersControl(
    baseGroups    = c("Aggregate exposure", "Manufacturing only",
                      "Agriculture only", "LISA clusters"),
    overlayGroups = NULL,
    options       = layersControlOptions(collapsed = FALSE),
    position      = "topright"
  ) |>
  hideGroup(c("Manufacturing only", "Agriculture only", "LISA clusters"))

# ---- 6. Save standalone HTML ------------------------------------------
out <- here("output", "figures", "tariff_exposure_interactive.html")
saveWidget(m, file = out, selfcontained = TRUE,
           title = "U.S. Commuting-Zone Tariff Exposure (2025-2026)")
cat("Saved:", out, "(", round(file.info(out)$size / 1e6, 2), "MB )\n")
