# Tariff Exposure — Analysis

R analysis for *County-Level Tariff Exposure and Labor Market Outcomes Under the 2025 Trade Regime*.

## Folder layout

```
analysis/
├── tariff-exposure.Rproj   # open this in RStudio
├── report.qmd              # the Quarto report (final deliverable)
├── R/                      # one numbered script per phase
├── data/
│   ├── raw/                # downloaded as-is, never edited (gitignored)
│   └── processed/          # regenerable intermediates (gitignored)
├── output/
│   ├── figures/            # .png maps and charts
│   └── tables/             # .csv / .tex tables
├── renv/                   # package library (gitignored)
└── renv.lock               # locked package versions (committed)
```

## Reproducing the analysis from scratch

```r
# In RStudio, open tariff-exposure.Rproj, then:
renv::restore()             # install exact package versions
source("R/01_load_geographies.R")
source("R/02_build_exposure_index.R")
# ... etc, in numbered order
quarto::quarto_render("report.qmd")
```

## Data sources

| Source | What it gives us | Acquired in |
|---|---|---|
| US Census TIGER (via `tigris`) | County polygons | Phase 1 |
| BLS QCEW | County-by-NAICS employment | Phase 2 |
| USITC HS↔NAICS crosswalk + 2025 tariff schedule | Tariff rates by industry | Phase 2 |
| BLS LAUS | County monthly unemployment | Phase 2 |
| USDA ERS | Commuting zone definitions | Phase 4 |
| ACS 5-year (via `tidycensus`) | County demographic controls | Phase 2 |
