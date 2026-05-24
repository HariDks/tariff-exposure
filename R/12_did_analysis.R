# ============================================================================
# Phase 6.5 — Difference-in-differences: did high-exposure CZs see worse
#             unemployment after the April 2025 tariff announcement and
#             August 2025 implementation?
#
# Inputs:
#   data/processed/cz_unemployment_panel.parquet  (CZ x month, Jan 2024 - Mar 2026)
#   data/processed/cz_exposure_index.parquet      (CZ-level tariff exposure)
#
# Outputs:
#   output/figures/06a_parallel_trends.png   (mean unemployment by exposure quartile)
#   output/figures/06b_event_study.png       (month-by-month DiD coefficients)
#   output/tables/06_did_results.csv         (two-way FE DiD coefficient table)
#
# Course concepts:
#   - Parallel trends as the identifying assumption of DiD (new for Phase 6.5)
#   - Two-way fixed effects regression  (new)
#   - Event study coefficients          (new)
#   - Cluster-robust standard errors    (new)
# ============================================================================

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(arrow)
library(lubridate)
library(fixest)

# ---- 1. Load panel + exposure ------------------------------------------
panel <- read_parquet(here("data", "processed", "cz_unemployment_panel.parquet"))
expo  <- read_parquet(here("data", "processed", "cz_exposure_index.parquet"))

# Join exposure (time-invariant) onto each CZ-month observation
df <- panel |>
  inner_join(expo |> select(cz20, exposure_pp), by = "cz20") |>
  filter(!is.na(unemp_rate), is.finite(unemp_rate))

# DROP months with sparse coverage. Diagnostic showed:
#   - Oct 2025 (M10): only 78 of 3,221 counties reported (real BLS data gap)
#   - Mar 2026: only 3 counties reported (preliminary release)
# Including either contaminates the panel. We drop by keeping only months
# with >= 50% CZ coverage.
n_cz_total <- n_distinct(df$cz20)
df <- df |>
  group_by(date) |>
  filter(n() >= 0.50 * n_cz_total) |>
  ungroup()
cat("Months retained after >=50% coverage filter:", n_distinct(df$date), "\n")
cat("Date range used:", as.character(range(df$date)), "\n")

cat("Analysis panel: ", nrow(df), "obs;",
    n_distinct(df$cz20), "CZs;",
    n_distinct(df$date), "months\n")

# Event dates from the project notes:
#   April 2025 = tariff announcement
#   August 2025 = tariffs took effect
df <- df |>
  mutate(
    post_apr   = as.integer(date >= ymd("2025-04-01")),
    post_aug   = as.integer(date >= ymd("2025-08-01"))
  )

# ---- 2. Exposure quartiles for the parallel-trends figure --------------
expo_q <- expo |>
  filter(cz20 %in% df$cz20) |>
  mutate(expo_q = ntile(exposure_pp, 4),
         expo_q_label = factor(expo_q,
           levels = 1:4,
           labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")))

df <- df |> left_join(expo_q |> select(cz20, expo_q_label), by = "cz20")

# Mean unemployment by quartile and month
trend_data <- df |>
  group_by(date, expo_q_label) |>
  summarise(unemp_rate = weighted.mean(unemp_rate, w = labor_force, na.rm = TRUE),
            .groups = "drop")

# ---- 3. Parallel-trends figure -----------------------------------------
event_dates <- tibble(
  date = c(ymd("2025-04-01"), ymd("2025-08-01")),
  label = c("Tariffs announced (Apr 2025)", "Tariffs in effect (Aug 2025)")
)

pt_fig <- ggplot(trend_data, aes(date, unemp_rate,
                                 colour = expo_q_label, group = expo_q_label)) +
  geom_vline(data = event_dates, aes(xintercept = date),
             linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  geom_text(data = event_dates,
            aes(x = date, y = 6.6, label = label),
            inherit.aes = FALSE, hjust = -0.05, size = 3, colour = "grey30") +
  scale_colour_brewer(palette = "YlOrRd", name = "Exposure quartile") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(name = "Unemployment rate (%)",
                     limits = c(2, 7)) +
  labs(
    title    = "Parallel-trends check: unemployment by tariff exposure quartile",
    subtitle = "Pre-April-2025: lines should track roughly parallel. Post-April-2025: divergence implies a tariff effect.",
    x        = NULL,
    caption  = "Source: BLS LAUS (Jan 2024 - Mar 2026), aggregated to commuting zones; labor-force-weighted means."
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey30", size = 10),
        plot.caption  = element_text(size = 8, colour = "grey50"),
        legend.position = c(0.12, 0.85),
        axis.text.x   = element_text(angle = 30, hjust = 1))

ggsave(here("output", "figures", "06a_parallel_trends.png"),
       pt_fig, width = 11, height = 6, dpi = 150)
cat("Saved: output/figures/06a_parallel_trends.png\n")

# ---- 4. Two-way fixed effects DiD --------------------------------------
# Spec 1: exposure x post-April interaction only
did_apr <- feols(unemp_rate ~ exposure_pp:post_apr | cz20 + date,
                 data = df, cluster = ~cz20)
# Spec 2: exposure x post-August interaction only
did_aug <- feols(unemp_rate ~ exposure_pp:post_aug | cz20 + date,
                 data = df, cluster = ~cz20)
# Spec 3: both interactions in one model — separates announcement vs implementation
did_both <- feols(unemp_rate ~ exposure_pp:post_apr + exposure_pp:post_aug |
                  cz20 + date,
                  data = df, cluster = ~cz20)

cat("\n=== DiD Spec 1: exposure x post-April ===\n")
print(summary(did_apr))
cat("\n=== DiD Spec 2: exposure x post-August ===\n")
print(summary(did_aug))
cat("\n=== DiD Spec 3: both events in one model ===\n")
print(summary(did_both))

# Save coefficient table
did_table <- bind_rows(
  broom::tidy(did_apr)  |> mutate(model = "Post-April only"),
  broom::tidy(did_aug)  |> mutate(model = "Post-August only"),
  broom::tidy(did_both) |> mutate(model = "Both events")
) |> select(model, term, estimate, std.error, p.value)
write.csv(did_table, here("output", "tables", "06_did_results.csv"), row.names = FALSE)

# ---- 5. Event study ----------------------------------------------------
# Coefficient on (exposure_pp x indicator-for-month-t) for each month,
# with March 2025 (the last pre-treatment month) omitted as reference.
# This gives us the dynamic effect, and lets us visually inspect whether
# anything was happening BEFORE April 2025 (which would violate parallel
# trends).

df <- df |>
  mutate(month_factor = factor(date),
         month_relative = as.integer(round(as.numeric(difftime(date, ymd("2025-03-01"), units = "days")) / 30.44)))

ref_month <- ymd("2025-03-01")
df <- df |>
  mutate(month_relative = factor(month_relative))

# fixest's i() function: interact exposure with month indicators, omitting
# the reference month (relative time = 0 = March 2025)
event_study <- feols(
  unemp_rate ~ i(month_relative, exposure_pp, ref = "0") | cz20 + date,
  data = df, cluster = ~cz20
)

# Build a plot dataframe from the fitted coefficients
es_coefs <- broom::tidy(event_study) |>
  mutate(month_rel = as.integer(stringr::str_extract(term, "-?\\d+"))) |>
  bind_rows(tibble(term = "ref", estimate = 0, std.error = 0,
                   statistic = NA, p.value = NA, month_rel = 0)) |>
  arrange(month_rel) |>
  mutate(date = ref_month %m+% months(month_rel),
         ci_lo = estimate - 1.96 * std.error,
         ci_hi = estimate + 1.96 * std.error)

es_fig <- ggplot(es_coefs, aes(date, estimate)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_vline(xintercept = ymd("2025-04-01"), linetype = "dashed", colour = "grey40") +
  geom_vline(xintercept = ymd("2025-08-01"), linetype = "dashed", colour = "grey40") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = "#d7191c", alpha = 0.15) +
  geom_line(colour = "#d7191c", linewidth = 0.8) +
  geom_point(colour = "#d7191c", size = 1.7) +
  annotate("text", x = ymd("2025-04-01"), y = max(es_coefs$ci_hi, na.rm=TRUE) * 0.95,
           label = "Apr 2025", hjust = -0.05, size = 3, colour = "grey30") +
  annotate("text", x = ymd("2025-08-01"), y = max(es_coefs$ci_hi, na.rm=TRUE) * 0.85,
           label = "Aug 2025", hjust = -0.05, size = 3, colour = "grey30") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title    = "Event study: differential unemployment effect per pp of tariff exposure",
    subtitle = "Coefficient on (exposure x month). Reference = March 2025 (last pre-treatment month).",
    x        = NULL,
    y        = "Effect of 1pp exposure on unemployment rate (pp)",
    caption  = "Two-way fixed effects, SEs clustered at CZ. Shaded band = 95% CI."
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey30", size = 10),
        plot.caption  = element_text(size = 8, colour = "grey50"),
        axis.text.x   = element_text(angle = 30, hjust = 1))

ggsave(here("output", "figures", "06b_event_study.png"),
       es_fig, width = 11, height = 6, dpi = 150)
cat("Saved: output/figures/06b_event_study.png\n")

cat("\nDone.\n")
