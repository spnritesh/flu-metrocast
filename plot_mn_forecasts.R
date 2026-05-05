library(tidyverse)
library(plotly)
library(htmltools)

# ── Paths ────────────────────────────────────────────────────────────────────
project_dir  <- "C:/Users/RS/Documents/GitHub/flu-metrocast"
target_file  <- file.path(project_dir, "target-data/latest-data.csv")
ensemble_dir <- file.path(project_dir, "model-output/epiENGAGE-ensemble_mean")

# ── Minnesota locations and display labels ────────────────────────────────────
mn_locations <- c("minnesota", "minneapolis", "st-paul", "duluth", "st-cloud", "rochester")

location_labels <- c(
  minnesota   = "Minnesota",
  minneapolis = "Minneapolis HSA",
  `st-paul`   = "St. Paul HSA",
  duluth      = "Duluth HSA",
  `st-cloud`  = "St. Cloud HSA",
  rochester   = "Rochester HSA"
)

# ── Season date range ─────────────────────────────────────────────────────────
# Note: no forecasts exist before 2025-11-22 — hub season had not started.
season_start <- as.Date("2025-10-01")
season_end   <- as.Date("2026-05-31")

# ── 1. Load actual observed data ──────────────────────────────────────────────
actual <- read_csv(target_file, show_col_types = FALSE) |>
  filter(
    location %in% mn_locations,
    target == "Flu ED visits pct",
    target_end_date >= season_start,
    target_end_date <= season_end
  ) |>
  mutate(target_end_date = as.Date(target_end_date))

# ── 2. Load all 2025-2026 ensemble forecast files ─────────────────────────────
all_files    <- list.files(ensemble_dir, pattern = "\\.csv$", full.names = TRUE)
season_files <- all_files[
  basename(all_files) >= "2025-11-22-epiENGAGE-ensemble_mean.csv"
]

ensemble_raw <- map_dfr(season_files, read_csv, show_col_types = FALSE)

# Helper to pivot quantiles to wide
pivot_ensemble <- function(df) {
  df |>
    filter(
      location %in% mn_locations,
      target == "Flu ED visits pct",
      output_type == "quantile",
      output_type_id %in% c(0.25, 0.5, 0.75)
    ) |>
    mutate(
      reference_date  = as.Date(reference_date),
      target_end_date = as.Date(target_end_date),
      quantile_label  = paste0("q", gsub("\\.", "", sprintf("%.2f", output_type_id)))
    ) |>
    pivot_wider(
      id_cols     = c(reference_date, location, target_end_date),
      names_from  = quantile_label,
      values_from = value
    ) |>
    rename(q25 = q025, q50 = q050, q75 = q075)
}

# All horizons (0–3) for spaghetti plot
ensemble <- pivot_ensemble(ensemble_raw)

# Horizon 1 only for 1-week-ahead plot
ensemble_1wk <- pivot_ensemble(ensemble_raw |> filter(horizon == 1))

# ── 3. Shared plotly helpers ──────────────────────────────────────────────────

# Push any trace whose name contains "50% Interval" to the bottom of the legend
order_interval_last <- function(py) {
  for (i in seq_along(py$x$data)) {
    nm <- py$x$data[[i]]$name
    if (!is.null(nm) && nchar(nm) > 0) {
      py$x$data[[i]]$legendrank <- if (grepl("50%", nm)) 500L else 100L
    }
  }
  py
}

# Remove duplicate legend entries that ggplotly creates for multi-group geoms
dedup_legend <- function(py) {
  seen <- character(0)
  for (i in seq_along(py$x$data)) {
    nm <- py$x$data[[i]]$name
    if (!is.null(nm) && nchar(nm) > 0) {
      if (nm %in% seen) {
        py$x$data[[i]]$showlegend <- FALSE
      } else {
        seen <- c(seen, nm)
        py$x$data[[i]]$showlegend <- TRUE
      }
    }
  }
  py
}

# Build plotly vertical dashed lines as layout shapes (avoids trace clutter)
make_vline_shapes <- function(dates) {
  lapply(sort(unique(dates)), function(d) {
    list(
      type = "line",
      x0 = as.character(d), x1 = as.character(d),
      y0 = 0, y1 = 1, yref = "paper",
      line = list(color = "rgba(130,130,130,0.45)", width = 0.9, dash = "dash")
    )
  })
}

# Standard plotly layout additions applied to both plot types
base_layout <- function(py, vline_shapes) {
  py |>
    layout(
      shapes    = vline_shapes,
      hovermode = "closest",
      legend    = list(
        orientation = "h",
        x = 0, xanchor = "left",
        y = -0.22, yanchor = "top",
        bgcolor     = "rgba(255,255,255,0.85)",
        bordercolor = "#cccccc",
        borderwidth = 1
      ),
      margin = list(b = 90)
    )
}

# ── 4. All-horizons spaghetti plot ────────────────────────────────────────────
make_plot_all <- function(loc) {

  act_loc <- actual   |> filter(location == loc)
  ens_loc <- ensemble |> filter(location == loc)

  act_loc <- act_loc |>
    mutate(tooltip = paste0(
      "<b>Week ending: ", format(target_end_date, "%b %d, %Y"), "</b><br>",
      "Observed: ", round(observation, 2), "%"
    ))

  ens_loc <- ens_loc |>
    mutate(tooltip = paste0(
      "<b>Week ending: ", format(target_end_date, "%b %d, %Y"), "</b><br>",
      "Forecast issued: ", format(reference_date, "%b %d, %Y"), "<br>",
      "Median: ", round(q50, 2), "%<br>",
      "50% Interval: [", round(q25, 2), "%, ", round(q75, 2), "%]"
    ))

  p <- ggplot() +
    geom_ribbon(
      data = ens_loc,
      aes(
        x = target_end_date, ymin = q25, ymax = q75,
        group = reference_date, fill = "50% Interval", text = tooltip
      ),
      alpha = 0.12
    ) +
    geom_line(
      data = ens_loc,
      aes(
        x = target_end_date, y = q50,
        group = reference_date, color = "Ensemble Median", text = tooltip
      ),
      linewidth = 0.65
    ) +
    geom_line(
      data = act_loc,
      aes(
        x = target_end_date, y = observation,
        group = 1, color = "Observed", text = tooltip
      ),
      linewidth = 1.1
    ) +
    geom_point(
      data = act_loc,
      aes(x = target_end_date, y = observation, color = "Observed", text = tooltip),
      size = 1.8
    ) +
    scale_color_manual(NULL,
      breaks = c("Observed", "Ensemble Median"),
      values = c("Observed" = "black", "Ensemble Median" = "steelblue")
    ) +
    scale_fill_manual(NULL,
      values = c("50% Interval" = "steelblue")
    ) +
    scale_x_date(
      limits      = c(season_start, season_end),
      date_breaks = "1 month", date_labels = "%b %Y"
    ) +
    scale_y_continuous(labels = scales::label_number(suffix = "%")) +
    labs(
      title = location_labels[loc],
      x     = NULL,
      y     = "% ED Visits Due to Influenza"
    ) +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      plot.title       = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  ggplotly(p, tooltip = "text", width = 1050, height = 450) |>
    base_layout(make_vline_shapes(ens_loc$reference_date)) |>
    dedup_legend() |>
    order_interval_last()
}

# ── 5. 1-week-ahead plot with interval lines ──────────────────────────────────
make_plot_1wk <- function(loc) {

  act_loc <- actual       |> filter(location == loc)
  ens_loc <- ensemble_1wk |> filter(location == loc)

  act_loc <- act_loc |>
    mutate(tooltip = paste0(
      "<b>Week ending: ", format(target_end_date, "%b %d, %Y"), "</b><br>",
      "Observed: ", round(observation, 2), "%"
    ))

  ens_loc <- ens_loc |>
    mutate(
      tooltip_med = paste0(
        "<b>Week ending: ", format(target_end_date, "%b %d, %Y"), "</b><br>",
        "Forecast issued: ", format(reference_date, "%b %d, %Y"), "<br>",
        "Median: ", round(q50, 2), "%"
      ),
      tooltip_q25 = paste0(
        "<b>Week ending: ", format(target_end_date, "%b %d, %Y"), "</b><br>",
        "Forecast issued: ", format(reference_date, "%b %d, %Y"), "<br>",
        "25th percentile: ", round(q25, 2), "%"
      ),
      tooltip_q75 = paste0(
        "<b>Week ending: ", format(target_end_date, "%b %d, %Y"), "</b><br>",
        "Forecast issued: ", format(reference_date, "%b %d, %Y"), "<br>",
        "75th percentile: ", round(q75, 2), "%"
      )
    )

  p <- ggplot() +
    # Q75 upper interval line
    geom_line(
      data = ens_loc,
      aes(
        x = target_end_date, y = q75,
        color = "50% Interval (Q25/Q75)", text = tooltip_q75
      ),
      linetype = "dashed", linewidth = 0.7
    ) +
    # Q25 lower interval line
    geom_line(
      data = ens_loc,
      aes(
        x = target_end_date, y = q25,
        color = "50% Interval (Q25/Q75)", text = tooltip_q25
      ),
      linetype = "dashed", linewidth = 0.7
    ) +
    # Median forecast line + points
    geom_line(
      data = ens_loc,
      aes(
        x = target_end_date, y = q50,
        color = "1-Wk Ahead Median", text = tooltip_med
      ),
      linewidth = 1
    ) +
    geom_point(
      data = ens_loc,
      aes(
        x = target_end_date, y = q50,
        color = "1-Wk Ahead Median", text = tooltip_med
      ),
      size = 1.8
    ) +
    # Observed line + points
    geom_line(
      data = act_loc,
      aes(x = target_end_date, y = observation, group = 1,
          color = "Observed", text = tooltip),
      linewidth = 1.1
    ) +
    geom_point(
      data = act_loc,
      aes(x = target_end_date, y = observation,
          color = "Observed", text = tooltip),
      size = 1.8
    ) +
    scale_color_manual(
      NULL,
      breaks = c("Observed", "1-Wk Ahead Median", "50% Interval (Q25/Q75)"),
      values = c(
        "Observed"               = "black",
        "1-Wk Ahead Median"      = "steelblue",
        "50% Interval (Q25/Q75)" = "steelblue"
      )
    ) +
    scale_x_date(
      limits      = c(season_start, season_end),
      date_breaks = "1 month", date_labels = "%b %Y"
    ) +
    scale_y_continuous(labels = scales::label_number(suffix = "%")) +
    labs(
      title = paste0(location_labels[loc], " — 1-Week-Ahead"),
      x     = NULL,
      y     = "% ED Visits Due to Influenza"
    ) +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      plot.title       = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  py <- ggplotly(p, tooltip = "text", width = 1050, height = 450)

  # Make the 50% interval legend key show as dashed in the plotly legend
  for (i in seq_along(py$x$data)) {
    nm <- py$x$data[[i]]$name
    if (!is.null(nm) && grepl("50% Interval", nm)) {
      py$x$data[[i]]$line$dash <- "dash"
    }
  }

  py |>
    base_layout(make_vline_shapes(ens_loc$reference_date)) |>
    dedup_legend() |>
    order_interval_last()
}

# ── 6. Build all plots ────────────────────────────────────────────────────────
message("Building all-horizon plots...")
plots_all <- map(mn_locations, make_plot_all)

message("Building 1-week-ahead plots...")
plots_1wk <- map(mn_locations, make_plot_1wk)

# ── 7. Save as standalone HTML files ─────────────────────────────────────────
header_style <- "font-family: sans-serif; margin: 20px 0 4px 12px; color: #222;"
note_style   <- "font-family: sans-serif; font-size: 13px; color: #555; margin: 0 0 6px 12px;"

save_html(
  browsable(tagList(
    tags$h2(
      "Minnesota 2025–2026 Flu Season — All-Horizon Ensemble Forecasts",
      style = header_style
    ),
    tags$p(
      paste(
        "Black line: observed % ED visits due to influenza.",
        "Blue lines: epiENGAGE ensemble median (each trajectory = one weekly forecast, horizons 0–3 weeks ahead).",
        "Blue shading: 50% prediction interval (Q25–Q75).",
        "Dashed vertical lines: forecast issue dates.",
        "No forecasts before Nov 22, 2025 (hub season start)."
      ),
      style = note_style
    ),
    tagList(plots_all)
  )),
  file = file.path(project_dir, "mn_flu_all_horizons_2526.html")
)
message("Saved: mn_flu_all_horizons_2526.html")

save_html(
  browsable(tagList(
    tags$h2(
      "Minnesota 2025–2026 Flu Season — 1-Week-Ahead Ensemble Forecasts",
      style = header_style
    ),
    tags$p(
      paste(
        "Black line: observed % ED visits due to influenza.",
        "Solid blue line: epiENGAGE ensemble median (1-week-ahead forecast).",
        "Dashed blue lines: 50% prediction interval bounds (Q25 and Q75).",
        "Dashed vertical lines: forecast issue dates.",
        "No forecasts before Nov 22, 2025 (hub season start)."
      ),
      style = note_style
    ),
    tagList(plots_1wk)
  )),
  file = file.path(project_dir, "mn_flu_1week_ahead_2526.html")
)
message("Saved: mn_flu_1week_ahead_2526.html")

# ── 8. Export all-horizon plots as PNGs ───────────────────────────────────────
png_dir <- file.path(project_dir, "forecast_plots_png")
dir.create(png_dir, showWarnings = FALSE)

make_plot_all_gg <- function(loc) {

  act_loc   <- actual   |> filter(location == loc)
  ens_loc   <- ensemble |> filter(location == loc)
  act_mn    <- actual   |> filter(location == "minnesota")
  region_nm <- location_labels[loc]

  lbl_actual  <- paste0(region_nm, " Actual % of ED Visits due to Influenza")
  lbl_forecast <- paste0(region_nm, " Forecasts")
  lbl_uncert  <- paste0(region_nm, " Forecast Uncertainty")

  # Build the MN state actual layer only for HSA locations
  mn_layer <- if (loc != "minnesota") {
    list(
      geom_line(
        data     = act_mn,
        aes(x = target_end_date, y = observation, group = 1,
            color = "MN State Actual % of ED Visits due to Influenza"),
        linetype  = "dotted",
        linewidth = 1.0
      )
    )
  } else {
    list()
  }

  color_breaks <- if (loc != "minnesota") {
    c(lbl_actual, "MN State Actual % of ED Visits due to Influenza", lbl_forecast)
  } else {
    c(lbl_actual, lbl_forecast)
  }

  color_values <- setNames(
    c("black", "black", "steelblue"),
    c(lbl_actual, "MN State Actual % of ED Visits due to Influenza", lbl_forecast)
  )

  ggplot() +
    geom_ribbon(
      data = ens_loc,
      aes(
        x = target_end_date, ymin = q25, ymax = q75,
        group = reference_date, fill = lbl_uncert
      ),
      alpha = 0.12
    ) +
    geom_line(
      data = ens_loc,
      aes(
        x = target_end_date, y = q50,
        group = reference_date, color = lbl_forecast
      ),
      linewidth = 0.65
    ) +
    mn_layer +
    geom_line(
      data = act_loc,
      aes(x = target_end_date, y = observation, group = 1,
          color = lbl_actual),
      linewidth = 1.1
    ) +
    geom_point(
      data = act_loc,
      aes(x = target_end_date, y = observation,
          color = lbl_actual),
      size = 1.8
    ) +
    scale_color_manual(NULL, breaks = color_breaks, values = color_values) +
    scale_fill_manual(NULL, values = setNames("steelblue", lbl_uncert)) +
    scale_x_date(
      limits      = c(season_start, season_end),
      date_breaks = "1 month", date_labels = "%b %Y"
    ) +
    scale_y_continuous(labels = scales::label_number(suffix = "%")) +
    labs(
      title = location_labels[loc],
      x     = NULL,
      y     = "% ED Visits Due to Influenza"
    ) +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      plot.title       = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    guides(color = guide_legend(order = 1), fill = guide_legend(order = 2))
}

hsa_locations <- c("minneapolis", "st-paul", "duluth", "st-cloud", "rochester")

message("Saving PNG exports to: ", png_dir)
walk(mn_locations, function(loc) {
  p   <- make_plot_all_gg(loc)
  out <- file.path(png_dir, paste0(loc, "_all_horizons.png"))
  ggsave(out, plot = p, width = 10, height = 4.5, dpi = 150)
  message("  Saved: ", basename(out))
})

# ── 9. State comparison plots (HSA vs. MN state) ──────────────────────────────
state_comp_dir <- file.path(png_dir, "state_comparison")
dir.create(state_comp_dir, showWarnings = FALSE)

ens_mn <- ensemble |> filter(location == "minnesota")

make_plot_state_comparison <- function(loc) {
  act_region <- actual   |> filter(location == loc)
  ens_region <- ensemble |> filter(location == loc)

  ggplot() +
    # MN state 50% ribbon (red)
    geom_ribbon(
      data = ens_mn,
      aes(x = target_end_date, ymin = q25, ymax = q75,
          group = reference_date, fill = "MN State Uncertainty"),
      alpha = 0.10
    ) +
    # MN state ensemble mean (red)
    geom_line(
      data = ens_mn,
      aes(x = target_end_date, y = q50,
          group = reference_date, color = "MN State Ensemble Mean"),
      linewidth = 0.55
    ) +
    # Region 50% ribbon (blue)
    geom_ribbon(
      data = ens_region,
      aes(x = target_end_date, ymin = q25, ymax = q75,
          group = reference_date, fill = "Uncertainty"),
      alpha = 0.12
    ) +
    # Region ensemble mean (blue)
    geom_line(
      data = ens_region,
      aes(x = target_end_date, y = q50,
          group = reference_date, color = "Forecast"),
      linewidth = 0.65
    ) +
    # Region actual (black)
    geom_line(
      data = act_region,
      aes(x = target_end_date, y = observation, group = 1, color = "Actual % of ED Visits due to Influenza"),
      linewidth = 1.1
    ) +
    geom_point(
      data = act_region,
      aes(x = target_end_date, y = observation, color = "Actual % of ED Visits due to Influenza"),
      size = 1.8
    ) +
    scale_color_manual(
      NULL,
      breaks = c("Actual % of ED Visits due to Influenza", "Forecast", "MN State Ensemble Mean"),
      values = c(
        "Actual % of ED Visits due to Influenza"       = "black",
        "Forecast" = "steelblue",
        "MN State Ensemble Mean" = "firebrick"
      )
    ) +
    scale_fill_manual(
      NULL,
      breaks = c("Uncertainty", "MN State Uncertainty"),
      values = c(
        "Uncertainty"          = "steelblue",
        "MN State Uncertainty" = "firebrick"
      )
    ) +
    scale_x_date(
      limits      = c(season_start, season_end),
      date_breaks = "1 month", date_labels = "%b %Y"
    ) +
    scale_y_continuous(labels = scales::label_number(suffix = "%")) +
    labs(
      title    = paste0(location_labels[loc], " vs. Minnesota State"),
      subtitle = "Blue: HSA forecast  |  Red: MN state forecast  |  Black: HSA observed",
      x        = NULL,
      y        = "% ED Visits Due to Influenza"
    ) +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      plot.title       = element_text(face = "bold"),
      plot.subtitle    = element_text(size = 10, color = "#555555"),
      panel.grid.minor = element_blank()
    ) +
    guides(color = guide_legend(order = 1), fill = guide_legend(order = 2))
}

message("Saving state comparison PNGs...")
walk(hsa_locations, function(loc) {
  p   <- make_plot_state_comparison(loc)
  out <- file.path(state_comp_dir, paste0(loc, "_vs_mn_state.png"))
  ggsave(out, plot = p, width = 10, height = 4.5, dpi = 150)
  message("  Saved: ", basename(out))
})

# ── 10. Region actuals (all locations, observed only) ─────────────────────────
region_actuals_dir <- file.path(png_dir, "region_actuals")
dir.create(region_actuals_dir, showWarnings = FALSE)

p_actuals <- actual |>
  mutate(label = factor(location_labels[location],
                        levels = location_labels[mn_locations])) |>
  ggplot(aes(x = target_end_date, y = observation,
             color = label, group = label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  scale_color_brewer("Location", palette = "Dark2") +
  scale_x_date(
    limits      = c(season_start, season_end),
    date_breaks = "1 month", date_labels = "%b %Y"
  ) +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  labs(
    title = "Actual % ED Visits due to Influenza by Region",
    x     = NULL,
    y     = "% ED Visits Due to Influenza"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(region_actuals_dir, "all_regions_observed.png"),
  plot = p_actuals, width = 10, height = 5, dpi = 150
)
message("Saved: all_regions_observed.png")
