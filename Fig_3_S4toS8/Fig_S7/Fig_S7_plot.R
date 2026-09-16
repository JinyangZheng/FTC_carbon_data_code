# Fig_S7 plotting code
# Input tables are stored together in Fig_S7_data.rds.
.script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.script_path <- if (length(.script_files)) tail(.script_files, 1L)[[1L]] else if (length(.script_arg)) sub("^--file=", "", .script_arg[[1L]]) else "Fig_S7_plot.R"
code_dir <- dirname(normalizePath(.script_path, winslash = "/", mustWork = TRUE))
.figure_data <- readRDS(file.path(code_dir, "Fig_S7_data.rds"))
data_dir <- code_dir
output_dir <- file.path(code_dir, "output")
figure_dir <- output_dir
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
.tidy_code_dir <- code_dir
.plot_file_exists <- function(path) basename(path) %in% names(.figure_data) | file.exists(path)
options(stringsAsFactors = FALSE)

figure_id <- "Fig_S7"
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(ragg)
})

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

write_csv_gz <- function(x, path) {
  connection <- gzfile(path, open = "wt")
  on.exit(close(connection), add = TRUE)
  utils::write.csv(x, connection, row.names = FALSE, na = "")
}

required_packages <- c("ggplot2", "patchwork", "ragg", "systemfonts")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
assert_true(length(missing_packages) == 0L, paste("Missing R packages", paste(missing_packages, collapse = ", ")))
font_match <- systemfonts::match_fonts("Times New Roman")
assert_true(nrow(font_match) == 1L && .plot_file_exists(font_match$path), "Times New Roman is unavailable")

hourly <- .figure_data[["model_ensemble.csv.gz"]]
observations <- .figure_data[["observations.csv"]]
rmse <- .figure_data[["RMSE.csv"]]
stopifnot(nrow(hourly) == 16L * 4L * 5760L)
stopifnot(identical(sort(unique(hourly$Hour)), seq_len(5760L)))
stopifnot(all(hourly$posterior_draws == 200L), length(unique(hourly$soil)) == 1L)
model_levels <- c("Base", "Physio-only", "Physic-only", "Full")
duration_levels <- c("LFLT", "LFST", "SFLT", "SFST")
response_levels <- c("Rs", "EEA", "DOC", "MBC")
model_colors <- c(
  Base = "#e4c8ff",
  `Physio-only` = "#8baade",
  `Physic-only` = "#f0b4c6",
  Full = "#9ed69e"
)
display_units <- c(Rs = 1, EEA = 1, DOC = 0.001, MBC = 0.001)
response_labels <- c(
  Rs = "Rs\n(\u00b5g C g\u207b\u00b9 SOC h\u207b\u00b9)",
  EEA = "EEA\n(\u00b5g C g\u207b\u00b9 SOC h\u207b\u00b9)",
  DOC = "DOC\n(mg C g\u207b\u00b9 SOC)",
  MBC = "MBC\n(mg C g\u207b\u00b9 SOC)"
)
x_padding_fraction <- 0.05
x_padding_days <- 240 * x_padding_fraction
x_panel_limits <- c(0 - x_padding_days, 240 + x_padding_days)

axis_contract <- data.frame(
  soil = rep(c(
    "2400_0_10", "2400_50_100", "3400_0_10",
    "3400_50_100", "4600_0_10", "4600_50_100"
  ), each = 4L),
  response = rep(response_levels, times = 6L),
  original_lower = c(
    0, 0, 8, 2,
    0, 0, 8, 0.3,
    0, 0, 13, 8,
    0, 0, 9, 0.3,
    0, 0, 10, 0.3,
    0, 0, 10, 0.3
  ),
  original_upper = c(
    8.2, 0.7, 25, 20,
    8.2, 0.7, 30, 8,
    8.2, 0.7, 26, 43,
    8.2, 0.7, 28, 8,
    8.2, 0.7, 27, 40,
    8.2, 0.7, 27, 8
  ),
  original_breaks = c(
    "1|4|7", "0.1|0.4|0.7", "10|16|23", "4|11|18",
    "1|4|7", "0.1|0.4|0.7", "11|19|27", "1|4|7",
    "1|4|7", "0.1|0.4|0.7", "14|19|24", "10|25|40",
    "1|4|7", "0.1|0.4|0.7", "12|19|26", "1|4|7",
    "1|4|7", "0.1|0.4|0.7", "12|19|26", "5|20|35",
    "1|4|7", "0.1|0.4|0.7", "12|19|26", "1|4|7"
  ),
  revised_lower = c(
    0, -0.35, 4.50, 0.50,
    0, -0.20, 7.00, -0.50,
    0, -0.30, 10.00, 7.50,
    0, -0.20, 5.00, 0.00,
    0, -0.30, 8.00, 9.00,
    0, -0.20, 5.00, 0.00
  ),
  revised_upper = c(
    8.2, 1.75, 25.50, 21.50,
    8.2, 1.00, 31.00, 8.50,
    8.2, 1.50, 28.00, 52.50,
    8.2, 1.00, 29.00, 12.00,
    8.2, 1.50, 26.00, 45.00,
    8.2, 1.00, 29.00, 12.00
  ),
  revised_breaks = c(
    "1|4|7", "0|0.7|1.4", "8|15|22", "4|11|18",
    "1|4|7", "0|0.4|0.8", "11|19|27", "1|4|7",
    "1|4|7", "0|0.6|1.2", "13|19|25", "15|30|45",
    "1|4|7", "0|0.4|0.8", "9|17|25", "2|6|10",
    "1|4|7", "0|0.6|1.2", "11|17|23", "15|27|39",
    "1|4|7", "0|0.4|0.8", "9|17|25", "2|6|10"
  ),
  stringsAsFactors = FALSE
)

hourly$model <- factor(hourly$model, levels = model_levels)
hourly$duration <- factor(hourly$duration, levels = duration_levels)
hourly$response <- factor(hourly$response, levels = response_levels)
hourly$Day <- hourly$Hour / 24
hourly$display_mean <- hourly$posterior_mean * display_units[as.character(hourly$response)]
hourly$display_sd <- hourly$posterior_sd * display_units[as.character(hourly$response)]
hourly$display_lower <- pmax(0, hourly$display_mean - hourly$display_sd)
hourly$display_upper <- hourly$display_mean + hourly$display_sd

observations$duration <- factor(observations$duration, levels = duration_levels)
observations$response <- factor(observations$response, levels = response_levels)
observations$display_observed <- observations$observed * display_units[as.character(observations$response)]

rmse$model <- factor(rmse$model, levels = model_levels)
rmse$duration <- factor(rmse$duration, levels = duration_levels)
rmse$response <- factor(rmse$response, levels = response_levels)

resolve_axis <- function(soil_name, response_name, line_data, point_data) {
  contract <- axis_contract[
    axis_contract$soil == soil_name & axis_contract$response == response_name,
    , drop = FALSE
  ]
  assert_true(nrow(contract) == 1L, "Axis contract key is missing")
  upper_values <- c(line_data$display_upper, point_data$display_observed)
  upper_values <- upper_values[is.finite(upper_values)]
  lower_values <- c(line_data$display_lower, point_data$display_observed)
  lower_values <- lower_values[is.finite(lower_values)]
  displayed_max <- if (length(upper_values)) max(upper_values) else contract$original_upper
  displayed_min <- if (length(lower_values)) min(lower_values) else contract$original_lower
  expanded_upper <- displayed_max > contract$original_upper
  expanded_lower <- displayed_min < contract$original_lower

  lower <- contract$revised_lower
  upper <- contract$revised_upper
  breaks <- as.numeric(strsplit(contract$revised_breaks, "|", fixed = TRUE)[[1L]])
  assert_true(length(breaks) == 3L, "Every y axis must have exactly three labels")
  assert_true(isTRUE(all.equal(diff(breaks)[1L], diff(breaks)[2L])), "Y-axis labels must be equally spaced")
  lower_padding <- min(breaks) - lower
  upper_padding <- upper - max(breaks)
  assert_true(lower_padding > 0 && upper_padding > 0, "Y-axis labels must be inside the panel boundaries")
  tick_positions <- (breaks - lower) / (upper - lower)
  if (identical(response_name, "Rs")) {
    tick_policy <- "original_Rs_axis"
  } else {
    assert_true(
      isTRUE(all.equal(tick_positions, c(1 / 6, 1 / 2, 5 / 6), tolerance = 1e-10)),
      "Non-Rs y-axis labels must occupy the inner sixths of the panel"
    )
    tick_policy <- "three_equal_labels_at_inner_sixths"
  }

  assert_true(lower <= displayed_min && upper >= displayed_max, "Resolved y-axis clips displayed data")
  data.frame(
    soil = soil_name,
    response = response_name,
    lower = lower,
    upper = upper,
    breaks = paste(format(signif(breaks, 10L), trim = TRUE, scientific = FALSE), collapse = "|"),
    tick_count = length(breaks),
    equal_tick_spacing = length(breaks) == 3L && isTRUE(all.equal(diff(breaks)[1L], diff(breaks)[2L])),
    tick_relative_positions = paste(format(signif(tick_positions, 10L), trim = TRUE, scientific = FALSE), collapse = "|"),
    y_lower_padding = lower_padding,
    y_upper_padding = upper_padding,
    y_outer_padding = min(lower_padding, upper_padding),
    tick_policy = tick_policy,
    x_panel_lower = x_panel_limits[[1L]],
    x_panel_upper = x_panel_limits[[2L]],
    x_padding_days = x_padding_days,
    original_lower = contract$original_lower,
    original_upper = contract$original_upper,
    expanded_lower_for_data_integrity = expanded_lower,
    expanded_upper_for_data_integrity = expanded_upper,
    displayed_minimum = displayed_min,
    displayed_maximum = displayed_max,
    stringsAsFactors = FALSE
  )
}

make_rmse_annotation <- function(panel_rmse, response_name, lower, upper) {
  y_range <- upper - lower
  y_positions <- c(title = upper - 0.055 * y_range, Calibration = upper - 0.14 * y_range, Validation = upper - 0.225 * y_range)
  annotation <- data.frame(
    x = c(63, 63, 63),
    y = unname(y_positions),
    label = c("RMSE", "Cal", "Val"),
    color = "black",
    hjust = 0,
    stringsAsFactors = FALSE
  )
  model_x <- c(Base = 103, `Physio-only` = 137, `Physic-only` = 171, Full = 205)
  for (period_name in c("Calibration", "Validation")) {
    for (model_name in model_levels) {
      value <- panel_rmse$RMSE_mean[
        as.character(panel_rmse$period) == period_name & as.character(panel_rmse$model) == model_name
      ]
      label <- if (length(value) == 1L && is.finite(value)) sprintf("%.2f", round(value, 2)) else "-"
      annotation <- rbind(
        annotation,
        data.frame(
          x = unname(model_x[[model_name]]),
          y = unname(y_positions[[period_name]]),
          label = label,
          color = unname(model_colors[[model_name]]),
          hjust = 0.5,
          stringsAsFactors = FALSE
        )
      )
    }
  }
  annotation
}

make_panel <- function(soil_name, response_name, duration_name, row_index, column_index) {
  line_data <- hourly[
    hourly$soil == soil_name & as.character(hourly$response) == response_name & as.character(hourly$duration) == duration_name,
    , drop = FALSE
  ]
  point_data <- observations[
    observations$soil == soil_name & as.character(observations$response) == response_name & as.character(observations$duration) == duration_name,
    , drop = FALSE
  ]
  panel_rmse <- rmse[
    rmse$soil == soil_name & as.character(rmse$response) == response_name & as.character(rmse$duration) == duration_name,
    , drop = FALSE
  ]
  axis_line_data <- hourly[
    hourly$soil == soil_name & as.character(hourly$response) == response_name,
    , drop = FALSE
  ]
  axis_point_data <- observations[
    observations$soil == soil_name & as.character(observations$response) == response_name,
    , drop = FALSE
  ]
  axis <- resolve_axis(soil_name, response_name, axis_line_data, axis_point_data)
  breaks <- as.numeric(strsplit(axis$breaks, "|", fixed = TRUE)[[1L]])
  annotation <- make_rmse_annotation(panel_rmse, response_name, axis$lower, axis$upper)

  plot <- ggplot(line_data, aes(x = Day, y = display_mean, color = model, fill = model, group = model)) +
    geom_ribbon(
      aes(ymin = display_lower, ymax = display_upper),
      alpha = 0.25,
      color = NA,
      show.legend = FALSE
    ) +
    geom_vline(xintercept = 192, linetype = "dashed", color = "grey60", linewidth = 0.42) +
    geom_line(linewidth = 0.38, show.legend = FALSE) +
    geom_point(
      data = point_data,
      aes(x = Day, y = display_observed),
      inherit.aes = FALSE,
      shape = 1,
      size = 1.45,
      stroke = 0.48,
      color = "grey40",
      alpha = 0.65,
      show.legend = FALSE
    ) +
    geom_text(
      data = annotation,
      aes(x = x, y = y, label = label, color = color, hjust = hjust),
      inherit.aes = FALSE,
      family = "Times New Roman",
      size = 2.05,
      vjust = 0.5,
      show.legend = FALSE
    ) +
    scale_color_manual(
      values = c(
        model_colors,
        black = "black",
        stats::setNames(unname(model_colors), unname(model_colors))
      ),
      guide = "none"
    ) +
    scale_fill_manual(values = model_colors, drop = FALSE) +
    scale_x_continuous(breaks = c(0, 96, 192), limits = x_panel_limits, expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(breaks = breaks, limits = c(axis$lower, axis$upper), expand = expansion(mult = c(0, 0))) +
    labs(
      title = if (row_index == 1L) duration_name else NULL,
      x = NULL,
      y = if (column_index == 1L) unname(response_labels[[response_name]]) else NULL
    ) +
    theme_bw(base_size = 6.1, base_family = "Times New Roman") +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.38),
      axis.line = element_blank(),
      axis.ticks = element_line(color = "black", linewidth = 0.32),
      axis.ticks.length = grid::unit(1.2, "mm"),
      axis.text = element_text(color = "black", size = 6.0),
      axis.title.y = element_text(color = "black", size = 6.8, lineheight = 0.92, margin = margin(r = 3.5)),
      plot.title = element_text(color = "black", size = 8.2, hjust = 0.5, margin = margin(b = 1.5)),
      plot.margin = margin(0, 0, 0, 0),
      legend.position = "none"
    )

  if (row_index < 4L) {
    plot <- plot + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  if (column_index > 1L) {
    plot <- plot + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }
  attr(plot, "axis_contract") <- axis
  plot
}

make_shared_legend <- function() {
  legend_models <- data.frame(
    model = factor(model_levels, levels = model_levels),
    x = c(2.8, 4.9, 7.6, 10.4),
    label = paste0("\u201c", model_levels, "\u201d"),
    stringsAsFactors = FALSE
  )
  ggplot() +
    geom_point(aes(x = 0.25, y = 0.5), shape = 1, size = 2.0, stroke = 0.55, color = "grey40") +
    annotate("text", x = 0.55, y = 0.5, label = "Observation data", hjust = 0, family = "Times New Roman", size = 3.0) +
    geom_segment(
      data = legend_models,
      aes(x = x, xend = x + 0.36, y = 0.5, yend = 0.5, color = model),
      linewidth = 0.65,
      show.legend = FALSE
    ) +
    geom_text(
      data = legend_models,
      aes(x = x + 0.48, y = 0.5, label = label),
      hjust = 0,
      family = "Times New Roman",
      size = 3.0,
      color = "black"
    ) +
    scale_color_manual(values = model_colors, drop = FALSE) +
    coord_cartesian(xlim = c(0, 12.5), ylim = c(0, 1), clip = "off") +
    theme_void(base_family = "Times New Roman") +
    theme(plot.margin = margin(0, 1, 0, 1))
}

make_x_label <- function() {
  ggplot() +
    annotate("text", x = 0.5, y = 0.55, label = "Incubation days", family = "Times New Roman", size = 3.0) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    theme_void()
}

build_figure <- function(soil_name) {
  plots <- list()
  axis_rows <- list()
  index <- 1L
  for (row_index in seq_along(response_levels)) {
    for (column_index in seq_along(duration_levels)) {
      panel <- make_panel(
        soil_name,
        response_levels[[row_index]],
        duration_levels[[column_index]],
        row_index,
        column_index
      )
      plots[[index]] <- panel
      axis_rows[[length(axis_rows) + 1L]] <- attr(panel, "axis_contract")
      index <- index + 1L
    }
  }
  grid_plot <- wrap_plots(plots, ncol = 4L, byrow = TRUE) +
    plot_layout(heights = rep(1, 4))
  complete <- make_shared_legend() / grid_plot / make_x_label() +
    plot_layout(heights = c(0.082, 1, 0.045))
  list(plot = complete, axis = unique(do.call(rbind, axis_rows)))
}

save_figure <- function(plot, stem, width_mm = 183, height_mm = 161.3, dpi = 600) {
  width_in <- width_mm / 25.4
  height_in <- height_mm / 25.4
  pdf_path <- file.path(figure_dir, paste0(stem, ".pdf"))
  png_path <- file.path(figure_dir, paste0(stem, ".png"))
  formats <- unique(trimws(strsplit(Sys.getenv("FTC_FIG_FORMATS", "pdf,png"), ",", fixed = TRUE)[[1L]]))
  assert_true(length(formats) > 0L && all(formats %in% c("pdf", "png")), "Only PDF and PNG exports are supported")

  if ("pdf" %in% formats) {
    grDevices::cairo_pdf(pdf_path, width = width_in, height = height_in, family = "Times New Roman")
    print(plot)
    grDevices::dev.off()
  }

  if ("png" %in% formats) {
    ragg::agg_png(png_path, width = width_in, height = height_in, units = "in", res = 300, background = "white")
    print(plot)
    grDevices::dev.off()
  }

  c(pdf = pdf_path, png = png_path)[formats]
}

soil_name <- unique(hourly$soil)
result <- build_figure(soil_name)
save_figure(result$plot, figure_id)
write.csv(result$axis, file.path(figure_dir, "axis_limits.csv"), row.names = FALSE)

