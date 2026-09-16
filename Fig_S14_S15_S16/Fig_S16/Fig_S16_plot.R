# Fig_S16 plotting code
# Input tables are stored together in Fig_S16_data.rds.
.script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.script_path <- if (length(.script_files)) tail(.script_files, 1L)[[1L]] else if (length(.script_arg)) sub("^--file=", "", .script_arg[[1L]]) else "Fig_S16_plot.R"
code_dir <- dirname(normalizePath(.script_path, winslash = "/", mustWork = TRUE))
.figure_data <- readRDS(file.path(code_dir, "Fig_S16_data.rds"))
data_dir <- code_dir
output_dir <- file.path(code_dir, "output")
figure_dir <- output_dir
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
.tidy_code_dir <- code_dir
.plot_file_exists <- function(path) basename(path) %in% names(.figure_data) | file.exists(path)
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages(library(data.table))
derived_assert <- function(condition, message) if (!isTRUE(condition)) stop(message)
derived_sha256 <- function(path) digest::digest(path, algo = "sha256", file = TRUE)
paths <- list(figure_dir = output_dir)
font_family <- "Times New Roman"
font_scale <- 1.1
installed_fonts <- systemfonts::system_fonts()
derived_assert(
  any(installed_fonts$family == font_family),
  "Times New Roman is not available to the R graphics devices"
)

metric_labels <- c(
  a_dormant_MBC_fraction = "Dormant MBC\n(%)",
  b_inactive_ENZ_fraction = "Inactive ENZ\n(%)",
  c_hourly_freeze_death_fraction = "Freeze-induced\nMBC mortality\n(%)",
  d_freeze_isolated_DOC_fraction = "Freeze-isolated DOC\n(%)",
  e_freeze_isolated_SOC_fraction = "Freeze-isolated SOC\n(%)",
  f_breakup_DOC_release_fraction = "Breakup DOC release\n(%)",
  g_breakup_SOC_release_fraction = "Breakup SOC release\n(%)"
)
metric_columns <- names(metric_labels)
derived_assert(
  identical(names(metric_labels), metric_columns) &&
    !any(grepl("^[a-g]\\s", metric_labels)) &&
    identical(
      unname(metric_labels[["c_hourly_freeze_death_fraction"]]),
      "Freeze-induced\nMBC mortality\n(%)"
    ),
  "Vertical row labels are invalid"
)
duration_levels <- c("LFLT", "LFST", "SFLT", "SFST")
soil_levels <- c(
  "2400_0_10", "2400_50_100",
  "3400_0_10", "3400_50_100",
  "4600_0_10", "4600_50_100"
)
group_members <- list(
  overall = soil_levels,
  depth_0_10 = soil_levels[c(1L, 3L, 5L)],
  depth_70_80 = soil_levels[c(2L, 4L, 6L)]
)

group_contract <- data.frame(
  group_key = names(group_members),
  group_title = c("Overall", "0-10 cm", "70-80 cm"),
  source_soil = vapply(group_members, paste, character(1L), collapse = "|"),
  source_depth_code = c("all six source soils", "0-10 cm", "50-100 cm"),
  figure_depth_label = c("overall", "0-10 cm", "70-80 cm"),
  expected_IDs_per_duration = c(6L, 3L, 3L),
  figure_stem = c(
    "model_derived_7x4_overall_Day1_Day96",
    "model_derived_7x4_0_10cm_Day1_Day96",
    "model_derived_7x4_70_80cm_Day1_Day96"
  ),
  stringsAsFactors = FALSE
)

group_contract <- group_contract[group_contract$group_key == "depth_70_80", , drop = FALSE]
group_contract$figure_stem <- "Fig_S16"
summary <- data.table::copy(.figure_data[["model_derived_7x4_70_80cm_Day1_Day96_source_data.csv.gz"]])
summary[, metric_label := factor(metric_label, levels = unname(metric_labels))]
summary[, duration := factor(duration, levels = duration_levels, ordered = TRUE)]
y_limits <- data.table::copy(.figure_data[["three_groups_row_y_limits.csv"]])
y_limits[, metric_label := gsub("\r", "", metric_label)]
y_limits[, metric_label := factor(metric_label, levels = unname(metric_labels))]
phase_data <- data.table::copy(.figure_data[["freeze_thaw_background_FigS12_contract.csv"]])
phase_data[, duration := factor(duration, levels = duration_levels, ordered = TRUE)]
stopifnot(nrow(summary) == 64512L, all(summary$posterior_draws == 200L), all(summary$q025_percent <= summary$q975_percent))
format_y_values <- function(values) {
  maximum <- max(abs(values), na.rm = TRUE)
  digits <- if (maximum < 0.1) 3L else if (maximum < 10) 2L else 1L
  formatC(values, format = "f", digits = digits)
}

make_axis_training <- function(group_limits) {
  base <- data.table::copy(group_limits[, .(
    metric, metric_label, axis_lower, axis_upper
  )])
  base_rows <- nrow(base)
  output <- base[rep(seq_len(base_rows), each = length(duration_levels))]
  output[, duration := factor(
    rep(duration_levels, times = base_rows),
    levels = duration_levels,
    ordered = TRUE
  )]
  output <- data.table::melt(
    output,
    id.vars = c("metric", "metric_label", "duration"),
    measure.vars = c("axis_lower", "axis_upper"),
    value.name = "axis_value"
  )
  output[, Day := 48]
  output
}

phase_colors <- c(Freeze = "#6A96E1", Thaw = "#EEA9BE")
figure_width_mm <- 183
figure_height_mm <- 185
figure_dpi <- 600L

finalize_figure_file <- function(temporary, target) {
  derived_assert(
    .plot_file_exists(temporary) && file.info(temporary)$size > 0,
    paste0("Temporary figure was not created for ", target)
  )
  if (.plot_file_exists(target)) {
    derived_assert(file.remove(target), paste0("Could not replace ", target))
  }
  derived_assert(file.rename(temporary, target), paste0("Could not finalize ", target))
  invisible(target)
}

save_pdf_png <- function(plot_object, stem) {
  pdf_path <- file.path(paths$figure_dir, paste0(stem, ".pdf"))
  png_path <- file.path(paths$figure_dir, paste0(stem, ".png"))
  pdf_temporary <- tempfile(
    pattern = paste0(".", stem, "_"),
    tmpdir = paths$figure_dir,
    fileext = ".pdf"
  )
  png_temporary <- tempfile(
    pattern = paste0(".", stem, "_"),
    tmpdir = paths$figure_dir,
    fileext = ".png"
  )
  on.exit(unlink(c(pdf_temporary, png_temporary), force = TRUE), add = TRUE)

  pdf_open <- FALSE
  tryCatch({
    grDevices::cairo_pdf(
      filename = pdf_temporary,
      width = figure_width_mm / 25.4,
      height = figure_height_mm / 25.4,
      family = font_family,
      bg = "white"
    )
    pdf_open <- TRUE
    print(plot_object)
    grDevices::dev.off()
    pdf_open <- FALSE
  }, finally = {
    if (pdf_open) try(grDevices::dev.off(), silent = TRUE)
  })
  finalize_figure_file(pdf_temporary, pdf_path)

  png_open <- FALSE
  tryCatch({
    ragg::agg_png(
      filename = png_temporary,
      width = figure_width_mm,
      height = figure_height_mm,
      units = "mm",
      res = figure_dpi,
      background = "white"
    )
    png_open <- TRUE
    print(plot_object)
    grDevices::dev.off()
    png_open <- FALSE
  }, finally = {
    if (png_open) try(grDevices::dev.off(), silent = TRUE)
  })
  finalize_figure_file(png_temporary, png_path)

  data.frame(
    pdf = normalizePath(pdf_path, winslash = "/", mustWork = TRUE),
    png = normalizePath(png_path, winslash = "/", mustWork = TRUE),
    stringsAsFactors = FALSE
  )
}

figure_records <- vector("list", nrow(group_contract))
for (group_index in seq_len(nrow(group_contract))) {
  group_row <- group_contract[group_index, , drop = FALSE]
  message("Drawing ", group_row$group_title)
  plot_data <- data.table::copy(summary[group_key == group_row$group_key])
  group_limits <- data.table::copy(
    y_limits[group_key == group_row$group_key]
  )
  axis_training <- make_axis_training(group_limits)

  plot_object <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = Day)
  ) +
    ggplot2::geom_rect(
      data = phase_data,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = -Inf,
        ymax = Inf,
        fill = phase
      ),
      inherit.aes = FALSE,
      colour = NA,
      alpha = 0.20
    ) +
    ggplot2::geom_blank(
      data = axis_training,
      ggplot2::aes(x = Day, y = axis_value),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = q025_percent, ymax = q975_percent),
      fill = "black",
      alpha = 0.30,
      colour = NA
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = mean_percent),
      colour = "black",
      linewidth = 0.36,
      lineend = "round"
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(metric_label),
      cols = ggplot2::vars(duration),
      scales = "free_y",
      switch = "y",
      drop = FALSE
    ) +
    ggplot2::scale_fill_manual(values = phase_colors, guide = "none") +
    ggplot2::scale_x_continuous(
      limits = c(0, 96),
      breaks = c(0, 48, 96),
      expand = ggplot2::expansion(mult = 0.05)
    ) +
    ggplot2::scale_y_continuous(
      breaks = function(limits) {
        nominal_span <- diff(limits) / 1.10
        nominal_lower <- limits[[1L]] + 0.05 * nominal_span
        nominal_upper <- limits[[2L]] - 0.05 * nominal_span
        seq(nominal_lower, nominal_upper, length.out = 4L)
      },
      labels = format_y_values,
      expand = ggplot2::expansion(mult = 0.05)
    ) +
    ggplot2::labs(
      title = group_row$group_title,
      x = "Day",
      y = NULL
    ) +
    ggplot2::theme_bw(
      base_size = 7.0 * font_scale,
      base_family = font_family
    ) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.38
      ),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(linewidth = 0.32, colour = "black"),
      axis.ticks.length = grid::unit(1.1, "mm"),
      axis.text = ggplot2::element_text(
        size = 6.3 * font_scale,
        colour = "black",
        family = font_family
      ),
      axis.title.x = ggplot2::element_text(
        size = 7.6 * font_scale,
        family = font_family,
        margin = ggplot2::margin(t = 2.5)
      ),
      strip.background = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.text.x = ggplot2::element_text(
        size = 7.7 * font_scale,
        face = "plain",
        family = font_family,
        margin = ggplot2::margin(b = 1.5)
      ),
      strip.text.y.left = ggplot2::element_text(
        angle = 90,
        size = 6.7 * font_scale,
        face = "plain",
        hjust = 0.5,
        vjust = 0.5,
        lineheight = 0.95,
        family = font_family,
        margin = ggplot2::margin(r = 2.0)
      ),
      panel.spacing.x = grid::unit(1.1, "mm"),
      panel.spacing.y = grid::unit(1.0, "mm"),
      plot.title = ggplot2::element_text(
        size = 9.2 * font_scale,
        face = "plain",
        hjust = 0.5,
        family = font_family,
        margin = ggplot2::margin(b = 3)
      ),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(4, 4, 4, 5, unit = "mm")
    )

  built <- ggplot2::ggplot_build(plot_object)
  y_break_counts <- vapply(
    built$layout$panel_params,
    function(panel) sum(is.finite(panel$y$get_breaks())),
    integer(1L)
  )
  derived_assert(
    length(y_break_counts) == 28L && all(y_break_counts == 4L),
    paste0(group_row$group_key, " does not have four y tick labels per panel")
  )

  saved <- save_pdf_png(plot_object, group_row$figure_stem)
  figure_records[[group_index]] <- data.frame(
    group_key = group_row$group_key,
    group_title = group_row$group_title,
    source_soil = group_row$source_soil,
    source_depth_code = group_row$source_depth_code,
    figure_depth_label = group_row$figure_depth_label,
    rows = 7L,
    columns = 4L,
    width_mm = figure_width_mm,
    height_mm = figure_height_mm,
    dpi = figure_dpi,
    pdf = saved$pdf,
    png = saved$png,
    pdf_sha256 = derived_sha256(saved$pdf),
    png_sha256 = derived_sha256(saved$png),
    stringsAsFactors = FALSE
  )
  rm(plot_data, group_limits, axis_training, plot_object, built, saved)
  invisible(gc())
}

