# Fig_S9 plotting code
# Input tables are stored together in Fig_S9_data.rds.
.script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.script_path <- if (length(.script_files)) tail(.script_files, 1L)[[1L]] else if (length(.script_arg)) sub("^--file=", "", .script_arg[[1L]]) else "Fig_S9_plot.R"
code_dir <- dirname(normalizePath(.script_path, winslash = "/", mustWork = TRUE))
.figure_data <- readRDS(file.path(code_dir, "Fig_S9_data.rds"))
data_dir <- code_dir
output_dir <- file.path(code_dir, "output")
figure_dir <- output_dir
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
.tidy_code_dir <- code_dir
.plot_file_exists <- function(path) basename(path) %in% names(.figure_data) | file.exists(path)
options(stringsAsFactors = FALSE)

figure_id <- "Fig_S9"
period <- "Calibration"
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
})
model_parameter_counts <- c(Full = 19L, `Physio-only` = 16L, `Physic-only` = 16L, Base = 13L)
analysis_assert <- function(condition, message) if (!isTRUE(condition)) stop(message, call. = FALSE)
duration_colors <- c(
  LFLT = "#D9B0FF",
  LFST = "#5985D0",
  SFLT = "#E994AE",
  SFST = "#9ED69E"
)
model_levels <- c("Base", "Physio-only", "Physic-only", "Full")
response_axis_contract <- list(
  Calibration = list(
    Rs = list(
      limits = c(-1.1, 2.1),
      breaks = c(-1, 0, 1, 2),
      x = "Modelled Rs (\u00b5g CO\u2082-C g\u207b\u00b9 SOC h\u207b\u00b9)",
      y = "Observed Rs (\u00b5g CO\u2082-C g\u207b\u00b9 SOC h\u207b\u00b9)"
    ),
    EEA = list(
      limits = c(-2.15, -0.4),
      breaks = c(-2.0, -1.5, -1.0, -0.5),
      x = "Modelled EEA (\u00b5g C g\u207b\u00b9 SOC h\u207b\u00b9)",
      y = "Observed EEA (\u00b5g C g\u207b\u00b9 SOC h\u207b\u00b9)"
    ),
    DOC = list(
      limits = c(9.68, 10.19),
      breaks = c(9.75, 9.95, 10.15),
      x = "Modelled DOC (\u00b5g DOC-C g\u207b\u00b9 SOC)",
      y = "Observed DOC (\u00b5g DOC-C g\u207b\u00b9 SOC)"
    ),
    MBC = list(
      limits = c(8.0, 10.7),
      breaks = c(8.4, 9.4, 10.4),
      x = "Modelled MBC (\u00b5g MBC-C g\u207b\u00b9 SOC)",
      y = "Observed MBC (\u00b5g MBC-C g\u207b\u00b9 SOC)"
    )
  ),
  Validation = list(
    Rs = list(
      limits = c(-6.0, 2.2),
      breaks = c(-5, -2, 1),
      x = "Modelled Rs (\u00b5g CO\u2082-C g\u207b\u00b9 SOC h\u207b\u00b9)",
      y = "Observed Rs (\u00b5g CO\u2082-C g\u207b\u00b9 SOC h\u207b\u00b9)"
    ),
    EEA = list(
      limits = c(-4.9, -0.2),
      breaks = c(-4.5, -2.5, -0.5),
      x = "Modelled EEA (\u00b5g C g\u207b\u00b9 SOC h\u207b\u00b9)",
      y = "Observed EEA (\u00b5g C g\u207b\u00b9 SOC h\u207b\u00b9)"
    ),
    DOC = list(
      limits = c(9.05, 10.2),
      breaks = c(9.1, 9.6, 10.1),
      x = "Modelled DOC (\u00b5g DOC-C g\u207b\u00b9 SOC)",
      y = "Observed DOC (\u00b5g DOC-C g\u207b\u00b9 SOC)"
    )
  )
)

base_figure_theme <- function(axis_text_size = 11, axis_title_size = 13) {
  ggplot2::theme_bw(base_size = axis_text_size, base_family = "Times New Roman") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.6),
      axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.6),
      axis.ticks.length = grid::unit(3, "pt"),
      axis.title = ggplot2::element_text(color = "black", size = axis_title_size),
      axis.text = ggplot2::element_text(color = "black", size = axis_text_size),
      plot.title = ggplot2::element_text(face = "plain", hjust = 0.5, size = 14),
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank()
    )
}

relationship_panel <- function(data, annotation, response, model, period, row_index, column_index, n_rows) {
  axis_contract <- response_axis_contract[[period]][[response]]
  limits <- axis_contract$limits
  blank <- data.frame(x = limits, y = limits)
  panel <- ggplot2::ggplot() +
    ggplot2::geom_blank(data = blank, ggplot2::aes(x = x, y = y))

  if (model %in% names(model_parameter_counts)) {
    plot_data <- data[
      data$response == response & data$model == model & data$period == period,
      ,
      drop = FALSE
    ]
    plot_data$log_observed <- log(plot_data$observed)
    plot_data$log_simulated <- log(plot_data$simulated_mean)
    plot_data <- plot_data[
      is.finite(plot_data$log_observed) & is.finite(plot_data$log_simulated),
      ,
      drop = FALSE
    ]
    panel <- panel +
      ggplot2::geom_abline(
        intercept = 0,
        slope = 1,
        color = "#D4372B",
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        data = plot_data,
        ggplot2::aes(x = log_simulated, y = log_observed, color = duration),
        size = 1.5,
        alpha = 0.45
      ) +
      ggplot2::geom_smooth(
        stat = "identity", se = FALSE,
        data = regression_lines[
          regression_lines$response == response & regression_lines$model == model &
            regression_lines$period == period, , drop = FALSE],
        ggplot2::aes(x = x, y = y, color = duration),
        linewidth = 0.75
      ) +
      ggplot2::scale_color_manual(values = duration_colors, drop = FALSE)

    labels <- annotation[
      annotation$period == period & annotation$response == response &
        annotation$model == model,
      ,
      drop = FALSE
    ]
    labels <- labels[match(names(duration_colors), labels$duration), , drop = FALSE]
    labels <- labels[
      !is.na(labels$duration) & is.finite(labels$mean_R2) & is.finite(labels$sd_R2),
      ,
      drop = FALSE
    ]
    span <- diff(limits)
    labels$x <- if (period == "Validation" && response %in% c("Rs", "EEA")) {
      limits[[1L]] + 0.38 * span
    } else {
      limits[[1L]] + 0.045 * span
    }
    labels$y <- if (period == "Validation" && response %in% c("Rs", "EEA")) {
      limits[[1L]] + c(0.37, 0.28, 0.19, 0.10)[seq_len(nrow(labels))] * span
    } else {
      limits[[2L]] - c(0.06, 0.145, 0.23, 0.315)[seq_len(nrow(labels))] * span
    }
    labels$plot_label <- paste0(
      "R^2==plain('",
      sprintf("%.2f", labels$mean_R2),
      "')%+-%plain('",
      sprintf("%.2f", labels$sd_R2),
      "')"
    )
    panel <- panel + ggplot2::geom_text(
      data = labels,
      ggplot2::aes(x = x, y = y, label = plot_label, color = duration),
      hjust = 0,
      vjust = 1,
      size = 3.0,
      family = "Times New Roman",
      parse = TRUE,
      show.legend = FALSE
    )
  }

  title <- if (row_index == 1L) paste0("\u201c", model, "\u201d") else NULL
  panel <- panel +
    ggplot2::scale_x_continuous(breaks = axis_contract$breaks) +
    ggplot2::scale_y_continuous(breaks = axis_contract$breaks) +
    ggplot2::coord_cartesian(xlim = limits, ylim = limits, clip = "off") +
    ggplot2::labs(title = title, x = axis_contract$x, y = axis_contract$y) +
    base_figure_theme() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(4, 2, 2, 5)
    )
  if (column_index == 1L) {
    letter <- letters[[row_index]]
    panel <- panel + ggplot2::annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = letter,
      hjust = 1.15,
      vjust = -0.15,
      fontface = "plain",
      family = "Times New Roman",
      size = 4.4
    )
  }
  panel + ggplot2::guides(color = "none")
}

metric_single_panel <- function(overall, metric_summary, letters, period, metric, panel_letter = NULL) {
  data <- overall[overall$period == period & overall$metric == metric, , drop = FALSE]
  data$model <- factor(data$model, levels = model_levels)
  data$duration <- factor(data$duration, levels = names(duration_colors))
  data$model_index <- as.numeric(data$model)
  duration_offsets <- stats::setNames(c(0.14, 0.17, 0.20, 0.23), names(duration_colors))

  annotation <- metric_summary[
    metric_summary$period == period & metric_summary$metric == metric,
    ,
    drop = FALSE
  ]
  annotation$model <- factor(annotation$model, levels = model_levels)
  annotation$duration <- factor(annotation$duration, levels = names(duration_colors))
  annotation$model_index <- as.numeric(annotation$model)

  letter_annotation <- letters[
    letters$period == period & letters$metric == metric,
    ,
    drop = FALSE
  ]
  letter_annotation$model <- factor(letter_annotation$model, levels = model_levels)
  letter_annotation$model_index <- as.numeric(letter_annotation$model)

  panel_range <- range(data$value, finite = TRUE)
  panel_span <- max(diff(panel_range), abs(panel_range[[2L]]) * 0.05, 1e-8)
  is_rmse <- identical(metric, "RMSE")
  means_at_top <- !(period == "Calibration" && metric %in% c("AIC", "BIC"))
  annotation <- annotation |>
    dplyr::group_by(model) |>
    dplyr::arrange(duration, .by_group = TRUE) |>
    dplyr::mutate(
      y = if (means_at_top) {
        panel_range[[2L]] + c(0.40, 0.31, 0.22, 0.13)[dplyr::row_number()] * panel_span
      } else {
        panel_range[[1L]] - c(0.13, 0.22, 0.31, 0.40)[dplyr::row_number()] * panel_span
      },
      value_label = if (is_rmse) {
        format(round(mean, 2), trim = TRUE, nsmall = 2)
      } else {
        format(round(mean, 0), trim = TRUE, nsmall = 0)
      }
    ) |>
    dplyr::ungroup()
  letter_annotation$y <- if (means_at_top) {
    panel_range[[1L]] - 0.08 * panel_span
  } else {
    panel_range[[2L]] + 0.08 * panel_span
  }
  plot_limits <- if (means_at_top) {
    c(panel_range[[1L]] - 0.14 * panel_span, panel_range[[2L]] + 0.48 * panel_span)
  } else {
    c(panel_range[[1L]] - 0.48 * panel_span, panel_range[[2L]] + 0.14 * panel_span)
  }

  show_model_labels <- identical(metric, "BIC")
  sina_layers <- lapply(seq_along(duration_colors), function(index) {
    duration_name <- names(duration_colors)[[index]]
    duration_data <- data[data$duration == duration_name, , drop = FALSE]
    ggforce::geom_sina(
      data = duration_data,
      ggplot2::aes(
        x = model_index + unname(duration_offsets[[duration_name]]),
        y = value,
        group = model,
        color = duration
      ),
      inherit.aes = FALSE,
      maxwidth = 0.10,
      size = 0.95,
      shape = 20,
      alpha = 0.08
    )
  })
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = model_index, y = value, group = model)) +
    gghalves::geom_half_violin(
      position = ggplot2::position_nudge(x = 0.15, y = 0),
      side = "r",
      adjust = 1.2,
      trim = FALSE,
      color = "black",
      fill = NA,
      linewidth = 0.55,
      alpha = 1
    ) +
    sina_layers +
    ggplot2::geom_boxplot(
      width = 0.10,
      outlier.shape = NA,
      alpha = 0.7,
      color = "black",
      fill = NA,
      linewidth = 0.45
    ) +
    ggplot2::geom_text(
      data = annotation,
      ggplot2::aes(x = model_index, y = y, label = value_label, color = duration),
      inherit.aes = FALSE,
      hjust = 0.5,
      vjust = 0.5,
      family = "Times New Roman",
      size = 2.65,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = letter_annotation,
      ggplot2::aes(x = model_index, y = y, label = letter),
      inherit.aes = FALSE,
      hjust = 0.5,
      vjust = 0.5,
      family = "Times New Roman",
      size = 3.2,
      color = "black"
    ) +
    ggplot2::scale_color_manual(values = duration_colors, drop = FALSE) +
    ggplot2::scale_x_continuous(
      breaks = seq_along(model_levels),
      labels = c("\u201cBase\u201d", "\u201cPhysio\u201d", "\u201cPhysic\u201d", "\u201cFull\u201d"),
      limits = c(0.55, 4.55),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = pretty(panel_range, n = 4),
      expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(ylim = plot_limits, clip = "on") +
    ggplot2::labs(title = panel_letter, x = NULL, y = metric, color = NULL) +
    base_figure_theme(axis_text_size = 10, axis_title_size = 12) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        nrow = 2,
        byrow = FALSE,
        override.aes = list(alpha = 0.6, size = 2.2)
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0,
        size = 15,
        margin = ggplot2::margin(b = 0)
      ),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 4)),
      axis.text.x = if (show_model_labels) {
        ggplot2::element_text(angle = 0, hjust = 0.5, size = 9.5)
      } else {
        ggplot2::element_blank()
      },
      axis.ticks.x = if (show_model_labels) ggplot2::element_line(linewidth = 0.6) else ggplot2::element_blank(),
      legend.position = if (show_model_labels) "bottom" else "none",
      legend.direction = "horizontal",
      legend.text = ggplot2::element_text(size = 10),
      legend.key.width = grid::unit(10, "pt"),
      legend.key.height = grid::unit(8, "pt"),
      legend.spacing.x = grid::unit(3, "pt"),
      plot.margin = ggplot2::margin(1.5, 2, 1.5, 2)
    )
  plot
}

metric_panel <- function(overall, metric_summary, letters, period, panel_letter) {
  metric_plots <- lapply(c("RMSE", "AIC", "BIC"), function(metric) {
    metric_single_panel(
      overall,
      metric_summary,
      letters,
      period,
      metric,
      panel_letter = if (metric == "RMSE") panel_letter else NULL
    )
  })
  patchwork::wrap_plots(metric_plots, ncol = 1, heights = c(1, 1, 1.12))
}

build_figure <- function(figure_data, period, responses, file_stem, width, height) {
  relationship_rows <- vector("list", length(responses))
  for (row_index in seq_along(responses)) {
    row_panels <- vector("list", length(model_levels))
    for (column_index in seq_along(model_levels)) {
      row_panels[[column_index]] <- relationship_panel(
        data = figure_data$relationship,
        annotation = figure_data$r2_annotation,
        response = responses[[row_index]],
        model = model_levels[[column_index]],
        period = period,
        row_index = row_index,
        column_index = column_index,
        n_rows = length(responses)
      )
    }
    relationship_rows[[row_index]] <- patchwork::wrap_plots(row_panels, nrow = 1) +
      patchwork::plot_layout(axes = "collect_y", axis_titles = "collect")
  }
  relationship_grid <- patchwork::wrap_plots(
    relationship_rows,
    ncol = 1,
    heights = rep(1, length(responses))
  )
  panel_letter <- if (period == "Calibration") "e" else "d"
  metrics <- metric_panel(
    figure_data$overall,
    figure_data$metric_summary,
    figure_data$letters,
    period,
    panel_letter
  )
  combined <- patchwork::wrap_plots(
    list(relationship_grid, metrics),
    nrow = 1,
    widths = c(4.7, 1.45),
    guides = "keep"
  )

  pdf_path <- file.path(figure_dir, paste0(file_stem, ".pdf"))
  png_path <- file.path(figure_dir, paste0(file_stem, ".png"))
  ggplot2::ggsave(
    pdf_path,
    combined,
    device = grDevices::cairo_pdf,
    width = width,
    height = height,
    units = "in"
  )
  ggplot2::ggsave(
    png_path,
    combined,
    device = ragg::agg_png,
    width = width,
    height = height,
    units = "in",
    dpi = 600,
    background = "white"
  )
  c(pdf_path, png_path)
}

regression_lines <- .figure_data[["regression_lines.csv"]]
figure_data <- list(
  relationship = .figure_data[["relationship.csv"]],
  r2_annotation = .figure_data[["figure_R2_annotation.csv"]],
  overall = .figure_data[["figure_overall_metric_distribution.csv"]],
  metric_summary = .figure_data[["figure_metric_summary_by_duration.csv"]],
  letters = .figure_data[["figure_significance_letters.csv"]]
)
responses <- if (period == "Calibration") c("Rs", "EEA", "DOC", "MBC") else c("Rs", "EEA", "DOC")
height <- if (period == "Calibration") 12.6 else 9.8
build_figure(figure_data, period, responses, figure_id, width = 15, height = height)

