# Fig_S22 plotting code
# Input tables are stored together in Fig_S22_data.rds.
.script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.script_path <- if (length(.script_files)) tail(.script_files, 1L)[[1L]] else if (length(.script_arg)) sub("^--file=", "", .script_arg[[1L]]) else "Fig_S22_plot.R"
code_dir <- dirname(normalizePath(.script_path, winslash = "/", mustWork = TRUE))
.figure_data <- readRDS(file.path(code_dir, "Fig_S22_data.rds"))
data_dir <- code_dir
output_dir <- file.path(code_dir, "output")
figure_dir <- output_dir
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
.tidy_code_dir <- code_dir
.plot_file_exists <- function(path) basename(path) %in% names(.figure_data) | file.exists(path)
options(stringsAsFactors = FALSE)

figure_dir <- output_dir
required_packages <- c("data.table", "ggplot2", "ragg")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing R packages  ", paste(missing_packages, collapse = ", "))
}

summary_data <- data.table::copy(.figure_data[["process_effect_summary_mean_95CrI.csv.gz"]])
soil_order <- c(
  "2400_0_10", "2400_50_100",
  "3400_0_10", "3400_50_100",
  "4600_0_10", "4600_50_100"
)
soil_labels <- c(
  "2400 m  0-10 cm", "2400 m  50-100 cm",
  "3400 m  0-10 cm", "3400 m  50-100 cm",
  "4600 m  0-10 cm", "4600 m  50-100 cm"
)
duration_order <- c("LFLT", "LFST", "SFLT", "SFST")
effect_order <- c("physio", "physical", "interaction")
effect_labels <- c(
  physio = "Physiological",
  physical = "Physical",
  interaction = "Interaction"
)
effect_colours <- c(
  physio = "#0072B2",
  physical = "#D55E00",
  interaction = "#000000"
)
phase_colours <- c(Freeze = "#6A96E1", Thaw = "#EEA9BE")

make_phase_intervals <- function() {
  specifications <- list(
    LFLT = c(Freeze = 12, Thaw = 12),
    LFST = c(Freeze = 12, Thaw = 4),
    SFLT = c(Freeze = 4, Thaw = 12),
    SFST = c(Freeze = 4, Thaw = 4)
  )
  output <- lapply(names(specifications), function(duration) {
    widths <- specifications[[duration]]
    rows <- list()
    position <- 0
    index <- 1L
    phase_index <- 1L
    while (position < 96) {
      phase <- names(widths)[phase_index]
      next_position <- min(96, position + unname(widths[phase_index]))
      rows[[index]] <- data.frame(
        duration = duration,
        xmin = position,
        xmax = next_position,
        phase = phase,
        stringsAsFactors = FALSE
      )
      position <- next_position
      index <- index + 1L
      phase_index <- if (phase_index == 1L) 2L else 1L
    }
    do.call(rbind, rows)
  })
  do.call(rbind, output)
}

phase_data <- make_phase_intervals()

summary_data[, soil_label := factor(
  soil,
  levels = soil_order,
  labels = soil_labels
)]
summary_data[, duration := factor(duration, levels = duration_order)]
summary_data[, effect := factor(effect, levels = effect_order)]
phase_data$duration <- factor(phase_data$duration, levels = duration_order)
phase_data$phase <- factor(phase_data$phase, levels = c("Freeze", "Thaw"))

theme_process_grid <- function() {
  ggplot2::theme_bw(base_size = 16, base_family = "Times New Roman") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        colour = "black", linewidth = 0.35, fill = NA
      ),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(linewidth = 0.3),
      axis.title = ggplot2::element_text(size = 17),
      axis.text = ggplot2::element_text(size = 14, colour = "black"),
      strip.text.x = ggplot2::element_text(size = 16, face = "bold"),
      strip.text.y = ggplot2::element_text(size = 15, face = "bold"),
      strip.background = ggplot2::element_blank(),
      panel.spacing = grid::unit(2.4, "mm"),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = ggplot2::element_text(size = 15, face = "bold"),
      legend.text = ggplot2::element_text(size = 14),
      plot.margin = ggplot2::margin(5, 7, 5, 5)
    )
}

make_row_limit_points <- function(plot_data) {
  row_ranges <- plot_data[, .(
    data_min = min(lower, 0, na.rm = TRUE),
    data_max = max(upper, 0, na.rm = TRUE)
  ), by = soil_label]
  limits <- lapply(seq_len(nrow(row_ranges)), function(index) {
    lower <- row_ranges$data_min[index]
    upper <- row_ranges$data_max[index]
    core_breaks <- pretty(c(lower, upper), n = 5)
    core_lower <- max(core_breaks[core_breaks <= lower])
    core_upper <- min(core_breaks[core_breaks >= upper])
    core_span <- core_upper - core_lower
    if (!is.finite(core_span) || core_span <= 0) core_span <- 1
    data.frame(
      soil_label = row_ranges$soil_label[index],
      y = c(core_lower - 0.05 * core_span, core_upper + 0.05 * core_span)
    )
  })
  output <- do.call(rbind, limits)
  output$soil_label <- factor(output$soil_label, levels = soil_labels)
  output
}

make_process_plot <- function(response) {
  target_response <- response
  plot_data <- summary_data[summary_data$response == target_response, ]
  row_limit_points <- make_row_limit_points(plot_data)
  y_label <- if (response == "R") {
    expression(paste("Effect on instantaneous ", R[s],
                     "  (", mu, "g CO"[2], "-C g"^{-1},
                     " SOC h"^{-1}, ")"))
  } else {
    expression(paste("Effect on cumulative mineralization  (", mu,
                     "g CO"[2], "-C g"^{-1}, " SOC)"))
  }
  ggplot2::ggplot(plot_data, ggplot2::aes(x = Day)) +
    ggplot2::geom_blank(
      data = row_limit_points,
      ggplot2::aes(x = 48, y = y),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_rect(
      data = phase_data,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
                   fill = phase),
      inherit.aes = FALSE,
      colour = NA,
      alpha = 0.20
    ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.25, colour = "#555555") +
    ggplot2::geom_ribbon(
      data = plot_data[plot_data$effect == "physio", ],
      ggplot2::aes(ymin = lower, ymax = upper),
      alpha = 0.16,
      fill = effect_colours[["physio"]],
      colour = NA,
      show.legend = FALSE
    ) +
    ggplot2::geom_ribbon(
      data = plot_data[plot_data$effect == "physical", ],
      ggplot2::aes(ymin = lower, ymax = upper),
      alpha = 0.16,
      fill = effect_colours[["physical"]],
      colour = NA,
      show.legend = FALSE
    ) +
    ggplot2::geom_ribbon(
      data = plot_data[plot_data$effect == "interaction", ],
      ggplot2::aes(ymin = lower, ymax = upper),
      alpha = 0.30,
      fill = effect_colours[["interaction"]],
      colour = NA,
      show.legend = FALSE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = mean, colour = effect),
      linewidth = 0.38
    ) +
    ggplot2::facet_grid(soil_label ~ duration, scales = "free_y") +
    ggplot2::scale_x_continuous(
      breaks = c(0, 48, 96),
      limits = c(0, 96),
      expand = ggplot2::expansion(mult = c(0.025, 0.025))
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_colour_manual(
      values = effect_colours,
      labels = effect_labels,
      name = "Process effect"
    ) +
    ggplot2::scale_fill_manual(
      values = phase_colours,
      name = "Temperature phase"
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(order = 1, nrow = 1),
      fill = ggplot2::guide_legend(
        order = 2, nrow = 1, override.aes = list(alpha = 0.20)
      )
    ) +
    ggplot2::labs(
      x = "Day",
      y = y_label
    ) +
    theme_process_grid()
}

save_plot <- function(plot, stem, width_mm = 360, height_mm = 410,
                      dpi = 300) {
  grDevices::cairo_pdf(
    file.path(figure_dir, paste0(stem, ".pdf")),
    width = width_mm / 25.4,
    height = height_mm / 25.4,
    family = "Times New Roman"
  )
  print(plot)
  grDevices::dev.off()
  ragg::agg_png(
    file.path(figure_dir, paste0(stem, ".png")),
    width = width_mm,
    height = height_mm,
    units = "mm",
    res = dpi,
    background = "white"
  )
  print(plot)
  grDevices::dev.off()
}

figure <- make_process_plot("C")
save_plot(figure, "Fig_S22")

