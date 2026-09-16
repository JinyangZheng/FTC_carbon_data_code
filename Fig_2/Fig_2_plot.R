# Fig_2 plotting code
# Input tables are stored together in Fig_2_data.rds.
.script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.script_path <- if (length(.script_files)) tail(.script_files, 1L)[[1L]] else if (length(.script_arg)) sub("^--file=", "", .script_arg[[1L]]) else "Fig_2_plot.R"
code_dir <- dirname(normalizePath(.script_path, winslash = "/", mustWork = TRUE))
.figure_data <- readRDS(file.path(code_dir, "Fig_2_data.rds"))
data_dir <- code_dir
output_dir <- file.path(code_dir, "output")
figure_dir <- output_dir
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
.tidy_code_dir <- code_dir
.plot_file_exists <- function(path) basename(path) %in% names(.figure_data) | file.exists(path)
options(stringsAsFactors = FALSE)

figure_id <- "Fig_2"
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
})
regime_levels <- c("LFLT", "LFST", "SFLT", "SFST")
phase_colors <- c(Freeze = "#a0bff6", Thaw = "#e994ae")
text_scale <- 1.2 * 1.1
all_emm <- .figure_data[["Fig2_estimated_marginal_means_95CI.csv"]]
all_points <- .figure_data[["Fig2_independent_jar_level_points.csv"]]
all_emm$regime <- factor(all_emm$regime, levels = regime_levels)
all_points$regime <- factor(all_points$regime, levels = regime_levels)
all_emm$phase <- factor(all_emm$phase, levels = c("Freeze", "Thaw"))
all_points$phase <- factor(all_points$phase, levels = c("Freeze", "Thaw"))
stopifnot(all(all_emm$lower <= all_emm$estimate), all(all_emm$estimate <= all_emm$upper))
emm_a_data <- all_emm[all_emm$panel == "a", ]
rs_auc_points <- all_points[all_points$panel == "a", ]
emm_b_data <- all_emm[all_emm$panel == "b", ]
rs_standardized_points <- all_points[all_points$panel == "b", ]
emm_c_data <- all_emm[all_emm$panel == "c", ]
doc_standardized_points <- all_points[all_points$panel == "c", ]
emm_d_data <- all_emm[all_emm$panel == "d", ]
eea_standardized_points <- all_points[all_points$panel == "d", ]
emm_e_data <- all_emm[all_emm$panel == "e", ]
mbc_standardized_points <- all_points[all_points$panel == "e", ]
emm_f_data <- all_emm[all_emm$panel == "f", ]
cue_standardized_points <- all_points[all_points$panel == "f", ]
theme_fig2 <- function() {
  theme_classic(base_size = 8.2 * text_scale, base_family = "Times New Roman") +
    theme(
      text = element_text(face = "plain"),
      axis.ticks = element_line(linewidth = 0.38, colour = "black"),
      axis.title = element_text(size = 8.2 * text_scale, colour = "black"),
      axis.text = element_text(size = 7.4 * text_scale, colour = "black"),
      plot.title = element_text(size = 9 * text_scale, face = "plain", hjust = 0.5),
      plot.tag = element_text(size = 11 * text_scale, face = "plain"),
      plot.tag.position = c(0.01, 0.99),
      legend.title = element_blank(),
      legend.text = element_text(size = 7.2 * text_scale),
      legend.key.height = grid::unit(3.2, "mm"),
      legend.key.width = grid::unit(4.2, "mm"),
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.55),
      axis.line = element_blank(),
      plot.margin = margin(5, 6, 4, 5)
    )
}

make_y_spec <- function(points, y_breaks) {
  panel_min <- min(points$estimate, na.rm = TRUE)
  panel_max <- max(points$estimate, na.rm = TRUE)
  point_span <- panel_max - panel_min
  if (!is.finite(point_span) || point_span <= 0) {
    stop("Each panel must contain a finite, non-zero range of jar-level estimates")
  }
  axis_span <- point_span / 0.70
  y_limits <- c(
    panel_min - 0.10 * axis_span,
    panel_min + 0.90 * axis_span
  )
  if (length(y_breaks) != 3L || any(abs(diff(y_breaks) - diff(y_breaks)[1]) > 1e-10)) {
    stop("Each panel must have exactly three evenly spaced y-axis breaks")
  }
  if (any(y_breaks <= y_limits[1] | y_breaks >= y_limits[2])) {
    stop("Y-axis breaks must remain inside the panel limits")
  }
  list(
    limits = y_limits,
    breaks = y_breaks,
    letter_y = y_limits[1] + 0.90 * axis_span
  )
}

make_nonphase_panel <- function(
    points, emm_data, title, y_label, y_breaks,
    show_letters = FALSE) {
  y_spec <- make_y_spec(points, y_breaks)
  p <- ggplot() +
    geom_point(
      data = points,
      aes(x = as.numeric(regime), y = estimate),
      position = position_jitter(width = 0.105, height = 0, seed = 20260826),
      size = 1.25, colour = "grey80"
    ) +
    geom_errorbar(
      data = emm_data,
      aes(x = as.numeric(regime), ymin = lower, ymax = upper),
      width = 0.13, linewidth = 0.65, colour = "black"
    ) +
    geom_point(
      data = emm_data,
      aes(x = as.numeric(regime), y = estimate),
      shape = 21, size = 3.0, stroke = 0.7, colour = "black", fill = "black"
    ) +
    scale_x_continuous(breaks = seq_along(regime_levels), labels = regime_levels) +
    scale_y_continuous(
      limits = y_spec$limits,
      breaks = y_spec$breaks,
      expand = expansion(mult = c(0, 0))
    ) +
    labs(x = NULL, y = y_label, title = title) +
    theme_fig2()
  if (show_letters && ".group" %in% names(emm_data)) {
    letter_data <- dplyr::select(emm_data, regime, .group) %>%
      distinct() %>%
      mutate(letter_y = y_spec$letter_y)
    p <- p + geom_text(
      data = letter_data,
      aes(x = as.numeric(regime), y = letter_y, label = .group),
      vjust = 0, size = 2.5 * text_scale, family = "Times New Roman"
    )
  }
  p
}

make_phase_panel <- function(
    points, emm_data, title, y_label, y_breaks,
    show_legend = TRUE) {
  offset <- c(Freeze = -0.16, Thaw = 0.16)
  y_spec <- make_y_spec(points, y_breaks)
  points$x_plot <- as.numeric(points$regime) + offset[as.character(points$phase)]
  emm_data$x_plot <- as.numeric(emm_data$regime) + offset[as.character(emm_data$phase)]
  letter_data <- dplyr::select(emm_data, regime, phase, .group) %>%
    distinct() %>%
    mutate(letter_y = y_spec$letter_y)
  letter_data$x_plot <- as.numeric(letter_data$regime) + offset[as.character(letter_data$phase)]
  ggplot() +
    geom_point(
      data = points,
      aes(x = x_plot, y = estimate, colour = phase),
      position = position_jitter(width = 0.055, height = 0, seed = 20260826),
      size = 1.15, alpha = 0.80
    ) +
    geom_errorbar(
      data = emm_data,
      aes(x = x_plot, ymin = lower, ymax = upper, colour = phase),
      width = 0.10, linewidth = 0.65
    ) +
    geom_point(
      data = emm_data,
      aes(x = x_plot, y = estimate, fill = phase),
      shape = 21, size = 2.8, stroke = 0.7, colour = "black"
    ) +
    scale_colour_manual(values = phase_colors, guide = if (show_legend) "legend" else "none") +
    scale_fill_manual(values = phase_colors, guide = if (show_legend) "legend" else "none") +
    scale_x_continuous(breaks = seq_along(regime_levels), labels = regime_levels) +
    scale_y_continuous(
      limits = y_spec$limits,
      breaks = y_spec$breaks,
      expand = expansion(mult = c(0, 0))
    ) +
    labs(x = NULL, y = y_label, title = title) +
    theme_fig2() +
    theme(legend.position = if (show_legend) c(0.80, 0.52) else "none") +
    geom_text(
      data = letter_data,
      aes(x = x_plot, y = letter_y, label = .group),
      vjust = 0, size = 2.35 * text_scale, family = "Times New Roman", colour = "black"
    )
}

p_a <- make_nonphase_panel(
  rs_auc_points, emm_a_data,
  NULL,
  "Cumulative Rs (mg C g-1 SOC)",
  y_breaks = c(5, 8, 11),
  show_letters = TRUE
)
p_b <- make_phase_panel(
  rs_standardized_points, emm_b_data,
  NULL,
  "Rs (ug C g-1 SOC h-1)",
  y_breaks = c(2, 4, 6),
  show_legend = TRUE
)
p_c <- make_phase_panel(
  doc_standardized_points, emm_c_data,
  NULL,
  "DOC (mg C g-1 SOC)",
  y_breaks = c(21, 22, 23),
  show_legend = FALSE
)
p_d <- make_nonphase_panel(
  eea_standardized_points, emm_d_data,
  NULL,
  "EEA (ug C g-1 SOC h-1)",
  y_breaks = c(0.22, 0.28, 0.34),
  show_letters = TRUE
)
p_e <- make_nonphase_panel(
  mbc_standardized_points, emm_e_data,
  NULL,
  "MBC (mg C g-1 SOC)",
  y_breaks = c(10, 12, 14),
  show_letters = TRUE
)
p_f <- make_nonphase_panel(
  cue_standardized_points, emm_f_data,
  NULL,
  "CUE",
  y_breaks = c(0.1, 0.2, 0.3),
  show_letters = TRUE
)

figure <- ((p_a | p_b) / (p_c | p_d) / (p_e | p_f)) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.background = element_rect(fill = "white", colour = NA)
  )

width_mm <- 183 * 0.9
height_mm <- 190
width_in <- width_mm / 25.4
height_in <- height_mm / 25.4

svglite::svglite(file.path(figure_dir, "Fig_2.svg"), width = width_in, height = height_in)
print(figure)
dev.off()

grDevices::cairo_pdf(file.path(figure_dir, "Fig_2.pdf"), width = width_in, height = height_in, family = "Times New Roman")
print(figure)
dev.off()

ragg::agg_tiff(
  file.path(figure_dir, "Fig_2.tiff"),
  width = width_in, height = height_in, units = "in", res = 600, compression = "lzw"
)
print(figure)
dev.off()

ragg::agg_png(
  file.path(figure_dir, "Fig_2.png"),
  width = width_in, height = height_in, units = "in", res = 600
)
print(figure)
dev.off()

