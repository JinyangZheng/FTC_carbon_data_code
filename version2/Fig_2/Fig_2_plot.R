library(ggplot2)
library(patchwork)
library(ragg)

source_files <- Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(source_files)) tail(source_files, 1L)[[1L]] else
  if (length(script_argument)) sub("^--file=", "", script_argument[1L]) else "Fig_2_plot.R"
data_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
data <- readRDS(file.path(data_dir, "Fig_2_data.rds"))

model_levels <- c("Base", "Physio-only", "Physic-only", "Full")
duration_levels <- c("LFLT", "LFST", "SFLT", "SFST")
response_levels <- c("Rs", "EEA", "DOC", "MBC")
model_colors <- c(Base = "#e4c8ff", `Physio-only` = "#8baade",
  `Physic-only` = "#f0b4c6", Full = "#9ed69e")
font_scale <- 1.5
response_labels <- c(Rs = "Rs\n(\u00b5g C g\u207b\u00b9 SOC h\u207b\u00b9)",
  EEA = "EEA\n(\u00b5g C g\u207b\u00b9 SOC h\u207b\u00b9)",
  DOC = "DOC\n(mg C g\u207b\u00b9 SOC)", MBC = "MBC\n(mg C g\u207b\u00b9 SOC)")
axis_settings <- list(
  Rs = list(limits = c(0, 8.2), breaks = c(1, 4, 7)),
  EEA = list(limits = c(-0.35, 1.75), breaks = c(0, 0.7, 1.4)),
  DOC = list(limits = c(4.5, 25.5), breaks = c(8, 15, 22)),
  MBC = list(limits = c(0.5, 21.5), breaks = c(4, 11, 18)))
make_panel <- function(response, duration, row, column) {
  curves <- data$curves[data$curves$response == response & data$curves$duration == duration, ]
  points <- data$observations[data$observations$response == response & data$observations$duration == duration, ]
  axis <- axis_settings[[response]]
  plot <- ggplot(curves, aes(Day, display_mean, colour = model, fill = model, group = model)) +
    geom_ribbon(aes(ymin = display_lower, ymax = display_upper), alpha = 0.25,
      colour = NA, show.legend = FALSE) +
    geom_vline(xintercept = 192, linetype = "dashed", colour = "grey60", linewidth = 0.42) +
    geom_line(linewidth = 0.38, show.legend = FALSE) +
    geom_point(data = points, aes(Day, display_observed), inherit.aes = FALSE,
      shape = 1, size = 1.45, stroke = 0.48, colour = "grey40", alpha = 0.65) +
    scale_colour_manual(values = model_colors, guide = "none") +
    scale_fill_manual(values = model_colors, guide = "none", drop = FALSE) +
    scale_x_continuous(breaks = c(0, 96, 192), limits = c(-12, 252), expand = c(0, 0)) +
    scale_y_continuous(breaks = axis$breaks, limits = axis$limits, expand = c(0, 0)) +
    labs(title = if (row == 1L) duration else NULL, x = NULL,
      y = if (column == 1L) response_labels[[response]] else NULL) +
    theme_bw(base_size = 6.1 * font_scale, base_family = "Times New Roman") +
    theme(panel.grid = element_blank(), panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.38),
      axis.line = element_blank(), axis.ticks = element_line(colour = "black", linewidth = 0.32),
      axis.ticks.length = grid::unit(1.2, "mm"), axis.text = element_text(colour = "black", size = 6 * font_scale),
      axis.title.y = element_text(colour = "black", size = 6.8 * font_scale, lineheight = 0.92,
        margin = margin(r = 3.5)), plot.title = element_text(colour = "black", size = 8.2 * font_scale,
        hjust = 0.5, margin = margin(b = 1.5)), plot.margin = margin(0, 0, 0, 0), legend.position = "none")
  if (row < 4L) plot <- plot + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  if (column > 1L) plot <- plot + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  plot
}
legend_models <- data.frame(model = factor(model_levels, levels = model_levels),
  x = c(59.96333, 79.66917, 112.46306, 145.01000), label = paste0("\u201c", model_levels, "\u201d"))
legend <- ggplot() +
  annotate("point", x = 23.8, y = 0.5, shape = 1, size = 2, stroke = 0.55, colour = "grey40") +
  annotate("text", x = 26.7, y = 0.5, label = "Observation data", hjust = 0,
    family = "Times New Roman", size = 3 * font_scale) +
  geom_segment(data = legend_models, aes(x = x, xend = x + 3, y = 0.5, yend = 0.5,
    colour = model), linewidth = 0.65, show.legend = FALSE) +
  geom_text(data = legend_models, aes(x = x + 4.4, y = 0.5, label = label), hjust = 0,
    family = "Times New Roman", size = 3 * font_scale, colour = "black") +
  scale_colour_manual(values = model_colors, drop = FALSE) +
  coord_cartesian(xlim = c(0, 183), ylim = c(0, 1), expand = FALSE, clip = "off") +
  theme_void(base_family = "Times New Roman") + theme(plot.margin = margin(0, 1, 0, 1))
x_label <- ggplot() + annotate("text", x = 0.5, y = 0.55, label = "Incubation days",
  family = "Times New Roman", size = 3 * font_scale) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") + theme_void()
panels <- lapply(seq_along(response_levels), function(row) lapply(seq_along(duration_levels),
  function(column) make_panel(response_levels[row], duration_levels[column], row, column)))
panel_grid <- wrap_plots(unlist(panels, recursive = FALSE), ncol = 4, byrow = TRUE)
figure <- wrap_elements(full = legend) / panel_grid / x_label + plot_layout(heights = c(0.082, 1, 0.045))
arguments <- commandArgs(TRUE)
output_dir <- if (length(arguments)) arguments[1L] else Sys.getenv("FTC_OUTPUT_DIR",
  unset = file.path(dirname(dirname(data_dir)), "version2_outputs", "Fig_2"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(output_dir, "Fig_2.png"), figure, device = ragg::agg_png,
  width = 7.20, height = 6.35, units = "in", dpi = 300, bg = "white")
