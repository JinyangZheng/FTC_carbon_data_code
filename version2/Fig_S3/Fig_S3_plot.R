library(ggplot2)
library(patchwork)
library(ragg)

source_files <- Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
script_argument <- grep('^--file=', commandArgs(FALSE), value = TRUE)
script_path <- if (length(source_files)) tail(source_files, 1L)[[1L]] else
  if (length(script_argument)) sub('^--file=', '', script_argument[1L]) else 'Fig_S3_plot.R'
code_dir <- dirname(normalizePath(script_path, winslash = '/', mustWork = FALSE))
data <- readRDS(file.path(code_dir, 'Fig_S3_data.rds'))

duration_colors <- c(
  LFLT = "#D9B0FF",
  LFST = "#5985D0",
  SFLT = "#E994AE",
  SFST = "#9ED69E"
)
model_levels <- c("Base", "Physio-only", "Physic-only", "Full")
figure_font_multiplier <- 1.50
response_axis_contract <- list(
  Calibration = list(
    Rs = list(
      limits = c(-1.10, 2.10),
      breaks = c(-1, 0, 1, 2),
      x = "Modelled Rs (\u00b5g CO\u2082-C g\u207b\u00b9 SOC h\u207b\u00b9)",
      y = "Observed Rs\n(\u00b5g CO\u2082-C g\u207b\u00b9 SOC h\u207b\u00b9)"
    ),
    EEA = list(
      limits = c(-2.15, -0.40),
      breaks = c(-2.00, -1.50, -1.00, -0.50),
      x = "Modelled EEA (\u00b5g C g\u207b\u00b9 SOC h\u207b\u00b9)",
      y = "Observed EEA\n(\u00b5g C g\u207b\u00b9 SOC h\u207b\u00b9)"
    ),
    DOC = list(
      limits = c(9.68, 10.19),
      breaks = c(9.75, 9.95, 10.15),
      x = "Modelled DOC (\u00b5g DOC-C g\u207b\u00b9 SOC)",
      y = "Observed DOC\n(\u00b5g DOC-C g\u207b\u00b9 SOC)"
    ),
    MBC = list(
      limits = c(8.00, 10.70),
      breaks = c(8.40, 9.40, 10.40),
      x = "Modelled MBC (\u00b5g MBC-C g\u207b\u00b9 SOC)",
      y = "Observed MBC\n(\u00b5g MBC-C g\u207b\u00b9 SOC)"
    )
  ),
  Validation = list(
    Rs = list(
      limits = c(-6.00, 2.20),
      breaks = c(-5, -2, 1),
      x = "Modelled Rs (\u00b5g CO\u2082-C g\u207b\u00b9 SOC h\u207b\u00b9)",
      y = "Observed Rs\n(\u00b5g CO\u2082-C g\u207b\u00b9 SOC h\u207b\u00b9)"
    ),
    EEA = list(
      limits = c(-4.90, -0.20),
      breaks = c(-4.50, -2.50, -0.50),
      x = "Modelled EEA (\u00b5g C g\u207b\u00b9 SOC h\u207b\u00b9)",
      y = "Observed EEA\n(\u00b5g C g\u207b\u00b9 SOC h\u207b\u00b9)"
    ),
    DOC = list(
      limits = c(9.05, 10.20),
      breaks = c(9.10, 9.60, 10.10),
      x = "Modelled DOC (\u00b5g DOC-C g\u207b\u00b9 SOC)",
      y = "Observed DOC\n(\u00b5g DOC-C g\u207b\u00b9 SOC)"
    )
  )
)

base_figure_theme <- function(
    axis_text_size = 11 * figure_font_multiplier,
    axis_title_size = 13 * figure_font_multiplier
) {
  ggplot2::theme_bw(base_size = axis_text_size, base_family = "Times New Roman") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.60),
      axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.60),
      axis.ticks.length = grid::unit(3, "pt"),
      axis.title = ggplot2::element_text(color = "black", size = axis_title_size),
      axis.text = ggplot2::element_text(color = "black", size = axis_text_size),
      plot.title = ggplot2::element_text(
        face = "plain",
        hjust = 0.50,
        size = 14 * figure_font_multiplier
      ),
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank()
    )
}

metric_settings <- list(Calibration_RMSE = list(limits = c(-0.02, 0.36), mean_y = c(LFLT = 0.34, 
LFST = 0.32, SFLT = 0.30, SFST = 0.28), letter_y = 0.00), 
    Calibration_AIC = list(limits = c(-228.24, 18.52), mean_y = c(LFLT = -174.93, 
    LFST = -188.64, SFLT = -202.35, SFST = -216.06), 
        letter_y = 9.38), Calibration_BIC = list(limits = c(-209.74, 
    15.75), mean_y = c(LFLT = -161.02, LFST = -173.55, 
    SFLT = -186.08, SFST = -198.60), letter_y = 7.40), 
    Validation_RMSE = list(limits = c(-0.14, 2.01
    ), mean_y = c(LFLT = 1.91, LFST = 1.79, 
    SFLT = 1.67, SFST = 1.55), letter_y = -0.06), 
    Validation_AIC = list(limits = c(-69.38, 81.48
    ), mean_y = c(LFLT = 74.03, LFST = 65.65, 
    SFLT = 57.27, SFST = 48.89), letter_y = -63.79), 
    Validation_BIC = list(limits = c(-66.27, 86.89
    ), mean_y = c(LFLT = 79.32, LFST = 70.82, 
    SFLT = 62.31, SFST = 53.80), letter_y = -60.60))

legend_order <- c('LFLT', 'SFLT', 'LFST', 'SFST')
relationship_panel <- function(period, response, model, row_index, tag) {
  d <- data$relationship[[paste(period, response, model, sep = '_')]]
  axis <- response_axis_contract[[period]][[response]]
  annotation <- d$annotations
  span <- diff(axis$limits)
  duration_index <- match(annotation$duration, names(duration_colors))
  bottom_labels <- period == 'Validation' && response %in% c('Rs', 'EEA')
  annotation$x <- axis$limits[1L] + if (bottom_labels) 0.38 * span else 0.04 * span
  annotation$y <- if (bottom_labels) axis$limits[1L] + c(0.37, 0.28, 0.19, 0.10)[duration_index] * span else
    axis$limits[2L] - c(0.06, 0.14, 0.23, 0.32)[duration_index] * span
  annotation$label <- paste0("R^2==plain('", sprintf('%.2f', annotation$mean_R2),
    "')%+-%plain('", sprintf('%.2f', annotation$sd_R2), "')")
  show_legend <- period == 'Calibration' && row_index == 1L && model == model_levels[1L]
  panel <- ggplot() +
    geom_blank(data = data.frame(x = axis$limits, y = axis$limits), aes(x, y)) +
    geom_abline(intercept = 0, slope = 1, color = '#D4372B', linewidth = 0.80) +
    geom_point(data = d$points, aes(x, y, color = duration), size = 1.50, alpha = 0.45,
      show.legend = show_legend) +
    geom_line(data = d$lines, aes(x, y, color = duration, group = group), linewidth = 0.75,
      show.legend = FALSE) +
    geom_text(data = annotation, aes(x, y, color = duration, label = label),
      hjust = 0, vjust = 1, size = 3.00 * figure_font_multiplier,
      family = 'Times New Roman', parse = TRUE, show.legend = FALSE) +
    scale_colour_manual(values = duration_colors, breaks = legend_order) +
    scale_x_continuous(breaks = axis$breaks) +
    scale_y_continuous(breaks = axis$breaks) +
    coord_cartesian(xlim = axis$limits, ylim = axis$limits, clip = 'off') +
    labs(title = if (row_index == 1L) paste0('\u201c', model, '\u201d') else NULL,
      x = axis$x, y = axis$y, color = NULL) + base_figure_theme() +
    guides(colour = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(alpha = 0.60, size = 2.20))) +
    theme(axis.title.y = element_text(lineheight = 0.92, margin = margin(r = 5)),
      axis.title.x = element_text(margin = margin(t = 3)), plot.margin = margin(4, 0, 2, 0),
      legend.title = element_blank(), legend.text = element_text(size = 15),
      legend.key.width = grid::unit(8.00, 'pt'), legend.key.height = grid::unit(8.00, 'pt'),
      legend.spacing.x = grid::unit(0, 'pt'), legend.margin = margin(0, 0, 0, 0))
  if (row_index == 1L && model == model_levels[1L]) {
    panel <- panel + labs(tag = tag) + theme(
      plot.tag = element_text(family = 'Times New Roman', size = 27.50, face = 'plain'),
      plot.tag.position = c(0, 1))
  }
  panel
}

relationship_grid <- function(period, responses, tag) {
  rows <- lapply(seq_along(responses), function(i) {
    panels <- lapply(model_levels, function(model) relationship_panel(period, responses[i], model, i, tag))
    wrap_plots(panels, nrow = 1) + plot_layout(axes = 'collect_y', axis_titles = 'collect')
  })
  wrap_plots(rows, ncol = 1, heights = rep(1, length(responses)))
}
metric_ticks <- list(Calibration = list(RMSE = c(0, 0.10, 0.20), AIC = c(-200, -100, 0),
  BIC = c(-200, -100, 0)), Validation = list(RMSE = c(0, 1, 2), AIC = c(-60, 0, 60),
  BIC = c(-60, 0, 60)))
metric_panel <- function(period, metric) {
  key <- paste(period, metric, sep = '_')
  d <- data$metric[[key]]
  settings <- metric_settings[[key]]
  means <- d$means
  means$x <- match(means$model, model_levels)
  means$y <- unname(settings$mean_y[means$duration])
  means$label <- if (metric == 'RMSE') sprintf('%.2f', means$mean) else sprintf('%.0f', means$mean)
  letters <- d$letters
  letters$x <- match(letters$model, model_levels)
  letters$y <- settings$letter_y
  show_model_labels <- metric == 'BIC'
  ggplot() +
    geom_polygon(data = d$violin, aes(x, y, group = group),
      color = 'black', fill = NA, linewidth = 0.55, alpha = 1) +
    geom_point(data = d$sina, aes(x, y, color = duration), size = 0.95, shape = 20, alpha = 0.08,
      show.legend = FALSE) +
    geom_boxplot(data = d$box, aes(x = x, ymin = ymin, lower = lower, middle = middle,
      upper = upper, ymax = ymax, group = group), stat = 'identity', width = 0.10,
      outlier.shape = NA, alpha = 0.70, color = 'black', fill = NA, linewidth = 0.45) +
    geom_text(data = means, aes(x, y, color = duration, label = label),
      hjust = 0.50, vjust = 0.50, family = 'Times New Roman',
      size = 2.65 * figure_font_multiplier, show.legend = FALSE) +
    geom_text(data = letters, aes(x, y, label = letter), hjust = 0.50, vjust = 0.50,
      family = 'Times New Roman', size = 3.20 * figure_font_multiplier, color = 'black') +
    scale_colour_manual(values = duration_colors, breaks = legend_order) +
    scale_x_continuous(breaks = seq_along(model_levels),
      labels = c('\u201cBase\u201d', '\u201cPhysio\u201d', '\u201cPhysic\u201d', '\u201cFull\u201d'),
      limits = c(0.55, 4.55), expand = c(0, 0)) +
    scale_y_continuous(breaks = metric_ticks[[period]][[metric]], expand = c(0, 0)) +
    coord_cartesian(ylim = settings$limits, clip = 'on') +
    labs(title = NULL, x = NULL, y = metric, color = NULL) +
    base_figure_theme(axis_text_size = 10 * figure_font_multiplier,
      axis_title_size = 12 * figure_font_multiplier) +
    theme(axis.title.y = element_text(margin = margin(r = 4)),
      axis.text.x = if (show_model_labels) element_text(angle = 0, hjust = 0.50,
        size = 9.50 * figure_font_multiplier) else element_blank(),
      axis.ticks.x = if (show_model_labels) element_line(linewidth = 0.60) else element_blank(),
      plot.margin = margin(0, 0, 0, 0))
}
metric_stack <- function(period) wrap_plots(lapply(c('RMSE', 'AIC', 'BIC'),
  function(metric) metric_panel(period, metric)), ncol = 1, heights = c(1, 1, 1))

figure_plot <- wrap_plots(
  A = relationship_grid('Calibration', c('Rs', 'EEA', 'DOC', 'MBC'), 'a'),
  B = metric_stack('Calibration'), C = guide_area(),
  D = relationship_grid('Validation', c('Rs', 'EEA', 'DOC'), 'b'), E = metric_stack('Validation'),
  design = 'AB\n#C\nDE', widths = c(4.70, 1.45), heights = c(4, 0.18, 3), guides = 'collect') &
  theme(legend.position = 'bottom')

arguments <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(arguments)) arguments[1L] else Sys.getenv('FTC_OUTPUT_DIR',
  unset = file.path(dirname(dirname(code_dir)), 'version2_outputs', 'Fig_S3'))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(output_dir, 'Fig_S3.png'), figure_plot, device = ragg::agg_png,
  width = 15.00, height = 19.70, units = 'in', dpi = 300, bg = 'white')
