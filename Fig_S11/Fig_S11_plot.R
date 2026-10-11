library(ggplot2)
library(ragg)

script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_files)) tail(script_files, 1L)[[1L]] else if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else "Fig_S11_plot.R"
code_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
figure_data <- readRDS(file.path(code_dir, "Fig_S11_data.rds"))
output_args <- commandArgs(trailingOnly = TRUE)
output_env <- Sys.getenv("FTC_OUTPUT_DIR")
output_dir <- if (length(output_args)) output_args[[1L]] else if (nzchar(output_env)) output_env else file.path(dirname(dirname(code_dir)), "version2_outputs", "Fig_S11")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

values <- figure_data$values
scope_labels <- c(Depth10 = "0\u201310 cm", Depth70 = "70\u201380 cm")
values$scope_label <- factor(unname(scope_labels[values$scope]), levels = unname(scope_labels))
phase_data <- figure_data$background
group_order <- c("Physical protection", "Microbial physiology", "State variables")
group_colors <- c("Physical protection" = "#F3BECE", "Microbial physiology" = "#A0BFF6", "State variables" = "#AFD1AF")
font_family <- "Times New Roman"
base_panel_theme <- function(base_size = 8) {
  theme_bw(base_size = base_size, base_family = font_family) +
    theme(panel.grid = element_blank(),
      panel.border = element_rect(colour = "#4D4D4D", linewidth = 0.35, fill = NA),
      axis.line = element_blank(), axis.ticks = element_line(linewidth = 0.32, colour = "black"),
      axis.text = element_text(colour = "black"), strip.background = element_blank(),
      panel.spacing.x = grid::unit(0.8, "mm"), plot.background = element_rect(fill = "white", colour = NA))
}
figure <- ggplot2::ggplot(
    values,
    ggplot2::aes(
      x = Day, y = mean,
      colour = group, fill = group, group = group
    )
  ) +
    geom_rect(data = phase_data[phase_data$phase == "Freeze", ], inherit.aes = FALSE,
      aes(xmin = xmin, xmax = xmax, ymin = -6.0, ymax = -2.5),
      fill = "#6A96E1", alpha = 0.62, colour = NA) +
    geom_rect(data = phase_data[phase_data$phase == "Thaw", ], inherit.aes = FALSE,
      aes(xmin = xmin, xmax = xmax, ymin = -6.0, ymax = -2.5),
      fill = "#EEA9BE", alpha = 0.62, colour = NA) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = q025, ymax = q975),
      alpha = 0.25, colour = NA, show.legend = FALSE
    ) +
    ggplot2::geom_line(linewidth = 0.62) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(scope_label),
      cols = ggplot2::vars(regime),
      switch = "y"
    ) +
    ggplot2::scale_colour_manual(
      values = group_colors, breaks = group_order,
      labels = c("Physical protection", "Microbial physiology", "State variables"),
      name = NULL
    ) +
    ggplot2::scale_fill_manual(values = group_colors, guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = c(0, 48, 96), expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 50, 100), expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(
      xlim = c(-5, 101),
      ylim = c(-6, 130)
    ) +
    ggplot2::labs(
      x = "Incubation days",
      y = "Group-level importance (%)"
    ) +
    base_panel_theme(base_size = 8.0) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 7.6),
      axis.title = ggplot2::element_text(size = 9.0),
      strip.text.x = ggplot2::element_text(size = 10.0),
      strip.text.y.left = ggplot2::element_text(
        size = 9.0, angle = 90, hjust = 0.5
      ),
      strip.placement = "outside",
      panel.spacing.x = grid::unit(0, "mm"),
      panel.spacing.y = grid::unit(0, "mm"),
      legend.position = c(0.014, 0.995),
      legend.justification = c(0, 1),
      legend.direction = "vertical",
      legend.background = ggplot2::element_rect(
        fill = grDevices::adjustcolor("white", alpha.f = 0.88),
        colour = NA
      ),
      legend.key.width = grid::unit(5.00, "mm"),
      legend.key.height = grid::unit(2.30, "mm"),
      legend.key.spacing.y = grid::unit(0.7, "mm"),
      legend.margin = ggplot2::margin(1.14, 4, 1.14, 4, unit = "pt"),
      legend.text = ggplot2::element_text(size = 7.4),
      plot.margin = ggplot2::margin(2, 2, 2, 3, unit = "mm")
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        override.aes = list(linewidth = 0.8)
      )
    )

ggsave(file.path(output_dir, "Fig_S11.png"), figure, device = ragg::agg_png,
       width = 183.00, height = 75.00, units = "mm", dpi = 600, bg = "white")
