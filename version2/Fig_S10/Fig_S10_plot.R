library(ggplot2)
library(patchwork)
library(ragg)

script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_files)) tail(script_files, 1L)[[1L]] else if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else "Fig_S10_plot.R"
code_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
figure_data <- readRDS(file.path(code_dir, "Fig_S10_data.rds"))
output_args <- commandArgs(trailingOnly = TRUE)
output_env <- Sys.getenv("FTC_OUTPUT_DIR")
output_dir <- if (length(output_args)) output_args[[1L]] else if (nzchar(output_env)) output_env else file.path(dirname(dirname(code_dir)), "version2_outputs", "Fig_S10")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
original_method_panel <- function(
  plot_data,
  phase_data,
  scope,
  show_column_titles
) {
  selected <- plot_data[
    plot_data$scope == scope,
    ,
    drop = FALSE
  ]
  regimes <- c("LFLT", "LFST", "SFLT", "SFST")
  strip_text <- if (show_column_titles) {
    ggplot2::element_text(
      colour = "black",
      size = 9,
      face = "plain",
      family = "Times New Roman",
      margin = ggplot2::margin(b = 2)
    )
  } else {
    ggplot2::element_blank()
  }
  ggplot2::ggplot(selected, ggplot2::aes(x = day, y = mean_D)) +
    ggplot2::geom_rect(
      data = phase_data,
      mapping = ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = -Inf,
        ymax = Inf,
        fill = phase
      ),
      inherit.aes = FALSE,
      alpha = 0.20,
      colour = NA
    ) +
    ggplot2::scale_fill_manual(
      values = c(Freeze = "#6A96E1", Thaw = "#EEA9BE"),
      guide = "none"
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower_D, ymax = upper_D),
      fill = "grey10",
      alpha = 0.15,
      colour = NA
    ) +
    ggplot2::geom_hline(
      yintercept = 0.5,
      linetype = "dashed",
      linewidth = 0.55,
      colour = "grey80"
    ) +
    ggplot2::geom_line(
      colour = "grey40",
      linewidth = 0.7,
      lineend = "round"
    ) +
    ggplot2::facet_grid(cols = ggplot2::vars(regime)) +
    ggplot2::scale_x_continuous(
      limits = c(0, 96),
      breaks = c(0, 48, 96),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.5, 1),
      labels = c("0.0", "0.5", "1.0"),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      x = "Incubation days",
      y = paste("Dominance index", scope, sep = "\n")
    ) +
    ggplot2::theme_bw(base_family = "Times New Roman", base_size = 8) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.55
      ),
      panel.spacing.x = grid::unit(0.34, "cm"),
      axis.text = ggplot2::element_text(
        colour = "black",
        size = 7.5,
        family = "Times New Roman"
      ),
      axis.title = ggplot2::element_text(
        colour = "black",
        size = 8.5,
        family = "Times New Roman"
      ),
      axis.ticks = ggplot2::element_line(
        colour = "black",
        linewidth = 0.55
      ),
      axis.ticks.length = grid::unit(0.10, "cm"),
      strip.background = ggplot2::element_blank(),
      strip.text.x = strip_text,
      legend.position = "none",
      plot.margin = ggplot2::margin(2, 2, 1, 4)
    )
}

plot_data <- figure_data$values
phase_data <- figure_data$background
  panels <- list(
    original_method_panel(plot_data, phase_data, "All data", TRUE),
    original_method_panel(plot_data, phase_data, "0-10 cm", FALSE),
    original_method_panel(plot_data, phase_data, "70-80 cm", FALSE)
  )
  figure <- patchwork::wrap_plots(
    panels,
    ncol = 1,
    heights = c(1, 1, 1)
  ) +
    patchwork::plot_annotation(tag_levels = "a") &
    ggplot2::theme(
      plot.tag = ggplot2::element_text(
        family = "Times New Roman",
        face = "plain",
        size = 10,
        colour = "black"
      ),
      plot.tag.position = c(0, 1)
    )

ggplot2::ggsave(file.path(output_dir, "Fig_S10.png"), figure, device = ragg::agg_png,
  width = 183.00, height = 121.40, units = "mm", dpi = 600, bg = "white")
