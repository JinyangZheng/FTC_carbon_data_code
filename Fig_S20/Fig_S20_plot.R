# Fig_S20 plotting code
# Input tables are stored together in Fig_S20_data.rds.
.script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.script_path <- if (length(.script_files)) tail(.script_files, 1L)[[1L]] else if (length(.script_arg)) sub("^--file=", "", .script_arg[[1L]]) else "Fig_S20_plot.R"
code_dir <- dirname(normalizePath(.script_path, winslash = "/", mustWork = TRUE))
.figure_data <- readRDS(file.path(code_dir, "Fig_S20_data.rds"))
data_dir <- code_dir
output_dir <- file.path(code_dir, "output")
figure_dir <- output_dir
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
.tidy_code_dir <- code_dir
.plot_file_exists <- function(path) basename(path) %in% names(.figure_data) | file.exists(path)
options(stringsAsFactors = FALSE)

original_method_panel <- function(
  plot_data,
  phase_data,
  scope,
  show_column_titles
) {
  selected <- plot_data[
    plot_data$variant == "legacy_code_exact" & plot_data$scope == scope,
    ,
    drop = FALSE
  ]
  regimes <- c("LFLT", "LFST", "SFLT", "SFST")
  selected$regime <- factor(selected$regime, levels = regimes)
  phase_data$regime <- factor(phase_data$regime, levels = regimes)
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

plot_data <- .figure_data[["FigS15_original_plot_data.rds"]]
phase_data <- .figure_data[["freeze_thaw_intervals.csv"]]
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

ggplot2::ggsave(file.path(output_dir, "Fig_S20.pdf"), figure, device = grDevices::cairo_pdf,
  width = 183, height = 121.4, units = "mm", family = "Times New Roman", bg = "white")
ggplot2::ggsave(file.path(output_dir, "Fig_S20.png"), figure, device = ragg::agg_png,
  width = 183, height = 121.4, units = "mm", dpi = 600, bg = "white")

