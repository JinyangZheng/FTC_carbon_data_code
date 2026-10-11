library(ggplot2)
library(ragg)

script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_files)) tail(script_files, 1L)[[1L]] else if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else "Fig_S8_plot.R"
code_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
figure_data <- readRDS(file.path(code_dir, "Fig_S8_data.rds"))
output_args <- commandArgs(trailingOnly = TRUE)
output_env <- Sys.getenv("FTC_OUTPUT_DIR")
output_dir <- if (length(output_args)) output_args[[1L]] else if (nzchar(output_env)) output_env else file.path(dirname(dirname(code_dir)), "version2_outputs", "Fig_S8")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

values <- figure_data$values
predictor_order <- c("f_iso", "b_D", "b_S", "k_D,sor", "k_D,des", "k_S,des", "k_S,sor", "f_dorm", "f_inact", "r_frz", "v_upt", "v_cat", "e", "r_prod", "r_death", "DOC", "MBC", "SOC", "ENZ")
predictor_labels <- c("italic(f[iso])", "italic(b[D])", "italic(b[S])", "italic(k[D*','*sor])", "italic(k[D*','*des])", "italic(k[S*','*des])", "italic(k[S*','*sor])", "italic(f[dorm])", "italic(f[inact])", "italic(r[frz])", "italic(v[upt])", "italic(v[cat])", "italic(e)", "italic(r[prod])", "italic(r[death])", "italic(DOC)", "italic(MBC)", "italic(SOC)", "italic(ENZ)")
values$predictor_label <- factor(predictor_labels[match(values$predictor, predictor_order)], levels = predictor_labels)
phase_data <- figure_data$background
legend_order <- c("Overall", "Depth10", "Depth70")
scope_colors <- c(Overall = "#000000", Depth10 = "#d9ae84", Depth70 = "#73442e")
scope_labels <- c(Overall = "Overall", Depth10 = "0\u201310 cm", Depth70 = "70\u201380 cm")
draw_order <- c("Overall", "Depth70", "Depth10")
phase_colors <- c(Freeze = "#6A96E1", Thaw = "#EEA9BE")
phase_layers <- lapply(names(phase_colors), function(phase) {
  ggplot2::geom_rect(
    data = phase_data[phase_data$phase == phase, ], inherit.aes = FALSE,
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = phase_colors[[phase]], alpha = 0.20, colour = NA
  )
})
font_family <- "Times New Roman"
scope_layers <- unlist(lapply(draw_order, function(scope) {
  scope_values <- values[as.character(values$scope) == scope, , drop = FALSE]
  list(
    ggplot2::geom_ribbon(data = scope_values,
                         ggplot2::aes(ymin = q025, ymax = q975, fill = scope),
                         alpha = 0.15, colour = NA, show.legend = FALSE),
    ggplot2::geom_line(data = scope_values, ggplot2::aes(colour = scope),
                       linewidth = 0.34, alpha = 0.70)
  )
}), recursive = FALSE)
combined_plot <- ggplot2::ggplot(values, ggplot2::aes(x = plot_day, y = mean, group = scope)) +
  phase_layers +
  ggplot2::geom_hline(yintercept = 20, colour = "#B8B8B8", linetype = "dashed", linewidth = 0.28) +
  scope_layers +
  ggplot2::scale_colour_manual(values = scope_colors, breaks = legend_order,
                               labels = scope_labels[legend_order], name = NULL) +
  ggplot2::scale_fill_manual(values = scope_colors, guide = "none") +
  ggplot2::facet_grid(rows = ggplot2::vars(predictor_label), cols = ggplot2::vars(regime),
                      labeller = ggplot2::labeller(predictor_label = ggplot2::label_parsed),
                      switch = "y", drop = FALSE) +
  ggplot2::scale_x_continuous(breaks = c(0, 48, 96), expand = c(0, 0)) +
  ggplot2::scale_y_continuous(breaks = c(0, 20, 40), expand = c(0, 0)) +
  ggplot2::coord_cartesian(xlim = c(-5, 101), ylim = c(0, 60)) +
  ggplot2::labs(x = "Incubation days", y = "Relative importance (%)") +
  ggplot2::theme_bw(base_size = 12.2, base_family = font_family) +
  ggplot2::theme(
    text = ggplot2::element_text(face = "plain"),
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "#4D4D4D", linewidth = 0.35, fill = NA),
    axis.line = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_line(linewidth = 0.32, colour = "black"),
    axis.text = ggplot2::element_text(size = 11.4, colour = "black", face = "plain"),
    axis.title = ggplot2::element_text(size = 14.4, face = "plain"),
    axis.title.x = ggplot2::element_text(size = 16, face = "plain"),
    strip.background = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_text(size = 14.4, face = "plain"),
    strip.text.y.left = ggplot2::element_text(size = 16, face = "italic", angle = 0, hjust = 1),
    strip.placement = "outside",
    panel.spacing.x = grid::unit(0.8, "mm"),
    panel.spacing.y = grid::unit(0.65, "mm"),
    legend.position = "top", legend.justification = "center",
    legend.text = ggplot2::element_text(size = 12.8, face = "plain"),
    legend.key.width = grid::unit(9.00, "mm"),
    legend.key.height = grid::unit(4.50, "mm"),
    legend.margin = ggplot2::margin(0, 0, 0, 0),
    legend.box.spacing = grid::unit(1, "mm"),
    plot.margin = ggplot2::margin(3, 3, 3, 3, unit = "mm"),
    plot.background = ggplot2::element_rect(fill = "white", colour = NA)
  ) +
  ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1,
                                                override.aes = list(linewidth = 0.65, alpha = 0.70)))


ggsave(file.path(output_dir, "Fig_S8.png"), combined_plot, device = ragg::agg_png,
       width = 210.00, height = 310.00, units = "mm", dpi = 600, bg = "white")
