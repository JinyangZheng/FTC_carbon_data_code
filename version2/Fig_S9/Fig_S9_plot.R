library(ggplot2)
library(patchwork)
library(ragg)

.script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.script_path <- if (length(.script_files)) tail(.script_files, 1L)[[1L]] else if (length(.script_arg)) sub("^--file=", "", .script_arg[[1L]]) else "Fig_S9_plot.R"
code_dir <- dirname(normalizePath(.script_path, winslash = "/", mustWork = TRUE))
b <- readRDS(file.path(code_dir, "Fig_S9_data.rds"))
.args <- commandArgs(trailingOnly = TRUE)
.output_env <- Sys.getenv("FTC_OUTPUT_DIR", unset = "")
output_dir <- if (length(.args)) .args[[1L]] else if (nzchar(.output_env)) .output_env else file.path(dirname(dirname(code_dir)), "version2_outputs", "Fig_S9")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

font_family <- "Times New Roman"
groups <- c("Overall", "Depth10", "Depth70", "LFLT", "LFST", "SFLT", "SFST", "Freeze", "Thaw")
group_labels <- c("All data", "0\u201310 cm", "70\u201380 cm", "LFLT", "LFST", "SFLT", "SFST", "Freeze", "Thaw")
components <- c("Physical_main", "Physiology_main", "Carbon_pool_main", "Physical_x_Physiology", "Physical_x_Carbon_pool", "Physiology_x_Carbon_pool")
component_labels <- c("Physical protection", "Microbial physiology", "State variables",
  "Physical protection \u00d7 Microbial physiology", "Physical protection \u00d7 State variables", "Microbial physiology \u00d7 State variables")
palette <- setNames(c("#E994AE", "#A0BFF6", "#AFD1AF", "#688EA9", "#A695B1", "#A9B783"), components)
cumulative_components <- c("Physical_main", "Physiology_main", "Initial_main", "Physical_x_Physiology", "Physical_x_Initial", "Physiology_x_Initial")
cumulative_palette <- setNames(unname(palette), cumulative_components)
theme_figure <- theme_bw(base_size = 11.5, base_family = font_family) +
  theme(panel.grid = element_blank(), panel.border = element_rect(colour = "grey35", linewidth = 0.35),
    axis.text = element_text(colour = "black"), axis.title = element_text(size = 13),
    strip.background = element_blank(), strip.text = element_text(size = 13.75),
    panel.spacing.x = grid::unit(7, "pt"), legend.title = element_blank(),
    legend.text = element_text(size = 11.875), legend.key.height = grid::unit(10.00, "pt"),
    legend.spacing.y = grid::unit(1, "pt"), plot.tag = element_text(size = 18, face = "plain"),
    plot.tag.location = "margin", plot.tag.position = "topleft", plot.margin = margin(5, 4, 3, 4))
d <- b$cumulative
d$x <- match(d$group, groups) + seq(-0.35, 0.35, length.out = 6)[match(d$component, cumulative_components)]
d$component <- factor(d$component, levels = cumulative_components)
label_offsets <- c(2, 5, 8, 2, 5, 8)
panel_a <- ggplot(d, aes(x = x, y = mean, fill = component)) +
  geom_vline(xintercept = c(1.5, 3.5, 7.5), linewidth = 0.6) +
  geom_vline(xintercept = c(2.5, 4.5, 5.5, 6.5, 8.5), linewidth = 0.3, linetype = "dashed", colour = "grey60") +
  geom_col(width = 0.09) + geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.00, linewidth = 0.35) +
  geom_text(aes(y = q975 + label_offsets[as.integer(component)], label = sprintf("%.0f", mean)),
    family = font_family, size = 10 / .pt, vjust = 0) +
  annotate("text", x = c(1, 2.5, 5.5, 8.5), y = Inf,
    label = c("Overall", "Soil depth", "FTC regimes", "Freeze vs. Thaw phases"), family = font_family, size = 13.75 / .pt, vjust = -1.1) +
  annotate("text", x = 1:9, y = Inf, label = group_labels, family = font_family, size = 11.25 / .pt, vjust = -0.1) +
  scale_x_continuous(limits = c(0.5, 9.5), breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0, 25, 50, 75), expand = c(0, 0)) +
  scale_fill_manual(values = cumulative_palette, limits = cumulative_components, labels = component_labels, drop = FALSE) +
  coord_cartesian(ylim = c(0, 75), clip = "off") + labs(x = NULL, y = "Group-level importance (%)", tag = "a") +
  theme_figure + theme(plot.margin = margin(34, 4, 3, 4), legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2, byrow = FALSE))
regimes <- c("LFLT", "LFST", "SFLT", "SFST")
make_temporal <- function(component_subset, upper, title, tag, show_x) {
  d <- b$temporal[b$temporal$component %in% component_subset, ]
  d$regime <- factor(d$regime, levels = regimes)
  d$component <- factor(d$component, levels = components)
  phase <- b$phases
  phase$regime <- factor(phase$regime, levels = regimes)
  p <- ggplot(d, aes(x = Day, y = mean, colour = component, group = component)) +
    geom_rect(data = phase, aes(xmin = xmin, xmax = xmax, ymin = -5.6 * upper / 70, ymax = -2.5 * upper / 70, fill = phase), inherit.aes = FALSE, colour = NA) +
    scale_fill_manual(values = c("Freeze" = "#D9E6F7", "Thaw" = "#F8E2EA"), guide = "none") +
    facet_grid(cols = vars(regime)) +
    scale_colour_manual(values = palette, limits = components, breaks = components, labels = component_labels, drop = FALSE) +
    scale_x_continuous(limits = c(-3, 99), breaks = c(0, 48, 96), expand = c(0, 0)) +
    scale_y_continuous(breaks = c(0, upper / 2, upper), expand = c(0, 0)) +
    coord_cartesian(ylim = c(-6 * upper / 70, upper)) + labs(x = if (show_x) "Incubation days" else NULL,
      y = paste0(title, " (%)"), tag = tag) + theme_figure +
    theme(legend.position = "bottom") + guides(colour = guide_legend(ncol = 2, byrow = FALSE, override.aes = list(linewidth = 0.8)))
  for (component_name in component_subset) {
    p <- p + geom_ribbon(data = d[d$component == component_name, ], aes(ymin = q025, ymax = q975),
      fill = palette[component_name], colour = NA, alpha = 0.08, show.legend = FALSE)
  }
  p <- p + geom_line(linewidth = 0.4, lineend = "round", show.legend = TRUE)
  if (!show_x) p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  if (show_x) p <- p + theme(strip.text.x = element_blank())
  p
}
panel_main <- make_temporal(components[1:3], 70, "Main effects", "b", FALSE)
panel_interactions <- make_temporal(components[4:6], 30, "Interactions", NULL, TRUE)
temporal <- (panel_main / panel_interactions) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
figure <- panel_a / temporal + plot_layout(heights = c(1.05, 1.55))
ggsave(file.path(output_dir, "Fig_S9.png"), figure, width = 9.17, height = 8.15, units = "in", dpi = 600, device = ragg::agg_png, bg = "white")
message(file.path(output_dir, "Fig_S9.png"))
