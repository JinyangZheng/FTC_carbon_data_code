library(ggplot2)
library(patchwork)
library(ragg)

.script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.script_path <- if (length(.script_files)) tail(.script_files, 1L)[[1L]] else if (length(.script_arg)) sub("^--file=", "", .script_arg[[1L]]) else "Fig_S4_plot.R"
code_dir <- dirname(normalizePath(.script_path, winslash = "/", mustWork = TRUE))
b <- readRDS(file.path(code_dir, "Fig_S4_data.rds"))
.args <- commandArgs(trailingOnly = TRUE)
.output_env <- Sys.getenv("FTC_OUTPUT_DIR", unset = "")
output_dir <- if (length(.args)) .args[[1L]] else if (nzchar(.output_env)) .output_env else file.path(dirname(dirname(code_dir)), "version2_outputs", "Fig_S4")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

font_family <- "Times New Roman"
groups <- c("overall", "Depth10", "Depth70", "LFLT", "LFST", "SFLT", "SFST", "Freeze", "Thaw")
group_labels <- setNames(c("All data", "0\u201310 cm", "70\u201380 cm", "LFLT", "LFST", "SFLT", "SFST", "Freeze", "Thaw"), groups)
panel_tags <- setNames(c("a", "b", "", "c", "", "", "", "d", ""), groups)
classes <- c("Physical protection", "Microbial physiology", "Initial state variables")
palette <- setNames(c("#E994AE", "#A0BFF6", "#AFD1AF"), classes)
parameter_labels <- c(b_D = "italic(b[D])", b_S = "italic(b[S])", f_iso = "italic(f[iso])",
  "k_D,sor" = "italic(k[D*','*sor])", "k_D,des" = "italic(k[D*','*des])",
  "k_S,sor" = "italic(k[S*','*sor])", "k_S,des" = "italic(k[S*','*des])",
  v_upt = "italic(v[upt])", v_cat = "italic(v[cat])", f_dorm = "italic(f[dorm])",
  f_inact = "italic(f[inact])", r_frz = "italic(r[frz])", e_slope = "italic(e[slope])",
  r_death = "italic(r[death])", r_prod = "italic(r[prod])", SOC = "italic(SOC)", DOC = "italic(DOC)", MBC = "italic(MBC)", ENZ = "italic(ENZ)")
make_panel <- function(group_key) {
  d <- b[b$group == group_key, ]
  d$class <- factor(d$class, levels = classes)
  d$predictor <- factor(d$predictor, levels = rev(d$predictor[order(d$class, -d$mean_percent)]))
  ggplot(d, aes(x = mean_percent, y = predictor, fill = class)) +
    geom_hline(yintercept = c(4.5, 12.5), linetype = "dashed", colour = "grey55", linewidth = 0.25) +
    geom_col(width = 0.70) +
    geom_segment(aes(x = q025_percent, xend = q975_percent, yend = predictor), colour = "black", linewidth = 0.32) +
    geom_text(aes(x = pmin(pmax(mean_percent, q975_percent) + 3, 56.4), label = sprintf("%.1f", mean_percent)),
      hjust = 0, family = font_family, size = 2.79) +
    annotate("text", x = 59.1, y = 12.7, label = sprintf("R\u00b2 %.2f\u2013%.2f", d$R2_min[1], d$R2_max[1]),
      family = font_family, size = 2.97, hjust = 1, vjust = 0) +
    scale_fill_manual(values = palette, limits = classes, drop = FALSE) +
    scale_y_discrete(labels = function(x) parse(text = parameter_labels[x]), expand = expansion(add = c(0.45, 0.45))) +
    scale_x_continuous(limits = c(0, 60), breaks = c(0, 30, 60), expand = c(0, 0)) +
    labs(x = "Relative importance (%)", y = NULL, title = group_labels[group_key],
      tag = if (nzchar(panel_tags[group_key])) panel_tags[group_key] else NULL, fill = NULL) +
    theme_bw(base_size = 9.9, base_family = font_family) +
    theme(panel.grid = element_blank(), panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.35),
      axis.text.x = element_text(size = 8.46, colour = "black"),
      axis.text.y = element_text(size = 7.92, face = "italic", colour = "black"),
      axis.title.x = element_text(size = 9.36), axis.ticks = element_line(linewidth = 0.28),
      axis.ticks.length = grid::unit(0.7, "mm"), plot.title = element_text(size = 11.16, face = "plain", hjust = 0.5),
      plot.tag = element_text(size = 18, face = "plain"), plot.tag.position = c(0.01, 1),
      legend.text = element_text(size = 9.36), legend.key.size = grid::unit(2.2, "mm"),
      legend.position = "bottom", legend.direction = "vertical", plot.margin = margin(2, 2, 1, 2, unit = "mm")) +
    guides(fill = guide_legend(ncol = 1))
}
panels <- lapply(groups, make_panel)
figure <- wrap_plots(c(panels, list(guide_area())), design = "ABC#\nDEFG\nHILL", guides = "collect") &
  theme(legend.position = "bottom", legend.direction = "vertical")
ggsave(file.path(output_dir, "Fig_S4.png"), figure, width = 8.49, height = 8.47, units = "in", dpi = 600, device = ragg::agg_png, bg = "white")
message(file.path(output_dir, "Fig_S4.png"))
