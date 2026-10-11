library(ggplot2)
library(patchwork)
library(ragg)

.script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.script_path <- if (length(.script_files)) tail(.script_files, 1L)[[1L]] else if (length(.script_arg)) sub("^--file=", "", .script_arg[[1L]]) else "Fig_3_plot.R"
code_dir <- dirname(normalizePath(.script_path, winslash = "/", mustWork = TRUE))
b <- readRDS(file.path(code_dir, "Fig_3_data.rds"))
.args <- commandArgs(trailingOnly = TRUE)
.output_env <- Sys.getenv("FTC_OUTPUT_DIR", unset = "")
output_dir <- if (length(.args)) .args[[1L]] else if (nzchar(.output_env)) .output_env else file.path(dirname(dirname(code_dir)), "version2_outputs", "Fig_3")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

font_family <- "Times New Roman"
groups <- c("overall", "Depth10", "Depth70", "LFLT", "LFST", "SFLT", "SFST", "Freeze", "Thaw")
group_labels <- c("All data", "0\u201310 cm", "70\u201380 cm", "LFLT", "LFST", "SFLT", "SFST", "Freeze", "Thaw")
classes <- c("Physical protection", "Microbial physiology", "Initial state variables")
palette <- setNames(c("#E994AE", "#A0BFF6", "#AFD1AF"), classes)
theme_figure <- theme_bw(base_size = 12, base_family = font_family) +
  theme(panel.grid = element_blank(), panel.border = element_rect(colour = "black", linewidth = 0.7),
    axis.text = element_text(colour = "black"), axis.ticks.length = grid::unit(2.5, "pt"),
    plot.tag = element_text(size = 16, face = "bold"), plot.tag.position = c(0, 1),
    plot.margin = margin(19, 3, 4, 3), legend.title = element_blank(),
    legend.text = element_text(size = 12), legend.background = element_rect(fill = "white", colour = NA))
add_headers <- function(plot, x_labels = waiver()) {
  plot + geom_vline(xintercept = c(1.5, 3.5, 7.5), linewidth = 0.6) +
    geom_vline(xintercept = c(2.5, 4.5, 5.5, 6.5, 8.5), linewidth = 0.35, linetype = "dashed", colour = "grey55") +
    annotate("text", x = c(1, 2.5, 5.5, 8.5), y = Inf,
      label = c("Overall", "Soil depth", "FTC regimes", "Freeze vs. Thaw phases"),
      vjust = -0.6, family = font_family, size = 12 / .pt) +
    annotate("text", x = 1:9, y = Inf, label = group_labels,
      vjust = 1.2, family = font_family, size = 11.3 / .pt) +
    scale_x_continuous(limits = c(0.5, 9.5), breaks = 1:9, labels = x_labels, expand = c(0, 0)) + theme_figure
}
a <- b$performance
a$x <- match(a$group, groups)
panel_a <- ggplot(a, aes(x = x, group = group, ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax)) +
  geom_boxplot(stat = "identity", width = 0.10, fill = "white", linewidth = 0.35) +
  scale_y_continuous(breaks = c(0.90, 0.93, 0.96), labels = function(x) sprintf("%.2f", x), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0.88, 0.98), clip = "off") +
  labs(x = NULL, y = expression("Emulator performance ("*R^2*")"), tag = "a")
panel_a <- add_headers(panel_a) + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
d <- b$predictors
d$x <- match(d$group, groups)
d$class <- factor(d$class, levels = classes)
predictor_labels <- c(b_D = "italic(b)[italic(D)]", v_upt = "italic(v)[italic(upt)]")
panel_b <- ggplot(d, aes(x = x, y = mean_percent, fill = class)) +
  geom_col(width = 0.26) +
  geom_errorbar(aes(ymin = q025_percent, ymax = q975_percent), width = 0.00, linewidth = 0.3) +
  geom_text(aes(y = q975_percent, label = sprintf("%.0f", mean_percent)), vjust = -0.6,
    family = font_family, size = 12 / .pt) +
  scale_fill_manual(values = palette, guide = "none") +
  scale_y_continuous(breaks = c(0, 30, 60), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 60), clip = "off") +
  labs(x = NULL, y = "Relative importance (%)", tag = "b")
panel_b <- add_headers(panel_b, parse(text = predictor_labels[d$predictor[match(groups, d$group)]])) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_text(size = 12))
d <- b$components
d$x <- match(d$group, groups)
d$class <- factor(d$class, levels = classes)
dodge <- position_dodge(width = 0.68)
panel_c <- ggplot(d, aes(x = x, y = mean_percent, fill = class, group = class)) +
  geom_col(position = dodge, width = 0.52) +
  geom_errorbar(aes(ymin = q025_percent, ymax = q975_percent), position = dodge, width = 0.00, linewidth = 0.5) +
  geom_text(aes(y = q975_percent, label = sprintf("%.0f", mean_percent)), position = dodge,
    vjust = -0.6, family = font_family, size = 10 / .pt) +
  scale_fill_manual(values = palette, drop = FALSE) +
  scale_y_continuous(breaks = c(0, 50, 100), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 107.540979620273), clip = "off") +
  labs(x = NULL, y = "Group level importance (%)", tag = "c")
panel_c <- add_headers(panel_c) + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
  legend.position = "inside", legend.position.inside = c(0.01, 0.78),
  legend.justification = c(0, 0.5), legend.key.size = grid::unit(7, "pt"), legend.margin = margin(2, 2, 2, 2))
figure <- panel_a / panel_b / panel_c + plot_layout(heights = c(0.8, 0.8, 1))
ggsave(file.path(output_dir, "Fig_3.png"), figure, width = 6.88, height = 8.46, units = "in", dpi = 600, device = ragg::agg_png, bg = "white")
message(file.path(output_dir, "Fig_3.png"))
