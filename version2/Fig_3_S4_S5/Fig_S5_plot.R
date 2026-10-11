library(ggplot2)
library(patchwork)
library(ragg)

.script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.script_path <- if (length(.script_files)) tail(.script_files, 1L)[[1L]] else if (length(.script_arg)) sub("^--file=", "", .script_arg[[1L]]) else "Fig_S5_plot.R"
code_dir <- dirname(normalizePath(.script_path, winslash = "/", mustWork = TRUE))
b <- readRDS(file.path(code_dir, "Fig_S5_data.rds"))
.args <- commandArgs(trailingOnly = TRUE)
.output_env <- Sys.getenv("FTC_OUTPUT_DIR", unset = "")
output_dir <- if (length(.args)) .args[[1L]] else if (nzchar(.output_env)) .output_env else file.path(dirname(dirname(code_dir)), "version2_outputs", "Fig_S5")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

font_family <- "Times New Roman"
groups <- c("overall", "Depth10", "Depth70", "LFLT", "LFST", "SFLT", "SFST", "Freeze", "Thaw")
group_labels <- setNames(c("All data", "0\u201310 cm", "70\u201380 cm", "LFLT", "LFST", "SFLT", "SFST", "Freeze", "Thaw"), groups)
predictors <- c("b_D", "b_S", "f_iso", "k_D,sor", "k_D,des", "k_S,sor", "k_S,des", "v_upt", "v_cat", "f_dorm", "f_inact", "r_frz", "e_slope", "r_death", "r_prod")
parameter_labels <- c(b_D = "italic(b)[italic(D)]", b_S = "italic(b)[italic(S)]", f_iso = "italic(f)[italic(iso)]",
  "k_D,sor" = "italic(k)[italic(D)*','*italic(sor)]", "k_D,des" = "italic(k)[italic(D)*','*italic(des)]",
  "k_S,sor" = "italic(k)[italic(S)*','*italic(sor)]", "k_S,des" = "italic(k)[italic(S)*','*italic(des)]",
  v_upt = "italic(v)[italic(upt)]", v_cat = "italic(v)[italic(cat)]", f_dorm = "italic(f)[italic(dorm)]",
  f_inact = "italic(f)[italic(inact)]", r_frz = "italic(r)[italic(frz)]", e_slope = "italic(e)[italic(slope)]",
  r_death = "italic(r)[italic(death)]", r_prod = "italic(r)[italic(prod)]")
palette <- c("Physical protection" = "#E994AE", "Microbial physiology" = "#A0BFF6")
d <- b
d$group <- factor(d$group, levels = groups)
d$predictor <- factor(d$predictor, levels = predictors)
panels <- ggplot(d, aes(x = x_normalized, y = signed_shap_mean, group = interaction(group, predictor))) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55", linewidth = 0.22) +
  geom_ribbon(aes(ymin = signed_shap_lower, ymax = signed_shap_upper, fill = class), alpha = 0.25, colour = NA, na.rm = TRUE) +
  geom_line(aes(colour = class), linewidth = 0.38, lineend = "round") +
  facet_grid(rows = vars(predictor), cols = vars(group),
    labeller = labeller(predictor = as_labeller(parameter_labels, default = label_parsed), group = as_labeller(group_labels)), switch = "y") +
  scale_colour_manual(values = palette, drop = FALSE) + scale_fill_manual(values = palette, drop = FALSE) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), labels = c("\u2009\u20090", "0.5", "1\u2009\u2009"), expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(breaks = c(-2000, 0, 2000), labels = function(x) format(x, scientific = FALSE, trim = TRUE), expand = c(0, 0)) +
  coord_cartesian(ylim = c(-3000, 4000)) + labs(x = "Normalized value", y = NULL) +
  theme_bw(base_size = 13.728, base_family = font_family) +
  theme(text = element_text(face = "plain"), panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.28),
    panel.spacing.x = grid::unit(1, "mm"), panel.spacing.y = grid::unit(0.35, "mm"),
    axis.text.x = element_text(size = 9.768, colour = "black", face = "plain"),
    axis.text.y = element_text(size = 8.6592, colour = "black", face = "plain"),
    axis.title.x = element_text(size = 14.52, face = "plain"), axis.ticks = element_line(linewidth = 0.24),
    axis.ticks.length = grid::unit(0.55, "mm"), strip.background = element_blank(), strip.placement = "outside", strip.clip = "off",
    strip.text.x = element_text(size = 10.6, face = "plain"),
    strip.text.y.left = element_text(angle = 0, size = 11.616, hjust = 1, face = "plain"),
    legend.position = "none", plot.margin = margin(0, 3, 3, 7, unit = "mm"))
headers <- ggplot() + annotate("text", x = c(1, 2.5, 5.5, 8.5), y = 0.5,
  label = c("Overall", "Soil depth", "FTC regimes", "FTC phases"), family = font_family, size = 12.408 / .pt) +
  coord_cartesian(xlim = c(0.5, 9.5), ylim = c(0, 1), expand = FALSE) + theme_void() +
  theme(plot.margin = margin(3, 8.5, 0, 0))
figure <- headers / panels + plot_layout(heights = c(0.045, 1))
ggsave(file.path(output_dir, "Fig_S5.png"), figure, width = 7.20, height = 9.05, units = "in", dpi = 600, device = ragg::agg_png, bg = "white")
message(file.path(output_dir, "Fig_S5.png"))
