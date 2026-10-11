library(ggplot2)
library(patchwork)
library(ragg)

script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_files)) tail(script_files, 1L)[[1L]] else if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else "Fig_4_plot.R"
code_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
figure_data <- readRDS(file.path(code_dir, "Fig_4_data.rds"))
output_args <- commandArgs(trailingOnly = TRUE)
output_env <- Sys.getenv("FTC_OUTPUT_DIR")
output_dir <- if (length(output_args)) output_args[[1L]] else if (nzchar(output_env)) output_env else file.path(dirname(dirname(code_dir)), "version2_outputs", "Fig_4")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

regime_order <- c("LFLT", "LFST", "SFLT", "SFST")
panel_a_data <- figure_data$panel_a
panel_b_data <- figure_data$panel_b
panel_c_data <- figure_data$panel_c
phase_data <- figure_data$background
panel_a_data$regime <- factor(panel_a_data$regime, levels = regime_order)
panel_b_data$regime <- factor(panel_b_data$regime, levels = regime_order)
panel_c_data$regime <- factor(panel_c_data$regime, levels = regime_order)
process_labels <- c(f_iso = "Physics", b_D = "Physics", v_upt = "Physiology", f_dorm = "Physiology", r_frz = "Physiology")
panel_b_data$process <- factor(unname(process_labels[panel_b_data$predictor]), levels = c("Physics", "Physiology"))
key_order <- c("b_D", "f_iso", "v_upt", "f_dorm", "r_frz")
panel_b_data$predictor <- factor(panel_b_data$predictor, levels = key_order)
key_colors <- c(b_D = "#E994AE", f_iso = "#41B6C4", v_upt = "#F1AA7B", f_dorm = "#6A96E1", r_frz = "#253494")
key_labels <- expression(italic(b)[italic(D)], italic(f)[italic(iso)], italic(v)[italic(upt)], italic(f)[italic(dorm)], italic(r)[italic(frz)])
component_order <- c("Physical protection", "Microbial physiology", "State variables")
component_colors <- c("Physical protection" = "#E994AE", "Microbial physiology" = "#A0BFF6", "State variables" = "#AFD1AF")
component_labels <- c("Physical protection", "Microbial physiology", "State variables")
panel_c_data$component <- factor(panel_c_data$component, levels = c("State variables", "Physical protection", "Microbial physiology"))
font_family <- "Times New Roman"
panel_theme <- theme_bw(base_size = 10, base_family = font_family) +
  theme(panel.grid = element_blank(),
    panel.border = element_rect(colour = "#4C4C4C", linewidth = 0.12, fill = NA),
    panel.spacing.x = grid::unit(0, "mm"), panel.spacing.y = grid::unit(0, "mm"),
    axis.line = element_blank(), axis.ticks = element_line(linewidth = 0.12),
    axis.ticks.length = grid::unit(2, "pt"),
    axis.text = element_text(colour = "black", size = 10),
    axis.title.x = element_text(size = 10, margin = margin(t = 3)),
    axis.title.y = element_text(size = 10, margin = margin(r = 3)),
    strip.background = element_blank(), strip.text = element_text(size = 10),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(2, 2, 2, 4, unit = "pt"),
    plot.tag = element_text(size = 12, face = "bold"),
    plot.tag.position = c(0, 1),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.title = element_blank(), legend.margin = margin(0, 0, 0, 0),
    legend.key = element_rect(fill = "white", colour = NA),
    legend.key.height = grid::unit(9.00, "pt"), legend.key.width = grid::unit(9.00, "pt"),
    legend.text = element_text(size = 9), legend.spacing.x = grid::unit(2, "pt"))

panel_a <- ggplot(panel_a_data, aes(Day, mean, group = regime)) +
  geom_rect(data = phase_data[phase_data$phase == "Freeze", ], inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = 0.718544963549707, ymax = 1.04506829515245), fill = "#D9E6F7", colour = NA) +
  geom_rect(data = phase_data[phase_data$phase == "Thaw", ], inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = 0.718544963549707, ymax = 1.04506829515245), fill = "#F8E2EA", colour = NA) +
  geom_ribbon(aes(ymin = q025, ymax = q975), fill = "#8A8588", alpha = 0.42, colour = NA) +
  geom_line(colour = "#3A3A3A", linewidth = 0.30) +
  facet_grid(cols = vars(regime)) +
  scale_x_continuous(breaks = c(0, 48, 96), expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0.8, 0.9, 1.0), labels = c("0.8", "0.9", "1.0"), expand = c(0, 0)) +
  coord_cartesian(xlim = c(-5, 101), ylim = c(0.718544963549707, 1.04506829515245)) +
  labs(x = "Incubation days", y = expression(atop("Emulator performance", R^2)), tag = "a") +
  panel_theme + theme(axis.title.y = element_text(size = 9))

panel_b <- ggplot(panel_b_data, aes(plot_day, mean, colour = predictor, fill = predictor, group = predictor)) +
  geom_rect(data = phase_data[phase_data$phase == "Freeze", ], inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -2.8, ymax = -1), fill = "#D9E6F7", colour = NA) +
  geom_rect(data = phase_data[phase_data$phase == "Thaw", ], inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -2.8, ymax = -1), fill = "#F8E2EA", colour = NA) +
  geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.13, colour = NA, show.legend = FALSE) +
  geom_line(linewidth = 0.40) +
  facet_grid(rows = vars(process), cols = vars(regime), switch = "y") +
  scale_colour_manual(values = key_colors, breaks = key_order, labels = key_labels) +
  scale_fill_manual(values = key_colors, guide = "none") +
  scale_x_continuous(breaks = c(0, 48, 96), expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0, 25, 50), expand = c(0, 0)) +
  coord_cartesian(xlim = c(-5, 101), ylim = c(-3.2, 60)) +
  labs(x = "Incubation days", y = "Relative importance (%)", tag = "b") +
  panel_theme +
  theme(strip.text.x = element_blank(), strip.text.y.left = element_text(angle = 90, size = 10),
    strip.placement = "outside", legend.position = "inside", legend.position.inside = c(0.013, 0.99),
    legend.justification = c(0, 1), legend.text = element_text(size = 9)) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(linewidth = 0.53)))

panel_c <- ggplot(panel_c_data, aes(Day, mean, colour = component, fill = component, group = component)) +
  geom_rect(data = phase_data[phase_data$phase == "Freeze", ], inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -5.6, ymax = -2.5), fill = "#D9E6F7", colour = NA) +
  geom_rect(data = phase_data[phase_data$phase == "Thaw", ], inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -5.6, ymax = -2.5), fill = "#F8E2EA", colour = NA) +
  geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.13, colour = NA) +
  geom_line(linewidth = 0.28, show.legend = FALSE) +
  facet_grid(cols = vars(regime)) +
  scale_colour_manual(values = component_colors, guide = "none") +
  scale_fill_manual(values = component_colors, breaks = component_order, labels = component_labels) +
  scale_x_continuous(breaks = c(0, 48, 96), expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0, 50, 100), expand = c(0, 0)) +
  coord_cartesian(xlim = c(-5, 101), ylim = c(-6, 140)) +
  labs(x = "Incubation days", y = "Group\nimportance (%)", tag = "c") +
  panel_theme +
  theme(strip.text.x = element_blank(), legend.position = "inside", legend.position.inside = c(0.015, 0.99),
    legend.justification = c(0, 1), legend.direction = "horizontal", legend.text = element_text(size = 8.47),
    legend.key.width = grid::unit(4.20, "pt"), legend.key.height = grid::unit(4.20, "pt")) +
  guides(fill = guide_legend(nrow = 1, override.aes = list(alpha = 1)))

figure <- panel_a / panel_b / panel_c + plot_layout(heights = c(1, 2.0719, 1.1487))
ggsave(file.path(output_dir, "Fig_4.png"), figure, device = ragg::agg_png,
  width = 3842.00, height = 2958.00, units = "px", dpi = 600, bg = "white")
