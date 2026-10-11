library(data.table)
library(ggplot2)
library(ragg)

script_frames <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_frames)) tail(script_frames, 1L)[[1L]] else if (length(script_argument)) sub("^--file=", "", script_argument[[1L]]) else "Fig_S7_plot.R"
code_dir <- dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
figure_data <- readRDS(file.path(code_dir, "Fig_S7_data.rds"))
arguments <- commandArgs(TRUE)
output_dir <- if (length(arguments)) arguments[[1L]] else Sys.getenv("FTC_OUTPUT_DIR", file.path(dirname(dirname(code_dir)), "version2_outputs", "Fig_S7"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

plot_data <- as.data.table(figure_data)
axis_training <- as.data.table(structure(list(metric = c("a_dormant_MBC_fraction", "a_dormant_MBC_fraction", 
"a_dormant_MBC_fraction", "a_dormant_MBC_fraction", "b_inactive_ENZ_fraction", 
"b_inactive_ENZ_fraction", "b_inactive_ENZ_fraction", "b_inactive_ENZ_fraction", 
"c_hourly_freeze_death_fraction", "c_hourly_freeze_death_fraction", 
"c_hourly_freeze_death_fraction", "c_hourly_freeze_death_fraction", 
"d_freeze_isolated_DOC_fraction", "d_freeze_isolated_DOC_fraction", 
"d_freeze_isolated_DOC_fraction", "d_freeze_isolated_DOC_fraction", 
"e_freeze_isolated_SOC_fraction", "e_freeze_isolated_SOC_fraction", 
"e_freeze_isolated_SOC_fraction", "e_freeze_isolated_SOC_fraction", 
"f_breakup_DOC_release_fraction", "f_breakup_DOC_release_fraction", 
"f_breakup_DOC_release_fraction", "f_breakup_DOC_release_fraction", 
"g_breakup_SOC_release_fraction", "g_breakup_SOC_release_fraction", 
"g_breakup_SOC_release_fraction", "g_breakup_SOC_release_fraction", 
"a_dormant_MBC_fraction", "a_dormant_MBC_fraction", "a_dormant_MBC_fraction", 
"a_dormant_MBC_fraction", "b_inactive_ENZ_fraction", "b_inactive_ENZ_fraction", 
"b_inactive_ENZ_fraction", "b_inactive_ENZ_fraction", "c_hourly_freeze_death_fraction", 
"c_hourly_freeze_death_fraction", "c_hourly_freeze_death_fraction", 
"c_hourly_freeze_death_fraction", "d_freeze_isolated_DOC_fraction", 
"d_freeze_isolated_DOC_fraction", "d_freeze_isolated_DOC_fraction", 
"d_freeze_isolated_DOC_fraction", "e_freeze_isolated_SOC_fraction", 
"e_freeze_isolated_SOC_fraction", "e_freeze_isolated_SOC_fraction", 
"e_freeze_isolated_SOC_fraction", "f_breakup_DOC_release_fraction", 
"f_breakup_DOC_release_fraction", "f_breakup_DOC_release_fraction", 
"f_breakup_DOC_release_fraction", "g_breakup_SOC_release_fraction", 
"g_breakup_SOC_release_fraction", "g_breakup_SOC_release_fraction", 
"g_breakup_SOC_release_fraction"), duration = structure(c(1L, 
2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 
2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 
2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 
2L, 3L, 4L, 1L, 2L, 3L, 4L), levels = c("LFLT", "LFST", "SFLT", 
"SFST"), class = c("ordered", "factor")), axis_value = c(-5.4664475982307, 
-5.4664475982307, -5.4664475982307, -5.4664475982307, -4.39962465596257, 
-4.39962465596257, -4.39962465596257, -4.39962465596257, -11.2579638491801, 
-11.2579638491801, -11.2579638491801, -11.2579638491801, -3.04073589516555, 
-3.04073589516555, -3.04073589516555, -3.04073589516555, -2.95792212776157, 
-2.95792212776157, -2.95792212776157, -2.95792212776157, -1.23468587116098, 
-1.23468587116098, -1.23468587116098, -1.23468587116098, -0.290268650189142, 
-0.290268650189142, -0.290268650189142, -0.290268650189142, 51.0932895196461, 
51.0932895196461, 51.0932895196461, 51.0932895196461, 40.8799249311925, 
40.8799249311925, 40.8799249311925, 40.8799249311925, 102.251592769836, 
102.251592769836, 102.251592769836, 102.251592769836, 30.6081471790331, 
30.6081471790331, 30.6081471790331, 30.6081471790331, 30.5915844255523, 
30.5915844255523, 30.5915844255523, 30.5915844255523, 11.1121728404488, 
11.1121728404488, 11.1121728404488, 11.1121728404488, 3.05805373003783, 
3.05805373003783, 3.05805373003783, 3.05805373003783), Day = c(48, 
48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 
48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 
48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 
48, 48, 48, 48, 48, 48, 48)), row.names = c(NA, 56L), class = "data.frame"))
y_limits <- as.data.table(structure(list(metric = c("a_dormant_MBC_fraction", "b_inactive_ENZ_fraction", 
"c_hourly_freeze_death_fraction", "d_freeze_isolated_DOC_fraction", 
"e_freeze_isolated_SOC_fraction", "f_breakup_DOC_release_fraction", 
"g_breakup_SOC_release_fraction"), axis_upper = c(51.0932895196461, 
40.8799249311925, 102.251592769836, 30.6081471790331, 30.5915844255523, 
11.1121728404488, 3.05805373003783), tick_step = c(25, 20, 50, 
15, 15, 5, 1.5)), row.names = c(NA, 7L), class = "data.frame"))
metric_labels <- c(a_dormant_MBC_fraction = "Dormant MBC\n(%)", b_inactive_ENZ_fraction = "Inactive ENZ\n(%)", 
c_hourly_freeze_death_fraction = "Freeze-induced\nMBC mortality\n(%)", 
d_freeze_isolated_DOC_fraction = "Freeze-isolated\nDOC\n(%)", 
e_freeze_isolated_SOC_fraction = "Freeze-isolated\nSOC\n(%)", 
f_breakup_DOC_release_fraction = "Breakup DOC\nrelease\n(%)", 
g_breakup_SOC_release_fraction = "Breakup SOC\nrelease\n(%)")
plot_data[, metric := factor(metric, levels = names(metric_labels))]
axis_training[, metric := factor(metric, levels = names(metric_labels))]
group_colours <- c(overall = "#000000", depth_0_10 = "#d9ae84", depth_70_80 = "#73442e")
group_levels <- c("overall", "depth_0_10", "depth_70_80")
group_labels <- c("Overall", "0\u201310 cm", "70\u201380 cm")
line_alpha <- 0.7
ribbon_alpha <- 0.15
draw_order <- c("overall", "depth_70_80", "depth_0_10")
font_family <- "Times New Roman"
font_scale <- 1.5
format_y_values <- function(values) {
    format(values, trim = TRUE, scientific = FALSE, nsmall = 0)
}
plot_object <- ggplot(plot_data, aes(x = DisplayDay)) +
    geom_blank(data = axis_training, aes(x = Day, y = axis_value), inherit.aes = FALSE)
for (draw_key in draw_order) {
    group_data <- copy(plot_data[group_key == draw_key])
    plot_object <- plot_object + geom_ribbon(data = group_data, aes(ymin = q025_percent, ymax = q975_percent, fill = group_key, 
        group = group_key), alpha = ribbon_alpha, colour = NA, show.legend = FALSE) + geom_line(data = group_data, 
        aes(y = mean_percent, colour = group_key, group = group_key), alpha = line_alpha, linewidth = 0.36, lineend = "round")
}
plot_object <- plot_object + facet_grid(rows = vars(metric), cols = vars(duration), scales = "free_y", switch = "y", 
    drop = FALSE, labeller = labeller(metric = metric_labels)) + scale_fill_manual(values = group_colours, breaks = group_levels, guide = "none") + scale_colour_manual(values = group_colours, 
    breaks = group_levels, labels = group_labels, name = NULL) + scale_x_continuous(breaks = c(0, 48, 96), expand = expansion(mult = 0.05)) + 
    coord_cartesian(xlim = c(0, 96)) + scale_y_continuous(breaks = local({
    row_axis_table <- copy(y_limits)
    function(limits) {
        nominal_span <- diff(limits)/1.1
        nominal_upper <- limits[[2L]] - 0.05 * nominal_span
        row_index <- which.min(abs(row_axis_table$axis_upper - nominal_upper))
        c(0, row_axis_table$tick_step[[row_index]], 2 * row_axis_table$tick_step[[row_index]])
    }
}), labels = format_y_values, expand = expansion(mult = 0.05)) + labs(x = "Incubation days", y = NULL) + guides(colour = guide_legend(nrow = 1L, 
    override.aes = list(linewidth = 0.7, alpha = line_alpha))) + theme_bw(base_size = 7 * font_scale, base_family = font_family) + 
    theme(panel.grid = element_blank(), panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.38), 
        axis.line = element_blank(), axis.ticks = element_line(linewidth = 0.32, colour = "black"), axis.ticks.length = grid::unit(1.1, 
            "mm"), axis.text = element_text(size = 6.3 * font_scale, colour = "black", family = font_family), axis.title.x = element_text(size = 7.6 * 
            font_scale, family = font_family, margin = margin(t = 2.5)), strip.background = element_blank(), strip.placement = "outside", 
        strip.text.x = element_text(size = 7.7 * font_scale, face = "plain", family = font_family, margin = margin(b = 1.5)), 
        strip.text.y.left = element_text(angle = 90, size = 6.7 * font_scale, face = "plain", hjust = 0.5, vjust = 0.5, 
            lineheight = 0.95, family = font_family, margin = margin(r = 2)), panel.spacing.x = grid::unit(1.1, 
            "mm"), panel.spacing.y = grid::unit(1, "mm"), legend.position = "top", legend.direction = "horizontal", 
        legend.text = element_text(size = 7.7 * font_scale, family = font_family), legend.key.width = grid::unit(7.00, 
            "mm"), legend.key.height = grid::unit(4.00, "mm"), legend.margin = margin(0, 0, 2, 0, unit = "mm"), legend.background = element_blank(), 
        legend.key = element_blank(), plot.background = element_rect(fill = "white", colour = NA), plot.margin = margin(4, 
            4, 4, 5, unit = "mm"))
figure_plot <- plot_object
ragg::agg_png(file.path(output_dir, "Fig_S7.png"), width = 7.20, height = 7.28, units = "in", res = 600, background = "white")
print(figure_plot)
grDevices::dev.off()
