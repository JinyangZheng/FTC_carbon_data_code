figure_data <- readRDS("Your_pathway/Fig_S8/Fig_S8_data.rds")

library(data.table)
library(ggplot2)

plot_data <- as.data.table(figure_data$trajectories)
phase_data <- as.data.table(figure_data$phases)

metric_labels <- c(
  a_dormant_MBC_fraction = "Dormant MBC\n(%)",
  b_inactive_ENZ_fraction = "Inactive ENZ\n(%)",
  c_hourly_freeze_death_fraction = "Freeze-induced\nMBC mortality\n(%)",
  d_freeze_isolated_DOC_fraction = "Freeze-isolated\nDOC\n(%)",
  e_freeze_isolated_SOC_fraction = "Freeze-isolated\nSOC\n(%)",
  f_breakup_DOC_release_fraction = "Breakup DOC\nrelease\n(%)",
  g_breakup_SOC_release_fraction = "Breakup SOC\nrelease\n(%)"
)
duration_levels <- c("LFLT", "LFST", "SFLT", "SFST")
group_levels <- c("overall", "depth_0_10", "depth_70_80")
group_labels <- c("Overall", "0\u201310 cm", "70\u201380 cm")
group_colours <- c(overall = "#000000", depth_0_10 = "#d9ae84", depth_70_80 = "#73442e")
line_alpha <- 0.7
ribbon_alpha <- 0.15
# Horizontal offsets separate the depth curves visually. Day is the model time.
day_shifts <- c(overall = 0, depth_0_10 = -1, depth_70_80 = 1)
font_family <- "Times New Roman"
font_scale <- 1.5
figure_width_mm <- 183
figure_height_mm <- 185
figure_dpi <- 600L
plot_data[, metric_label := factor(unname(metric_labels[as.character(metric)]),
                                  levels = unname(metric_labels), ordered = TRUE)]
plot_data[, duration := factor(duration, levels = duration_levels, ordered = TRUE)]
plot_data[, group_key := factor(group_key, levels = group_levels, ordered = TRUE)]
plot_data[, DisplayDay := Day + unname(day_shifts[as.character(group_key)])]
phase_data[, duration := factor(duration, levels = duration_levels, ordered = TRUE)]
# Each group's ribbon and line form one layer pair, drawn from bottom to top.
draw_order <- c("overall", "depth_70_80", "depth_0_10")
setorder(plot_data, metric_label, duration, group_key, Hour)

# Share each metric's axis across all three groups and all four durations.
y_limits <- plot_data[, .(
  data_min = min(q025_percent, mean_percent, q975_percent),
  data_max = max(q025_percent, mean_percent, q975_percent)
), by = .(metric, metric_label)]
y_limits[, data_span := data_max - data_min]
y_limits[data_span <= 0, data_span := pmax(abs(data_max), 1) * 0.1]
nice_tick_step <- function(maximum) {
  raw_step <- maximum / 2
  if (raw_step <= 0) return(0.5)
  magnitude <- 10 ^ floor(log10(raw_step))
  candidates <- c(1, 1.5, 2, 2.5, 3, 4, 5, 7.5, 10) * magnitude
  min(candidates[candidates >= raw_step])
}
y_limits[, tick_step := vapply(data_max, nice_tick_step, numeric(1L))]
y_limits[, `:=`(tick_0 = 0, tick_1 = tick_step, tick_2 = 2 * tick_step,
                axis_lower = data_min - data_span / 8,
                axis_upper = pmax(data_max + data_span / 8, 2 * tick_step + 0.025 * data_span))]
axis_training <- y_limits[rep(seq_len(.N), each = 4L), .(metric, metric_label, axis_lower, axis_upper)]
axis_training[, duration := factor(rep(duration_levels, times = 7L), levels = duration_levels, ordered = TRUE)]
axis_training <- melt(axis_training, id.vars = c("metric", "metric_label", "duration"),
                      measure.vars = c("axis_lower", "axis_upper"), value.name = "axis_value")
axis_training[, Day := 48]
format_y_values <- function(values) {
  format(values, trim = TRUE, scientific = FALSE, nsmall = 0)
}

plot_object <- ggplot(plot_data, aes(x = DisplayDay)) +
  geom_rect(data = phase_data[phase == "Freeze"],
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "#6A96E1", colour = NA, alpha = 0.20) +
  geom_rect(data = phase_data[phase == "Thaw"],
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "#EEA9BE", colour = NA, alpha = 0.20) +
  geom_blank(data = axis_training, aes(x = Day, y = axis_value), inherit.aes = FALSE)
for (draw_key in draw_order) {
  group_data <- copy(plot_data[group_key == draw_key])
  plot_object <- plot_object +
    geom_ribbon(data = group_data,
                aes(ymin = q025_percent, ymax = q975_percent, fill = group_key, group = group_key),
                alpha = ribbon_alpha, colour = NA, show.legend = FALSE) +
    geom_line(data = group_data, aes(y = mean_percent, colour = group_key, group = group_key),
              alpha = line_alpha, linewidth = 0.36, lineend = "round")
}
plot_object <- plot_object +
  facet_grid(rows = vars(metric_label), cols = vars(duration), scales = "free_y",
             switch = "y", drop = FALSE) +
  scale_fill_manual(values = group_colours, breaks = group_levels, guide = "none") +
  scale_colour_manual(values = group_colours, breaks = group_levels, labels = group_labels, name = NULL) +
  scale_x_continuous(breaks = c(0, 48, 96), expand = expansion(mult = 0.05)) +
  coord_cartesian(xlim = c(0, 96)) +
  scale_y_continuous(breaks = local({
    row_axis_table <- copy(y_limits)
    function(limits) {
      nominal_span <- diff(limits) / 1.10
      nominal_upper <- limits[[2L]] - 0.05 * nominal_span
      row_index <- which.min(abs(row_axis_table$axis_upper - nominal_upper))
      c(0, row_axis_table$tick_step[[row_index]], 2 * row_axis_table$tick_step[[row_index]])
    }
  }), labels = format_y_values, expand = expansion(mult = 0.05)) +
  labs(x = "Incubation days", y = NULL) +
  guides(colour = guide_legend(nrow = 1L, override.aes = list(linewidth = 0.7, alpha = line_alpha))) +
  theme_bw(base_size = 7.0 * font_scale, base_family = font_family) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.38),
    axis.line = element_blank(),
    axis.ticks = element_line(linewidth = 0.32, colour = "black"),
    axis.ticks.length = grid::unit(1.1, "mm"),
    axis.text = element_text(size = 6.3 * font_scale, colour = "black", family = font_family),
    axis.title.x = element_text(size = 7.6 * font_scale, family = font_family, margin = margin(t = 2.5)),
    strip.background = element_blank(), strip.placement = "outside",
    strip.text.x = element_text(size = 7.7 * font_scale, face = "plain", family = font_family,
                                margin = margin(b = 1.5)),
    strip.text.y.left = element_text(angle = 90, size = 6.7 * font_scale, face = "plain",
                                     hjust = 0.5, vjust = 0.5, lineheight = 0.95,
                                     family = font_family, margin = margin(r = 2.0)),
    panel.spacing.x = grid::unit(1.1, "mm"), panel.spacing.y = grid::unit(1.0, "mm"),
    legend.position = "top", legend.direction = "horizontal",
    legend.text = element_text(size = 7.7 * font_scale, family = font_family),
    legend.key.width = grid::unit(7, "mm"), legend.key.height = grid::unit(4, "mm"),
    legend.margin = margin(0, 0, 2, 0, unit = "mm"),
    legend.background = element_blank(), legend.key = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(4, 4, 4, 5, unit = "mm")
  )

ggsave("Your_pathway_outputs/Fig_S8/Fig_S8.png", plot_object, device = ragg::agg_png,
       width = figure_width_mm, height = figure_height_mm, units = "mm", dpi = figure_dpi,
       bg = "white", create.dir = TRUE)
