plot_data <- readRDS("Your_pathway/Fig_S10/Fig_S10_data.rds")

library(ggplot2)
library(patchwork)

group_section <- function(group) {
  sections <- c("Overall", rep("Soil depth", 2), rep("FTC regimes", 4), rep("FTC phases", 2))
  factor(sections[as.integer(group)], levels = unique(sections))
}

components <- levels(plot_data$cumulative$component)
colours <- setNames(c("#E994AE", "#A0BFF6", "#AFD1AF", "#688EA9", "#A695B1", "#A9B783"), components)
phase_colours <- c(Freeze = "#D9E6F7", Thaw = "#F8E2EA")
style <- theme_bw(base_size = 10) + theme(panel.grid = element_blank(), legend.position = "bottom")
dodge <- position_dodge(width = 0.85)
bars <- plot_data$cumulative
bars$section <- group_section(bars$group)

cumulative <- ggplot(bars, aes(group, mean, group = component, fill = component)) +
  geom_col(position = dodge, width = 0.8) +
  geom_errorbar(aes(ymin = q025, ymax = q975), position = dodge, width = 0.1) +
  geom_text(aes(y = q975, label = round(mean)), position = dodge, vjust = -0.4, size = 2.7) +
  facet_grid(cols = vars(section), scales = "free_x", space = "free_x") +
  scale_fill_manual(NULL, values = colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  guides(fill = guide_legend(ncol = 2)) + style +
  labs(x = NULL, y = "Group-level importance, %", tag = "a")

instantaneous <- ggplot(plot_data$temporal, aes(Day, mean, colour = component, fill = component)) +
  geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.18, colour = NA) + geom_line() +
  geom_segment(data = plot_data$phases, aes(x = start_day, xend = end_day, colour = phase),
               y = -Inf, yend = -Inf, linewidth = 3, inherit.aes = FALSE, show.legend = FALSE) +
  facet_grid(effect ~ regime, scales = "free_y", switch = "y") +
  scale_x_continuous(breaks = c(0, 48, 96)) + expand_limits(y = 0) +
  scale_colour_manual(NULL, values = c(colours, phase_colours), breaks = components) +
  scale_fill_manual(values = colours, guide = "none") +
  guides(colour = guide_legend(ncol = 2)) + style +
  labs(x = "Incubation time, d", y = "Group-level importance, %", tag = "b")
figure <- cumulative / instantaneous + plot_layout(heights = c(1, 1.5))

ggsave("Your_pathway_outputs/Fig_S10/Fig_S10.png", figure,
       width = 12, height = 9, units = "in", dpi = 300, bg = "white", create.dir = TRUE)
