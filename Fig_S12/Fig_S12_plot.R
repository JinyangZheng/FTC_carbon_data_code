plot_data <- readRDS("Your_pathway/Fig_S12/Fig_S12_data.rds")

library(ggplot2)

colours <- c("Physical protection" = "#F3BECE", "Microbial physiology" = "#A0BFF6",
             "State variables" = "#AFD1AF")
phase_colours <- c(Freeze = "#D9E6F7", Thaw = "#F8E2EA")
figure <- ggplot(plot_data$importance, aes(Day, mean, colour = group, fill = group)) +
  geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.22, colour = NA) + geom_line() +
  geom_segment(data = plot_data$phases, aes(x = start_day, xend = end_day, colour = phase),
               y = -Inf, yend = -Inf, linewidth = 3, inherit.aes = FALSE, show.legend = FALSE) +
  facet_grid(scope ~ regime, switch = "y") +
  scale_colour_manual(NULL, values = c(colours, phase_colours), breaks = names(colours)) +
  scale_fill_manual(values = colours, guide = "none") +
  scale_x_continuous(breaks = c(0, 48, 96)) + expand_limits(y = 0) +
  labs(x = "Incubation time, d", y = "Group-level importance, %") +
  theme_bw(base_size = 10) + theme(panel.grid = element_blank(), legend.position = "top")

ggsave("Your_pathway_outputs/Fig_S12/Fig_S12.png", figure,
       width = 9, height = 5, units = "in", dpi = 300, bg = "white", create.dir = TRUE)
