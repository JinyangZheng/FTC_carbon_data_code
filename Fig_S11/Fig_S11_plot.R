plot_data <- readRDS("Your_pathway/Fig_S11/Fig_S11_data.rds")

library(ggplot2)

scopes <- levels(plot_data$dominance$scope)
row_labels <- setNames(paste(letters[seq_along(scopes)], scopes, sep = "   "), scopes)
figure <- ggplot(plot_data$dominance, aes(day, mean_D)) +
  geom_rect(data = plot_data$phases, aes(xmin = start_day, xmax = end_day, fill = phase),
            ymin = -Inf, ymax = Inf, inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = lower_D, ymax = upper_D), fill = "grey55", alpha = 0.35) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey55") +
  geom_line(colour = "grey25") +
  facet_grid(scope ~ regime, labeller = labeller(scope = row_labels), switch = "y") +
  scale_fill_manual(values = c(Freeze = "#D9E6F7", Thaw = "#F8E2EA"), guide = "none") +
  scale_x_continuous(breaks = c(0, 48, 96), expand = expansion(0)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  labs(x = "Incubation time, d", y = "Dominance index") +
  theme_bw(base_size = 10) + theme(panel.grid = element_blank())

ggsave("Your_pathway_outputs/Fig_S11/Fig_S11.png", figure,
       width = 9, height = 6, units = "in", dpi = 300, bg = "white", create.dir = TRUE)
