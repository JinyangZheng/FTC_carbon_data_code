plot_data <- readRDS("Your_pathway/Fig_4_S9/Fig_S9_data.rds")

library(ggplot2)

parameter_notation <- function(x) {
  symbol <- sub("_([^_]+).*", "[\\1]", as.character(x))
  symbol <- sub(",", "*','*", symbol, fixed = TRUE)
  paste0("italic(", symbol, ")")
}

scope_colours <- c("black", "#D9AE84", "#73442E")
names(scope_colours) <- levels(plot_data$importance$scope)
figure <- ggplot(plot_data$importance, aes(Day, mean, group = scope)) +
  geom_rect(data = plot_data$phases, aes(xmin = start_day, xmax = end_day, fill = phase),
            ymin = -Inf, ymax = Inf, inherit.aes = FALSE) +
  geom_hline(yintercept = 20, linetype = "dashed", colour = "grey70", linewidth = 0.3) +
  geom_ribbon(aes(ymin = q025, ymax = q975), fill = "grey55", alpha = 0.18, colour = NA) +
  geom_line(aes(colour = scope), linewidth = 0.4) +
  facet_grid(predictor ~ regime, switch = "y",
             labeller = labeller(predictor = as_labeller(parameter_notation, default = label_parsed))) +
  scale_fill_manual(values = c(Freeze = "#D9E6F7", Thaw = "#F8E2EA"), guide = "none") +
  scale_colour_manual(NULL, values = scope_colours) + scale_x_continuous(breaks = c(0, 48, 96)) +
  labs(x = "Incubation time, d", y = "Relative importance, %") +
  theme_bw(base_size = 8) + theme(panel.grid = element_blank(), legend.position = "top",
                                   strip.text.y.left = element_text(angle = 0))

ggsave("Your_pathway_outputs/Fig_S9/Fig_S9.png", figure,
       width = 9, height = 16, units = "in", dpi = 300, bg = "white", create.dir = TRUE)
