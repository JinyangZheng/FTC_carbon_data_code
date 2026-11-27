plot_data <- readRDS("Your_pathway/Fig_3_S5_S6/Fig_S6_data.rds")

library(ggplot2)

parameter_notation <- function(x) {
  symbol <- sub("_([^_]+).*", "[\\1]", as.character(x))
  symbol <- sub(",", "*','*", symbol, fixed = TRUE)
  paste0("italic(", symbol, ")")
}

colours <- c("Physical protection" = "#E994AE", "Microbial physiology" = "#A0BFF6")
groups <- levels(plot_data$group)
group_labels <- setNames(paste(c("Overall", rep("Soil depth", 2), rep("FTC regimes", 4),
                                 rep("FTC phases", 2)), groups, sep = "\n"), groups)
figure <- ggplot(plot_data, aes(x_normalized, signed_shap_mean, colour = class, fill = class)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55", linewidth = 0.3) +
  geom_ribbon(aes(ymin = signed_shap_lower, ymax = signed_shap_upper),
              alpha = 0.2, colour = NA, na.rm = TRUE) +
  geom_line(linewidth = 0.4, na.rm = TRUE) +
  facet_grid(predictor ~ group, switch = "y", labeller = labeller(
    predictor = as_labeller(parameter_notation, default = label_parsed), group = group_labels)) +
  scale_colour_manual(NULL, values = colours) + scale_fill_manual(values = colours, guide = "none") +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  labs(x = "Normalized predictor value", y = "Signed SHAP value") +
  theme_bw(base_size = 8) + theme(panel.grid = element_blank(), legend.position = "bottom",
                                   strip.text.y.left = element_text(angle = 0))

ggsave("Your_pathway_outputs/Fig_S6/Fig_S6.png", figure,
       width = 13, height = 14, units = "in", dpi = 300, bg = "white", create.dir = TRUE)
