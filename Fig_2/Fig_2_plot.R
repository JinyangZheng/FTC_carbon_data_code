plot_data <- readRDS("Your_pathway/Fig_2/Fig_2_data.rds")

library(ggplot2)

colours <- c(Observed = "grey35", Base = "#E4C8FF", "Physio-only" = "#8BAADE",
             "Physic-only" = "#F0B4C6", Full = "#9ED69E")
response_labels <- c(
  Rs = "Rs\n\u00b5g C g\u207b\u00b9 SOC\u2080 h\u207b\u00b9",
  EEA = "EEA\n\u00b5g C g\u207b\u00b9 SOC\u2080 h\u207b\u00b9",
  DOC = "DOC\nmg C g\u207b\u00b9 SOC\u2080", MBC = "MBC\nmg C g\u207b\u00b9 SOC\u2080",
  CUE = "Microbial carbon\nuse efficiency"
)
scores <- plot_data$rmse
scores <- scores[order(scores$model), ]
scores$value <- sprintf("%.2f", scores$rmse)
scores$value[is.na(scores$rmse)] <- "\u2013"
rows <- aggregate(value ~ response + regime + period, scores, paste, collapse = "   ")
rows$text <- paste(rows$period, rows$value)
labels <- aggregate(text ~ response + regime, rows, paste, collapse = "\n")

figure <- ggplot(plot_data$predictions, aes(day, mean)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = model),
              alpha = 0.20, colour = NA, show.legend = FALSE) +
  geom_line(aes(colour = model), linewidth = 0.4) +
  geom_point(data = plot_data$observations, aes(day, observed_mean, colour = "Observed"),
             inherit.aes = FALSE, shape = 1, size = 1) +
  geom_vline(xintercept = 192, linetype = "dashed", colour = "grey55") +
  geom_text(data = labels, aes(x = -Inf, y = Inf, label = text), inherit.aes = FALSE,
            hjust = 0, vjust = 1, size = 2.7) +
  facet_grid(response ~ regime, scales = "free_y", switch = "y",
             labeller = labeller(response = response_labels)) +
  scale_colour_manual(NULL, values = colours, breaks = names(colours)) +
  scale_fill_manual(values = colours) +
  scale_x_continuous(breaks = c(0, 96, 192, 240)) +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.32))) +
  labs(x = "Incubation time, d", y = NULL) +
  theme_bw(base_size = 10) + theme(panel.grid = element_blank(), legend.position = "top")

ggsave("Your_pathway_outputs/Fig_2/Fig_2.png", figure,
       width = 10, height = 10, units = "in", dpi = 300, bg = "white", create.dir = TRUE)
