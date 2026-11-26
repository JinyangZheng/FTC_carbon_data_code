plot_data <- readRDS("Your_pathway/Fig_S4/Fig_S4_data.rds")

library(ggplot2)
library(patchwork)

regime_colours <- c(LFLT = "#D9B0FF", LFST = "#5985D0", SFLT = "#E994AE", SFST = "#9ED69E")
units <- c(Rs = "\u00b5g CO\u2082-C g\u207b\u00b9 SOC\u2080 h\u207b\u00b9",
           EEA = "\u00b5g C g\u207b\u00b9 SOC\u2080 h\u207b\u00b9",
           DOC = "\u00b5g DOC-C g\u207b\u00b9 SOC\u2080", MBC = "\u00b5g MBC-C g\u207b\u00b9 SOC\u2080")
style <- theme_bw(base_size = 9) + theme(panel.grid = element_blank(), legend.position = "bottom")
model_levels <- levels(plot_data$scores$model)

relationship_row <- function(period_name, response_name) {
  select <- function(d) d[d$period == period_name & d$response == response_name, ]
  points <- select(plot_data$points)
  fits <- select(plot_data$regressions)
  labels <- select(plot_data$r_squared)
  limits <- range(points$modelled_log, points$observed_log, fits$y_start, fits$y_end)
  ggplot(points, aes(modelled_log, observed_log)) +
    geom_abline(slope = 1, intercept = 0, colour = "#C74136") +
    geom_point(aes(colour = regime), alpha = 0.4, size = 0.8) +
    geom_segment(data = fits, aes(x = x_start, y = y_start, xend = x_end, yend = y_end, colour = regime),
                 inherit.aes = FALSE, show.legend = FALSE) +
    geom_text(data = labels, aes(x = -Inf, y = Inf, colour = regime, vjust = as.integer(regime),
              label = sprintf("R\u00b2 %.2f \u00b1 %.2f", mean_R2, sd_R2)),
              inherit.aes = FALSE, hjust = 0, size = 2.4, show.legend = FALSE) +
    facet_grid(~model) + coord_cartesian(xlim = limits, ylim = limits) +
    scale_colour_manual(NULL, values = regime_colours) + style +
    labs(x = paste("Modelled", response_name, "\n", units[[response_name]]),
         y = paste("Observed", response_name, "\n", units[[response_name]]))
}

metric_panels <- function(period_name) {
  scores <- subset(plot_data$scores, period == period_name)
  means <- subset(plot_data$score_means, period == period_name)
  letters <- subset(plot_data$letters, period == period_name)
  label_format <- rep("%.0f", nrow(means))
  label_format[means$metric == "RMSE"] <- "%.2f"
  means$label <- sprintf(label_format, means$mean)
  ggplot(scores, aes(y = value)) +
    stat_density(aes(group = model, xmin = after_stat(group), xmax = after_stat(group + 0.4 * scaled)),
                 geom = "ribbon", orientation = "y", position = "identity", adjust = 1.2,
                 fill = NA, colour = "grey35", linewidth = 0.35, outline.type = "full") +
    geom_point(aes(x = as.integer(model), colour = regime), size = 0.45, alpha = 0.1, show.legend = FALSE,
               position = position_jitter(width = 0.07, height = 0, seed = 1)) +
    geom_boxplot(aes(x = as.integer(model), group = model), width = 0.12,
                 outlier.shape = NA, fill = NA, linewidth = 0.4) +
    geom_text(data = means, aes(x = as.integer(model), y = Inf, label = label, colour = regime,
              vjust = as.integer(regime)), inherit.aes = FALSE, size = 2.5, show.legend = FALSE) +
    geom_text(data = letters, aes(x = as.integer(model), y = -Inf, label = letter),
              inherit.aes = FALSE, vjust = 0, size = 3) +
    facet_grid(metric ~ ., scales = "free_y", switch = "y") +
    scale_x_continuous(breaks = seq_along(model_levels), labels = c("Base", "Physio", "Physic", "Full"),
                       limits = c(0.5, 4.5)) +
    scale_y_continuous(expand = expansion(mult = c(0.08, 0.32))) +
    scale_colour_manual(NULL, values = regime_colours) + style + labs(x = NULL, y = NULL)
}

period_panel <- function(period_name, response_names, tag) {
  rows <- lapply(response_names, function(name) relationship_row(period_name, name))
  rows[[1]] <- rows[[1]] + labs(tag = tag, title = period_name)
  relationships <- wrap_plots(rows, ncol = 1)
  wrap_plots(relationships, metric_panels(period_name), widths = c(3, 1))
}
calibration <- period_panel("Calibration", c("Rs", "EEA", "DOC", "MBC"), "a")
validation <- period_panel("Validation", c("Rs", "EEA", "DOC"), "b")
figure <- (calibration / validation + plot_layout(heights = c(4, 3), guides = "collect")) &
  theme(legend.position = "bottom")

ggsave("Your_pathway_outputs/Fig_S4/Fig_S4.png", figure,
       width = 15, height = 18, units = "in", dpi = 300, bg = "white", create.dir = TRUE)
