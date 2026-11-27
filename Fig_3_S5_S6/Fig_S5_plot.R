plot_data <- readRDS("Your_pathway/Fig_3_S5_S6/Fig_S5_data.rds")

library(ggplot2)
library(patchwork)

parameter_notation <- function(x) {
  symbol <- sub("_([^_]+).*", "[\\1]", as.character(x))
  symbol <- sub(",", "*','*", symbol, fixed = TRUE)
  paste0("italic(", symbol, ")")
}

colours <- c("Physical protection" = "#E994AE", "Microbial physiology" = "#A0BFF6",
             "Initial state variables" = "#AFD1AF")
groups <- levels(plot_data$importance$group)
tags <- c("a", "b", "", "c", "", "", "", "d", "")
group_panel <- function(i) {
  d <- subset(plot_data$importance, group == groups[i])
  performance <- subset(plot_data$performance, group == groups[i])
  d$predictor <- factor(d$predictor, levels = rev(d$predictor[order(d$class, -d$mean_percent)]))
  ggplot(d, aes(mean_percent, predictor, fill = class)) +
    geom_col() +
    geom_errorbar(aes(xmin = q025_percent, xmax = q975_percent), orientation = "y", width = 0.15) +
    geom_text(aes(x = q975_percent, label = sprintf("%.1f", mean_percent)), hjust = -0.15, size = 2.6) +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
    expand_limits(x = 60) +
    scale_y_discrete(labels = function(x) parse(text = parameter_notation(x))) +
    scale_fill_manual(NULL, values = colours) +
    labs(title = groups[i], subtitle = sprintf("R\u00b2 %.2f\u2013%.2f", performance$R2_min, performance$R2_max),
         x = "Relative importance, %", y = NULL, tag = tags[i]) +
    theme_bw(base_size = 9) + theme(panel.grid = element_blank())
}
figure <- wrap_plots(lapply(seq_along(groups), group_panel), design = "ABC#\nDEFG\nHI##",
                     guides = "collect") & theme(legend.position = "bottom")

ggsave("Your_pathway_outputs/Fig_S5/Fig_S5.png", figure,
       width = 14, height = 12, units = "in", dpi = 300, bg = "white", create.dir = TRUE)
