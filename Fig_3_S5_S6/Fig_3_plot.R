plot_data <- readRDS("Your_pathway/Fig_3_S5_S6/Fig_3_data.rds")

library(ggplot2)
library(patchwork)

group_section <- function(group) {
  sections <- c("Overall", rep("Soil depth", 2), rep("FTC regimes", 4), rep("FTC phases", 2))
  factor(sections[as.integer(group)], levels = unique(sections))
}

colours <- c("Physical protection" = "#E994AE", "Microbial physiology" = "#A0BFF6",
             "Initial state variables" = "#AFD1AF")
panels <- lapply(plot_data, function(values) {
  values$section <- group_section(values$group)
  values
})
sections <- facet_grid(cols = vars(section), scales = "free_x", space = "free_x")
style <- theme_bw(base_size = 10) + theme(panel.grid = element_blank(), legend.position = "bottom")

performance <- ggplot(panels$performance,
  aes(group, ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax)) +
  geom_boxplot(stat = "identity", width = 0.35) + sections + style +
  labs(x = NULL, y = expression("Emulator performance ("*R^2*")"), tag = "a")

top <- panels$predictors
predictor_labels <- function(groups) {
  symbols <- c(b_D = "italic(b[D])", v_upt = "italic(v[upt])")
  parameter <- top$predictor[match(groups, top$group)]
  parse(text = paste0("atop(plain('", groups, "'),", symbols[parameter], ")"))
}
predictors <- ggplot(top, aes(group, mean_percent, fill = class)) +
  geom_col(width = 0.6) + geom_errorbar(aes(ymin = q025_percent, ymax = q975_percent), width = 0.12) +
  geom_text(aes(y = q975_percent, label = round(mean_percent)), vjust = -0.4, size = 3) +
  scale_fill_manual(values = colours, guide = "none") + scale_x_discrete(labels = predictor_labels) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + sections + style +
  labs(x = NULL, y = "Relative importance, %", tag = "b")

dodge <- position_dodge(width = 0.8)
components <- ggplot(panels$components, aes(group, mean_percent, fill = class, group = class)) +
  geom_col(position = dodge, width = 0.75) +
  geom_errorbar(aes(ymin = q025_percent, ymax = q975_percent), position = dodge, width = 0.1) +
  geom_text(aes(y = q975_percent, label = round(mean_percent)), position = dodge, vjust = -0.4, size = 3) +
  scale_fill_manual(NULL, values = colours) + scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  sections + style + labs(x = NULL, y = "Group-level importance, %", tag = "c")
figure <- performance / predictors / components

ggsave("Your_pathway_outputs/Fig_3/Fig_3.png", figure,
       width = 10, height = 9, units = "in", dpi = 300, bg = "white", create.dir = TRUE)
