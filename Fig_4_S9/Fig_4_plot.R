plot_data <- readRDS("Your_pathway/Fig_4_S9/Fig_4_data.rds")

library(ggplot2)
library(patchwork)

phase_colours <- c(Freeze = "#D9E6F7", Thaw = "#F8E2EA")
parameter_colours <- c(b_D = "#E994AE", f_iso = "#41B6C4", v_upt = "#F1AA7B",
                       f_dorm = "#6A96E1", r_frz = "#253494")
group_colours <- c("Physical protection" = "#E994AE", "Microbial physiology" = "#A0BFF6",
                   "State variables" = "#AFD1AF")
parameter_labels <- expression(italic(b[D]), italic(f[iso]), italic(v[upt]), italic(f[dorm]), italic(r[frz]))
days <- scale_x_continuous(breaks = c(0, 48, 96))
style <- theme_bw(base_size = 10) + theme(panel.grid = element_blank(), legend.position = "bottom")

performance <- ggplot(plot_data$performance, aes(Day, mean)) +
  geom_rect(data = plot_data$phases, aes(xmin = start_day, xmax = end_day, fill = phase),
            ymin = -Inf, ymax = Inf, inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = q025, ymax = q975), fill = "grey60", alpha = 0.4) +
  geom_line() + scale_fill_manual(values = phase_colours, guide = "none") +
  facet_grid(~regime) + days + style +
  labs(x = NULL, y = expression("Emulator performance ("*R^2*")"), tag = "a")

predictors <- ggplot(plot_data$predictors, aes(Day, mean, colour = predictor, fill = predictor)) +
  geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.18, colour = NA) + geom_line() +
  facet_grid(process ~ regime, switch = "y") +
  scale_colour_manual(NULL, values = parameter_colours, breaks = names(parameter_colours), labels = parameter_labels) +
  scale_fill_manual(values = parameter_colours, guide = "none") + days + style +
  labs(x = NULL, y = "Relative importance, %", tag = "b")

components <- ggplot(plot_data$components, aes(Day, mean, colour = component, fill = component)) +
  geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.2, colour = NA) + geom_line() +
  geom_segment(data = plot_data$phases, aes(x = start_day, xend = end_day, colour = phase),
               y = -Inf, yend = -Inf, linewidth = 3, inherit.aes = FALSE, show.legend = FALSE) +
  facet_grid(~regime) + days + style +
  scale_colour_manual(NULL, values = c(group_colours, phase_colours), breaks = names(group_colours)) +
  scale_fill_manual(values = group_colours, guide = "none") +
  labs(x = "Incubation time, d", y = "Group-level importance, %", tag = "c")
figure <- performance / predictors / components + plot_layout(heights = c(1, 2, 1))

ggsave("Your_pathway_outputs/Fig_4/Fig_4.png", figure,
       width = 10, height = 10, units = "in", dpi = 300, bg = "white", create.dir = TRUE)
