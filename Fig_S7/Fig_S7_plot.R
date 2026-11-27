figure_data <- readRDS("Your_pathway/Fig_S7/Fig_S7_data.rds")

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
})

posterior_parameters <- c(
  b_D = "doc_breakup_0", b_S = "soc_breakup_0", f_iso = "isolation_fraction",
  v_upt = "doc_uptake_rate", v_cat = "soc_catalysis_rate", f_dorm = "dormancy_fraction",
  r_frz = "freeze_mortality_rate", r_death = "microbial_death_rate", r_prod = "enzyme_production_rate"
)
group_columns <- c("ID", "treatment", "depth")
posterior <- as.data.table(figure_data$posterior_draws)[,
  c(group_columns, unname(posterior_parameters)), with = FALSE]
setnames(posterior, unname(posterior_parameters), names(posterior_parameters))

slopes <- as.data.table(figure_data$lhs_endpoints)[, .(
  ID, treatment, depth,
  "k_D,sor" = (doc_sorption_10 - doc_sorption_minus10) / 20,
  "k_D,des" = (doc_desorption_10 - doc_desorption_minus10) / 20,
  "k_S,sor" = (soc_protection_10 - soc_protection_minus10) / 20,
  "k_S,des" = (soc_release_10 - soc_release_minus10) / 20,
  e_slope = (measured_cue_10 - cue_minus10) / 20
)]
samples <- rbind(melt(posterior, id.vars = group_columns, variable.factor = FALSE),
                 melt(slopes, id.vars = group_columns, variable.factor = FALSE))

parameter_summary <- samples[, {
  n <- .N
  average <- mean(value)
  half_width <- qt(0.975, n - 1L) * sd(value) / sqrt(n)
  list(n = n, mean = average, ci_lower = average - half_width, ci_upper = average + half_width)
}, by = .(treatment, depth, variable)]

treatment_levels <- c("LFLT", "LFST", "SFLT", "SFST")
colour_values <- c(LFLT = "#d9b0ff", LFST = "#5985d0", SFLT = "#e994ae", SFST = "#9ed69e")
depths <- c("0-10", "70-80")
linetype_values <- c("0-10" = "solid", "70-80" = "dashed")
position_depth <- position_dodge(width = 0.35)

rate_unit <- "plain(g)~plain(g)^{-1}~plain(h)^{-1}"
slope_unit <- paste0(rate_unit, "~degree*C^{-1}")
facet_label <- function(symbol, unit) {
  paste0("atop(italic(", symbol, "), paste('(', ", unit, ", ')'))")
}
parameter_labels <- list(
  physical = c(
    b_D = facet_label("b[D]", rate_unit),
    b_S = facet_label("b[S]", rate_unit),
    f_iso = "atop(italic(f[iso]), '(-)')",
    "k_D,sor" = facet_label("k[D * ',' * sor]", slope_unit),
    "k_D,des" = facet_label("k[D * ',' * des]", slope_unit),
    "k_S,sor" = facet_label("k[S * ',' * sor]", slope_unit),
    "k_S,des" = facet_label("k[S * ',' * des]", slope_unit)
  ),
  physiological = c(
    v_upt = facet_label("v[upt]", rate_unit),
    v_cat = facet_label("v[cat]", rate_unit),
    f_dorm = "atop(italic(f[dorm]), '(-)')",
    r_frz = facet_label("r[frz]", rate_unit),
    e_slope = facet_label("e[slope]", "degree*C^{-1}"),
    r_death = facet_label("r[death]", rate_unit),
    r_prod = facet_label("r[prod]", rate_unit)
  )
)

make_parameter_plot <- function(title, variable_labels) {
  plot_data <- parameter_summary[variable %in% names(variable_labels)]
  plot_data[, treatment_factor := factor(treatment, levels = treatment_levels)]
  plot_data[, depth_factor := factor(depth, levels = depths)]
  plot_data[, facet_factor := factor(variable, levels = names(variable_labels))]
  ggplot(plot_data, aes(x = treatment_factor, y = mean)) +
    geom_line(aes(group = depth_factor, linetype = depth_factor),
              colour = "grey50", linewidth = 0.7, position = position_depth) +
    geom_linerange(aes(ymin = ci_lower, ymax = ci_upper, colour = treatment_factor, group = depth_factor),
                   linewidth = 0.7, position = position_depth, show.legend = FALSE) +
    geom_point(aes(colour = treatment_factor, group = depth_factor),
               size = 3, position = position_depth, show.legend = FALSE) +
    facet_grid(rows = vars(facet_factor), scales = "free_y", switch = "y", drop = TRUE,
               labeller = labeller(facet_factor = as_labeller(variable_labels, default = label_parsed))) +
    scale_colour_manual(values = colour_values, drop = FALSE) +
    scale_linetype_manual(values = linetype_values, breaks = depths, labels = depths, drop = FALSE) +
    scale_x_discrete(drop = FALSE, expand = expansion(add = 0.32)) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) +
    labs(title = title, x = NULL, y = NULL, linetype = "Soil depth (cm)") +
    theme_bw(base_size = 10.5, base_family = "Times New Roman") +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "#333333", fill = NA, linewidth = 0.55),
      panel.spacing.y = grid::unit(0.5, "mm"),
      axis.text.x = element_text(size = 10.5, colour = "black", margin = margin(t = 0.8, unit = "mm")),
      axis.text.y = element_text(size = 9.2, colour = "black", margin = margin(r = 0.6, unit = "mm")),
      axis.ticks = element_line(linewidth = 0.45, colour = "#333333"),
      axis.ticks.length = grid::unit(1, "mm"),
      strip.background = element_blank(), strip.placement = "outside",
      strip.text.y.left = element_text(angle = 90, size = 9.8, colour = "black",
                                       margin = margin(0, 1.2, 0, 0, unit = "mm")),
      plot.title = element_text(size = 14, face = "plain", hjust = 0.5, margin = margin(b = 1.5, unit = "mm")),
      legend.position = "bottom", legend.direction = "horizontal",
      legend.title = element_text(size = 10.5), legend.text = element_text(size = 9.8),
      legend.key.width = grid::unit(10, "mm"), legend.key.height = grid::unit(4, "mm"),
      plot.margin = margin(3, 2, 2, 3, unit = "mm")
    )
}
physical_plot <- make_parameter_plot("Physical parameters", parameter_labels$physical)
physiological_plot <- make_parameter_plot("Physiological parameters", parameter_labels$physiological) +
  theme(legend.position = "none")
figure_plot <- physical_plot + physiological_plot + plot_layout(ncol = 2, widths = c(1, 1), guides = "keep")

ggsave("Your_pathway_outputs/Fig_S7/Fig_S7.png", figure_plot, device = ragg::agg_png,
       width = 176.48, height = 272.39, units = "mm", dpi = 600, bg = "white", create.dir = TRUE)
