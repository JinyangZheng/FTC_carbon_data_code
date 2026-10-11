library(ggplot2)
library(patchwork)
library(ragg)

source_files <- Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
script_argument <- grep('^--file=', commandArgs(FALSE), value = TRUE)
script_path <- if (length(source_files)) tail(source_files, 1L)[[1L]] else
  if (length(script_argument)) sub('^--file=', '', script_argument[1L]) else 'Fig_S6_plot.R'
code_dir <- dirname(normalizePath(script_path, winslash = '/', mustWork = FALSE))
data <- readRDS(file.path(code_dir, 'Fig_S6_data.rds'))

colour_values <- c(LFLT = '#d9b0ff', LFST = '#5985d0', SFLT = '#e994ae', SFST = '#9ed69e')
treatment_levels <- c('LFLT', 'LFST', 'SFLT', 'SFST')
depths <- c('0-10', '70-80')
linetype_values <- c('0-10' = 'solid', '70-80' = 'dashed')
depth_legend_labels <- c('0-10', '70-80')
position_depth <- position_dodge(width = 0.35)
parameter_labels <- list(physical = c(b_D = "atop(italic(b[D]), paste('(', plain(g)~plain(g)^{-1}~plain(h)^{-1}, ')'))", 
b_S = "atop(italic(b[S]), paste('(', plain(g)~plain(g)^{-1}~plain(h)^{-1}, ')'))", 
f_iso = "atop(italic(f[iso]), '(-)')", "k_D,sor" = "atop(italic(k[D * ',' * sor]), paste('(', plain(g)~plain(g)^{-1}~plain(h)^{-1}, ')'))", 
"k_D,des" = "atop(italic(k[D * ',' * des]), paste('(', plain(g)~plain(g)^{-1}~plain(h)^{-1}, ')'))", 
"k_S,sor" = "atop(italic(k[S * ',' * sor]), paste('(', plain(g)~plain(g)^{-1}~plain(h)^{-1}, ')'))", 
"k_S,des" = "atop(italic(k[S * ',' * des]), paste('(', plain(g)~plain(g)^{-1}~plain(h)^{-1}, ')'))"
), physiological = c(v_upt = "atop(italic(v[upt]), paste('(', plain(g)~plain(g)^{-1}~plain(h)^{-1}, ')'))", 
v_cat = "atop(italic(v[cat]), paste('(', plain(g)~plain(g)^{-1}~plain(h)^{-1}, ')'))", 
f_dorm = "atop(italic(f[dorm]), '(-)')", r_frz = "atop(italic(r[frz]), paste('(', plain(g)~plain(g)^{-1}~plain(h)^{-1}, ')'))", 
e_slope = "atop(italic(e[slope]), '(-)')", r_death = "atop(italic(r[death]), paste('(', plain(g)~plain(g)^{-1}~plain(h)^{-1}, ')'))", 
r_prod = "atop(italic(r[prod]), paste('(', plain(g)~plain(g)^{-1}~plain(h)^{-1}, ')'))"
))

make_parameter_plot <- function(data, title, variable_labels) {
    data$treatment_factor <- factor(data$treatment, levels = treatment_levels)
    data$depth_factor <- factor(data$depth, levels = depths)
    data$facet_factor <- factor(data$variable, levels = names(variable_labels))
    ggplot2::ggplot(data, ggplot2::aes(x = treatment_factor, y = mean)) + ggplot2::geom_line(ggplot2::aes(group = depth_factor, 
        linetype = depth_factor), colour = "grey50", linewidth = 0.7, position = position_depth) + ggplot2::geom_linerange(ggplot2::aes(ymin = ci_lower, 
        ymax = ci_upper, colour = treatment_factor, group = depth_factor), linewidth = 0.7, position = position_depth, 
        show.legend = FALSE) + ggplot2::geom_point(ggplot2::aes(colour = treatment_factor, group = depth_factor), 
        size = 3, position = position_depth, show.legend = FALSE) + ggplot2::facet_grid(rows = ggplot2::vars(facet_factor), 
        scales = "free_y", switch = "y", drop = TRUE, labeller = ggplot2::labeller(facet_factor = ggplot2::as_labeller(variable_labels, default = ggplot2::label_parsed))) + 
        ggplot2::scale_colour_manual(values = colour_values, drop = FALSE) + ggplot2::scale_linetype_manual(values = linetype_values, 
        breaks = depths, labels = depth_legend_labels, drop = FALSE) + ggplot2::scale_x_discrete(drop = FALSE, 
        expand = ggplot2::expansion(add = 0.32)) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 
        0.3))) + ggplot2::labs(title = title, x = NULL, y = NULL, linetype = "Soil depth (cm)") + ggplot2::theme_bw(base_size = 10.5, 
        base_family = "Times New Roman") + ggplot2::theme(panel.grid = ggplot2::element_blank(), panel.border = ggplot2::element_rect(colour = "#333333", 
        fill = NA, linewidth = 0.55), panel.spacing.y = grid::unit(0.5, "mm"), axis.text.x = ggplot2::element_text(size = 10.5, 
        colour = "black", margin = ggplot2::margin(t = 0.8, unit = "mm")), axis.text.y = ggplot2::element_text(size = 9.2, 
        colour = "black", margin = ggplot2::margin(r = 0.6, unit = "mm")), axis.ticks = ggplot2::element_line(linewidth = 0.45, 
        colour = "#333333"), axis.ticks.length = grid::unit(1, "mm"), strip.background = ggplot2::element_blank(), 
        strip.placement = "outside", strip.text.y.left = ggplot2::element_text(angle = 90, size = 9.8, colour = "black", 
            margin = ggplot2::margin(0, 1.2, 0, 0, unit = "mm")), plot.title = ggplot2::element_text(size = 14, 
            face = "plain", hjust = 0.5, margin = ggplot2::margin(b = 1.5, unit = "mm")), legend.position = "bottom", 
        legend.direction = "horizontal", legend.title = ggplot2::element_text(size = 10.5), legend.text = ggplot2::element_text(size = 9.8), 
        legend.key.width = grid::unit(10.00, "mm"), legend.key.height = grid::unit(4.00, "mm"), plot.margin = ggplot2::margin(3, 
            2, 2, 3, unit = "mm"))
}
physical_plot <- make_parameter_plot(data$physical, 'Physical parameters', parameter_labels$physical)
physiological_plot <- make_parameter_plot(data$physiological, 'Physiological parameters', parameter_labels$physiological) +
  theme(legend.position = 'none')
figure_plot <- physical_plot + physiological_plot + plot_layout(ncol = 2, widths = c(1, 1), guides = 'keep')

arguments <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(arguments)) arguments[1L] else Sys.getenv('FTC_OUTPUT_DIR',
  unset = file.path(dirname(dirname(code_dir)), 'version2_outputs', 'Fig_S6'))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(output_dir, 'Fig_S6.png'), figure_plot, device = ragg::agg_png,
  width = 176.48, height = 272.39, units = 'mm', dpi = 300, bg = 'white')
