# Fig_S13 plotting code
# Input tables are stored together in Fig_S13_data.rds.
.script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.script_path <- if (length(.script_files)) tail(.script_files, 1L)[[1L]] else if (length(.script_arg)) sub("^--file=", "", .script_arg[[1L]]) else "Fig_S13_plot.R"
code_dir <- dirname(normalizePath(.script_path, winslash = "/", mustWork = TRUE))
.figure_data <- readRDS(file.path(code_dir, "Fig_S13_data.rds"))
data_dir <- code_dir
output_dir <- file.path(code_dir, "output")
figure_dir <- output_dir
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
.tidy_code_dir <- code_dir
.plot_file_exists <- function(path) basename(path) %in% names(.figure_data) | file.exists(path)
options(stringsAsFactors = FALSE)

figs11_assert <- function(condition, message) {
    if (!isTRUE(condition))
        stop(message, call. = FALSE)
}

figs11_require_packages <- function(packages) {
    missing <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1L))]
    if (length(missing)) {
        stop("Missing R packages  ", paste(missing, collapse = " | "), call. = FALSE)
    }
    invisible(packages)
}

figs11_treatment_specs <- function() {
    data.frame(treatment = c("LFLT", "LFST", "SFLT", "SFST"), treatment_order = 1:4, colour = c("#d9b0ff", "#5985d0",
        "#e994ae", "#9ed69e"), stringsAsFactors = FALSE)
}

figs11_depth_specs <- function() {
    data.frame(source_depth_code = c("0-10 cm", "50-100 cm"), depth_display = c("0-10 cm", "70-80 cm"), depth_order = 1:2,
        linetype = c("solid", "dashed"), stringsAsFactors = FALSE)
}

figs11_variable_specs <- function() {
    data.frame(variable = c("BDOCp", "BSOCp", "fisolation", "CDOCsorption", "CDOCdesorption", "CSOCsorption", "CSOCdesorption",
        "V1", "V2", "fdormancy", "rfreeze.death", "CUE", "rdeath", "rproduction"), variable_order = c(1:7, 1:7),
        class = c(rep("Physical parameters", 7L), rep("Physiological parameters", 7L)), class_order = c(rep(1L,
            7L), rep(2L, 7L)), source_table = c(rep("selected native posterior", 3L), rep("time-integrated LHS-derived predictor",
            4L), rep("selected native posterior", 4L), "time-integrated LHS-derived predictor", rep("selected native posterior",
            2L)), source_column = c("intercept_P.DOC", "intercept_P.NOC", "f1.f2", "CDOCsorption", "CDOCdesorption",
            "CSOCsorption", "CSOCdesorption", "V1", "V2", "dormancy.ratio", "MBC.D.ice", "CUE", "MBC.D", "Enz.P"),
        transformation = c("exp", "exp", "identity", rep("identity", 4L), "exp", "exp", "identity", "exp", "identity",
            "exp", "exp"), display = c("b_D", "b_S", "f_iso", "k_D,sor", "k_D,des", "k_S,sor", "k_S,des", "v_upt",
            "v_cat", "f_dorm", "r_frz", "CUE_slope", "r_death", "r_prod"), plotmath_label = c("italic(b[D])", "italic(b[S])",
            "italic(f[iso])", "italic(k[D * ',' * sor])", "italic(k[D * ',' * des])", "italic(k[S * ',' * sor])",
            "italic(k[S * ',' * des])", "italic(v[upt])", "italic(v[cat])", "italic(f[dorm])", "italic(r[frz])",
            "italic(CUE[slope])", "italic(r[death])", "italic(r[prod])"), unit = c(rep("g g^-1 h^-1", 2L), "dimensionless",
            rep("g g^-1 h^-1", 4L), rep("g g^-1 h^-1", 2L), "dimensionless", "g g^-1 h^-1", "dimensionless", rep("g g^-1 h^-1",
                2L)), stringsAsFactors = FALSE)
}

figs11_export_plot <- function(plot, base_path, width_mm = 176.48, height_mm = 272.39, dpi = 300L, font_family = "Times New Roman") {
    figs11_require_packages("ragg")
    width_in <- width_mm/25.4
    height_in <- height_mm/25.4
    grDevices::cairo_pdf(paste0(base_path, ".pdf"), width = width_in, height = height_in, family = font_family,
        bg = "white")
    tryCatch(print(plot), finally = grDevices::dev.off())
    ragg::agg_png(paste0(base_path, ".png"), width = width_in, height = height_in, units = "in", res = dpi, background = "white")
    tryCatch(print(plot), finally = grDevices::dev.off())
    invisible(paste0(base_path, c(".pdf", ".png")))
}

figs11_sha256 <- function(path) {
    figs11_require_packages("digest")
    figs11_assert(.plot_file_exists(path), paste0("Missing file  ", path))
    unname(digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE))
}

figs11_paths <- function() list(summary_data = file.path(data_dir, "FigS11_summary_95CI.csv"),
  plot_contract = file.path(output_dir, "plot_contract.csv"), figure_base = file.path(output_dir, "Fig_S13"))
figs11_ensure_directories <- function(paths) invisible(paths)
figs11_write_csv <- function(object, path) data.table::fwrite(object, path)


figs11_require_packages(c(
  "data.table", "ggplot2", "patchwork", "ragg", "scales"
))
paths <- figs11_paths()
figs11_ensure_directories(paths)
figs11_assert(.plot_file_exists(paths$summary_data), "Missing FigS11 summary data")

summary_data <- data.table::copy(.figure_data[[basename(paths$summary_data)]])
treatments <- figs11_treatment_specs()
depths <- figs11_depth_specs()
variables <- figs11_variable_specs()

required_columns <- c(
  "class", "class_order", "variable", "variable_order",
  "plotmath_label", "unit", "treatment", "treatment_order",
  "source_depth_code", "depth_display", "depth_order",
  "n", "mean", "ci_lower", "ci_upper"
)
figs11_assert(
  all(required_columns %in% names(summary_data)),
  "FigS11 summary data are missing required columns"
)
figs11_assert(
  nrow(summary_data) == 112L &&
    data.table::uniqueN(summary_data$variable) == 14L &&
    all(summary_data$n == 600L),
  "FigS11 summary-data dimensions are invalid"
)

summary_data[, treatment_factor := factor(
  treatment,
  levels = treatments$treatment
)]
summary_data[, depth_factor := factor(
  depth_display,
  levels = depths$depth_display
)]
summary_data[, facet_label := ifelse(
  unit == "dimensionless",
  paste0("atop(", plotmath_label, ", '(-)')"),
  paste0(
    "atop(", plotmath_label,
    ", paste('(', plain(g)~plain(g)^{-1}~plain(h)^{-1}, ')'))"
  )
)]

colour_values <- stats::setNames(treatments$colour, treatments$treatment)
linetype_values <- stats::setNames(depths$linetype, depths$depth_display)
depth_legend_labels <- stats::setNames(c("0-10", "70-80"), depths$depth_display)
position_depth <- ggplot2::position_dodge(width = 0.35)

make_parameter_plot <- function(data, title, variable_order) {
  label_levels <- variables$plotmath_label[
    variables$class == title & variables$variable_order %in% variable_order
  ]
  label_levels <- vapply(label_levels, function(label) {
    unit <- variables$unit[
      variables$class == title & variables$plotmath_label == label
    ][[1L]]
    if (unit == "dimensionless") {
      paste0("atop(", label, ", '(-)')")
    } else {
      paste0(
        "atop(", label,
        ", paste('(', plain(g)~plain(g)^{-1}~plain(h)^{-1}, ')'))"
      )
    }
  }, FUN.VALUE = character(1L))
  data[, facet_factor := factor(facet_label, levels = label_levels)]

  ggplot2::ggplot(
    data,
    ggplot2::aes(x = treatment_factor, y = mean)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(group = depth_factor, linetype = depth_factor),
      colour = "grey50",
      linewidth = 0.7,
      position = position_depth
    ) +
    ggplot2::geom_linerange(
      ggplot2::aes(
        ymin = ci_lower,
        ymax = ci_upper,
        colour = treatment_factor,
        group = depth_factor
      ),
      linewidth = 0.7,
      position = position_depth,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        colour = treatment_factor,
        group = depth_factor
      ),
      size = 3,
      position = position_depth,
      show.legend = FALSE
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(facet_factor),
      scales = "free_y",
      switch = "y",
      drop = TRUE,
      labeller = ggplot2::labeller(facet_factor = ggplot2::label_parsed)
    ) +
    ggplot2::scale_colour_manual(values = colour_values, drop = FALSE) +
    ggplot2::scale_linetype_manual(
      values = linetype_values,
      breaks = depths$depth_display,
      labels = depth_legend_labels,
      drop = FALSE
    ) +
    ggplot2::scale_x_discrete(
      drop = FALSE,
      expand = ggplot2::expansion(add = 0.32)
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0.05, 0.30))
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = NULL,
      linetype = "Soil depth (cm)"
    ) +
    ggplot2::theme_bw(base_size = 10.5, base_family = "Times New Roman") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        colour = "#333333",
        fill = NA,
        linewidth = 0.55
      ),
      panel.spacing.y = grid::unit(0.5, "mm"),
      axis.text.x = ggplot2::element_text(
        size = 10.5,
        colour = "black",
        margin = ggplot2::margin(t = 0.8, unit = "mm")
      ),
      axis.text.y = ggplot2::element_text(
        size = 9.2,
        colour = "black",
        margin = ggplot2::margin(r = 0.6, unit = "mm")
      ),
      axis.ticks = ggplot2::element_line(linewidth = 0.45, colour = "#333333"),
      axis.ticks.length = grid::unit(1.0, "mm"),
      strip.background = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.text.y.left = ggplot2::element_text(
        angle = 90,
        size = 9.8,
        colour = "black",
        margin = ggplot2::margin(0, 1.2, 0, 0, unit = "mm")
      ),
      plot.title = ggplot2::element_text(
        size = 14,
        face = "plain",
        hjust = 0.5,
        margin = ggplot2::margin(b = 1.5, unit = "mm")
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = 10.5),
      legend.text = ggplot2::element_text(size = 9.8),
      legend.key.width = grid::unit(10, "mm"),
      legend.key.height = grid::unit(4, "mm"),
      plot.margin = ggplot2::margin(3, 2, 2, 3, unit = "mm")
    )
}

physical_data <- summary_data[class == "Physical parameters"]
physiological_data <- summary_data[class == "Physiological parameters"]
figs11_assert(
  data.table::uniqueN(physical_data$variable) == 7L &&
    data.table::uniqueN(physiological_data$variable) == 7L,
  "The FigS11 two-column variable contract failed"
)

physical_plot <- make_parameter_plot(
  physical_data,
  "Physical parameters",
  1:7
)
physiological_plot <- make_parameter_plot(
  physiological_data,
  "Physiological parameters",
  1:7
) +
  ggplot2::theme(legend.position = "none")

fig_s11 <- physical_plot + physiological_plot +
  patchwork::plot_layout(ncol = 2, widths = c(1, 1), guides = "keep")

figs11_export_plot(
  fig_s11,
  paths$figure_base,
  width_mm = 176.48,
  height_mm = 272.39,
  dpi = 300,
  font_family = "Times New Roman"
)

variable_extent <- summary_data[, .(
  observed_min = min(ci_lower),
  observed_max = max(ci_upper)
), by = .(class, variable, variable_order)]
figs11_assert(
  nrow(variable_extent) == 14L &&
    all(is.finite(variable_extent$observed_min)) &&
    all(is.finite(variable_extent$observed_max)),
  "The FigS11 variable-extent contract failed"
)

plot_contract <- data.frame(
  field = c(
    "layout", "source_summary_rows", "physical_parameter_rows",
    "physiological_parameter_rows", "treatments", "soil_depths",
    "point_definition", "interval_definition", "y_scale",
    "width_mm", "height_mm", "png_dpi", "font",
    "treatment_colours", "soil_depth_linetypes",
    "line_width_mm", "point_size_mm", "position_dodge_width",
    "panel_spacing_y_mm", "legend_placement", "outputs"
  ),
  value = c(
    "two columns by seven parameter rows per column",
    nrow(summary_data),
    data.table::uniqueN(physical_data$variable),
    data.table::uniqueN(physiological_data$variable),
    paste(treatments$treatment, collapse = " | "),
    paste(depths$depth_display, collapse = " | "),
    "arithmetic mean from n equals 600",
    "t-based 95-percent confidence interval around the mean",
    "free for each parameter with 5-percent lower and 30-percent upper expansion",
    176.48,
    272.39,
    300,
    "Times New Roman",
    paste(treatments$colour, collapse = " | "),
    paste(depths$linetype, collapse = " | "),
    0.7,
    3,
    0.35,
    0.5,
    "soil-depth linetype legend below the physical-parameter column only",
    "PDF | PNG"
  ),
  stringsAsFactors = FALSE
)
figs11_write_csv(plot_contract, paths$plot_contract)

message("FigS11 PDF and PNG saved")

