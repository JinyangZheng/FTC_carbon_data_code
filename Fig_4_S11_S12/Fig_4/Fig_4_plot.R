# Fig_4 plotting code
# Input tables are stored together in Fig_4_data.rds.
.script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.script_path <- if (length(.script_files)) tail(.script_files, 1L)[[1L]] else if (length(.script_arg)) sub("^--file=", "", .script_arg[[1L]]) else "Fig_4_plot.R"
code_dir <- dirname(normalizePath(.script_path, winslash = "/", mustWork = TRUE))
.figure_data <- readRDS(file.path(code_dir, "Fig_4_data.rds"))
data_dir <- code_dir
output_dir <- file.path(code_dir, "output")
figure_dir <- output_dir
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
.tidy_code_dir <- code_dir
.plot_file_exists <- function(path) basename(path) %in% names(.figure_data) | file.exists(path)
options(stringsAsFactors = FALSE)

fig4a19_require_packages <- function(packages) {
    missing <- packages[!vapply(packages, requireNamespace, logical(1L), quietly = TRUE)]
    if (length(missing)) {
        stop("Missing R packages  ", paste(missing, collapse = ", "), call. = FALSE)
    }
    invisible(TRUE)
}

fig4a19v3_require_packages <- fig4a19_require_packages

fig4a19_atomic_write_csv <- function(object, path, gzip = FALSE) {
    directory <- dirname(path)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    temporary <- tempfile(pattern = paste0(basename(path), "."), tmpdir = directory)
    on.exit(unlink(temporary, force = TRUE), add = TRUE)
    connection <- if (gzip)
        gzfile(temporary, open = "wt")
    else temporary
    utils::write.csv(object, connection, row.names = FALSE, na = "")
    if (gzip)
        close(connection)
    if (.plot_file_exists(path) && !file.remove(path)) {
        stop("Could not replace output  ", path)
    }
    if (!file.rename(temporary, path))
        stop("Could not finalize output  ", path)
    invisible(path)
}

fig4a19v3_atomic_write_csv <- fig4a19_atomic_write_csv

fig4a19v3_group_specs <- function() {
    specs <- data.frame(key = c("overall", "Depth10", "Depth70", "LFLT", "LFST", "SFLT", "SFST", "Freeze", "Thaw"),
        file_stem = c("Overall", "Depth10", "Depth70", "LFLT", "LFST", "SFLT", "SFST", "Freeze", "Thaw"), folder = c("overall",
            "soil_depth", "soil_depth", rep("FTC_regimes", 4L), rep("FT_phase", 2L)), target = c(rep("MBC_R", 7L),
            "MBC_R_freeze", "MBC_R_thaw"), display = c("All data", "0-10 cm", "70-80 cm", "LFLT", "LFST", "SFLT",
            "SFST", "Freeze", "Thaw"), super_group = c("Overall", rep("Soil depth", 2L), rep("FTC regimes", 4L),
            rep("FTC phases", 2L)), panel_tag = c("a", "b", "", "c", "", "", "", "d", ""), mtry = rep(8L, 9L),
        stringsAsFactors = FALSE)
    specs$IDs <- I(list(1:24, seq.int(1L, 23L, by = 2L), seq.int(2L, 24L, by = 2L), 1:6, 7:12, 13:18, 19:24, 1:24,
        1:24))
    specs
}

fig4a19v3_shap_variable_contract <- function() {
    predictor <- c("BDOCp", "BSOCp", "fisolation", "CDOCsorption", "CDOCdesorption", "CSOCsorption", "CSOCdesorption",
        "V1", "V2", "fdormancy", "finactive", "rfreeze.death", "CUE", "rdeath", "rproduction", "NOC", "DOC", "MBC",
        "Enz")
    display <- c("B_DOCp", "B_SOCp", "f_isolation", "C_DOCsorption", "C_DOCdesorption", "C_SOCsorption", "C_SOCdesorption",
        "V1", "V2", "f_dormancy", "f_inactive", "r_freeze.death", "CUE", "r_death", "r_production", "SOC", "DOC",
        "MBC", "ENZ")
    class <- c(rep("Physical protection", 7L), rep("Microbial physiology", 8L), rep("Initial state variables",
        4L))
    data.frame(predictor = predictor, display = display, class = class, class_order = match(class, c("Physical protection",
        "Microbial physiology", "Initial state variables")), include_in_direction = seq_along(predictor) <= 15L,
        stringsAsFactors = FALSE)
}

fig4a19v3_export_plot <- function(plot, base_path, width_mm, height_mm, dpi = 600L, font_family = "Times New Roman") {
    width_in <- width_mm/25.4
    height_in <- height_mm/25.4
    grDevices::cairo_pdf(paste0(base_path, ".pdf"), width = width_in, height = height_in, family = font_family,
        bg = "white")
    print(plot)
    grDevices::dev.off()
    ragg::agg_png(paste0(base_path, ".png"), width = width_in, height = height_in, units = "in", res = dpi, background = "white")
    print(plot)
    grDevices::dev.off()
    invisible(paste0(base_path, c(".pdf", ".png")))
}

fig4a19_x_names <- function() {
    c("MBC", "Enz", "DOC", "NOC", "CUE", "fisolation", "BDOCp", "BSOCp", "finactive", "fdormancy", "rfreeze.death",
        "CDOCsorption", "CDOCdesorption", "CSOCsorption", "CSOCdesorption", "V1", "V2", "rdeath", "rproduction")
}

fig4a19v3_x_names <- fig4a19_x_names

fig4a19v3_paths <- function() list(
  plot_data_dir = data_dir, shap_dir = data_dir,
  shap_plot_dir = output_dir, figure_dir = output_dir, time_dir = data_dir)
fig4a19v3_ensure_directories <- function(paths) invisible(paths)
fig4a19v3_convergence_label <- function(paths) {
  x <- .figure_data[["Fig4a_19pars_v3_input_convergence_status.csv"]]
  paste(sum(x$converged), "of", nrow(x), "converged inputs")
}
fig4a19v3_atomic_write_csv <- function(object, path, gzip = FALSE) {
  path <- file.path(output_dir, basename(path))
  data.table::fwrite(object, path, compress = if (gzip) "gzip" else "none")
  invisible(path)
}
original_export <- fig4a19v3_export_plot
fig4a19v3_export_plot <- function(plot, base_path, width_mm, height_mm, dpi = 600L, font_family = "Times New Roman") {
  stem <- basename(base_path)
  if (grepl("^Fig4abc", stem)) stem <- "Fig_4"
  if (grepl("^Fig4b_", stem)) stem <- "Fig_4b"
  if (grepl("^Fig4c_", stem)) stem <- "Fig_4c"
  original_export(plot, file.path(output_dir, stem), width_mm, height_mm, dpi, font_family)
}


fig4a19v3_require_packages(c(
  "ggplot2", "data.table", "patchwork", "ragg"
))
paths <- fig4a19v3_paths()
fig4a19v3_ensure_directories(paths)

panel_a_environment <- new.env(parent = .GlobalEnv)
eval(quote({

fig4a19v3_require_packages(c("ggplot2", "data.table", "ragg"))
paths <- fig4a19v3_paths()
fig4a19v3_ensure_directories(paths)
r2_path <- file.path(
  paths$plot_data_dir,
  "Fig4a_19pars_v3_random_forest_200trees_R2_200_bootstrap.rds"
)
if (!.plot_file_exists(r2_path)) stop("Missing 200-tree random-forest R2 data")
r2 <- data.table::as.data.table(.figure_data[[basename(r2_path)]])
specs <- fig4a19v3_group_specs()
group_levels <- specs$key
display_labels <- stats::setNames(specs$display, specs$key)
if (
  nrow(r2) != 1800L ||
    any(table(r2$group) != 200L) ||
    any(!is.finite(r2$R2)) ||
    !"trees" %in% names(r2) ||
    any(r2$trees != 200L) ||
    !"mtry" %in% names(r2) ||
    any(r2$mtry != 8L)
) {
  stop("Fig.4a 200-tree R2 source-data contract failed")
}
r2[, type := factor(group, levels = group_levels)]
r2[, display_label := unname(display_labels[group])]
r2[, plot_order := match(group, group_levels)]
data.table::setorder(r2, plot_order, bootstrap)
fig4a19v3_atomic_write_csv(
  r2,
  file.path(
    paths$plot_data_dir,
    "Fig4a_19pars_v3_200trees_panel_a_source_data.csv"
  )
)

r2_range <- range(r2$R2)
original_limits <- c(0.85, 1.006)
if (
  r2_range[[1L]] >= original_limits[[1L]] &&
    r2_range[[2L]] <= original_limits[[2L]]
) {
  y_limits <- original_limits
  y_breaks <- c(0.86, 0.93, 1.00)
  scale_mode <- "original manuscript scale"
} else {
  y_limits <- c(
    floor((r2_range[[1L]] - 0.005) * 10) / 10,
    min(1.02, ceiling((r2_range[[2L]] + 0.003) * 100) / 100)
  )
  y_breaks <- pretty(y_limits, n = 4)
  y_breaks <- y_breaks[y_breaks >= y_limits[[1L]] & y_breaks <= y_limits[[2L]]]
  scale_mode <- "expanded to avoid clipping version3 R2 values"
}
label_y <- y_limits[[2L]] - diff(y_limits) * 0.012
header_y <- y_limits[[2L]] + diff(y_limits) * 0.075
category_labels <- data.frame(
  x = seq_along(group_levels),
  label = unname(display_labels[group_levels]),
  y = label_y
)
group_headers <- data.frame(
  x = c(1, 2.5, 5.5, 8.5),
  y = header_y,
  label = c("Overall", "Soil depth", "FTC regimes", "Freeze vs. Thaw phases")
)

font_family <- "Times New Roman"
plot <- ggplot2::ggplot(
  r2,
  ggplot2::aes(x = plot_order, y = R2, group = type)
) +
  ggplot2::geom_vline(
    xintercept = c(2.5, 4.5, 5.5, 6.5, 8.5),
    linetype = "dashed",
    colour = "grey55",
    linewidth = 0.30
  ) +
  ggplot2::geom_vline(
    xintercept = c(1.5, 3.5, 7.5),
    colour = "black",
    linewidth = 0.38
  ) +
  ggplot2::geom_boxplot(
    outlier.shape = NA,
    width = 0.10,
    linewidth = 0.38,
    colour = "black",
    fill = "white"
  ) +
  ggplot2::geom_text(
    data = category_labels,
    ggplot2::aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    family = font_family,
    size = 2.65,
    vjust = 1
  ) +
  ggplot2::geom_text(
    data = group_headers,
    ggplot2::aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    family = font_family,
    size = 2.75,
    vjust = 0
  ) +
  ggplot2::annotate(
    "text",
    x = 0.52,
    y = header_y,
    label = "a",
    family = font_family,
    fontface = "bold",
    size = 3.6,
    hjust = 0,
    vjust = 0
  ) +
  ggplot2::scale_x_continuous(
    limits = c(0.5, 9.5),
    breaks = NULL,
    expand = c(0, 0)
  ) +
  ggplot2::scale_y_continuous(
    breaks = y_breaks,
    labels = function(value) format(value, nsmall = 2),
    expand = c(0, 0)
  ) +
  ggplot2::coord_cartesian(ylim = y_limits, clip = "off") +
  ggplot2::labs(
    x = NULL,
    y = expression("Emulator performance (" * R^2 * ")")
  ) +
  ggplot2::theme_classic(base_size = 7.5, base_family = font_family) +
  ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(colour = "black"),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 4)),
    axis.ticks.y = ggplot2::element_line(colour = "black", linewidth = 0.35),
    axis.ticks.length.y = grid::unit(1.2, "mm"),
    panel.border = ggplot2::element_rect(
      colour = "black", fill = NA, linewidth = 0.45
    ),
    panel.grid = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(t = 10, r = 4, b = 3, l = 8, unit = "mm")
  )

width_mm <- 183
height_mm <- 58
base <- file.path(
  paths$figure_dir,
  "Fig4a_19pars_v3_200trees_200samples_200bootstrap_ID001_ID024"
)
grDevices::cairo_pdf(
  paste0(base, ".pdf"),
  width = width_mm / 25.4,
  height = height_mm / 25.4,
  family = font_family,
  bg = "white"
)
print(plot)
grDevices::dev.off()
ragg::agg_png(
  paste0(base, ".png"),
  width = width_mm / 25.4,
  height = height_mm / 25.4,
  units = "in",
  res = 600,
  background = "white"
)
print(plot)
grDevices::dev.off()

fig4a19v3_atomic_write_csv(
  data.frame(
    field = c(
      "figure", "width_mm", "height_mm", "groups", "bootstrap_per_group",
      "predictors", "trees", "mtry", "R2_min", "R2_max", "y_min", "y_max",
      "scale_mode", "metric", "exports", "source_R2_file",
      "convergence_status"
    ),
    value = c(
      "Fig.4a 19-parameter version3 time-integrated 200-tree random-forest R2",
      width_mm,
      height_mm,
      paste(group_levels, collapse = " | "),
      200,
      19,
      200,
      8,
      r2_range[[1L]],
      r2_range[[2L]],
      y_limits[[1L]],
      y_limits[[2L]],
      scale_mode,
      "ranger out-of-bag R2",
      "PDF | PNG",
      gsub("\\\\", "/", r2_path),
      fig4a19v3_convergence_label(paths)
    ),
    stringsAsFactors = FALSE
  ),
  file.path(paths$plot_data_dir, "Fig4a_19pars_v3_200trees_plot_contract.csv")
)
message("Fig.4a version3 200-tree PDF and PNG saved")

}), envir = panel_a_environment)
panel_bc_environment <- new.env(parent = .GlobalEnv)
eval(quote({

fig4a19v3_require_packages(c("ggplot2", "data.table", "ragg"))
paths <- fig4a19v3_paths()
fig4a19v3_ensure_directories(paths)
specs <- fig4a19v3_group_specs()
variables <- fig4a19v3_shap_variable_contract()
font_family <- "Times New Roman"
forest_trees <- 200L
fastshap_nsim <- 20L
fastshap_seed <- 1L
explain_rows_per_ID <- 10L

top_summary_path <- file.path(
  paths$shap_dir,
  "Fig4b_19pars_v3_top_predictor_summary.csv"
)
class_summary_path <- file.path(
  paths$shap_dir,
  "Fig4c_19pars_v3_class_importance_summary.csv"
)
method_contract_path <- file.path(
  paths$shap_dir,
  "Fig4bc_19pars_v3_method_contract.csv"
)
if (any(!.plot_file_exists(c(
  top_summary_path,
  class_summary_path,
  method_contract_path
)))) {
  stop("Run 08_prepare_Fig4b_Fig4c_19pars_v3.R first")
}

top_summary <- data.table::copy(.figure_data[[basename(top_summary_path)]])
class_summary <- data.table::copy(.figure_data[[basename(class_summary_path)]])
method_contract <- data.table::copy(.figure_data[[basename(method_contract_path)]])
expected_groups <- specs$key
class_levels <- c(
  "Physical protection", "Microbial physiology", "Initial state variables"
)
if (
  nrow(top_summary) != 9L ||
    !identical(top_summary$group, expected_groups) ||
    nrow(class_summary) != 27L ||
    any(table(class_summary$group) != 3L) ||
    !identical(sort(unique(class_summary$class)), sort(class_levels)) ||
    any(top_summary$bootstrap_n != 200L) ||
    any(class_summary$bootstrap_n != 200L) ||
    nrow(method_contract) != 2L ||
    any(method_contract$bootstrap_replicates != 200L) ||
    any(method_contract$forest_trees != forest_trees) ||
    any(method_contract$fastshap_nsim != fastshap_nsim) ||
    any(method_contract$fastshap_seed != fastshap_seed) ||
    any(method_contract$explain_rows_per_ID != explain_rows_per_ID) ||
    any(!is.finite(as.matrix(top_summary[, .(
      mean_percent, q025_percent, q975_percent
    )]))) ||
    any(!is.finite(as.matrix(class_summary[, .(
      mean_percent, q025_percent, q975_percent
    )])))
) {
  stop("Fig.4b or Fig.4c source-data contract failed")
}
class_mean_totals <- class_summary[, .(
  total_percent = sum(mean_percent)
), by = group]
if (
  max(abs(class_mean_totals$total_percent - 100)) > 1e-8 ||
    any(top_summary$q025_percent > top_summary$q975_percent) ||
    any(class_summary$q025_percent > class_summary$q975_percent)
) {
  stop("Fig.4b or Fig.4c interval and 100-percent contracts failed")
}

palette <- c(
  "Physical protection" = "#E994AE",
  "Microbial physiology" = "#A0BFF6",
  "Initial state variables" = "#AFD1AF"
)
top_summary[, class := factor(class, levels = class_levels)]
class_summary[, class := factor(class, levels = class_levels)]
top_summary[, x_position := group_order]
class_summary[, x_position := group_order]

predictor_expression <- c(
  BDOCp = "b[D]",
  BSOCp = "b[S]",
  fisolation = "f[iso]",
  CDOCsorption = "k[D * ',' * sor]",
  CDOCdesorption = "k[D * ',' * des]",
  CSOCsorption = "k[S * ',' * sor]",
  CSOCdesorption = "k[S * ',' * des]",
  V1 = "v[upt]",
  V2 = "v[cat]",
  fdormancy = "f[dorm]",
  finactive = "f[inact]",
  rfreeze.death = "r[frz]",
  CUE = "CUE",
  rdeath = "r[death]",
  rproduction = "r[prod]",
  NOC = "SOC",
  DOC = "DOC",
  MBC = "MBC",
  Enz = "ENZ"
)
if (any(!top_summary$predictor %in% names(predictor_expression))) {
  stop("Missing plotmath label for a Fig.4b predictor")
}
top_labels <- parse(text = unname(predictor_expression[top_summary$predictor]))

major_boundaries <- c(1.5, 3.5, 7.5)
minor_boundaries <- c(2.5, 4.5, 5.5, 6.5, 8.5)
header_data <- data.frame(
  x = c(1, 2.5, 5.5, 8.5),
  label = c("Overall", "Soil depth", "FTC regimes", "Freeze vs. Thaw phases")
)
slot_data <- data.frame(
  x = seq_len(nrow(specs)),
  label = specs$display
)

add_panel_structure <- function(plot, y_limit, panel_tag, x_labels) {
  plot +
    ggplot2::geom_vline(
      xintercept = minor_boundaries,
      linetype = "dashed",
      colour = "grey55",
      linewidth = 0.30
    ) +
    ggplot2::geom_vline(
      xintercept = major_boundaries,
      colour = "black",
      linewidth = 0.45
    ) +
    ggplot2::geom_text(
      data = slot_data,
      ggplot2::aes(x = x, y = y_limit * 0.995, label = label),
      inherit.aes = FALSE,
      family = font_family,
      size = 2.25,
      vjust = 1.05,
      colour = "black"
    ) +
    ggplot2::geom_text(
      data = header_data,
      ggplot2::aes(x = x, y = y_limit * 1.095, label = label),
      inherit.aes = FALSE,
      family = font_family,
      size = 2.45,
      colour = "black"
    ) +
    ggplot2::annotate(
      "text",
      x = 0.52,
      y = y_limit * 1.10,
      label = panel_tag,
      family = font_family,
      fontface = "bold",
      size = 3.4,
      hjust = 0
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0.5, 9.5),
      breaks = seq_len(nrow(specs)),
      labels = x_labels,
      expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(ylim = c(0, y_limit), clip = "off") +
    ggplot2::theme_bw(base_size = 7.0, base_family = font_family) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        colour = "black", fill = NA, linewidth = 0.45
      ),
      axis.text.y = ggplot2::element_text(size = 6.4, colour = "black"),
      axis.text.x = ggplot2::element_text(size = 6.0, colour = "black"),
      axis.title.y = ggplot2::element_text(size = 7.0),
      axis.title.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(linewidth = 0.35),
      axis.ticks.length = grid::unit(1.0, "mm"),
      plot.margin = ggplot2::margin(10, 2, 2, 8, unit = "mm")
    )
}

fig4b_limit <- max(
  60,
  ceiling((max(top_summary$q975_percent) + 5) / 10) * 10
)
fig4b <- ggplot2::ggplot(
  top_summary,
  ggplot2::aes(
    x = x_position,
    y = mean_percent,
    fill = class,
    colour = class
  )
) +
  ggplot2::geom_col(width = 0.24) +
  ggplot2::geom_linerange(
    ggplot2::aes(ymin = q025_percent, ymax = q975_percent),
    linewidth = 0.38,
    colour = "black"
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      y = q975_percent + fig4b_limit * 0.025,
      label = sprintf("%.0f", mean_percent)
    ),
    family = font_family,
    size = 2.45,
    colour = "black",
    vjust = 0
  ) +
  ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
  ggplot2::scale_colour_manual(values = palette, drop = FALSE) +
  ggplot2::scale_y_continuous(
    breaks = c(0, 30, 60),
    expand = c(0, 0)
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Relative importance (%)",
    fill = NULL,
    colour = NULL
  ) +
  ggplot2::theme(legend.position = "none")
fig4b <- add_panel_structure(fig4b, fig4b_limit, "b", top_labels) +
  ggplot2::theme(legend.position = "none")

fig4c_limit <- max(
  140,
  ceiling((max(class_summary$q975_percent) + 8) / 10) * 10
)
dodge <- ggplot2::position_dodge(width = 0.68)
fig4c <- ggplot2::ggplot(
  class_summary,
  ggplot2::aes(
    x = x_position,
    y = mean_percent,
    fill = class,
    colour = class,
    group = class
  )
) +
  ggplot2::geom_col(width = 0.40, position = dodge) +
  ggplot2::geom_linerange(
    ggplot2::aes(ymin = q025_percent, ymax = q975_percent),
    position = dodge,
    linewidth = 0.36,
    colour = "black"
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      y = q975_percent + fig4c_limit * 0.018,
      label = sprintf("%.0f", mean_percent)
    ),
    position = dodge,
    family = font_family,
    size = 2.25,
    colour = "black",
    vjust = 0
  ) +
  ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
  ggplot2::scale_colour_manual(values = palette, drop = FALSE) +
  ggplot2::scale_y_continuous(
    breaks = c(0, 50, 100),
    expand = c(0, 0)
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Group level importance (%)",
    fill = NULL,
    colour = NULL
  )
fig4c <- add_panel_structure(
  fig4c,
  fig4c_limit,
  "c",
  rep("", nrow(specs))
) +
  ggplot2::theme(
    legend.position = c(0.015, 0.97),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.background = ggplot2::element_rect(
      fill = "white", colour = NA
    ),
    legend.key.size = grid::unit(2.5, "mm"),
    legend.text = ggplot2::element_text(size = 5.8)
  )

fig4a19v3_atomic_write_csv(
  top_summary,
  file.path(paths$shap_plot_dir, "Fig4b_19pars_v3_panel_source_data.csv")
)
fig4a19v3_atomic_write_csv(
  class_summary,
  file.path(paths$shap_plot_dir, "Fig4c_19pars_v3_panel_source_data.csv")
)

plot_contract <- data.frame(
  figure = c("Fig.4b", "Fig.4c"),
  width_mm = 183,
  height_mm = c(72, 82),
  dpi = 600,
  output_formats = "PDF and PNG",
  forest_trees = forest_trees,
  fastshap_nsim = fastshap_nsim,
  fastshap_seed = fastshap_seed,
  explain_rows_per_ID = explain_rows_per_ID,
  y_axis = c("Relative importance (%)", "Group level importance (%)"),
  uncertainty = paste0(
    "2.5-97.5 percentiles across the 200 per-forest fastSHAP results ",
    "within each analysis group"
  ),
  font = font_family,
  stringsAsFactors = FALSE
)
fig4a19v3_atomic_write_csv(
  plot_contract,
  file.path(paths$shap_plot_dir, "Fig4b_Fig4c_19pars_v3_plot_contract.csv")
)

fig4a19v3_export_plot(
  fig4b,
  file.path(
    paths$figure_dir,
    paste0(
      "Fig4b_19pars_v3_200trees_nsim20_explain10_",
      "200samples_200bootstrap_ID001_ID024"
    )
  ),
  width_mm = 183,
  height_mm = 72,
  dpi = 600,
  font_family = font_family
)
fig4a19v3_export_plot(
  fig4c,
  file.path(
    paths$figure_dir,
    paste0(
      "Fig4c_19pars_v3_200trees_nsim20_explain10_",
      "200samples_200bootstrap_ID001_ID024"
    )
  ),
  width_mm = 183,
  height_mm = 82,
  dpi = 600,
  font_family = font_family
)

message("Fig.4b and Fig.4c PDF and PNG saved")

}), envir = panel_bc_environment)

panel_a <- panel_a_environment$plot
panel_b <- panel_bc_environment$fig4b
panel_c <- panel_bc_environment$fig4c +
  ggplot2::theme(legend.position = c(0.015, 0.90))
if (
  !inherits(panel_a, "ggplot") ||
    !inherits(panel_b, "ggplot") ||
    !inherits(panel_c, "ggplot")
) {
  stop("One or more Fig.4 panel objects are unavailable", call. = FALSE)
}

font_scale <- 1.1
scale_plot_text <- function(plot, multiplier) {
  scale_size <- function(value) {
    if (is.null(value)) {
      return(value)
    }
    if (inherits(value, "rel")) {
      return(ggplot2::rel(as.numeric(value) * multiplier))
    }
    if (is.numeric(value) && length(value) == 1L && is.finite(value)) {
      return(value * multiplier)
    }
    value
  }

  for (theme_name in names(plot$theme)) {
    element <- plot$theme[[theme_name]]
    if (inherits(element, "element_text")) {
      element$size <- scale_size(element$size)
      plot$theme[[theme_name]] <- element
    }
  }
  for (layer_index in seq_along(plot$layers)) {
    layer <- plot$layers[[layer_index]]
    if (inherits(layer$geom, "GeomText") || inherits(layer$geom, "GeomLabel")) {
      layer$aes_params$size <- scale_size(layer$aes_params$size)
      layer$geom_params$size <- scale_size(layer$geom_params$size)
      plot$layers[[layer_index]] <- layer
    }
  }
  plot
}

panel_a <- scale_plot_text(panel_a, font_scale)
panel_b <- scale_plot_text(panel_b, font_scale)
panel_c <- scale_plot_text(panel_c, font_scale)

fig4abc <- patchwork::wrap_plots(
  panel_a,
  panel_b,
  panel_c,
  ncol = 1,
  heights = c(1, 1, 1)
)

width_mm <- 183
height_mm <- 243
output_stem <- paste0(
  "Fig4abc_19pars_v3_200trees_nsim20_explain10_",
  "200samples_200bootstrap_ID001_ID024"
)
output_base <- file.path(paths$figure_dir, output_stem)
fig4a19v3_export_plot(
  fig4abc,
  output_base,
  width_mm = width_mm,
  height_mm = height_mm,
  dpi = 600,
  font_family = "Times New Roman"
)

source_files <- data.frame(
  panel = c("a", "b", "c"),
  source_file = c(
    file.path(
      output_dir,
      "Fig4a_19pars_v3_200trees_panel_a_source_data.csv"
    ),
    file.path(
      paths$shap_plot_dir,
      "Fig4b_19pars_v3_panel_source_data.csv"
    ),
    file.path(
      paths$shap_plot_dir,
      "Fig4c_19pars_v3_panel_source_data.csv"
    )
  ),
  stringsAsFactors = FALSE
)
if (!all(.plot_file_exists(source_files$source_file))) {
  stop("One or more combined Fig.4 source-data files are missing", call. = FALSE)
}
source_files$source_file <- normalizePath(
  source_files$source_file,
  winslash = "/",
  mustWork = TRUE
)
source_files$bytes <- as.numeric(file.info(source_files$source_file)$size)
source_files$md5 <- unname(tools::md5sum(source_files$source_file))
fig4a19v3_atomic_write_csv(
  source_files,
  file.path(
    paths$shap_plot_dir,
    "Fig4abc_19pars_v3_source_data_manifest.csv"
  )
)

plot_contract <- data.frame(
  field = c(
    "figure",
    "panel_order",
    "layout",
    "height_ratios",
    "width_mm",
    "height_mm",
    "font",
    "font_size_multiplier",
    "Fig4b_predictor_symbol_style",
    "Fig4c_bar_width_multiplier",
    "random_forest_trees",
    "fastSHAP_nsim",
    "fastSHAP_explain_rows_per_ID",
    "bootstrap_per_group",
    "exports",
    "source_manifest"
  ),
  value = c(
    "Fig.4a, Fig.4b and Fig.4c combined in the manuscript layout",
    "a | b | c",
    "one column by three vertically stacked rows",
    "1 | 1 | 1",
    width_mm,
    height_mm,
    "Times New Roman",
    font_scale,
    "Mathematical parameter notation",
    2,
    200,
    20,
    10,
    200,
    "PDF | PNG",
    "Fig4abc_19pars_v3_source_data_manifest.csv"
  ),
  stringsAsFactors = FALSE
)
fig4a19v3_atomic_write_csv(
  plot_contract,
  file.path(paths$shap_plot_dir, "Fig4abc_19pars_v3_plot_contract.csv")
)

message("Combined Fig.4a, Fig.4b and Fig.4c PDF and PNG saved")

