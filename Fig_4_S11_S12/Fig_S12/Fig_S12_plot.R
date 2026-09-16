# Fig_S12 plotting code
# Input tables are stored together in Fig_S12_data.rds.
.script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.script_path <- if (length(.script_files)) tail(.script_files, 1L)[[1L]] else if (length(.script_arg)) sub("^--file=", "", .script_arg[[1L]]) else "Fig_S12_plot.R"
code_dir <- dirname(normalizePath(.script_path, winslash = "/", mustWork = TRUE))
.figure_data <- readRDS(file.path(code_dir, "Fig_S12_data.rds"))
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
fig4a19v3_atomic_write_csv <- function(object, path, gzip = FALSE) {
  path <- file.path(output_dir, basename(path))
  data.table::fwrite(object, path, compress = if (gzip) "gzip" else "none")
  invisible(path)
}
original_export <- fig4a19v3_export_plot
fig4a19v3_export_plot <- function(plot, base_path, width_mm, height_mm, dpi = 600L, font_family = "Times New Roman") {
  stem <- basename(base_path)
  if (!grepl("^FigS10_", stem)) return(invisible(NULL))
  stem <- "Fig_S12"
  original_export(plot, file.path(output_dir, stem), width_mm, height_mm, dpi, font_family)
}


fig4a19v3_require_packages(c(
  "ggplot2", "data.table", "patchwork", "ragg", "scales", "gtable"
))
paths <- fig4a19v3_paths()
fig4a19v3_ensure_directories(paths)
specs <- fig4a19v3_group_specs()
variables <- fig4a19v3_shap_variable_contract()
x_names <- fig4a19v3_x_names()
direction_predictors <- variables$predictor[variables$include_in_direction]
font_family <- "Times New Roman"
font_scale <- 1.2
n_bootstrap <- 200L
forest_trees <- 200L
fastshap_nsim <- 20L
fastshap_seed <- 1L
explain_rows_per_ID <- 10L

variable_plotmath <- c(
  BDOCp = "italic(b)[italic(D)]",
  BSOCp = "italic(b)[italic(S)]",
  fisolation = "italic(f)[italic(iso)]",
  CDOCsorption = "italic(k)[italic(D)*','*italic(sor)]",
  CDOCdesorption = "italic(k)[italic(D)*','*italic(des)]",
  CSOCsorption = "italic(k)[italic(S)*','*italic(sor)]",
  CSOCdesorption = "italic(k)[italic(S)*','*italic(des)]",
  V1 = "italic(v)[italic(upt)]",
  V2 = "italic(v)[italic(cat)]",
  fdormancy = "italic(f)[italic(dorm)]",
  finactive = "italic(f)[italic(inact)]",
  rfreeze.death = "italic(r)[italic(frz)]",
  CUE = "CUE",
  rdeath = "italic(r)[italic(death)]",
  rproduction = "italic(r)[italic(prod)]",
  NOC = "SOC",
  DOC = "DOC",
  MBC = "MBC",
  Enz = "ENZ"
)
if (!identical(names(variable_plotmath), variables$predictor)) {
  stop("The Fig.S9 and Fig.S10 display-label contract failed")
}
plotmath_by_display <- stats::setNames(
  unname(variable_plotmath[variables$predictor]),
  variables$display
)
direction_plotmath_by_display <- stats::setNames(
  unname(variable_plotmath[direction_predictors]),
  variables$display[variables$include_in_direction]
)
direction_variable_labeller <- ggplot2::as_labeller(
  direction_plotmath_by_display,
  default = ggplot2::label_parsed
)

fig4a19v3_atomic_write_csv(
  data.frame(
    predictor = variables$predictor,
    previous_display = variables$display,
    plotmath_display = unname(variable_plotmath[variables$predictor]),
    stringsAsFactors = FALSE
  ),
  file.path(
    paths$shap_plot_dir,
    "FigS9_S10_19pars_v3_display_label_mapping.csv"
  )
)

importance_summary <- data.table::copy(.figure_data[["FigS9_19pars_v3_relative_importance_summary.csv"]])
direction_bins <- data.table::copy(.figure_data[["FigS10_19pars_v3_100bin_summary.csv.gz"]])
class_levels <- c(
  "Physical protection", "Microbial physiology", "Initial state variables"
)
palette <- c(
  "Physical protection" = "#E994AE",
  "Microbial physiology" = "#A0BFF6",
  "Initial state variables" = "#AFD1AF"
)
importance_summary[, class := factor(class, levels = class_levels)]
direction_bins[, class := factor(class, levels = class_levels)]

importance_extent <- max(
  importance_summary$q975_percent,
  importance_summary$mean_percent + 3,
  na.rm = TRUE
)
importance_limit <- if (importance_extent <= 60) {
  60
} else {
  ceiling((importance_extent + 2) / 10) * 10
}
importance_breaks <- if (importance_limit == 60) {
  c(0, 30, 60)
} else {
  pretty(c(0, importance_limit), n = 3)
}
fig_s9_width_multiplier <- 1.2
fig_s9_width_mm <- 183 * fig_s9_width_multiplier
fig_s9_r2_y <- 12.7

make_importance_panel <- function(group_key) {
  panel_data <- data.table::copy(importance_summary[group == group_key])
  data.table::setorder(panel_data, class_order, -mean_percent)
  panel_data[, display_factor := factor(display, levels = rev(display))]
  panel_data[, text_x := pmin(mean_percent + 3, importance_limit * 0.94)]
  panel_title <- unique(panel_data$group_display)
  panel_tag <- unique(panel_data$panel_tag)
  r2_label <- sprintf(
    "R\u00b2 %.2f\u2013%.2f",
    unique(panel_data$R2_min),
    unique(panel_data$R2_max)
  )
  ggplot2::ggplot(
    panel_data,
    ggplot2::aes(x = mean_percent, y = display_factor, fill = class)
  ) +
    ggplot2::geom_hline(
      yintercept = c(4.5, 12.5),
      linetype = "dashed",
      colour = "grey55",
      linewidth = 0.25
    ) +
    ggplot2::geom_col(width = 0.70, colour = NA) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = q025_percent,
        xend = q975_percent,
        y = display_factor,
        yend = display_factor
      ),
      inherit.aes = FALSE,
      colour = "black",
      linewidth = 0.32
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = text_x, label = sprintf("%.1f", mean_percent)),
      hjust = 0,
      family = font_family,
      size = 1.55 * font_scale,
      colour = "black"
    ) +
    ggplot2::annotate(
      "text",
      x = importance_limit * 0.985,
      y = fig_s9_r2_y,
      label = r2_label,
      family = font_family,
      size = 1.65 * font_scale,
      hjust = 1,
      vjust = 0
    ) +
    ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
    ggplot2::scale_y_discrete(
      expand = ggplot2::expansion(add = c(0.45, 0.45)),
      labels = function(label) {
        parsed_label <- unname(plotmath_by_display[as.character(label)])
        if (anyNA(parsed_label)) {
          stop("Missing Fig.S9 plotmath display label")
        }
        parse(text = parsed_label)
      }
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, importance_limit),
      breaks = importance_breaks,
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      x = "Relative importance (%)",
      y = NULL,
      title = panel_title,
      tag = if (nzchar(panel_tag)) panel_tag else NULL,
      fill = NULL
    ) +
    ggplot2::theme_bw(
      base_size = 5.5 * font_scale,
      base_family = font_family
    ) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.35
      ),
      axis.text.x = ggplot2::element_text(
        size = 4.7 * font_scale,
        colour = "black"
      ),
      axis.text.y = ggplot2::element_text(
        size = 4.4 * font_scale,
        colour = "black"
      ),
      axis.title.x = ggplot2::element_text(size = 5.2 * font_scale),
      axis.ticks = ggplot2::element_line(linewidth = 0.28),
      axis.ticks.length = grid::unit(0.7, "mm"),
      plot.title = ggplot2::element_text(
        size = 6.2 * font_scale,
        face = "bold",
        hjust = 0.5
      ),
      plot.tag = ggplot2::element_text(
        size = 7.5 * font_scale,
        face = "bold"
      ),
      plot.tag.position = c(0.01, 1.00),
      legend.text = ggplot2::element_text(size = 5.2 * font_scale),
      legend.key.size = grid::unit(2.2, "mm"),
      plot.margin = ggplot2::margin(2, 2, 1, 2, unit = "mm")
    )
}

importance_panels <- setNames(
  lapply(specs$key, make_importance_panel),
  specs$key
)
fig_s9_design <- "
ABC#
DEFG
HIL#
"
fig_s9 <- importance_panels$overall +
  importance_panels$Depth10 +
  importance_panels$Depth70 +
  importance_panels$LFLT +
  importance_panels$LFST +
  importance_panels$SFLT +
  importance_panels$SFST +
  importance_panels$Freeze +
  importance_panels$Thaw +
  patchwork::guide_area() +
  patchwork::plot_layout(
    design = fig_s9_design,
    guides = "collect",
    widths = rep(1, 4L),
    heights = rep(1, 3L)
  ) &
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  )
fig4a19v3_export_plot(
  fig_s9,
  file.path(
    paths$figure_dir,
    paste0(
      "FigS9_19pars_v3_200trees_nsim20_explain10_",
      "200samples_200bootstrap_ID001_ID024"
    )
  ),
  width_mm = fig_s9_width_mm,
  height_mm = 220,
  dpi = 600,
  font_family = font_family
)

direction_variables <- data.table::as.data.table(
  variables[variables$include_in_direction, , drop = FALSE]
)
direction_bins[, group_display := factor(group_display, levels = specs$display)]
direction_bins[, super_group_factor := factor(
  super_group,
  levels = c("Overall", "Soil depth", "FTC regimes", "FTC phases")
)]
direction_bins[, variable_display := factor(
  display,
  levels = direction_variables$display
)]
data.table::setorder(direction_bins, group_order, variable_order, x_normalized)

finite_sd <- data.table::fifelse(
  is.finite(direction_bins$signed_shap_sd),
  direction_bins$signed_shap_sd,
  0
)
direction_extent <- range(
  c(
    direction_bins$signed_shap_mean - finite_sd,
    direction_bins$signed_shap_mean + finite_sd
  ),
  finite = TRUE
)
direction_y_limits <- c(-1700, 3000)
direction_scale_mode <- "original fixed scale"
if (
  direction_extent[[1L]] < direction_y_limits[[1L]] ||
    direction_extent[[2L]] > direction_y_limits[[2L]]
) {
  direction_y_limits <- c(
    min(-1700, floor(direction_extent[[1L]] / 500) * 500),
    max(3000, ceiling(direction_extent[[2L]] / 500) * 500)
  )
  direction_scale_mode <- "expanded to avoid clipping version3 mean plus or minus SD"
}
direction_y_breaks <- c(-1000, 0, 1000, 2000)
if (
  direction_y_limits[[1L]] < -2000 ||
    direction_y_limits[[2L]] > 3500
) {
  direction_y_breaks <- pretty(direction_y_limits, n = 5)
  direction_y_breaks <- direction_y_breaks[
    direction_y_breaks >= direction_y_limits[[1L]] &
      direction_y_breaks <= direction_y_limits[[2L]]
  ]
}

direction_palette <- palette[c("Physical protection", "Microbial physiology")]
fig_s10 <- ggplot2::ggplot(
  direction_bins,
  ggplot2::aes(x = x_normalized, y = signed_shap_mean)
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey55",
    linewidth = 0.22
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = signed_shap_lower,
      ymax = signed_shap_upper,
      fill = class,
      group = interaction(group, predictor)
    ),
    alpha = 0.25,
    colour = NA
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
      colour = class,
      group = interaction(group, predictor)
    ),
    linewidth = 0.38,
    lineend = "round"
  ) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(variable_display),
    cols = ggplot2::vars(group_display),
    labeller = ggplot2::labeller(
      variable_display = direction_variable_labeller
    ),
    switch = "y",
    drop = TRUE
  ) +
  ggplot2::scale_colour_manual(values = direction_palette, drop = FALSE) +
  ggplot2::scale_fill_manual(values = direction_palette, drop = FALSE) +
  ggplot2::scale_x_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    labels = c("0", "0.5", "1"),
    expand = ggplot2::expansion(mult = c(0.02, 0.02))
  ) +
  ggplot2::scale_y_continuous(
    breaks = direction_y_breaks,
    labels = scales::label_number(big.mark = "", accuracy = 1),
    expand = c(0, 0)
  ) +
  ggplot2::coord_cartesian(ylim = direction_y_limits) +
  ggplot2::labs(x = "Normalized value", y = NULL) +
  ggplot2::theme_bw(
    base_size = 5.2 * font_scale,
    base_family = font_family
  ) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.28
    ),
    panel.spacing.x = grid::unit(0.65, "mm"),
    panel.spacing.y = grid::unit(0.35, "mm"),
    axis.text.x = ggplot2::element_text(
      size = 3.7 * font_scale,
      colour = "black"
    ),
    axis.text.y = ggplot2::element_text(
      size = 4.1 * font_scale,
      colour = "black"
    ),
    axis.title.x = ggplot2::element_text(size = 5.5 * font_scale),
    axis.ticks = ggplot2::element_line(linewidth = 0.24),
    axis.ticks.length = grid::unit(0.55, "mm"),
    strip.background = ggplot2::element_blank(),
    strip.placement = "outside",
    strip.text.x = ggplot2::element_text(
      size = 4.7 * font_scale,
      face = "bold"
    ),
    strip.text.y.left = ggplot2::element_text(
      angle = 0,
      size = 4.4 * font_scale,
      hjust = 1
    ),
    legend.position = "none",
    plot.margin = ggplot2::margin(4, 3, 3, 7, unit = "mm")
  )

add_spanning_group_headers <- function(plot, font_family, font_scale) {
  plot_gtable <- ggplot2::ggplotGrob(plot)
  strip_rows <- unique(plot_gtable$layout$t[
    grepl("^strip-t-", plot_gtable$layout$name)
  ])
  panel_columns <- sort(unique(plot_gtable$layout$l[
    grepl("^panel-", plot_gtable$layout$name)
  ]))
  if (length(strip_rows) != 1L || length(panel_columns) != 9L) {
    stop("Fig.S10 spanning column-header layout contract failed")
  }
  header_spec <- data.frame(
    label = c("Overall", "Soil depth", "FTC regimes", "FTC phases"),
    first_column = c(1L, 2L, 4L, 8L),
    last_column = c(1L, 3L, 7L, 9L),
    stringsAsFactors = FALSE
  )
  header_row <- min(strip_rows)
  plot_gtable <- gtable::gtable_add_rows(
    plot_gtable,
    heights = grid::unit(3.5, "mm"),
    pos = header_row - 1L
  )
  for (header_index in seq_len(nrow(header_spec))) {
    plot_gtable <- gtable::gtable_add_grob(
      plot_gtable,
      grobs = grid::textGrob(
        label = header_spec$label[[header_index]],
        gp = grid::gpar(
          fontfamily = font_family,
          fontsize = 4.7 * font_scale,
          fontface = "bold",
          col = "black"
        )
      ),
      t = header_row,
      b = header_row,
      l = panel_columns[[header_spec$first_column[[header_index]]]],
      r = panel_columns[[header_spec$last_column[[header_index]]]],
      clip = "off",
      name = paste0("spanning-group-header-", header_index)
    )
  }
  patchwork::wrap_elements(full = plot_gtable)
}

grDevices::pdf(file.path(output_dir, "layout_measurement.pdf"))
fig_s10 <- add_spanning_group_headers(fig_s10, font_family, font_scale)
grDevices::dev.off()
fig4a19v3_export_plot(
  fig_s10,
  file.path(
    paths$figure_dir,
    paste0(
      "FigS10_19pars_v3_200trees_nsim20_explain10_",
      "200samples_200bootstrap_ID001_ID024"
    )
  ),
  width_mm = 183,
  height_mm = 230,
  dpi = 600,
  font_family = font_family
)

