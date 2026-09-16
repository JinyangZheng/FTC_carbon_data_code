# Fig_5 plotting code
# Input tables are stored together in Fig_5_data.rds.
.script_files <- Filter(Negate(is.null), lapply(sys.frames(), function(frame) frame$ofile))
.script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.script_path <- if (length(.script_files)) tail(.script_files, 1L)[[1L]] else if (length(.script_arg)) sub("^--file=", "", .script_arg[[1L]]) else "Fig_5_plot.R"
code_dir <- dirname(normalizePath(.script_path, winslash = "/", mustWork = TRUE))
.figure_data <- readRDS(file.path(code_dir, "Fig_5_data.rds"))
data_dir <- code_dir
output_dir <- file.path(code_dir, "output")
figure_dir <- output_dir
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
.tidy_code_dir <- code_dir
.plot_file_exists <- function(path) basename(path) %in% names(.figure_data) | file.exists(path)
options(stringsAsFactors = FALSE)



procedural_dir <- data_dir
figure_dir <- output_dir
source_dir <- output_dir
old_build_only <- Sys.getenv("FIGV3_BUILD_ONLY", unset = NA_character_)
Sys.setenv(FIGV3_BUILD_ONLY = "1")
plot_environment <- new.env(parent = .GlobalEnv)
eval(quote({

options(stringsAsFactors = FALSE)

required_packages <- c("ggplot2", "patchwork", "ragg")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing R packages: ", paste(missing_packages, collapse = ", "),
    ". Install them before running this script."
  )
}

procedural_dir <- data_dir
figure_dir <- output_dir
input_paths <- list(
  r2 = file.path(
    procedural_dir,

    "Fig5a_19pars_v3_R2_summary.rds"
  ),
  predictor = file.path(
    procedural_dir,

    "FigS_19pars_v3_predictor_importance_summary.rds"
  ),
  group = file.path(
    procedural_dir,

    "FigS16_19pars_v3_group_importance_summary.rds"
  )
)

missing_inputs <- unlist(input_paths)[!.plot_file_exists(unlist(input_paths))]
if (length(missing_inputs) > 0L) {
  stop(
    "Required summary files are missing:\n",
    paste(missing_inputs, collapse = "\n")
  )
}
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

read_summary <- function(path) {
  value <- .figure_data[[basename(path)]]
  if (!is.data.frame(value)) {
    stop("Summary object is not a data frame: ", path)
  }
  as.data.frame(value, stringsAsFactors = FALSE)
}

require_columns <- function(data, columns, object_name) {
  absent <- setdiff(columns, names(data))
  if (length(absent) > 0L) {
    stop(
      object_name, " is missing columns: ",
      paste(absent, collapse = ", ")
    )
  }
}

coerce_summary_numbers <- function(data, object_name) {
  numeric_columns <- c(
    "time_index", "Hour", "Day", "Temp", "mean", "median",
    "sd", "q025", "q975", "n_forests"
  )
  for (column in intersect(numeric_columns, names(data))) {
    data[[column]] <- as.numeric(data[[column]])
  }
  required_numeric <- intersect(numeric_columns, names(data))
  if (any(!is.finite(as.matrix(data[required_numeric])))) {
    stop(object_name, " contains missing or non-finite numeric values")
  }
  data
}

canonical_scope <- function(value) {
  key <- gsub("[^a-z0-9]", "", tolower(as.character(value)))
  result <- rep(NA_character_, length(key))
  result[key %in% c("overall", "all", "ftcregimes")] <- "Overall"
  result[key %in% c(
    "depth10", "010", "0to10", "shallow", "soildepth010cm"
  )] <- "Depth10"
  result[key %in% c(
    "depth70", "50100", "50to100", "deep", "soildepth50100cm",
    "7080", "70to80"
  )] <- "Depth70"
  result
}

canonical_predictor <- function(value) {
  key <- gsub("[^a-z0-9]", "", tolower(as.character(value)))
  aliases <- c(
    mbc = "MBC",
    enz = "Enz",
    enzyme = "Enz",
    doc = "DOC",
    noc = "NOC",
    soc = "NOC",
    cue = "CUE.",
    fisolation = "fisolation",
    bdocp = "BDOCp",
    bsocp = "BSOCp",
    finactive = "finactive",
    fdormancy = "fdormancy",
    rfreezedeath = "rfreeze.death",
    cdocsorption = "CDOCsorption",
    cdocdesorption = "CDOCdesorption",
    csocsorption = "CSOCsorption",
    csocdesorption = "CSOCdesorption",
    v1 = "V1",
    v2 = "V2",
    rdeath = "rdeath",
    rproduction = "rproduction"
  )
  unname(aliases[key])
}

canonical_group <- function(value) {
  key <- gsub("[^a-z0-9]", "", tolower(as.character(value)))
  result <- rep(NA_character_, length(key))
  result[key %in% c(
    "physicalprotection", "physicalproperties", "physics"
  )] <- "Physical protection"
  result[key %in% c(
    "microbialphysiology", "physiologicalproperties", "physiology"
  )] <- "Microbial physiology"
  result[key %in% c("carbonpool", "carbonpools")] <- "Carbon pool"
  result
}

assert_unique_rows <- function(data, columns, object_name) {
  key <- do.call(
    paste,
    c(lapply(data[columns], as.character), sep = "\r")
  )
  if (anyDuplicated(key)) {
    stop(object_name, " has duplicate summary rows")
  }
}

assert_all_100_forests <- function(data, object_name) {
  if (any(data$n_forests != 100)) {
    observed <- sort(unique(data$n_forests))
    stop(
      object_name,
      " is not based on 100 bootstrap forests. Observed values: ",
      paste(observed, collapse = ", ")
    )
  }
}

importance_to_percent <- function(data, item_column, object_name) {
  key_columns <- c("scope", "regime", "Hour")
  sums <- stats::aggregate(
    data$mean,
    data[c(key_columns)],
    sum
  )[["x"]]
  centre <- stats::median(sums)
  summary_columns <- c("mean", "median", "sd", "q025", "q975")
  if (centre >= 0.8 && centre <= 1.2) {
    data[summary_columns] <- lapply(data[summary_columns], function(x) x * 100)
    sums <- sums * 100
  } else if (!(centre >= 80 && centre <= 120)) {
    stop(object_name, " has an unrecognized importance scale")
  }
  if (any(abs(sums - 100) > 0.75)) {
    stop(
      object_name,
      " does not sum to 100 percent within scope, regime, and hour"
    )
  }
  data
}

check_complete_items <- function(data, item_column, expected, object_name) {
  split_key <- interaction(
    data$scope, data$regime, data$Hour,
    drop = TRUE, lex.order = TRUE
  )
  observed <- split(as.character(data[[item_column]]), split_key)
  complete <- vapply(
    observed,
    function(x) identical(sort(unique(x)), sort(expected)),
    logical(1)
  )
  if (!all(complete)) {
    stop(object_name, " is incomplete for one or more time points")
  }
}

summary_columns <- c(
  "scope", "regime", "time_index", "Hour", "Day", "Temp",
  "mean", "median", "sd", "q025", "q975", "n_forests"
)

r2_data <- read_summary(input_paths$r2)
require_columns(r2_data, summary_columns, "R2 summary")
r2_data <- coerce_summary_numbers(r2_data, "R2 summary")
r2_data$scope <- canonical_scope(r2_data$scope)
if (anyNA(r2_data$scope)) stop("R2 summary contains an unknown scope")
r2_data <- r2_data[r2_data$scope == "Overall", , drop = FALSE]

predictor_data <- read_summary(input_paths$predictor)
require_columns(
  predictor_data,
  c(summary_columns, "predictor"),
  "Predictor importance summary"
)
predictor_data <- coerce_summary_numbers(
  predictor_data, "Predictor importance summary"
)
predictor_data$scope <- canonical_scope(predictor_data$scope)
predictor_data$predictor <- canonical_predictor(predictor_data$predictor)
if (anyNA(predictor_data$scope)) {
  stop("Predictor importance summary contains an unknown scope")
}
if (anyNA(predictor_data$predictor)) {
  stop("Predictor importance summary contains an unknown predictor")
}

group_data <- read_summary(input_paths$group)
require_columns(
  group_data,
  c(summary_columns, "group"),
  "Group importance summary"
)
group_data <- coerce_summary_numbers(group_data, "Group importance summary")
group_data$scope <- canonical_scope(group_data$scope)
group_data$group <- canonical_group(group_data$group)
if (anyNA(group_data$scope)) {
  stop("Group importance summary contains an unknown scope")
}
if (anyNA(group_data$group)) {
  stop("Group importance summary contains an unknown group")
}

regime_levels <- c("LFLT", "LFST", "SFLT", "SFST")
for (object_name in c("r2_data", "predictor_data", "group_data")) {
  object <- get(object_name)
  if (!setequal(unique(object$regime), regime_levels)) {
    stop(object_name, " does not contain all four FTC regimes")
  }
  object$regime <- factor(object$regime, levels = regime_levels)
  object <- object[order(object$regime, object$Hour), , drop = FALSE]
  assign(object_name, object)
}

assert_all_100_forests(r2_data, "R2 summary")
assert_all_100_forests(predictor_data, "Predictor importance summary")
assert_all_100_forests(group_data, "Group importance summary")
assert_unique_rows(r2_data, c("scope", "regime", "Hour"), "R2 summary")
assert_unique_rows(
  predictor_data,
  c("scope", "regime", "Hour", "predictor"),
  "Predictor importance summary"
)
assert_unique_rows(
  group_data,
  c("scope", "regime", "Hour", "group"),
  "Group importance summary"
)

if (any(r2_data$q025 > r2_data$q975)) stop("R2 confidence limits are reversed")
if (any(predictor_data$q025 > predictor_data$q975)) {
  stop("Predictor confidence limits are reversed")
}
if (any(group_data$q025 > group_data$q975)) {
  stop("Group confidence limits are reversed")
}
if (any(r2_data$Day < 0 | r2_data$Day > 96)) {
  stop("R2 summary contains days outside 0 to 96")
}
if (any(predictor_data$Day < 0 | predictor_data$Day > 96)) {
  stop("Predictor summary contains days outside 0 to 96")
}
if (any(group_data$Day < 0 | group_data$Day > 96)) {
  stop("Group summary contains days outside 0 to 96")
}

predictor_order <- c(
  "fisolation", "BDOCp", "BSOCp",
  "CDOCsorption", "CDOCdesorption",
  "CSOCdesorption", "CSOCsorption",
  "fdormancy", "finactive", "rfreeze.death",
  "V1", "V2", "CUE.", "rproduction", "rdeath",
  "DOC", "MBC", "NOC", "Enz"
)
group_order <- c(
  "Physical protection", "Microbial physiology", "Carbon pool"
)
scope_order <- c("Overall", "Depth10", "Depth70")

if (!setequal(unique(predictor_data$scope), scope_order)) {
  stop("Predictor summary must contain Overall, Depth10, and Depth70")
}
if (!setequal(unique(group_data$scope), c("Depth10", "Depth70"))) {
  stop("Group summary must contain Depth10 and Depth70")
}
check_complete_items(
  predictor_data, "predictor", predictor_order,
  "Predictor importance summary"
)
check_complete_items(
  group_data, "group", group_order,
  "Group importance summary"
)
predictor_data <- importance_to_percent(
  predictor_data, "predictor", "Predictor importance summary"
)
group_data <- importance_to_percent(
  group_data, "group", "Group importance summary"
)

extend_last_value_to_day96 <- function(data, grouping_columns) {
  split_key <- do.call(
    interaction,
    c(
      unname(data[grouping_columns]),
      list(drop = TRUE, lex.order = TRUE)
    )
  )
  pieces <- split(data, split_key)
  extended <- lapply(pieces, function(piece) {
    piece <- piece[order(piece$Day), , drop = FALSE]
    if (max(piece$Day) < 96) {
      last <- piece[nrow(piece), , drop = FALSE]
      last$Day <- 96
      last$Hour <- 2304
      last$time_index <- max(piece$time_index) + 1
      piece <- rbind(piece, last)
    }
    piece
  })
  result <- do.call(rbind, extended)
  rownames(result) <- NULL
  result
}

r2_data <- extend_last_value_to_day96(
  r2_data,
  c("scope", "regime")
)
predictor_data <- extend_last_value_to_day96(
  predictor_data,
  c("scope", "regime", "predictor")
)
group_data <- extend_last_value_to_day96(
  group_data,
  c("scope", "regime", "group")
)

make_phase_intervals <- function() {
  specifications <- list(
    LFLT = c(Freeze = 12, Thaw = 12),
    LFST = c(Freeze = 12, Thaw = 4),
    SFLT = c(Freeze = 4, Thaw = 12),
    SFST = c(Freeze = 4, Thaw = 4)
  )
  output <- lapply(names(specifications), function(regime) {
    widths <- specifications[[regime]]
    rows <- list()
    position <- 0
    index <- 1L
    phase_index <- 1L
    while (position < 96) {
      phase <- names(widths)[phase_index]
      next_position <- min(96, position + unname(widths[phase_index]))
      rows[[index]] <- data.frame(
        regime = regime,
        xmin = position,
        xmax = next_position,
        phase = phase,
        stringsAsFactors = FALSE
      )
      position <- next_position
      index <- index + 1L
      phase_index <- if (phase_index == 1L) 2L else 1L
    }
    do.call(rbind, rows)
  })
  output <- do.call(rbind, output)
  output$regime <- factor(output$regime, levels = regime_levels)
  rownames(output) <- NULL
  output
}

phase_data <- make_phase_intervals()
phase_colors <- c(Freeze = "#6A96E1", Thaw = "#EEA9BE")

full_phase_layers <- function(alpha = 0.20) {
  list(
    ggplot2::geom_rect(
      data = phase_data[phase_data$phase == "Freeze", , drop = FALSE],
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE, fill = phase_colors[["Freeze"]],
      colour = NA, alpha = alpha
    ),
    ggplot2::geom_rect(
      data = phase_data[phase_data$phase == "Thaw", , drop = FALSE],
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE, fill = phase_colors[["Thaw"]],
      colour = NA, alpha = alpha
    )
  )
}

strip_phase_layers <- function(ymin, ymax, alpha = 0.58) {
  list(
    ggplot2::geom_rect(
      data = phase_data[phase_data$phase == "Freeze", , drop = FALSE],
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = phase_colors[["Freeze"]],
      colour = NA, alpha = alpha
    ),
    ggplot2::geom_rect(
      data = phase_data[phase_data$phase == "Thaw", , drop = FALSE],
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = phase_colors[["Thaw"]],
      colour = NA, alpha = alpha
    )
  )
}

font_family <- "Times New Roman"

base_panel_theme <- function(base_size = 8) {
  ggplot2::theme_bw(base_size = base_size, base_family = font_family) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        colour = "#4D4D4D", linewidth = 0.35, fill = NA
      ),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(linewidth = 0.32, colour = "black"),
      axis.text = ggplot2::element_text(colour = "black"),
      strip.background = ggplot2::element_blank(),
      panel.spacing.x = grid::unit(0.8, "mm"),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

fig5a_plot <- ggplot2::ggplot(
  r2_data,
  ggplot2::aes(x = Day, y = mean)
) +
  full_phase_layers(alpha = 0.20) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = q025, ymax = q975),
    fill = "#8A8588", alpha = 0.42, colour = NA
  ) +
  ggplot2::geom_line(colour = "#3A3A3A", linewidth = 0.42) +
  ggplot2::facet_grid(cols = ggplot2::vars(regime)) +
  ggplot2::scale_x_continuous(
    breaks = c(0, 48, 96), expand = c(0, 0)
  ) +
  ggplot2::scale_y_continuous(
    breaks = c(0.7, 0.8, 0.9, 1.0),
    labels = c("0.7", "0.8", "0.9", "1.0"),
    expand = c(0, 0)
  ) +
  ggplot2::coord_cartesian(xlim = c(0, 96), ylim = c(0.65, 1.0)) +
  ggplot2::labs(
    x = "Incubation days",
    y = expression(atop("Emulator performance", (R^2))),
    tag = "a"
  ) +
  base_panel_theme(base_size = 8.4) +
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 9.6),
    axis.text = ggplot2::element_text(size = 8.2),
    strip.text.x = ggplot2::element_text(size = 10.2),
    plot.tag = ggplot2::element_text(size = 13, face = "bold"),
    plot.tag.position = c(0.001, 0.998),
    plot.margin = ggplot2::margin(5, 2, 2, 10, unit = "mm")
  )

predictor_display <- c(
  fisolation = "f[iso]",
  BDOCp = "b[D]",
  BSOCp = "b[S]",
  CDOCsorption = "k[D*','*sor]",
  CDOCdesorption = "k[D*','*des]",
  CSOCdesorption = "k[S*','*des]",
  CSOCsorption = "k[S*','*sor]",
  fdormancy = "f[dorm]",
  finactive = "f[inact]",
  rfreeze.death = "r[frz]",
  V1 = "v[upt]",
  V2 = "v[cat]",
  CUE. = "CUE",
  rproduction = "r[prod]",
  rdeath = "r[death]",
  DOC = "DOC",
  MBC = "MBC",
  NOC = "SOC",
  Enz = "ENZ"
)

make_predictor_plot <- function(scope, title) {
  values <- predictor_data[predictor_data$scope == scope, , drop = FALSE]
  values$predictor_label <- predictor_display[values$predictor]
  values$predictor_label <- factor(
    values$predictor_label,
    levels = unname(predictor_display[predictor_order])
  )
  values <- values[order(values$predictor_label, values$regime, values$Day), ]

  ggplot2::ggplot(
    values,
    ggplot2::aes(x = Day, y = mean)
  ) +
    full_phase_layers(alpha = 0.20) +
    ggplot2::geom_hline(
      yintercept = 20, colour = "#B8B8B8",
      linetype = "dashed", linewidth = 0.28
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = q025, ymax = q975),
      fill = "#9B9B9B", alpha = 0.30, colour = NA
    ) +
    ggplot2::geom_line(colour = "black", linewidth = 0.34) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(predictor_label),
      cols = ggplot2::vars(regime),
      labeller = ggplot2::labeller(
        predictor_label = ggplot2::label_parsed
      ),
      switch = "y",
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(0, 48, 96), expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 20, 40), expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 96), ylim = c(0, 60)) +
    ggplot2::labs(
      title = title,
      x = "Incubation days",
      y = "Relative importance (%)"
    ) +
    base_panel_theme(base_size = 6.1) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 5.7),
      axis.title = ggplot2::element_text(size = 7.2),
      strip.text.x = ggplot2::element_text(size = 7.2),
      strip.text.y.left = ggplot2::element_text(
        size = 6.4, angle = 0, hjust = 1
      ),
      strip.placement = "outside",
      panel.spacing.x = grid::unit(1.2, "mm"),
      panel.spacing.y = grid::unit(0.65, "mm"),
      plot.title = ggplot2::element_text(size = 8.2, hjust = 0.5),
      plot.margin = ggplot2::margin(3, 3, 3, 3, unit = "mm")
    )
}

key_colors <- c(
  BDOCp = "#E994AE",
  fisolation = "#41B6C4",
  V1 = "#F1AA7B",
  fdormancy = "#6A96E1",
  rfreeze.death = "#253494"
)
key_labels <- c(
  BDOCp = "B[DOCp]",
  fisolation = "f[isolation]",
  V1 = "V[1]",
  fdormancy = "f[dormancy]",
  rfreeze.death = "r[freeze.death]"
)
key_offsets <- c(
  BDOCp = 0.3,
  fisolation = -0.3,
  V1 = 0,
  fdormancy = -0.3,
  rfreeze.death = 0.3
)

make_fig5b_row <- function(variables, row_label, show_x_title) {
  values <- predictor_data[
    predictor_data$scope == "Overall" &
      predictor_data$predictor %in% variables,
    , drop = FALSE
  ]
  values$predictor <- factor(values$predictor, levels = variables)
  values$plot_day <- values$Day + key_offsets[as.character(values$predictor)]
  values <- values[order(values$regime, values$predictor, values$Day), ]

  ggplot2::ggplot(
    values,
    ggplot2::aes(
      x = plot_day, y = mean,
      colour = predictor, fill = predictor, group = predictor
    )
  ) +
    strip_phase_layers(ymin = -3.2, ymax = -1.0, alpha = 0.62) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = q025, ymax = q975),
      alpha = 0.13, colour = NA, show.legend = FALSE
    ) +
    ggplot2::geom_line(linewidth = 0.63) +
    ggplot2::facet_grid(cols = ggplot2::vars(regime)) +
    ggplot2::scale_colour_manual(
      values = key_colors[variables],
      breaks = variables,
      labels = parse(text = unname(key_labels[variables])),
      name = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = key_colors[variables], guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(0, 48, 96), expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 25, 50), expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 96), ylim = c(-3.2, 60)) +
    ggplot2::labs(
      x = if (show_x_title) "Incubation days" else NULL,
      y = row_label
    ) +
    base_panel_theme(base_size = 7.5) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 7.2),
      axis.title.x = ggplot2::element_text(size = 8.4),
      axis.title.y = ggplot2::element_text(
        size = 8.5, margin = ggplot2::margin(r = 3)
      ),
      strip.text.x = ggplot2::element_blank(),
      legend.position = c(0.014, 0.975),
      legend.justification = c(0, 1),
      legend.direction = "horizontal",
      legend.background = ggplot2::element_rect(
        fill = grDevices::adjustcolor("white", alpha.f = 0.86), colour = NA
      ),
      legend.key.width = grid::unit(4.5, "mm"),
      legend.key.height = grid::unit(2.4, "mm"),
      legend.spacing.x = grid::unit(1.0, "mm"),
      legend.text = ggplot2::element_text(size = 7.0),
      panel.spacing.x = grid::unit(0, "mm"),
      plot.margin = ggplot2::margin(1, 1, 1, 8, unit = "mm")
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        nrow = 1,
        override.aes = list(linewidth = 0.8)
      )
    )
}

fig5b_physics <- make_fig5b_row(
  c("BDOCp", "fisolation"), "Physics", FALSE
)
fig5b_physiology <- make_fig5b_row(
  c("V1", "fdormancy", "rfreeze.death"), "Physiology", TRUE
)
fig5b_core <- fig5b_physics / fig5b_physiology +
  patchwork::plot_layout(heights = c(1, 1))
fig5b_plot <- patchwork::wrap_elements(full = fig5b_core) +
  patchwork::inset_element(
    ggplot2::ggplot() + ggplot2::theme_void() +
      ggplot2::annotate(
        "text", x = 0.5, y = 0.5,
        label = "Relative importance (%)", angle = 90,
        family = font_family, size = 3.0
      ),
    left = -0.025, bottom = 0.08, right = 0.018, top = 0.92,
    align_to = "full", on_top = TRUE
  ) +
  patchwork::inset_element(
    ggplot2::ggplot() + ggplot2::theme_void() +
      ggplot2::annotate(
        "text", x = 0, y = 1, label = "b",
        hjust = 0, vjust = 1,
        family = font_family, fontface = "bold", size = 4.5
      ),
    left = 0.001, bottom = 0.925, right = 0.045, top = 0.999,
    align_to = "full", on_top = TRUE
  )

group_colors <- c(
  "Physical protection" = "#F3BECE",
  "Microbial physiology" = "#A0BFF6",
  "Carbon pool" = "#AFD1AF"
)

make_group_plot <- function() {
  values <- group_data
  values$scope_label <- ifelse(
    values$scope == "Depth10", "0–10 cm", "50–100 cm"
  )
  values$scope_label <- factor(
    values$scope_label, levels = c("0–10 cm", "50–100 cm")
  )
  values$group <- factor(values$group, levels = group_order)
  values <- values[order(values$scope_label, values$regime, values$group, values$Day), ]

  ggplot2::ggplot(
    values,
    ggplot2::aes(
      x = Day, y = mean,
      colour = group, fill = group, group = group
    )
  ) +
    strip_phase_layers(ymin = -6.0, ymax = -2.5, alpha = 0.62) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = q025, ymax = q975),
      alpha = 0.25, colour = NA, show.legend = FALSE
    ) +
    ggplot2::geom_line(linewidth = 0.62) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(scope_label),
      cols = ggplot2::vars(regime),
      switch = "y"
    ) +
    ggplot2::scale_colour_manual(
      values = group_colors, breaks = group_order, name = NULL
    ) +
    ggplot2::scale_fill_manual(values = group_colors, guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = c(0, 48, 96), expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 50, 100), expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 96), ylim = c(-6, 120)) +
    ggplot2::labs(
      x = "Incubation days",
      y = "Group-level importance (%)"
    ) +
    base_panel_theme(base_size = 8.0) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 7.6),
      axis.title = ggplot2::element_text(size = 9.0),
      strip.text.x = ggplot2::element_text(size = 10.0),
      strip.text.y.left = ggplot2::element_text(
        size = 9.0, angle = 90, hjust = 0.5
      ),
      strip.placement = "outside",
      panel.spacing.x = grid::unit(0, "mm"),
      panel.spacing.y = grid::unit(0, "mm"),
      legend.position = c(0.014, 0.975),
      legend.justification = c(0, 1),
      legend.direction = "vertical",
      legend.background = ggplot2::element_rect(
        fill = grDevices::adjustcolor("white", alpha.f = 0.88), colour = NA
      ),
      legend.key.width = grid::unit(5.0, "mm"),
      legend.key.height = grid::unit(2.3, "mm"),
      legend.text = ggplot2::element_text(size = 7.4),
      plot.margin = ggplot2::margin(2, 2, 2, 3, unit = "mm")
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        override.aes = list(linewidth = 0.8)
      )
    )
}

figures <- list(
  Fig5a = fig5a_plot,
  Fig5b = fig5b_plot,
  FigS12 = make_predictor_plot("Overall", "Overall"),
  FigS13 = make_predictor_plot("Depth10", "Soil depth 0\u201310 cm"),
  FigS14 = make_predictor_plot("Depth70", "Soil depth 50\u2013100 cm"),
  FigS16 = make_group_plot()
)

save_pdf_png <- function(plot, stem, width_mm, height_mm) {
  pdf_path <- file.path(figure_dir, paste0(stem, ".pdf"))
  png_path <- file.path(figure_dir, paste0(stem, ".png"))

  grDevices::cairo_pdf(
    filename = pdf_path,
    width = width_mm / 25.4,
    height = height_mm / 25.4,
    family = font_family,
    bg = "white"
  )
  print(plot)
  grDevices::dev.off()

  ragg::agg_png(
    filename = png_path,
    width = width_mm,
    height = height_mm,
    units = "mm",
    res = 600,
    background = "white"
  )
  print(plot)
  grDevices::dev.off()

  invisible(c(pdf = pdf_path, png = png_path))
}

}), envir = plot_environment)
if (is.na(old_build_only)) {
  Sys.unsetenv("FIGV3_BUILD_ONLY")
} else {
  Sys.setenv(FIGV3_BUILD_ONLY = old_build_only)
}

group_path <- file.path(
  procedural_dir,
  "Fig_all_scopes_19pars_v3_group_importance_summary.rds"
)
if (!.plot_file_exists(group_path)) stop("Overall group summary is missing")

group_data <- plot_environment$read_summary(group_path)
plot_environment$require_columns(
  group_data,
  c(plot_environment$summary_columns, "group"),
  "All-scope group importance summary"
)
group_data <- plot_environment$coerce_summary_numbers(
  group_data,
  "All-scope group importance summary"
)
group_data$scope <- plot_environment$canonical_scope(group_data$scope)
group_data$group <- plot_environment$canonical_group(group_data$group)
if (anyNA(group_data$scope) || anyNA(group_data$group)) {
  stop("All-scope group summary contains unknown scope or group values")
}
group_data <- group_data[group_data$scope == "Overall", , drop = FALSE]
if (!nrow(group_data)) stop("Overall group summary is empty")

regime_levels <- plot_environment$regime_levels
group_order <- plot_environment$group_order
if (!setequal(unique(group_data$regime), regime_levels)) {
  stop("Overall group summary does not contain all FTC regimes")
}
group_data$regime <- factor(group_data$regime, levels = regime_levels)
group_data <- group_data[order(group_data$regime, group_data$Hour), , drop = FALSE]
plot_environment$assert_all_100_forests(group_data, "Overall group summary")
plot_environment$assert_unique_rows(
  group_data,
  c("scope", "regime", "Hour", "group"),
  "Overall group summary"
)
plot_environment$check_complete_items(
  group_data,
  "group",
  group_order,
  "Overall group summary"
)
if (any(group_data$q025 > group_data$q975)) {
  stop("Overall group uncertainty limits are reversed")
}
group_data <- plot_environment$importance_to_percent(
  group_data,
  "group",
  "Overall group summary"
)
group_data <- plot_environment$extend_last_value_to_day96(
  group_data,
  c("scope", "regime", "group")
)
group_data$group <- factor(group_data$group, levels = group_order)
group_data <- group_data[
  order(group_data$regime, group_data$group, group_data$Day),
  , drop = FALSE
]

group_colors <- c(
  "Physical protection" = "#F3BECE",
  "Microbial physiology" = "#A0BFF6",
  "Carbon pool" = "#AFD1AF"
)

fig5c_core <- ggplot2::ggplot(
  group_data,
  ggplot2::aes(
    x = Day,
    y = mean,
    colour = group,
    fill = group,
    group = group
  )
) +
  plot_environment$strip_phase_layers(ymin = -6, ymax = -2.5, alpha = 0.62) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = q025, ymax = q975),
    alpha = 0.25,
    colour = NA,
    show.legend = FALSE
  ) +
  ggplot2::geom_line(linewidth = 0.62) +
  ggplot2::facet_grid(cols = ggplot2::vars(regime)) +
  ggplot2::scale_colour_manual(
    values = group_colors,
    breaks = group_order,
    name = NULL
  ) +
  ggplot2::scale_fill_manual(values = group_colors, guide = "none") +
  ggplot2::scale_x_continuous(
    breaks = c(0, 48, 96),
    expand = c(0, 0)
  ) +
  ggplot2::scale_y_continuous(
    breaks = c(0, 50, 100),
    expand = c(0, 0)
  ) +
  ggplot2::coord_cartesian(xlim = c(0, 96), ylim = c(-6, 120)) +
  ggplot2::labs(
    x = "Incubation days",
    y = "Group level\nimportance (%)",
    tag = "c"
  ) +
  plot_environment$base_panel_theme(base_size = 7.5) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 7.2),
    axis.title.x = ggplot2::element_text(size = 8.4),
    axis.title.y = ggplot2::element_text(
      size = 8.5,
      margin = ggplot2::margin(r = 3)
    ),
    strip.text.x = ggplot2::element_blank(),
    legend.position = c(0.014, 0.975),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.background = ggplot2::element_rect(
      fill = grDevices::adjustcolor("white", alpha.f = 0.88),
      colour = NA
    ),
    legend.key.width = grid::unit(5, "mm"),
    legend.key.height = grid::unit(2.3, "mm"),
    legend.text = ggplot2::element_text(size = 7.2),
    panel.spacing.x = grid::unit(0, "mm"),
    plot.tag = ggplot2::element_text(size = 13, face = "bold"),
    plot.tag.position = c(0.001, 0.998),
    plot.margin = ggplot2::margin(3.5, 1, 1, 11, unit = "mm")
  ) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(
      override.aes = list(linewidth = 0.8)
    )
  )

common_row_theme <- ggplot2::theme(
  plot.margin = ggplot2::margin(3.5, 1, 1, 11, unit = "mm")
)

fig5b_label_map <- c(
  BDOCp = "b[D]",
  fisolation = "f[iso]",
  V1 = "v[upt]",
  fdormancy = "f[dorm]",
  rfreeze.death = "r[frz]"
)
plot_environment$key_labels[names(fig5b_label_map)] <- fig5b_label_map
fig5b_physics_named <- plot_environment$make_fig5b_row(
  c("BDOCp", "fisolation"), "Physics", FALSE
)
fig5b_physiology_named <- plot_environment$make_fig5b_row(
  c("V1", "fdormancy", "rfreeze.death"), "Physiology", TRUE
)

fig5a_row <- plot_environment$fig5a_plot + common_row_theme
fig5b_physics_row <- fig5b_physics_named +
  ggplot2::labs(tag = "b") +
  common_row_theme +
  ggplot2::theme(
    plot.tag = ggplot2::element_text(size = 13, face = "bold"),
    plot.tag.position = c(0.001, 0.998)
  )
fig5b_physiology_row <- fig5b_physiology_named + common_row_theme
fig5c_row <- fig5c_core + common_row_theme

fig5abc_core <- fig5a_row /
  fig5b_physics_row /
  fig5b_physiology_row /
  fig5c_row +
  patchwork::plot_layout(heights = c(1, 1, 1, 1))

fig5abc_plot <- patchwork::wrap_elements(full = fig5abc_core) +
  patchwork::inset_element(
    ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::annotate(
        "text",
        x = 0.5,
        y = 0.5,
        label = "Relative importance (%)",
        angle = 90,
        family = plot_environment$font_family,
        size = 3.0
      ),
    left = 0.001,
    bottom = 0.255,
    right = 0.031,
    top = 0.745,
    align_to = "full",
    on_top = TRUE
  ) +
  patchwork::plot_annotation(
    theme = ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
  )

pdf_path <- file.path(figure_dir, "Fig_5.pdf")
png_path <- file.path(figure_dir, "Fig_5.png")
source_path <- file.path(source_dir, "Fig5c_19pars_v3_source_data.csv")
contract_path <- file.path(source_dir, "Fig5abc_19pars_v3_plot_contract.csv")

utils::write.csv(group_data, source_path, row.names = FALSE, na = "")
utils::write.csv(
  data.frame(
    item = c(
      "figure_archetype",
      "panel_map",
      "panel_b_labels",
      "panel_c_scope",
      "interval",
      "normalization",
      "width_mm",
      "height_mm",
      "backend",
      "outputs"
    ),
    value = c(
      "quantitative grid",
      "a emulator OOB R2 | b key predictor SHAP importance | c group SHAP importance",
      "BDOCp=b_D | fisolation=f_iso | V1=v_upt | fdormancy=f_dorm | rfreeze.death=r_frz",
      "Overall within each FTC regime",
      "pointwise 2.5 and 97.5 percentiles across 100 bootstrap forests",
      "within-forest predictor importance sums to 100 percent before group aggregation",
      "183",
      "150",
      "R only",
      "PDF and PNG"
    ),
    stringsAsFactors = FALSE
  ),
  contract_path,
  row.names = FALSE,
  na = ""
)

grDevices::cairo_pdf(
  filename = pdf_path,
  width = 183 / 25.4,
  height = 150 / 25.4,
  family = plot_environment$font_family,
  bg = "white"
)
print(fig5abc_plot)
grDevices::dev.off()

ragg::agg_png(
  filename = png_path,
  width = 183,
  height = 150,
  units = "mm",
  res = 600,
  background = "white"
)
print(fig5abc_plot)
grDevices::dev.off()

if (!.plot_file_exists(pdf_path) || !.plot_file_exists(png_path)) {
  stop("Combined Fig.5 outputs were not created")
}
message("Saved combined Fig.5a-c PDF and PNG")

