# Assemble final R figures from frozen plot-ready data.
# The script does not refit longitudinal or immune-cell models.

options(stringsAsFactors = FALSE)

module06_walk_up <- function(start, max_levels = 12L) {
  out <- character()
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(max_levels)) {
    out <- c(out, current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  unique(out)
}

module06_find_project_root <- function() {
  for (current in module06_walk_up(getwd())) {
    candidates <- c(
      current,
      file.path(current, "binge-eating-bariatric-analysis")
    )
    for (candidate in candidates) {
      if (dir.exists(file.path(candidate, "code", "06_figure_assembly")) &&
          dir.exists(file.path(candidate, "outputs", "06_final_figures", "_plot_data"))) {
        return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
      }
    }
  }
  stop(
    "Cannot locate binge-eating-bariatric-analysis. Open the binge-eating-bariatric-analysis RStudio Project and rerun.",
    call. = FALSE
  )
}

MODULE06_PROJECT_ROOT <- module06_find_project_root()
MODULE06_SCRIPT_DIR <- file.path(MODULE06_PROJECT_ROOT, "code", "06_figure_assembly")
MODULE06_STYLE_FILE <- file.path(MODULE06_SCRIPT_DIR, "00_figure_style_manifest.csv")
MODULE06_PLOT_DATA_DIR <- file.path(MODULE06_PROJECT_ROOT, "outputs", "06_final_figures", "_plot_data")
MODULE06_OUTPUT_ROOT <- file.path(MODULE06_PROJECT_ROOT, "outputs", "06_final_figures")
MODULE06_EPS_DIR <- file.path(MODULE06_OUTPUT_ROOT, "eps")
MODULE06_SVG_DIR <- file.path(MODULE06_OUTPUT_ROOT, "svg")
MODULE06_PNG_DIR <- file.path(MODULE06_OUTPUT_ROOT, "png_600dpi")

MODULE06_REQUIRED_PACKAGES <- c("ggplot2", "readr", "png")
missing_packages <- MODULE06_REQUIRED_PACKAGES[
  !vapply(MODULE06_REQUIRED_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Missing required R package(s): ", paste(missing_packages, collapse = ", "),
    ". Run renv::restore() in the binge-eating-bariatric-analysis project.",
    call. = FALSE
  )
}
MODULE06_HAS_SVGLITE <- requireNamespace("svglite", quietly = TRUE)

module06_open_png_device <- function(path, width_in, height_in, dpi, background, font_family) {
  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    grDevices::quartz(
      file = path, type = "png", width = width_in, height = height_in,
      dpi = dpi, bg = background, family = font_family
    )
  } else {
    grDevices::png(
      filename = path, width = width_in, height = height_in,
      units = "in", res = dpi, bg = background
    )
  }
}

module06_open_eps_device <- function(path, width_in, height_in, background) {
  fonts <- grDevices::postscriptFonts()
  if (!"Arial" %in% names(fonts)) {
    grDevices::postscriptFonts(Arial = fonts[["Helvetica"]])
  }
  grDevices::postscript(
    file = path, onefile = FALSE, paper = "special", horizontal = FALSE,
    width = width_in, height = height_in, bg = background, family = "Arial"
  )
}

module06_open_svg_device <- function(path, width_in, height_in, background, font_family) {
  if (!MODULE06_HAS_SVGLITE) return(FALSE)
  svglite::svglite(
    file = path, width = width_in, height = height_in, pointsize = 12,
    bg = background, system_fonts = list(sans = font_family)
  )
  TRUE
}

module06_require_export_devices <- function(font_family) {
  png_path <- tempfile(pattern = ".module06_device_", fileext = ".png")
  eps_path <- tempfile(pattern = ".module06_device_", fileext = ".eps")
  svg_path <- tempfile(pattern = ".module06_device_", fileext = ".svg")
  on.exit(unlink(c(png_path, eps_path, svg_path)), add = TRUE)

  module06_open_png_device(png_path, 1, 1, 72, "white", font_family)
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
  grDevices::dev.off()

  module06_open_eps_device(eps_path, 1, 1, "white")
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
  grDevices::dev.off()

  svg_ok <- module06_open_svg_device(svg_path, 1, 1, "white", font_family)
  if (isTRUE(svg_ok)) {
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
    grDevices::dev.off()
  }

  if (!file.exists(png_path) || file.info(png_path)$size <= 0) {
    stop("PNG export is unavailable.", call. = FALSE)
  }
  if (!file.exists(eps_path) || file.info(eps_path)$size <= 0) {
    stop("EPS export is unavailable.", call. = FALSE)
  }
  if (isTRUE(svg_ok) && (!file.exists(svg_path) || file.info(svg_path)$size <= 0)) {
    stop("SVG export is unavailable.", call. = FALSE)
  }
  if (isTRUE(svg_ok)) {
    message("[PASS] PNG, EPS, and SVG export devices available")
  } else {
    message("[PASS] PNG and EPS export devices available; SVG skipped because svglite is unavailable")
  }
  invisible(TRUE)
}

module06_load_style <- function() {
  if (!file.exists(MODULE06_STYLE_FILE)) {
    stop("Missing style manifest: ", MODULE06_STYLE_FILE, call. = FALSE)
  }
  manifest <- utils::read.csv(
    MODULE06_STYLE_FILE, check.names = FALSE, stringsAsFactors = FALSE
  )
  if (!all(c("Key", "Value") %in% names(manifest))) {
    stop("Style manifest must contain Key and Value columns.", call. = FALSE)
  }
  values <- as.list(setNames(trimws(as.character(manifest$Value)), trimws(as.character(manifest$Key))))
  numeric_keys <- c(
    "png_dpi", "combined_width_cm", "panel_letter_size_pt",
    "combined_axis_title_size_pt", "combined_tick_size_pt",
    "combined_legend_size_pt", "single_axis_title_size_pt",
    "single_tick_size_pt", "single_legend_size_pt",
    "line_main_width_pt", "line_main_single_width_pt",
    "reference_line_width_pt"
  )
  for (key in numeric_keys) {
    if (!key %in% names(values)) stop("Style manifest is missing: ", key, call. = FALSE)
    values[[key]] <- as.numeric(values[[key]])
  }
  values
}

module06_require_font <- function(font_family) {
  system_name <- Sys.info()[["sysname"]]
  found <- FALSE
  if (identical(system_name, "Darwin")) {
    roots <- c("/System/Library/Fonts", "/Library/Fonts", path.expand("~/Library/Fonts"))
    files <- unlist(lapply(roots[dir.exists(roots)], function(root) {
      list.files(root, pattern = "\\.(ttf|ttc|otf)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    }), use.names = FALSE)
    found <- any(grepl("arial", basename(files), ignore.case = TRUE))
  } else if (identical(system_name, "Windows")) {
    files <- list.files(file.path(Sys.getenv("WINDIR", "C:/Windows"), "Fonts"), full.names = TRUE)
    found <- any(grepl("^arial", basename(files), ignore.case = TRUE))
  } else {
    fc_match <- Sys.which("fc-match")
    if (nzchar(fc_match)) {
      match <- suppressWarnings(system2(
        fc_match, c("-f", "%{family}", shQuote(font_family)), stdout = TRUE, stderr = FALSE
      ))
      found <- length(match) > 0L && any(grepl(font_family, match, fixed = TRUE, ignore.case = TRUE))
    }
  }
  if (!found) {
    stop("Required font '", font_family, "' is unavailable. Install it before running.", call. = FALSE)
  }
  message("[PASS] required font available: ", font_family)
  invisible(TRUE)
}

module06_read_csv <- function(name, required_columns, expected_rows = NULL) {
  path <- file.path(MODULE06_PLOT_DATA_DIR, name)
  if (!file.exists(path)) stop("Missing plot-ready file: ", path, call. = FALSE)
  frame <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  missing <- setdiff(required_columns, names(frame))
  if (length(missing)) {
    stop(name, " is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!is.null(expected_rows) && nrow(frame) != expected_rows) {
    stop(name, " has ", nrow(frame), " rows; expected ", expected_rows, ".", call. = FALSE)
  }
  frame
}

module06_blend_white <- function(colour, white_fraction = 0.84) {
  rgb <- grDevices::col2rgb(colour) / 255
  mixed <- rgb * (1 - white_fraction) + white_fraction
  grDevices::rgb(mixed[1L], mixed[2L], mixed[3L])
}

module06_pt_to_mm <- function(value) {
  as.numeric(value) * 25.4 / 72.27
}

module06_marker_pch <- function(marker) {
  switch(
    tolower(as.character(marker)),
    circle = 16L,
    square = 15L,
    triangle = 17L,
    stop("Unsupported marker in style manifest: ", marker, call. = FALSE)
  )
}


module06_panel_ids <- function(n) {
  vapply(seq_len(n), function(index) {
    out <- character()
    while (index > 0L) {
      index <- index - 1L
      out <- c(letters[index %% 26L + 1L], out)
      index <- index %/% 26L
    }
    paste(out, collapse = "")
  }, character(1))
}

module06_theme <- function(style) {
  size <- as.numeric(style[["combined_tick_size_pt"]])
  ggplot2::theme_classic(
    base_family = as.character(style[["font_family"]]),
    base_size = size
  ) +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", linewidth = 0.24, fill = NA),
      axis.title = ggplot2::element_text(size = size, face = "plain"),
      axis.title.x = ggplot2::element_text(size = size, face = "plain", hjust = 0.5),
      axis.text = ggplot2::element_text(size = size, colour = "black", face = "plain"),
      axis.text.x = ggplot2::element_text(size = size, colour = "black", face = "plain", hjust = 0.5),
      axis.ticks = ggplot2::element_line(linewidth = 0.22),
      plot.title = ggplot2::element_blank(),
      plot.tag = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(0, 0, 0, 0),
      legend.position = "none"
    )
}

module06_sum_units <- function(values) {
  if (!length(values)) return(grid::unit(0, "cm"))
  Reduce(`+`, as.list(values))
}

module06_is_eps_device <- function() {
  identical(names(grDevices::dev.cur()), "postscript")
}

module06_eps_ribbon_fills <- function(grob, in_ribbon = FALSE) {
  # The EPS device cannot draw alpha fills. Change only ribbon polygons after
  # ggplotGrob(), including plots constructed before any export device opens.
  in_ribbon <- in_ribbon || (
    !is.null(grob$name) && grepl("^geom_ribbon([.]|$)", grob$name)
  )
  if (in_ribbon && inherits(grob, "polygon") && !is.null(grob$gp$fill)) {
    grob$gp$fill <- vapply(grob$gp$fill, function(fill) {
      if (is.na(fill)) return(NA_character_)
      module06_blend_white(fill, white_fraction = 0.84)
    }, character(1), USE.NAMES = FALSE)
    grob$gp$alpha <- 1
  }
  if (!is.null(grob$children)) {
    for (index in seq_along(grob$children)) {
      grob$children[[index]] <- module06_eps_ribbon_fills(grob$children[[index]], in_ribbon)
    }
  }
  if (!is.null(grob$grobs)) {
    grob$grobs <- lapply(grob$grobs, module06_eps_ribbon_fills, in_ribbon = in_ribbon)
  }
  grob
}

module06_resize_grob <- function(plot, total_width_cm, total_height_cm, panel_width_cm, panel_height_cm) {
  grob <- ggplot2::ggplotGrob(plot)
  if (module06_is_eps_device()) grob <- module06_eps_ribbon_fills(grob)
  panel_rows <- grepl("^panel", grob$layout$name)
  panel_cols <- sort(unique(grob$layout$l[panel_rows]))
  panel_heights <- sort(unique(grob$layout$t[panel_rows]))
  if (!length(panel_cols) || !length(panel_heights)) {
    stop("Unable to locate ggplot panel in gtable.", call. = FALSE)
  }
  grob$widths[panel_cols] <- grid::unit(panel_width_cm / length(panel_cols), "cm")
  grob$heights[panel_heights] <- grid::unit(panel_height_cm / length(panel_heights), "cm")

  other_cols <- setdiff(seq_along(grob$widths), panel_cols)
  other_rows <- setdiff(seq_along(grob$heights), panel_heights)
  target_other_width <- total_width_cm - panel_width_cm
  target_other_height <- total_height_cm - panel_height_cm
  current_other_width <- grid::convertWidth(
    module06_sum_units(grob$widths[other_cols]), "cm", valueOnly = TRUE
  )
  current_other_height <- grid::convertHeight(
    module06_sum_units(grob$heights[other_rows]), "cm", valueOnly = TRUE
  )
  if (length(other_cols) && current_other_width > 0) {
    grob$widths[other_cols] <- grob$widths[other_cols] * (target_other_width / current_other_width)
  }
  if (length(other_rows) && current_other_height > 0) {
    grob$heights[other_rows] <- grob$heights[other_rows] * (target_other_height / current_other_height)
  }
  grob
}

module06_draw_grob_cm <- function(grob, x_cm, y_cm, width_cm, height_cm) {
  grid::pushViewport(grid::viewport(
    x = grid::unit(x_cm, "cm"), y = grid::unit(y_cm, "cm"),
    width = grid::unit(width_cm, "cm"), height = grid::unit(height_cm, "cm"),
    just = c("left", "bottom")
  ))
  grid::grid.draw(grob)
  grid::popViewport()
}

module06_draw_text_cm <- function(text, x_cm, y_cm, style, rot = 0, just = c("centre", "centre"), bold = FALSE) {
  grid::grid.text(
    text,
    x = grid::unit(x_cm, "cm"), y = grid::unit(y_cm, "cm"),
    rot = rot, just = just,
    gp = grid::gpar(
      fontfamily = as.character(style[["font_family"]]),
      fontsize = if (bold) as.numeric(style[["panel_letter_size_pt"]]) else as.numeric(style[["combined_tick_size_pt"]]),
      fontface = if (bold) "bold" else "plain"
    )
  )
}

module06_draw_standard_cell <- function(
  plot, y_label, tag, style, x_cm, y_cm,
  y_label_x_offset_cm = 0.27,
  tag_x_offset_cm = 0.02
) {
  grob <- module06_resize_grob(
    plot,
    total_width_cm = 4.82,
    total_height_cm = 3.20,
    panel_width_cm = 4.27,
    panel_height_cm = 2.55
  )
  module06_draw_text_cm(
    y_label, x_cm + y_label_x_offset_cm, y_cm + 1.90, style, rot = 90
  )
  module06_draw_grob_cm(grob, x_cm + 0.58, y_cm, 4.82, 3.20)
  module06_draw_text_cm(
    tag, x_cm + tag_x_offset_cm, y_cm + 3.34, style,
    just = c("left", "bottom"), bold = TRUE
  )
}

module06_immune_axis_label <- function(outcome, delta = FALSE) {
  prefix <- if (delta) paste0("Δ", outcome) else outcome
  bquote(paste(.(prefix), " (", "×", 10^9, "/L)"))
}

module06_trajectory_label <- function(frame) {
  outcome_key <- as.character(frame$outcome_key[1L])
  outcome <- as.character(frame$outcome[1L])
  unit <- as.character(frame$unit[1L])
  if (identical(outcome_key, "rbc")) {
    bquote(paste(.(outcome), " (", "×", 10^12, "/L)"))
  } else if (outcome_key %in% c("wbc", "neutrophils", "lymphocytes", "monocytes", "eosinophils", "basophils")) {
    bquote(paste(.(outcome), " (", "×", 10^9, "/L)"))
  } else if (identical(outcome_key, "bmi")) {
    bquote(paste("BMI (kg/", m^2, ")"))
  } else {
    paste0(outcome, " (", unit, ")")
  }
}

module06_adiposity_axis_label <- function(exposure, unit, delta = FALSE) {
  prefix <- if (delta) paste0("Δ", exposure) else exposure
  if (identical(unit, "kg/m²")) {
    bquote(paste(.(prefix), " (kg/", m^2, ")"))
  } else {
    paste0(prefix, " (", unit, ")")
  }
}

module06_draw_group_legend <- function(style) {
  colours <- c(
    "Non-BE" = as.character(style[["color_non_be"]]),
    "BE" = as.character(style[["color_be"]])
  )
  fills <- vapply(colours, module06_blend_white, character(1), white_fraction = 0.84)
  centres <- c(0.43, 0.60)
  for (i in seq_along(centres)) {
    group <- names(colours)[i]
    x <- centres[i]
    grid::grid.rect(
      x = grid::unit(x, "npc"), y = grid::unit(0.5, "npc"),
      width = grid::unit(0.070, "npc"), height = grid::unit(0.36, "npc"),
      gp = grid::gpar(fill = fills[[group]], col = NA)
    )
    grid::grid.lines(
      x = grid::unit(c(x - 0.035, x + 0.035), "npc"),
      y = grid::unit(c(0.5, 0.5), "npc"),
      gp = grid::gpar(
        col = colours[[group]],
        lwd = as.numeric(style[["line_main_width_pt"]]),
        lty = if (identical(group, "Non-BE")) as.character(style[["linetype_non_be"]]) else as.character(style[["linetype_be"]])
      )
    )
    grid::grid.points(
      x = grid::unit(x, "npc"), y = grid::unit(0.5, "npc"),
      pch = if (identical(group, "Non-BE")) module06_marker_pch(style[["marker_non_be"]]) else module06_marker_pch(style[["marker_be"]]),
      size = grid::unit(1.45, "mm"),
      gp = grid::gpar(col = colours[[group]], fill = colours[[group]])
    )
    grid::grid.text(
      group, x = grid::unit(x + 0.052, "npc"), y = grid::unit(0.5, "npc"),
      just = "left",
      gp = grid::gpar(
        fontfamily = as.character(style[["font_family"]]),
        fontsize = as.numeric(style[["combined_legend_size_pt"]])
      )
    )
  }
}

module06_draw_visit_legend <- function(style, visits, shapes) {
  spacing <- 0.145
  first <- 0.5 - spacing * (length(visits) - 1) / 2
  for (i in seq_along(visits)) {
    x <- first + (i - 1) * spacing
    grid::grid.points(
      x = grid::unit(x, "npc"), y = grid::unit(0.5, "npc"),
      pch = shapes[[visits[i]]], size = grid::unit(1.55, "mm"),
      gp = grid::gpar(col = "grey45", fill = "grey45", lwd = 0.65)
    )
    grid::grid.text(
      visits[i], x = grid::unit(x + 0.025, "npc"), y = grid::unit(0.5, "npc"),
      just = "left",
      gp = grid::gpar(
        fontfamily = as.character(style[["font_family"]]),
        fontsize = as.numeric(style[["combined_legend_size_pt"]])
      )
    )
  }
}

module06_rewrite_rgb_png <- function(source, target, dpi, background_color, width_cm, height_cm) {
  image <- png::readPNG(source)
  dimensions <- dim(image)
  if (length(dimensions) != 3L || !dimensions[3L] %in% c(3L, 4L)) {
    stop("Unexpected PNG channel structure: ", source, call. = FALSE)
  }
  if (dimensions[3L] == 4L) {
    alpha <- image[, , 4L]
    background <- as.numeric(grDevices::col2rgb(background_color)) / 255
    rgb <- image[, , 1:3, drop = FALSE]
    for (channel in 1:3) {
      rgb[, , channel] <- rgb[, , channel] * alpha + background[channel] * (1 - alpha)
    }
  } else {
    rgb <- image
  }
  png::writePNG(rgb, target = target, dpi = c(dpi, dpi))
  written <- png::readPNG(target, info = TRUE)
  written_dimensions <- dim(written)
  expected_height <- as.integer(round(height_cm / 2.54 * dpi))
  expected_width <- as.integer(round(width_cm / 2.54 * dpi))
  if (length(written_dimensions) != 3L || written_dimensions[3L] != 3L) {
    stop("Final PNG is not RGB: ", target, call. = FALSE)
  }
  if (abs(written_dimensions[1L] - expected_height) > 2L ||
      abs(written_dimensions[2L] - expected_width) > 2L) {
    stop("Unexpected final PNG dimensions: ", target, call. = FALSE)
  }
  recorded_dpi <- suppressWarnings(as.numeric(attr(written, "info")$dpi))
  if (length(recorded_dpi) < 2L || any(!is.finite(recorded_dpi)) ||
      any(abs(recorded_dpi[1:2] - dpi) > 1)) {
    stop("Final PNG lacks requested DPI metadata: ", target, call. = FALSE)
  }
  invisible(target)
}

module06_save_figure <- function(draw_function, stem, width_cm, height_cm, style) {
  for (directory in c(MODULE06_EPS_DIR, MODULE06_SVG_DIR, MODULE06_PNG_DIR)) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  width_in <- width_cm / 2.54
  height_in <- height_cm / 2.54
  font <- as.character(style[["font_family"]])
  background <- as.character(style[["background_color"]])
  dpi <- as.numeric(style[["png_dpi"]])
  eps_path <- file.path(MODULE06_EPS_DIR, paste0(stem, ".eps"))
  svg_path <- file.path(MODULE06_SVG_DIR, paste0(stem, ".svg"))
  png_path <- file.path(MODULE06_PNG_DIR, paste0(stem, ".png"))
  temporary_png <- tempfile(pattern = ".module06_", fileext = ".png")
  on.exit(unlink(temporary_png), add = TRUE)

  module06_open_png_device(temporary_png, width_in, height_in, dpi, background, font)
  draw_function()
  grDevices::dev.off()
  module06_rewrite_rgb_png(
    source = temporary_png, target = png_path, dpi = dpi,
    background_color = background, width_cm = width_cm, height_cm = height_cm
  )

  module06_open_eps_device(eps_path, width_in, height_in, background)
  draw_function()
  grDevices::dev.off()

  svg_written <- module06_open_svg_device(svg_path, width_in, height_in, background, font)
  if (isTRUE(svg_written)) {
    draw_function()
    grDevices::dev.off()
  } else if (file.exists(svg_path)) {
    unlink(svg_path)
  }

  if (!file.exists(eps_path) || file.info(eps_path)$size <= 0) {
    stop("Final EPS figure was not written: ", eps_path, call. = FALSE)
  }
  if (isTRUE(svg_written) && (!file.exists(svg_path) || file.info(svg_path)$size <= 0)) {
    stop("Final SVG figure was not written: ", svg_path, call. = FALSE)
  }
  message("[PASS] combined figure written: ", stem, " (", width_cm, " × ", height_cm, " cm)")
}

style <- module06_load_style()
module06_require_font(as.character(style[["font_family"]]))
module06_require_export_devices(as.character(style[["font_family"]]))
combined_width_cm <- as.numeric(style[["combined_width_cm"]])

trajectory <- module06_read_csv(
  "_fig2_figs3_trajectory_estimates.csv",
  c("outcome_key", "outcome", "unit", "outcome_label", "part", "panel_id", "display_order", "group", "visit", "estimate", "lower", "upper"),
  240L
)
forest <- module06_read_csv(
  "_fig3_forest_estimates.csv",
  c("panel_id", "analysis_type", "exposure_key", "effect_scale", "visit", "outcome", "outcome_order", "estimate", "lower", "upper"),
  60L
)
baseline_scatter <- module06_read_csv(
  "_figs4_baseline_scatter.csv",
  c("panel_id", "panel_order", "exposure_key", "exposure", "exposure_unit", "outcome_key", "outcome", "participant_order", "x", "y"),
  1464L
)
current_paths <- module06_read_csv(
  "_figs5_current_value_paths.csv",
  c("panel_id", "panel_order", "exposure_key", "exposure", "exposure_unit", "outcome_key", "outcome", "participant_order", "visit", "visit_month", "x", "y"),
  3960L
)
change_paths <- module06_read_csv(
  "_figs6_change_from_baseline_paths.csv",
  c("panel_id", "panel_order", "exposure_key", "exposure", "exposure_unit", "outcome_key", "outcome", "participant_order", "visit", "visit_month", "x", "y"),
  2544L
)

if (!setequal(unique(trajectory$panel_id), module06_panel_ids(30L))) {
  stop("Trajectory panel IDs are not the frozen a–ad sequence.", call. = FALSE)
}
if (!setequal(unique(baseline_scatter$panel_id), letters[1:12]) ||
    !setequal(unique(current_paths$panel_id), letters[1:12]) ||
    !setequal(unique(change_paths$panel_id), letters[1:12])) {
  stop("Figure S4–S6 panel IDs are not the frozen a–l sequence.", call. = FALSE)
}

module06_trajectory_plot <- function(frame, style) {
  frame$visit <- factor(frame$visit, levels = c("M0", "M1", "M3", "M6"))
  frame$group <- factor(frame$group, levels = c("Non-BE", "BE"))
  colours <- c("Non-BE" = style[["color_non_be"]], "BE" = style[["color_be"]])
  ggplot2::ggplot(
    frame,
    ggplot2::aes(x = visit, y = estimate, group = group, colour = group, linetype = group, shape = group)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper, fill = group),
      colour = NA, linewidth = 0, alpha = 0.16, show.legend = FALSE
    ) +
    ggplot2::geom_line(linewidth = module06_pt_to_mm(style[["line_main_width_pt"]])) +
    ggplot2::geom_point(size = module06_pt_to_mm(3.1), stroke = 0.24) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_linetype_manual(values = c(
      "Non-BE" = as.character(style[["linetype_non_be"]]),
      "BE" = as.character(style[["linetype_be"]])
    )) +
    ggplot2::scale_shape_manual(values = c(
      "Non-BE" = module06_marker_pch(style[["marker_non_be"]]),
      "BE" = module06_marker_pch(style[["marker_be"]])
    )) +
    ggplot2::scale_x_discrete(
      drop = FALSE,
      labels = c("M0" = "Baseline", "M1" = "M1", "M3" = "M3", "M6" = "M6"),
      expand = ggplot2::expansion(add = c(0.12, 0.12))
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.06, 0.09))) +
    ggplot2::labs(x = NULL, y = NULL) +
    module06_theme(style)
}

figure2_keys <- c("total_protein", "calcium", "folate", "alt", "ast", "ggt")
figure2_panels <- lapply(seq_along(figure2_keys), function(index) {
  frame <- trajectory[trajectory$outcome_key == figure2_keys[index], , drop = FALSE]
  if (nrow(frame) != 8L) stop("Incomplete Figure 2 trajectory: ", figure2_keys[index], call. = FALSE)
  list(
    plot = module06_trajectory_plot(frame, style),
    label = module06_trajectory_label(frame),
    tag = letters[index]
  )
})

module06_draw_figure2 <- function() {
  grid::grid.newpage()
  unit_starts <- c(0.30, 5.985, 11.67)
  row_bottoms <- c(4.55, 0.90)
  for (index in seq_along(figure2_panels)) {
    row <- if (index <= 3L) 1L else 2L
    column <- if (index <= 3L) index else index - 3L
    module06_draw_standard_cell(
      figure2_panels[[index]]$plot,
      figure2_panels[[index]]$label,
      figure2_panels[[index]]$tag,
      style,
      unit_starts[column],
      row_bottoms[row]
    )
  }
  grid::pushViewport(grid::viewport(
    x = grid::unit(0.30, "cm"), y = grid::unit(0.20, "cm"),
    width = grid::unit(16.77, "cm"), height = grid::unit(0.55, "cm"),
    just = c("left", "bottom")
  ))
  module06_draw_group_legend(style)
  grid::popViewport()
}

module06_forest_plot <- function(frame, x_limits, breaks, style, visit_label = NULL) {
  frame$plot_y <- 7L - as.integer(frame$outcome_order)
  plot <- ggplot2::ggplot(frame, ggplot2::aes(x = estimate, y = plot_y)) +
    ggplot2::geom_vline(
      xintercept = 0, linetype = "dashed", colour = style[["reference_line_color"]],
      linewidth = module06_pt_to_mm(style[["reference_line_width_pt"]])
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = lower, xend = upper, yend = plot_y),
      linewidth = module06_pt_to_mm(0.90), colour = "black"
    ) +
    ggplot2::geom_point(shape = 16, size = module06_pt_to_mm(3.4), colour = "black") +
    ggplot2::scale_x_continuous(
      breaks = breaks,
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::coord_cartesian(xlim = x_limits) +
    ggplot2::scale_y_continuous(
      breaks = 1:6, labels = rep("", 6),
      limits = c(0.5, 6.5), expand = c(0, 0)
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    module06_theme(style) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  if (!is.null(visit_label)) {
    plot <- plot + ggplot2::annotate(
      "label", x = Inf, y = Inf, label = visit_label,
      hjust = 1.06, vjust = 1.12,
      family = as.character(style[["font_family"]]),
      size = as.numeric(style[["combined_tick_size_pt"]]) / ggplot2::.pt,
      fontface = "plain", colour = "black", fill = "#F2F2F2",
      label.padding = grid::unit(0.08, "lines")
    )
  }
  plot
}

module06_forest_axis_spec <- function(frame, panel_ids, original_limits, original_breaks) {
  selected <- frame[frame$panel_id %in% panel_ids, , drop = FALSE]
  bounds <- c(selected$lower, selected$upper)
  if (!length(bounds) || any(!is.finite(bounds))) {
    stop("Forest display limits require all finite confidence bounds.", call. = FALSE)
  }
  if (min(bounds) >= original_limits[1L] && max(bounds) <= original_limits[2L]) {
    return(list(limits = original_limits, breaks = original_breaks))
  }
  # One shared symmetric range across each paired panel and all its visits.
  limit <- ceiling(max(abs(bounds)) * 1.05 * 10) / 10
  ticks <- pretty(c(0, limit), n = 2L)
  tick <- max(ticks[ticks > 0 & ticks <= limit])
  list(limits = c(-limit, limit), breaks = c(-tick, 0, tick))
}

forest_axes <- list(
  ad = module06_forest_axis_spec(forest, c("a", "d"), c(-0.20, 0.20), c(-0.2, 0, 0.2)),
  be = module06_forest_axis_spec(forest, c("b", "e"), c(-0.30, 0.30), c(-0.3, 0, 0.3)),
  cf = module06_forest_axis_spec(forest, c("c", "f"), c(-0.80, 0.80), c(-0.5, 0, 0.5))
)

forest_plots <- list(
  a = module06_forest_plot(forest[forest$panel_id == "a", , drop = FALSE], forest_axes$ad$limits, forest_axes$ad$breaks, style),
  b = module06_forest_plot(forest[forest$panel_id == "b", , drop = FALSE], forest_axes$be$limits, forest_axes$be$breaks, style),
  d = module06_forest_plot(forest[forest$panel_id == "d", , drop = FALSE], forest_axes$ad$limits, forest_axes$ad$breaks, style),
  e = module06_forest_plot(forest[forest$panel_id == "e", , drop = FALSE], forest_axes$be$limits, forest_axes$be$breaks, style)
)
forest_plots$c <- lapply(c("M1", "M3", "M6"), function(visit) {
  module06_forest_plot(
    forest[forest$panel_id == "c" & forest$visit == visit, , drop = FALSE],
    forest_axes$cf$limits, forest_axes$cf$breaks, style, visit_label = visit
  )
})
forest_plots$f <- lapply(c("M1", "M3", "M6"), function(visit) {
  module06_forest_plot(
    forest[forest$panel_id == "f" & forest$visit == visit, , drop = FALSE],
    forest_axes$cf$limits, forest_axes$cf$breaks, style, visit_label = visit
  )
})

module06_draw_forest_grob <- function(plot, x_cm, row_bottom_cm) {
  grob <- module06_resize_grob(
    plot,
    total_width_cm = 2.75,
    total_height_cm = 6.05,
    panel_width_cm = 2.70,
    panel_height_cm = 5.50
  )
  module06_draw_grob_cm(grob, x_cm, row_bottom_cm + 0.30, 2.75, 6.05)
}

module06_draw_forest_outcomes <- function(row_bottom_cm) {
  labels <- forest[forest$panel_id == "a", c("outcome", "outcome_order")]
  labels <- labels[!duplicated(labels$outcome_order), , drop = FALSE]
  labels <- labels[order(labels$outcome_order), , drop = FALSE]
  panel_bottom <- row_bottom_cm + 0.85
  for (i in seq_len(nrow(labels))) {
    plot_y <- 7L - as.integer(labels$outcome_order[i])
    y_cm <- panel_bottom + ((plot_y - 0.5) / 6) * 5.50
    module06_draw_text_cm(
      as.character(labels$outcome[i]), 2.40, y_cm, style,
      just = c("right", "centre")
    )
  }
}

module06_draw_figure3 <- function() {
  grid::grid.newpage()
  beta_label <- if (module06_is_eps_device()) {
    expression(paste("Standardized ", beta, " (95% CI)"))
  } else {
    "Standardized β (95% CI)"
  }
  row_bottoms <- c(7.00, 0.30)
  row_labels <- c("BMI (per 2 kg/m²)", "Weight (per 5 kg)")
  single_x <- c(2.45, 5.55)
  compound_x <- c(8.80, 11.55, 14.30)
  rows <- list(
    list(a = forest_plots$a, b = forest_plots$b, c = forest_plots$c, tags = c("a", "b", "c")),
    list(a = forest_plots$d, b = forest_plots$e, c = forest_plots$f, tags = c("d", "e", "f"))
  )
  for (row in 1:2) {
    row_bottom <- row_bottoms[row]
    module06_draw_text_cm(row_labels[row], 0.55, row_bottom + 3.60, style, rot = 90)
    module06_draw_forest_outcomes(row_bottom)
    module06_draw_forest_grob(rows[[row]]$a, single_x[1], row_bottom)
    module06_draw_forest_grob(rows[[row]]$b, single_x[2], row_bottom)
    for (visit_index in 1:3) {
      module06_draw_forest_grob(rows[[row]]$c[[visit_index]], compound_x[visit_index], row_bottom)
    }
    module06_draw_text_cm(rows[[row]]$tags[1], 1.25, row_bottom + 6.48, style, just = c("left", "bottom"), bold = TRUE)
    module06_draw_text_cm(rows[[row]]$tags[2], 5.20, row_bottom + 6.48, style, just = c("left", "bottom"), bold = TRUE)
    module06_draw_text_cm(rows[[row]]$tags[3], 8.60, row_bottom + 6.48, style, just = c("left", "bottom"), bold = TRUE)
    module06_draw_text_cm(beta_label, single_x[1] + 1.375, row_bottom + 0.24, style)
    module06_draw_text_cm(beta_label, single_x[2] + 1.375, row_bottom + 0.24, style)
    module06_draw_text_cm(beta_label, compound_x[1] + 4.125, row_bottom + 0.24, style)
  }
}

module06_part_positions <- function(part) {
  if (part == 1L) {
    data.frame(
      display_order = 1:10,
      row = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4),
      column = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 2)
    )
  } else if (part == 2L) {
    data.frame(
      display_order = 11:24,
      row = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5),
      column = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2)
    )
  } else {
    data.frame(display_order = 25:30, row = rep(1:2, each = 3), column = rep(1:3, 2))
  }
}

module06_draw_s3_part <- function(part, nrows) {
  positions <- module06_part_positions(part)
  panels <- lapply(seq_len(nrow(positions)), function(index) {
    frame <- trajectory[trajectory$display_order == positions$display_order[index], , drop = FALSE]
    list(
      plot = module06_trajectory_plot(frame, style),
      label = module06_trajectory_label(frame),
      tag = as.character(frame$panel_id[1L])
    )
  })
  function() {
    grid::grid.newpage()
    unit_starts <- c(0.30, 5.985, 11.67)
    for (index in seq_along(panels)) {
      row_bottom <- 0.90 + (nrows - positions$row[index]) * 3.65
      is_part3_third_column <- part == 3L && positions$column[index] == 3L
      module06_draw_standard_cell(
        panels[[index]]$plot,
        panels[[index]]$label,
        panels[[index]]$tag,
        style,
        unit_starts[positions$column[index]],
        row_bottom,
        y_label_x_offset_cm = if (is_part3_third_column) 0.15 else 0.27,
        tag_x_offset_cm = if (is_part3_third_column) -0.10 else 0.02
      )
    }
    grid::pushViewport(grid::viewport(
      x = grid::unit(0.30, "cm"), y = grid::unit(0.20, "cm"),
      width = grid::unit(16.77, "cm"), height = grid::unit(0.55, "cm"),
      just = c("left", "bottom")
    ))
    module06_draw_group_legend(style)
    grid::popViewport()
  }
}

module06_expanded_range <- function(x, include_zero = FALSE, fraction = 0.04) {
  x <- x[is.finite(x)]
  if (!length(x)) stop("Cannot derive plotting range from empty data.", call. = FALSE)
  limits <- range(x)
  if (include_zero) limits <- range(c(limits, 0))
  span <- diff(limits)
  if (!is.finite(span) || span <= 0) span <- max(abs(limits), 1)
  limits + c(-1, 1) * span * fraction
}

module06_range_maps <- function(frame, include_zero = FALSE) {
  x_limits <- lapply(c("weight", "bmi"), function(key) {
    module06_expanded_range(frame$x[frame$exposure_key == key], include_zero)
  })
  names(x_limits) <- c("weight", "bmi")
  outcome_keys <- unique(frame$outcome_key[order(frame$panel_order)])
  y_limits <- lapply(outcome_keys, function(key) {
    module06_expanded_range(frame$y[frame$outcome_key == key], include_zero)
  })
  names(y_limits) <- outcome_keys
  list(x = x_limits, y = y_limits)
}

module06_path_line_data <- function(frame) {
  counts <- table(frame$participant_order)
  keep <- names(counts)[counts >= 2L]
  frame[as.character(frame$participant_order) %in% keep, , drop = FALSE]
}

module06_raw_plot <- function(frame, type, x_limits, y_limits, style) {
  exposure <- as.character(frame$exposure[1L])
  exposure_unit <- as.character(frame$exposure_unit[1L])
  base <- ggplot2::ggplot(frame, ggplot2::aes(x = x, y = y))
  if (identical(type, "baseline")) {
    plot <- base + ggplot2::geom_point(
      colour = "grey45", size = module06_pt_to_mm(1.55), alpha = 1
    )
    x_label <- module06_adiposity_axis_label(exposure, exposure_unit, FALSE)
  } else {
    line_data <- module06_path_line_data(frame)
    if (identical(type, "change")) {
      base <- base +
        ggplot2::geom_hline(
          yintercept = 0, linetype = "dashed", colour = "#A6A6A6",
          linewidth = module06_pt_to_mm(0.48)
        ) +
        ggplot2::geom_vline(
          xintercept = 0, linetype = "dashed", colour = "#A6A6A6",
          linewidth = module06_pt_to_mm(0.48)
        )
    }
    visits <- if (identical(type, "current")) c("Baseline", "M1", "M3", "M6") else c("M1", "M3", "M6")
    shapes <- if (identical(type, "current")) {
      c("Baseline" = 1L, "M1" = 16L, "M3" = 17L, "M6" = 15L)
    } else {
      c("M1" = 16L, "M3" = 17L, "M6" = 15L)
    }
    frame$visit <- factor(frame$visit, levels = visits)
    line_data$visit <- factor(line_data$visit, levels = visits)
    plot <- base +
      ggplot2::geom_path(
        data = line_data, ggplot2::aes(group = participant_order),
        colour = "grey88", linewidth = module06_pt_to_mm(0.28), na.rm = TRUE
      ) +
      ggplot2::geom_point(
        ggplot2::aes(shape = visit), colour = "grey40",
        size = module06_pt_to_mm(1.65), stroke = 0.22
      ) +
      ggplot2::scale_shape_manual(values = shapes, breaks = visits, drop = FALSE)
    x_label <- module06_adiposity_axis_label(exposure, exposure_unit, identical(type, "change"))
  }
  plot +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.03, 0.03))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.04, 0.04))) +
    ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits, clip = "off") +
    ggplot2::labs(x = x_label, y = NULL) +
    module06_theme(style)
}

baseline_limits <- module06_range_maps(baseline_scatter, FALSE)
current_limits <- module06_range_maps(current_paths, FALSE)
change_limits <- module06_range_maps(change_paths, TRUE)

module06_raw_panel_list <- function(frame, type, limits) {
  panels <- split(frame, factor(frame$panel_id, levels = letters[1:12]))
  lapply(seq_along(panels), function(index) {
    panel <- panels[[index]]
    list(
      plot = module06_raw_plot(
        panel, type,
        limits$x[[as.character(panel$exposure_key[1L])]],
        limits$y[[as.character(panel$outcome_key[1L])]],
        style
      ),
      label = module06_immune_axis_label(
        as.character(panel$outcome[1L]),
        delta = identical(type, "change")
      ),
      tag = letters[index]
    )
  })
}

s4_plots <- module06_raw_panel_list(baseline_scatter, "baseline", baseline_limits)
s5_plots <- module06_raw_panel_list(current_paths, "current", current_limits)
s6_plots <- module06_raw_panel_list(change_paths, "change", change_limits)

module06_draw_raw_grid <- function(
  panels, legend_visits = NULL, legend_shapes = NULL,
  third_column_label_shift_cm = 0,
  third_column_tag_shift_cm = 0
) {
  function() {
    grid::grid.newpage()
    has_legend <- !is.null(legend_visits)
    base_bottom <- if (has_legend) 0.90 else 0.25
    unit_starts <- c(0.30, 5.985, 11.67)
    for (index in seq_along(panels)) {
      row <- (index - 1L) %/% 3L + 1L
      column <- (index - 1L) %% 3L + 1L
      row_bottom <- base_bottom + (4L - row) * 3.65
      module06_draw_standard_cell(
        panels[[index]]$plot,
        panels[[index]]$label,
        panels[[index]]$tag,
        style,
        unit_starts[column],
        row_bottom,
        y_label_x_offset_cm = if (column == 3L) 0.27 - third_column_label_shift_cm else 0.27,
        tag_x_offset_cm = if (column == 3L) 0.02 - third_column_tag_shift_cm else 0.02
      )
    }
    if (has_legend) {
      grid::pushViewport(grid::viewport(
        x = grid::unit(0.30, "cm"), y = grid::unit(0.20, "cm"),
        width = grid::unit(16.77, "cm"), height = grid::unit(0.55, "cm"),
        just = c("left", "bottom")
      ))
      module06_draw_visit_legend(style, legend_visits, legend_shapes)
      grid::popViewport()
    }
  }
}

module06_save_figure(module06_draw_figure2, "Fig2_postoperative_trajectories", combined_width_cm, 8.25, style)
module06_save_figure(module06_draw_figure3, "Fig3_adiposity_immune_associations", combined_width_cm, 13.8, style)
module06_save_figure(
  module06_draw_s3_part(1L, 4L),
  "FigS3_part1_anthropometrics_nutrition", combined_width_cm, 15.60, style
)
module06_save_figure(
  module06_draw_s3_part(2L, 5L),
  "FigS3_part2_metabolic", combined_width_cm, 19.20, style
)
module06_save_figure(
  module06_draw_s3_part(3L, 2L),
  "FigS3_part3_immune", combined_width_cm, 8.25, style
)
module06_save_figure(
  module06_draw_raw_grid(s4_plots),
  "FigS4_baseline_scatter", combined_width_cm, 14.85, style
)
module06_save_figure(
  module06_draw_raw_grid(
    s5_plots,
    c("Baseline", "M1", "M3", "M6"),
    c("Baseline" = 1L, "M1" = 16L, "M3" = 17L, "M6" = 15L)
  ),
  "FigS5_current_value_paths", combined_width_cm, 15.60, style
)
module06_save_figure(
  module06_draw_raw_grid(
    s6_plots,
    c("M1", "M3", "M6"),
    c("M1" = 16L, "M3" = 17L, "M6" = 15L),
    third_column_label_shift_cm = 0.12,
    third_column_tag_shift_cm = 0.06
  ),
  "FigS6_change_from_baseline_paths", combined_width_cm, 15.60, style
)

message("[PASS] R final-figure assembly completed")
