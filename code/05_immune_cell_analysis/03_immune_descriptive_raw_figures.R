# Descriptive immune-cell raw-data panels for Supplementary Figures S4–S6.
# No fitted line, confidence interval, p-value, or significance encoding is added.

module05_walk_up_bootstrap <- function(start, max_levels = 12L) {
  out <- character()
  cur <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(max_levels)) {
    out <- c(out, cur)
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  unique(out)
}

module05_locate_shared <- function() {
  for (cur in module05_walk_up_bootstrap(getwd())) {
    candidates <- c(
      file.path(cur, "code", "05_immune_cell_analysis", "00_immune_shared.R"),
      file.path(cur, "binge-eating-bariatric-analysis", "code", "05_immune_cell_analysis", "00_immune_shared.R")
    )
    hit <- candidates[file.exists(candidates)]
    if (length(hit)) return(hit[1L])
  }
  stop(
    "Cannot locate 00_immune_shared.R. Open the binge-eating-bariatric-analysis RStudio Project and rerun.",
    call. = FALSE
  )
}

source(module05_locate_shared(), local = FALSE)
rm(module05_locate_shared, module05_walk_up_bootstrap)

if (!identical(MODULE05_FILESET_ID, "module05-2026-08-25-display-refactor-v1")) {
  stop("Module 05 files are from different releases. Replace 00_ through 04_ together.", call. = FALSE)
}

module05_check_environment()
style <- module05_load_style()
module05_require_font(as.character(style$font_family))
for (directory in c(
  MODULE05_FIGS4_DIR, MODULE05_FIGS5_DIR, MODULE05_FIGS6_DIR,
  MODULE05_PLOT_DATA_DIR
)) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
}

unlink(list.files(MODULE05_FIGS4_DIR, pattern = "^FigS4[a-l]_.*\\.png$", full.names = TRUE))
unlink(list.files(MODULE05_FIGS5_DIR, pattern = "^FigS5[a-l]_.*\\.png$", full.names = TRUE))
unlink(list.files(MODULE05_FIGS6_DIR, pattern = "^FigS6[a-l]_.*\\.png$", full.names = TRUE))

plot_files <- c(
  baseline = file.path(MODULE05_PLOT_DATA_DIR, "_figs4_baseline_scatter.csv"),
  current = file.path(MODULE05_PLOT_DATA_DIR, "_figs5_current_value_paths.csv"),
  change = file.path(MODULE05_PLOT_DATA_DIR, "_figs6_change_from_baseline_paths.csv")
)
unlink(unname(plot_files))

dat <- module05_load_data()
participant_ids <- sort(unique(dat$id))
participant_map <- setNames(seq_along(participant_ids), participant_ids)

baseline_rows <- list()
current_rows <- list()
change_rows <- list()
row_i <- 0L

for (i in seq_len(nrow(MODULE05_RAW_PANEL_MAP))) {
  map <- MODULE05_RAW_PANEL_MAP[i, , drop = FALSE]
  exposure <- module05_get_adiposity(map$exposure_key)
  outcome <- module05_get_immune(map$outcome_key)
  row_i <- row_i + 1L
  message(sprintf("[%02d/12] %s vs %s", row_i, outcome$label, exposure$label))

  baseline <- module05_prepare_baseline_raw(dat, outcome$key, exposure$key)
  baseline_rows[[i]] <- data.frame(
    panel_id = map$panel_id,
    panel_order = map$panel_order,
    exposure_key = exposure$key,
    exposure = exposure$label,
    exposure_unit = exposure$unit,
    outcome_key = outcome$key,
    outcome = outcome$label,
    outcome_unit = outcome$unit,
    participant_order = unname(participant_map[as.character(baseline$id)]),
    x = baseline$x,
    y = baseline$y,
    stringsAsFactors = FALSE
  )

  current <- module05_prepare_current_paths(dat, outcome$key, exposure$key)
  current_rows[[i]] <- data.frame(
    panel_id = map$panel_id,
    panel_order = map$panel_order,
    exposure_key = exposure$key,
    exposure = exposure$label,
    exposure_unit = exposure$unit,
    outcome_key = outcome$key,
    outcome = outcome$label,
    outcome_unit = outcome$unit,
    participant_order = unname(participant_map[as.character(current$id)]),
    visit = as.character(current$visit),
    visit_month = current$visit_month,
    x = current$x,
    y = current$y,
    stringsAsFactors = FALSE
  )

  change <- module05_prepare_change_paths(dat, outcome$key, exposure$key)
  change_rows[[i]] <- data.frame(
    panel_id = map$panel_id,
    panel_order = map$panel_order,
    exposure_key = exposure$key,
    exposure = exposure$label,
    exposure_unit = exposure$unit,
    outcome_key = outcome$key,
    outcome = outcome$label,
    outcome_unit = outcome$unit,
    participant_order = unname(participant_map[as.character(change$id)]),
    visit = as.character(change$visit),
    visit_month = change$visit_month,
    x = change$x,
    y = change$y,
    stringsAsFactors = FALSE
  )
}

baseline_data <- do.call(rbind, baseline_rows)
current_data <- do.call(rbind, current_rows)
change_data <- do.call(rbind, change_rows)
for (name in c("baseline_data", "current_data", "change_data")) {
  frame <- get(name)
  row_order <- if ("visit_month" %in% names(frame)) {
    order(frame$panel_order, frame$participant_order, frame$visit_month)
  } else {
    order(frame$panel_order, frame$participant_order)
  }
  frame <- frame[row_order, , drop = FALSE]
  rownames(frame) <- NULL
  assign(name, frame)
}

if (anyNA(baseline_data$participant_order) || anyNA(current_data$participant_order) ||
    anyNA(change_data$participant_order)) {
  stop("Could not construct deidentified participant order.", call. = FALSE)
}
expected_rows <- c(baseline = 1464L, current = 3960L, change = 2544L)
actual_rows <- c(
  baseline = nrow(baseline_data), current = nrow(current_data),
  change = nrow(change_data)
)
if (!identical(as.integer(actual_rows), as.integer(expected_rows))) {
  stop(
    "Unexpected Figure S4–S6 plot-ready row counts: ",
    paste(names(actual_rows), actual_rows, collapse = "; "),
    call. = FALSE
  )
}
module05_write_csv(baseline_data, plot_files[["baseline"]])
module05_write_csv(current_data, plot_files[["current"]])
module05_write_csv(change_data, plot_files[["change"]])
message(
  "[PASS] Figure S4–S6 plot-ready data written: ",
  actual_rows[["baseline"]], "/", actual_rows[["current"]], "/",
  actual_rows[["change"]], " rows"
)

expanded_range <- function(x, include_zero = FALSE, fraction = 0.04) {
  x <- x[is.finite(x)]
  if (!length(x)) stop("Cannot derive plotting range from empty data.", call. = FALSE)
  limits <- range(x)
  if (include_zero) limits <- range(c(limits, 0))
  span <- diff(limits)
  if (!is.finite(span) || span <= 0) span <- max(abs(limits), 1)
  limits + c(-1, 1) * span * fraction
}

range_maps <- function(frame, include_zero = FALSE) {
  x_limits <- lapply(c("weight", "bmi"), function(key) {
    expanded_range(frame$x[frame$exposure_key == key], include_zero)
  })
  names(x_limits) <- c("weight", "bmi")
  y_limits <- lapply(names(MODULE05_IMMUNE), function(key) {
    expanded_range(frame$y[frame$outcome_key == key], include_zero)
  })
  names(y_limits) <- names(MODULE05_IMMUNE)
  list(x = x_limits, y = y_limits)
}

baseline_limits <- range_maps(baseline_data)
current_limits <- range_maps(current_data)
change_limits <- range_maps(change_data, include_zero = TRUE)

for (i in seq_len(nrow(MODULE05_RAW_PANEL_MAP))) {
  map <- MODULE05_RAW_PANEL_MAP[i, , drop = FALSE]
  exposure <- module05_get_adiposity(map$exposure_key)
  outcome <- module05_get_immune(map$outcome_key)
  suffix <- paste0(exposure$safe_name, "_", outcome$safe_name)

  baseline_panel <- baseline_data[baseline_data$panel_id == map$panel_id, , drop = FALSE]
  current_panel <- current_data[current_data$panel_id == map$panel_id, , drop = FALSE]
  change_panel <- change_data[change_data$panel_id == map$panel_id, , drop = FALSE]
  current_panel$visit <- factor(
    current_panel$visit, levels = MODULE05_CONFIG$current_visit_levels
  )
  change_panel$visit <- factor(
    change_panel$visit, levels = MODULE05_CONFIG$post_visit_levels
  )

  baseline_plot <- module05_make_baseline_raw_plot(
    baseline_panel, exposure, outcome, style,
    x_limits = baseline_limits$x[[exposure$key]],
    y_limits = baseline_limits$y[[outcome$key]]
  )
  current_plot <- module05_make_current_path_plot(
    current_panel, exposure, outcome, style,
    x_limits = current_limits$x[[exposure$key]],
    y_limits = current_limits$y[[outcome$key]]
  )
  change_plot <- module05_make_change_path_plot(
    change_panel, exposure, outcome, style,
    x_limits = change_limits$x[[exposure$key]],
    y_limits = change_limits$y[[outcome$key]]
  )

  module05_save_rgb_png(
    baseline_plot,
    file.path(MODULE05_FIGS4_DIR, paste0("FigS4", map$panel_id, "_", suffix, ".png")),
    MODULE05_CONFIG$raw_width_cm, MODULE05_CONFIG$raw_height_cm, style
  )
  module05_save_rgb_png(
    current_plot,
    file.path(MODULE05_FIGS5_DIR, paste0("FigS5", map$panel_id, "_", suffix, ".png")),
    MODULE05_CONFIG$raw_width_cm, MODULE05_CONFIG$raw_height_cm, style
  )
  module05_save_rgb_png(
    change_plot,
    file.path(MODULE05_FIGS6_DIR, paste0("FigS6", map$panel_id, "_", suffix, ".png")),
    MODULE05_CONFIG$raw_width_cm, MODULE05_CONFIG$raw_height_cm, style
  )
}

counts <- c(
  S4 = length(list.files(MODULE05_FIGS4_DIR, pattern = "^FigS4[a-l]_.*\\.png$")),
  S5 = length(list.files(MODULE05_FIGS5_DIR, pattern = "^FigS5[a-l]_.*\\.png$")),
  S6 = length(list.files(MODULE05_FIGS6_DIR, pattern = "^FigS6[a-l]_.*\\.png$"))
)
if (any(counts != 12L)) stop("Expected 12 standalone panels for each of Figures S4–S6.", call. = FALSE)

message("[PASS] Figure S4 standalone panels written: 12")
message("[PASS] Figure S5 standalone panels written: 12")
message("[PASS] Figure S6 standalone panels written: 12")
message("[PASS] module 05 descriptive raw-figure workflow completed")
