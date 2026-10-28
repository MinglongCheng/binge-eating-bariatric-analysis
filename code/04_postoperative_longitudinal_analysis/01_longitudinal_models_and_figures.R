# Fits the two prespecified longitudinal models, exports Table 3, and writes
# one publication-ready trajectory PNG per outcome.

module04_walk_up_bootstrap <- function(start, max_levels = 12L) {
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

module04_locate_shared <- function() {
  for (cur in module04_walk_up_bootstrap(getwd())) {
    candidates <- c(
      file.path(cur, "code", "04_postoperative_longitudinal_analysis", "00_longitudinal_shared.R"),
      file.path(cur, "binge-eating-bariatric-analysis", "code", "04_postoperative_longitudinal_analysis", "00_longitudinal_shared.R")
    )
    hit <- candidates[file.exists(candidates)]
    if (length(hit)) return(hit[1L])
  }
  stop("Cannot locate 00_longitudinal_shared.R. Open the binge-eating-bariatric-analysis project and rerun.", call. = FALSE)
}

source(module04_locate_shared(), local = FALSE)
rm(module04_locate_shared, module04_walk_up_bootstrap)

if (!exists("MODULE04_FILESET_ID", inherits = TRUE) ||
    !identical(MODULE04_FILESET_ID, "module04-2026-08-25-display-refactor-v1")) {
  stop(
    "Module 04 files are from different releases. Replace 00_, 01_, and 02_ together from the same bundle.",
    call. = FALSE
  )
}

module04_check_environment()
style <- module04_load_style()
module04_require_font(as.character(style$font_family))
dat <- module04_load_data()

for (directory in c(
  MODULE04_TABLE_DIR,
  MODULE04_FIGURE_DIR,
  MODULE04_INTERMEDIATE_DIR,
  MODULE04_PLOT_DATA_DIR
)) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
}

table_path <- file.path(MODULE04_TABLE_DIR, "Table3_postoperative_longitudinal.xlsx")
log_path <- file.path(
  MODULE04_INTERMEDIATE_DIR,
  "_longitudinal_analysis_model_run_log.csv"
)
plot_data_path <- file.path(
  MODULE04_PLOT_DATA_DIR,
  "_fig2_figs3_trajectory_estimates.csv"
)

unlink(c(table_path, log_path, plot_data_path))
unlink(list.files(
  MODULE04_FIGURE_DIR,
  pattern = "^[0-9]{2}_trajectory_.*\\.png$",
  full.names = TRUE
))

rows <- vector("list", length(MODULE04_OUTCOMES))
logs <- vector("list", length(MODULE04_OUTCOMES) * 2L)
plot_rows <- vector("list", length(MODULE04_OUTCOMES))
log_i <- 0L

for (i in seq_along(MODULE04_OUTCOMES)) {
  spec <- MODULE04_OUTCOMES[[i]]
  message(sprintf("[%02d/%02d] %s", i, length(MODULE04_OUTCOMES), spec$display_label))

  primary_data <- module04_prepare_primary(dat, spec$key)
  primary_formula <- module04_fixed_formula("primary", spec$key)
  primary_fit <- module04_fit_with_fallback(primary_data, primary_formula)
  primary_est <- module04_extract_primary_estimands(primary_fit$fit)

  baseline_data <- module04_prepare_baseline_included(dat, spec$key)
  baseline_formula <- module04_fixed_formula("baseline_included", spec$key)
  baseline_fit <- module04_fit_with_fallback(baseline_data, baseline_formula)
  baseline_est <- module04_extract_baseline_estimands(baseline_fit$fit)

  rows[[i]] <- data.frame(
    Outcome = spec$display_label,
    Time_P = module04_format_p(baseline_est$p_time),
    Average_group_M0_M6_P = module04_format_p(baseline_est$p_group_average),
    Group_by_visit_P = module04_format_p(primary_est$p_interaction),
    Average_group_M1_M6_P = module04_format_p(primary_est$p_group_average),
    Reversal = baseline_est$reversal,
    Delta_delta_P = module04_format_p(baseline_est$p_dd),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  log_i <- log_i + 1L
  logs[[log_i]] <- module04_model_log_row(
    spec$display_label,
    "Baseline-adjusted MMRM (M1–M6)",
    primary_data,
    primary_fit,
    primary_formula
  )
  log_i <- log_i + 1L
  logs[[log_i]] <- module04_model_log_row(
    spec$display_label,
    "Baseline-included model (M0–M6)",
    baseline_data,
    baseline_fit,
    baseline_formula
  )

  pdat <- module04_plot_data(baseline_est, spec)
  plot_rows[[i]] <- pdat
  plot_object <- module04_make_plot(pdat, spec, style, show_legend = TRUE)
  filename <- module04_safe_filename(i, spec)
  module04_save_rgb_png(
    plot = plot_object,
    path = file.path(MODULE04_FIGURE_DIR, filename),
    width_cm = MODULE04_CONFIG$single_width_cm,
    height_cm = MODULE04_CONFIG$single_height_cm,
    dpi = as.numeric(style$png_dpi),
    background_color = as.character(style$background_color)
  )
}

table3 <- do.call(rbind, rows)
model_log <- do.call(rbind, logs)
plot_data <- do.call(rbind, plot_rows)
rownames(table3) <- NULL
rownames(model_log) <- NULL
rownames(plot_data) <- NULL
plot_data$group <- as.character(plot_data$group)
plot_data$visit <- as.character(plot_data$visit)

module04_assert_outcome_order(table3$Outcome, context = "Table 3", with_units = TRUE)
if (anyDuplicated(table3$Outcome)) stop("Duplicate outcomes in Table 3.", call. = FALSE)
if (nrow(model_log) != 2L * length(MODULE04_OUTCOMES)) {
  stop("Model log row-count mismatch.", call. = FALSE)
}
if (nrow(plot_data) != 8L * length(MODULE04_OUTCOMES)) {
  stop("Trajectory plot-ready row-count mismatch.", call. = FALSE)
}
if (anyDuplicated(plot_data[c("outcome_key", "group", "visit")])) {
  stop("Duplicate outcome-by-group-by-visit rows in trajectory plot-ready data.", call. = FALSE)
}

figure_files <- list.files(
  MODULE04_FIGURE_DIR,
  pattern = "^[0-9]{2}_trajectory_.*\\.png$",
  full.names = TRUE
)
if (length(figure_files) != length(MODULE04_OUTCOMES)) {
  stop("Expected exactly 30 trajectory PNG files.", call. = FALSE)
}

module04_write_table3_xlsx(table3, table_path)
module04_write_csv(model_log, log_path)
module04_write_csv(plot_data, plot_data_path)

message("[PASS] sample check: 488 rows; 122 participants; Non-BE=66; BE=56")
message("[PASS] longitudinal models completed: 30 outcomes × 2 models")
message("[PASS] article table written: ", table_path)
message("[PASS] model run log written: ", log_path)
message("[PASS] trajectory plot-ready data written: ", plot_data_path)
message("[PASS] standalone trajectory figures written: ", MODULE04_FIGURE_DIR)
message("[PASS] module 04 longitudinal workflow completed")
