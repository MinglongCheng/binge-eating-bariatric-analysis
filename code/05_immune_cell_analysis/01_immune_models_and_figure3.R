# Baseline and postoperative immune-cell association models for Figure 3.
# Outputs: six standalone forest plots, plot-ready estimates, and model log rows.

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
module05_cleanup_legacy_outputs()

for (directory in c(
  MODULE05_TABLE_DIR, MODULE05_FIG3_DIR, MODULE05_INTERMEDIATE_DIR,
  MODULE05_PLOT_DATA_DIR
)) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
}

unlink(list.files(MODULE05_FIG3_DIR, pattern = "^Fig3[a-f]_.*\\.png$", full.names = TRUE))
plot_data_file <- file.path(MODULE05_PLOT_DATA_DIR, "_fig3_forest_estimates.csv")
table_path <- file.path(MODULE05_TABLE_DIR, "TableS11_figure3_adiposity_immune_associations.xlsx")
unlink(c(plot_data_file, table_path))

dat <- module05_load_data()
message("[PASS] input validated: 488 rows; 122 participants; six immune outcomes")

baseline_rows <- list()
overall_rows <- list()
visit_rows <- list()
log_rows <- list()
baseline_i <- overall_i <- visit_i <- log_i <- 0L

panel_map <- data.frame(
  exposure_key = c("bmi", "bmi", "bmi", "weight", "weight", "weight"),
  analysis_type = c(
    "baseline", "overall change", "visit-specific",
    "baseline", "overall change", "visit-specific"
  ),
  panel_order = seq_len(6L),
  panel_id = letters[1:6],
  stringsAsFactors = FALSE
)

panel_info <- function(exposure_key, analysis_type) {
  z <- panel_map[
    panel_map$exposure_key == exposure_key &
      panel_map$analysis_type == analysis_type,
    , drop = FALSE
  ]
  if (nrow(z) != 1L) stop("Could not resolve Figure 3 panel mapping.", call. = FALSE)
  z
}

for (exposure_key in c("bmi", "weight")) {
  exposure <- module05_get_adiposity(exposure_key)
  message("\nExposure: ", exposure$label)

  for (outcome_key in names(MODULE05_IMMUNE)) {
    outcome <- module05_get_immune(outcome_key)
    message("  - ", outcome$label)
    sd0 <- module05_baseline_sd(dat, outcome_key)

    baseline_data <- module05_prepare_baseline(dat, outcome_key, exposure_key)
    baseline_fit <- module05_fit_baseline_ols(baseline_data)
    baseline_scaled <- module05_scale_effect(
      baseline_fit$estimate, baseline_fit$lcl, baseline_fit$ucl,
      exposure$unit_scale, sd0
    )
    baseline_panel <- panel_info(exposure_key, "baseline")
    baseline_i <- baseline_i + 1L
    baseline_rows[[baseline_i]] <- data.frame(
      panel_id = baseline_panel$panel_id,
      panel_order = baseline_panel$panel_order,
      analysis_type = "baseline",
      exposure_key = exposure_key,
      exposure = exposure$label,
      effect_scale = exposure$effect_scale,
      visit = "",
      outcome_key = outcome_key,
      outcome = outcome$label,
      unit = outcome$unit,
      outcome_label = outcome$display_label,
      outcome_order = outcome$display_order,
      estimate = baseline_scaled$estimate_sd,
      se = baseline_fit$se * exposure$unit_scale / sd0,
      lower = baseline_scaled$lcl_sd,
      upper = baseline_scaled$ucl_sd,
      p = baseline_fit$p,
      interaction_p = NA_real_,
      n_subjects = length(unique(baseline_data$id)),
      n_observations = nrow(baseline_data),
      covariance = "Independent OLS",
      fallback_used = "No",
      stringsAsFactors = FALSE
    )
    log_i <- log_i + 1L
    log_rows[[log_i]] <- module05_ols_log_row(
      outcome$label, exposure$label, baseline_data, baseline_fit
    )

    cumulative_data <- module05_prepare_cumulative(dat, outcome_key, exposure_key)
    overall_formula <- outcome_value ~ visit + delta_adiposity +
      baseline_adiposity + baseline_outcome + sex + age
    overall_fit <- module05_fit_with_fallback(
      cumulative_data, overall_formula, time_var = "visit", reml = TRUE
    )
    overall_trend <- module05_emtrends_summary(
      overall_fit$fit, specs = ~ 1, var = "delta_adiposity"
    )
    overall_sm <- overall_trend$table[1L, , drop = FALSE]
    overall_scaled <- module05_scale_effect(
      overall_sm$estimate, overall_sm$lcl, overall_sm$ucl,
      exposure$unit_scale, sd0
    )
    overall_panel <- panel_info(exposure_key, "overall change")
    overall_i <- overall_i + 1L
    overall_rows[[overall_i]] <- data.frame(
      panel_id = overall_panel$panel_id,
      panel_order = overall_panel$panel_order,
      analysis_type = "overall change",
      exposure_key = exposure_key,
      exposure = exposure$label,
      effect_scale = exposure$effect_scale,
      visit = "",
      outcome_key = outcome_key,
      outcome = outcome$label,
      unit = outcome$unit,
      outcome_label = outcome$display_label,
      outcome_order = outcome$display_order,
      estimate = overall_scaled$estimate_sd,
      se = overall_sm$se * exposure$unit_scale / sd0,
      lower = overall_scaled$lcl_sd,
      upper = overall_scaled$ucl_sd,
      p = overall_sm$p,
      interaction_p = NA_real_,
      n_subjects = length(unique(cumulative_data$id)),
      n_observations = nrow(cumulative_data),
      covariance = overall_fit$covariance,
      fallback_used = if (overall_fit$fallback) "Yes" else "No",
      stringsAsFactors = FALSE
    )
    log_i <- log_i + 1L
    log_rows[[log_i]] <- module05_model_log_row(
      outcome$label, exposure$label, "overall change",
      "Overall postoperative change-from-baseline MMRM",
      cumulative_data, overall_fit, overall_formula, "visit"
    )

    visit_formula <- outcome_value ~ visit * delta_adiposity +
      baseline_adiposity + baseline_outcome + sex + age
    visit_fit <- module05_fit_with_fallback(
      cumulative_data, visit_formula, time_var = "visit", reml = TRUE
    )
    visit_trends <- module05_emtrends_summary(
      visit_fit$fit, specs = ~ visit, var = "delta_adiposity"
    )
    visit_sm <- visit_trends$table
    if (!"visit" %in% names(visit_sm)) {
      stop("Visit-specific emtrends output lacks visit.", call. = FALSE)
    }
    visit_sm$visit <- as.character(visit_sm$visit)
    visit_sm <- visit_sm[
      match(MODULE05_CONFIG$post_visit_levels, visit_sm$visit),
      , drop = FALSE
    ]
    if (anyNA(visit_sm$visit)) {
      stop("Could not align M1/M3/M6 visit-specific slopes.", call. = FALSE)
    }
    interaction_p <- module05_joint_slope_heterogeneity_p(
      visit_trends$object, MODULE05_CONFIG$post_visit_levels
    )
    visit_panel <- panel_info(exposure_key, "visit-specific")

    for (j in seq_len(nrow(visit_sm))) {
      scaled_j <- module05_scale_effect(
        visit_sm$estimate[j], visit_sm$lcl[j], visit_sm$ucl[j],
        exposure$unit_scale, sd0
      )
      visit_label <- visit_sm$visit[j]
      visit_data <- cumulative_data[
        cumulative_data$visit == visit_label, , drop = FALSE
      ]
      visit_i <- visit_i + 1L
      visit_rows[[visit_i]] <- data.frame(
        panel_id = visit_panel$panel_id,
        panel_order = visit_panel$panel_order,
        analysis_type = "visit-specific",
        exposure_key = exposure_key,
        exposure = exposure$label,
        effect_scale = exposure$effect_scale,
        visit = visit_label,
        outcome_key = outcome_key,
        outcome = outcome$label,
        unit = outcome$unit,
        outcome_label = outcome$display_label,
        outcome_order = outcome$display_order,
        estimate = scaled_j$estimate_sd,
        se = visit_sm$se[j] * exposure$unit_scale / sd0,
        lower = scaled_j$lcl_sd,
        upper = scaled_j$ucl_sd,
        p = visit_sm$p[j],
        interaction_p = interaction_p,
        n_subjects = length(unique(visit_data$id)),
        n_observations = nrow(visit_data),
        covariance = visit_fit$covariance,
        fallback_used = if (visit_fit$fallback) "Yes" else "No",
        stringsAsFactors = FALSE
      )
    }
    log_i <- log_i + 1L
    log_rows[[log_i]] <- module05_model_log_row(
      outcome$label, exposure$label, "visit-specific",
      "Visit-specific cumulative-change MMRM",
      cumulative_data, visit_fit, visit_formula, "visit"
    )
  }
}

plot_data <- do.call(rbind, c(baseline_rows, overall_rows, visit_rows))
plot_data <- plot_data[order(
  plot_data$panel_order, plot_data$outcome_order,
  match(plot_data$visit, c("", MODULE05_CONFIG$post_visit_levels))
), , drop = FALSE]
rownames(plot_data) <- NULL

if (nrow(plot_data) != 60L) stop("Figure 3 plot-ready row-count mismatch.", call. = FALSE)
if (sum(plot_data$analysis_type == "baseline") != 12L ||
    sum(plot_data$analysis_type == "overall change") != 12L ||
    sum(plot_data$analysis_type == "visit-specific") != 36L) {
  stop("Figure 3 plot-ready analysis-type counts are incorrect.", call. = FALSE)
}
if (any(!is.finite(plot_data$estimate)) || any(!is.finite(plot_data$se)) ||
    any(!is.finite(plot_data$lower)) || any(!is.finite(plot_data$upper)) ||
    any(!is.finite(plot_data$p))) {
  stop("Figure 3 plot-ready estimates contain non-finite values.", call. = FALSE)
}
module05_write_csv(plot_data, plot_data_file)
message("[PASS] Figure 3 plot-ready data written: ", plot_data_file)


single_sample_size <- function(values, label) {
  sizes <- unique(as.integer(values[!is.na(values)]))
  if (length(sizes) != 1L || !is.finite(sizes)) {
    stop("Could not resolve Figure 3 sample size for ", label, ".", call. = FALSE)
  }
  sizes[[1L]]
}

make_figure3_table_body <- function(exposure_key) {
  rows <- vector("list", length(MODULE05_IMMUNE))
  for (i in seq_along(MODULE05_IMMUNE)) {
    outcome <- MODULE05_IMMUNE[[i]]
    baseline_z <- plot_data[
      plot_data$exposure_key == exposure_key &
        plot_data$outcome_key == outcome$key &
        plot_data$analysis_type == "baseline",
      , drop = FALSE
    ]
    overall_z <- plot_data[
      plot_data$exposure_key == exposure_key &
        plot_data$outcome_key == outcome$key &
        plot_data$analysis_type == "overall change",
      , drop = FALSE
    ]
    visit_z <- plot_data[
      plot_data$exposure_key == exposure_key &
        plot_data$outcome_key == outcome$key &
        plot_data$analysis_type == "visit-specific",
      , drop = FALSE
    ]
    visit_z <- visit_z[
      match(MODULE05_CONFIG$post_visit_levels, visit_z$visit),
      , drop = FALSE
    ]
    interaction_values <- unique(visit_z$interaction_p)
    if (nrow(baseline_z) != 1L || nrow(overall_z) != 1L ||
        nrow(visit_z) != 3L || anyNA(visit_z$visit) ||
        length(interaction_values) != 1L || !is.finite(interaction_values)) {
      stop(
        "Could not construct Table S11 row for ", exposure_key,
        " / ", outcome$key, ".", call. = FALSE
      )
    }
    rows[[i]] <- data.frame(
      Outcome = outcome$label,
      Baseline_Effect = module05_format_effect_ci(
        baseline_z$estimate, baseline_z$lower, baseline_z$upper
      ),
      Baseline_P = module05_format_p(baseline_z$p),
      Overall_Effect = module05_format_effect_ci(
        overall_z$estimate, overall_z$lower, overall_z$upper
      ),
      Overall_P = module05_format_p(overall_z$p),
      M1_Effect = module05_format_effect_ci(
        visit_z$estimate[1L], visit_z$lower[1L], visit_z$upper[1L]
      ),
      M1_P = module05_format_p(visit_z$p[1L]),
      M3_Effect = module05_format_effect_ci(
        visit_z$estimate[2L], visit_z$lower[2L], visit_z$upper[2L]
      ),
      M3_P = module05_format_p(visit_z$p[2L]),
      M6_Effect = module05_format_effect_ci(
        visit_z$estimate[3L], visit_z$lower[3L], visit_z$upper[3L]
      ),
      M6_P = module05_format_p(visit_z$p[3L]),
      Interaction_P = module05_format_p(interaction_values),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

sample_sizes <- c(
  baseline = single_sample_size(
    plot_data$n_subjects[plot_data$analysis_type == "baseline"], "baseline"
  ),
  overall_subjects = single_sample_size(
    plot_data$n_subjects[plot_data$analysis_type == "overall change"],
    "overall participants"
  ),
  overall_observations = single_sample_size(
    plot_data$n_observations[plot_data$analysis_type == "overall change"],
    "overall observations"
  ),
  M1 = single_sample_size(
    plot_data$n_subjects[
      plot_data$analysis_type == "visit-specific" & plot_data$visit == "M1"
    ], "M1"
  ),
  M3 = single_sample_size(
    plot_data$n_subjects[
      plot_data$analysis_type == "visit-specific" & plot_data$visit == "M3"
    ], "M3"
  ),
  M6 = single_sample_size(
    plot_data$n_subjects[
      plot_data$analysis_type == "visit-specific" & plot_data$visit == "M6"
    ], "M6"
  )
)

module05_write_table_s11_xlsx(
  make_figure3_table_body("bmi"),
  make_figure3_table_body("weight"),
  sample_sizes,
  table_path
)

figure_specs <- list(
  list(panel = "a", exposure = "bmi", type = "baseline", file = "Fig3a_forest_baseline_bmi.png",
       title = "Baseline association: BMI (per 2 kg/m²)", xlim = MODULE05_CONFIG$forest_baseline_xlim,
       width = MODULE05_CONFIG$forest_width_cm, height = MODULE05_CONFIG$forest_height_cm),
  list(panel = "b", exposure = "bmi", type = "overall change", file = "Fig3b_forest_overall_change_bmi.png",
       title = "Overall postoperative change: ΔBMI (per 2 kg/m²)", xlim = MODULE05_CONFIG$forest_overall_xlim,
       width = MODULE05_CONFIG$forest_width_cm, height = MODULE05_CONFIG$forest_height_cm),
  list(panel = "c", exposure = "bmi", type = "visit-specific", file = "Fig3c_forest_visit_specific_change_bmi.png",
       title = "Visit-specific postoperative change: ΔBMI (per 2 kg/m²)", xlim = MODULE05_CONFIG$forest_visit_xlim,
       width = MODULE05_CONFIG$forest_visit_width_cm, height = MODULE05_CONFIG$forest_visit_height_cm),
  list(panel = "d", exposure = "weight", type = "baseline", file = "Fig3d_forest_baseline_weight.png",
       title = "Baseline association: Weight (per 5 kg)", xlim = MODULE05_CONFIG$forest_baseline_xlim,
       width = MODULE05_CONFIG$forest_width_cm, height = MODULE05_CONFIG$forest_height_cm),
  list(panel = "e", exposure = "weight", type = "overall change", file = "Fig3e_forest_overall_change_weight.png",
       title = "Overall postoperative change: ΔWeight (per 5 kg)", xlim = MODULE05_CONFIG$forest_overall_xlim,
       width = MODULE05_CONFIG$forest_width_cm, height = MODULE05_CONFIG$forest_height_cm),
  list(panel = "f", exposure = "weight", type = "visit-specific", file = "Fig3f_forest_visit_specific_change_weight.png",
       title = "Visit-specific postoperative change: ΔWeight (per 5 kg)", xlim = MODULE05_CONFIG$forest_visit_xlim,
       width = MODULE05_CONFIG$forest_visit_width_cm, height = MODULE05_CONFIG$forest_visit_height_cm)
)

for (spec in figure_specs) {
  z <- plot_data[
    plot_data$panel_id == spec$panel,
    , drop = FALSE
  ]
  if (identical(spec$type, "visit-specific")) {
    z$visit <- factor(z$visit, levels = MODULE05_CONFIG$post_visit_levels)
  }
  plot <- module05_make_forest(
    z, spec$title, spec$xlim, style,
    facet_column = if (identical(spec$type, "visit-specific")) "visit" else NULL
  )
  module05_save_rgb_png(
    plot, file.path(MODULE05_FIG3_DIR, spec$file),
    spec$width, spec$height, style
  )
}

figure_files <- list.files(
  MODULE05_FIG3_DIR, pattern = "^Fig3[a-f]_.*\\.png$", full.names = TRUE
)
if (length(figure_files) != 6L) stop("Expected exactly six Figure 3 standalone panels.", call. = FALSE)

model_log <- do.call(rbind, log_rows)
if (nrow(model_log) != 36L) stop("Primary immune model-log row-count mismatch.", call. = FALSE)
module05_update_model_log(
  model_log, replace_analysis_types = c("baseline", "overall change", "visit-specific")
)

message("[PASS] sample check: 488 rows; 122 participants; Non-BE=66; BE=56")
message("[PASS] primary immune models completed: 12 baseline OLS + 24 MMRM")
message("[PASS] Figure 3 standalone panels written: 6")
message("[PASS] module 05 primary workflow completed")
