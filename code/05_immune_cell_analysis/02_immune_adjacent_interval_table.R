# Adjacent-interval immune-cell change models for Supplementary Table S12.
# This secondary analysis uses true M0→M1, M1→M3, and M3→M6 changes.

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
for (directory in c(MODULE05_TABLE_DIR, MODULE05_INTERMEDIATE_DIR)) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
}

dat <- module05_load_data()
table_path <- file.path(MODULE05_TABLE_DIR, "TableS12_adjacent_change_associations.xlsx")
legacy_table_path <- file.path(MODULE05_TABLE_DIR, "TableS11_adjacent_change_associations.xlsx")
unlink(c(table_path, legacy_table_path))

rows <- list()
logs <- list()
row_i <- log_i <- 0L

for (exposure_key in c("weight", "bmi")) {
  exposure <- module05_get_adiposity(exposure_key)
  message("\nExposure: ", exposure$label)

  for (outcome_key in names(MODULE05_IMMUNE)) {
    outcome <- module05_get_immune(outcome_key)
    message("  - ", outcome$label)
    sd0 <- module05_baseline_sd(dat, outcome_key)
    interval_data <- module05_prepare_adjacent(dat, outcome_key, exposure_key)

    fixed_formula <- delta_outcome ~ interval * delta_adiposity +
      start_adiposity + start_outcome + sex + age
    fit_result <- module05_fit_with_fallback(
      interval_data, fixed_formula, time_var = "interval", reml = TRUE
    )
    trends <- module05_emtrends_summary(
      fit_result$fit, specs = ~ interval, var = "delta_adiposity"
    )
    sm <- trends$table
    if (!"interval" %in% names(sm)) {
      stop("Adjacent-interval emtrends output lacks interval.", call. = FALSE)
    }
    sm$interval <- as.character(sm$interval)
    sm <- sm[
      match(MODULE05_CONFIG$interval_levels, sm$interval),
      , drop = FALSE
    ]
    if (anyNA(sm$interval)) {
      stop("Could not align M0→M1, M1→M3, and M3→M6 slopes.", call. = FALSE)
    }
    interaction_p <- module05_joint_slope_heterogeneity_p(
      trends$object, MODULE05_CONFIG$interval_levels
    )

    for (j in seq_len(nrow(sm))) {
      interval_code <- sm$interval[j]
      subset_j <- interval_data[
        interval_data$interval == interval_code, , drop = FALSE
      ]
      scaled_j <- module05_scale_effect(
        sm$estimate[j], sm$lcl[j], sm$ucl[j],
        exposure$unit_scale, sd0
      )
      row_i <- row_i + 1L
      rows[[row_i]] <- data.frame(
        outcome_key = outcome_key,
        outcome = outcome$label,
        outcome_order = outcome$display_order,
        exposure_key = exposure_key,
        exposure = exposure$label,
        interval = interval_code,
        interval_order = match(interval_code, MODULE05_CONFIG$interval_levels),
        n_pairs = nrow(subset_j),
        n_participants = length(unique(subset_j$id)),
        estimate = scaled_j$estimate_sd,
        lower = scaled_j$lcl_sd,
        upper = scaled_j$ucl_sd,
        p = sm$p[j],
        interaction_p = interaction_p,
        covariance = fit_result$covariance,
        fallback_used = if (fit_result$fallback) "Yes" else "No",
        stringsAsFactors = FALSE
      )
    }

    log_i <- log_i + 1L
    logs[[log_i]] <- module05_model_log_row(
      outcome$label, exposure$label, "adjacent interval",
      "Adjacent-interval change-score MMRM",
      interval_data, fit_result, fixed_formula, "interval"
    )
  }
}

result <- do.call(rbind, rows)
model_log <- do.call(rbind, logs)
if (nrow(result) != 36L) stop("Table S12 result row-count mismatch.", call. = FALSE)
if (nrow(model_log) != 12L) stop("Adjacent model-log row-count mismatch.", call. = FALSE)

result <- result[order(
  match(result$exposure_key, c("weight", "bmi")),
  result$outcome_order, result$interval_order
), , drop = FALSE]
rownames(result) <- NULL

interval_n_list <- lapply(
  MODULE05_CONFIG$interval_levels,
  function(code) unique(result$n_pairs[result$interval == code])
)
if (any(lengths(interval_n_list) != 1L)) {
  stop("Interval sample sizes vary across outcomes; cannot construct Table S12 headings.", call. = FALSE)
}
interval_n <- as.numeric(vapply(interval_n_list, `[`, numeric(1), 1L))

make_body <- function(exposure_key) {
  body_rows <- vector("list", length(MODULE05_IMMUNE))
  for (i in seq_along(MODULE05_IMMUNE)) {
    outcome <- MODULE05_IMMUNE[[i]]
    z <- result[
      result$exposure_key == exposure_key & result$outcome_key == outcome$key,
      , drop = FALSE
    ]
    z <- z[match(MODULE05_CONFIG$interval_levels, z$interval), , drop = FALSE]
    if (nrow(z) != 3L || anyNA(z$interval)) {
      stop("Could not construct Table S12 row for ", exposure_key, " / ", outcome$key, ".", call. = FALSE)
    }
    body_rows[[i]] <- data.frame(
      Outcome = outcome$label,
      Effect_01 = module05_format_effect_ci(z$estimate[1L], z$lower[1L], z$upper[1L]),
      P_01 = module05_format_p(z$p[1L]),
      Effect_13 = module05_format_effect_ci(z$estimate[2L], z$lower[2L], z$upper[2L]),
      P_13 = module05_format_p(z$p[2L]),
      Effect_36 = module05_format_effect_ci(z$estimate[3L], z$lower[3L], z$upper[3L]),
      P_36 = module05_format_p(z$p[3L]),
      Interaction_P = module05_format_p(z$interaction_p[1L]),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, body_rows)
}

weight_body <- make_body("weight")
bmi_body <- make_body("bmi")
module05_write_table_s12_xlsx(
  weight_body, bmi_body, interval_n, table_path
)
module05_update_model_log(
  model_log, replace_analysis_types = "adjacent interval"
)

message("[PASS] adjacent-interval models completed: 12")
message(
  "[PASS] interval pair counts: M0→M1=", interval_n[1L],
  "; M1→M3=", interval_n[2L], "; M3→M6=", interval_n[3L]
)
message("[PASS] module 05 adjacent-interval workflow completed")
