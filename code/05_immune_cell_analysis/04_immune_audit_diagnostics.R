# Internal diagnostics and prespecified sensitivity checks for module 05.
# Results remain in the R session; no diagnostic files are written.

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
dat <- module05_load_data()
show_plots <- interactive() && !identical(Sys.getenv("MODULE05_SHOW_DIAGNOSTIC_PLOTS"), "0")
heavy_audit <- identical(Sys.getenv("MODULE05_RUN_HEAVY_AUDIT"), "1")

module05_vcov_hc3 <- function(fit) {
  X <- stats::model.matrix(fit)
  e <- stats::residuals(fit)
  h <- stats::hatvalues(fit)
  if (any(1 - h <= 1e-10)) {
    stop("HC3 cannot be computed because leverage is effectively one.", call. = FALSE)
  }
  w <- (e / (1 - h))^2
  bread <- solve(crossprod(X))
  meat <- crossprod(X, sweep(X, 1L, w, `*`))
  bread %*% meat %*% bread
}

module05_baseline_spline_p <- function(linear_fit, data) {
  spline_fit <- stats::lm(
    outcome_value ~ splines::ns(adiposity, df = 3) + age + sex,
    data = data
  )
  comparison <- try(stats::anova(linear_fit, spline_fit), silent = TRUE)
  if (inherits(comparison, "try-error") || nrow(comparison) < 2L ||
      !"Pr(>F)" %in% names(comparison)) {
    return(list(
      p = NA_real_, AIC_linear = stats::AIC(linear_fit),
      AIC_spline = stats::AIC(spline_fit)
    ))
  }
  list(
    p = as.numeric(comparison$`Pr(>F)`[2L]),
    AIC_linear = stats::AIC(linear_fit),
    AIC_spline = stats::AIC(spline_fit)
  )
}

module05_residual_plot <- function(fitted_values, residual_values, y_label) {
  frame <- data.frame(fitted = fitted_values, residual = residual_values)
  ggplot2::ggplot(frame, ggplot2::aes(x = fitted, y = residual)) +
    ggplot2::geom_hline(
      yintercept = 0, linetype = "dashed", colour = "grey65", linewidth = 0.25
    ) +
    ggplot2::geom_point(colour = "grey30", size = 1.1, alpha = 0.75) +
    ggplot2::labs(x = "Fitted value", y = y_label) +
    module05_theme_sci(style)
}

module05_qq_plot <- function(residual_values) {
  qq <- stats::qqnorm(residual_values, plot.it = FALSE)
  frame <- data.frame(theoretical = qq$x, sample = qq$y)
  theoretical_quartiles <- stats::qnorm(c(0.25, 0.75))
  sample_quartiles <- stats::quantile(
    residual_values, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE
  )
  slope <- diff(sample_quartiles) / diff(theoretical_quartiles)
  intercept <- sample_quartiles[1L] - slope * theoretical_quartiles[1L]
  ggplot2::ggplot(frame, ggplot2::aes(x = theoretical, y = sample)) +
    ggplot2::geom_abline(
      intercept = intercept, slope = slope,
      linetype = "dashed", colour = "grey65", linewidth = 0.25
    ) +
    ggplot2::geom_point(colour = "grey30", size = 1.1, alpha = 0.75) +
    ggplot2::labs(x = "Theoretical normal quantiles", y = "Residual quantiles") +
    module05_theme_sci(style)
}

module05_show_plot <- function(plot, label) {
  if (show_plots) {
    print(plot)
    message("[PLOT] ", label)
  }
  invisible(NULL)
}

module05_add_nonlinear_basis <- function(data, xvar = "delta_adiposity") {
  x <- data[[xvar]]
  if (length(unique(x)) < 8L) return(list(data = data, terms = character()))
  basis <- splines::ns(x, df = 3, intercept = FALSE)
  linear_space <- cbind(1, x)
  residual_basis <- qr.resid(qr(linear_space), basis)
  qr_basis <- qr(residual_basis, tol = 1e-9)
  rank <- qr_basis$rank
  if (rank < 1L) return(list(data = data, terms = character()))
  Q <- qr.Q(qr_basis, complete = FALSE)[, seq_len(rank), drop = FALSE]
  terms <- paste0("delta_nl", seq_len(rank))
  for (j in seq_len(rank)) data[[terms[j]]] <- Q[, j]
  list(data = data, terms = terms)
}

module05_joint_wald_p <- function(fit, terms) {
  if (!length(terms)) return(NA_real_)
  beta <- stats::coef(fit)
  covariance <- stats::vcov(fit)
  index <- match(terms, names(beta))
  if (anyNA(index)) return(NA_real_)
  q <- as.numeric(beta[index])
  U <- as.matrix(covariance[index, index, drop = FALSE])
  statistic <- try(as.numeric(t(q) %*% solve(U, q)), silent = TRUE)
  if (inherits(statistic, "try-error") || !is.finite(statistic)) return(NA_real_)
  stats::pchisq(statistic, df = length(terms), lower.tail = FALSE)
}

module05_try_mmrm <- function(data, fixed_formula, time_var = "visit") {
  result <- try(
    module05_fit_with_fallback(data, fixed_formula, time_var = time_var, reml = TRUE),
    silent = TRUE
  )
  if (inherits(result, "try-error")) return(NULL)
  result
}

module05_try_overall_trend <- function(fit_result) {
  if (is.null(fit_result)) return(NULL)
  result <- try(
    module05_emtrends_summary(
      fit_result$fit, specs = ~ 1, var = "delta_adiposity"
    ),
    silent = TRUE
  )
  if (inherits(result, "try-error")) return(NULL)
  result$table[1L, , drop = FALSE]
}

baseline_audit_rows <- list()
baseline_influence_rows <- list()
longitudinal_audit_rows <- list()
bes_sensitivity_rows <- list()
log1p_rows <- list()
targeted_deletion_rows <- list()
full_loo_rows <- list()
bi <- bii <- li <- bsi <- lgi <- tdi <- looi <- 0L

message("1/4 Baseline OLS diagnostics")
for (exposure_key in c("bmi", "weight")) {
  exposure <- module05_get_adiposity(exposure_key)
  for (outcome_key in names(MODULE05_IMMUNE)) {
    outcome <- module05_get_immune(outcome_key)
    baseline_data <- module05_prepare_baseline(dat, outcome_key, exposure_key)
    baseline_fit <- stats::lm(
      outcome_value ~ adiposity + age + sex, data = baseline_data
    )
    coefficient <- summary(baseline_fit)$coefficients["adiposity", , drop = TRUE]
    hc3 <- module05_vcov_hc3(baseline_fit)
    adiposity_index <- match("adiposity", names(stats::coef(baseline_fit)))
    hc3_se <- sqrt(hc3[adiposity_index, adiposity_index])
    hc3_t <- unname(stats::coef(baseline_fit)[adiposity_index]) / hc3_se
    hc3_p <- 2 * stats::pt(-abs(hc3_t), df = baseline_fit$df.residual)
    spline_check <- module05_baseline_spline_p(baseline_fit, baseline_data)

    bi <- bi + 1L
    baseline_audit_rows[[bi]] <- data.frame(
      Outcome = outcome$label,
      Exposure = exposure$label,
      N = nrow(baseline_data),
      OLS_estimate_per_1_unit = unname(coefficient["Estimate"]),
      OLS_SE = unname(coefficient["Std. Error"]),
      OLS_P = unname(coefficient["Pr(>|t|)"]),
      HC3_SE = hc3_se,
      HC3_P = hc3_p,
      Natural_spline_nonlin_P = spline_check$p,
      AIC_linear = spline_check$AIC_linear,
      AIC_spline = spline_check$AIC_spline,
      Max_Cooks_distance = max(stats::cooks.distance(baseline_fit), na.rm = TRUE),
      Max_leverage = max(stats::hatvalues(baseline_fit), na.rm = TRUE),
      stringsAsFactors = FALSE
    )

    influence <- data.frame(
      id = as.character(baseline_data$id),
      Cooks_distance = as.numeric(stats::cooks.distance(baseline_fit)),
      Leverage = as.numeric(stats::hatvalues(baseline_fit)),
      Studentized_residual = as.numeric(stats::rstudent(baseline_fit)),
      stringsAsFactors = FALSE
    )
    influence$Flag <- influence$Cooks_distance > 4 / nrow(baseline_data) |
      abs(influence$Studentized_residual) > 3
    influence$Influence_rank <- rank(
      -influence$Cooks_distance, ties.method = "first"
    )
    keep <- influence$Flag | influence$Influence_rank <= 5L
    if (any(keep)) {
      selected <- influence[keep, , drop = FALSE]
      selected$Outcome <- outcome$label
      selected$Exposure <- exposure$label
      bii <- bii + 1L
      baseline_influence_rows[[bii]] <- selected[c(
        "Outcome", "Exposure", "id", "Cooks_distance", "Leverage",
        "Studentized_residual", "Flag", "Influence_rank"
      )]
    }

    module05_show_plot(
      module05_residual_plot(
        stats::fitted(baseline_fit), stats::residuals(baseline_fit), "OLS residual"
      ),
      paste(exposure$label, outcome$label, "baseline residuals")
    )
    module05_show_plot(
      module05_qq_plot(stats::residuals(baseline_fit)),
      paste(exposure$label, outcome$label, "baseline Q-Q")
    )

    if (outcome_key %in% c("eosinophils", "basophils")) {
      log_data <- baseline_data
      log_data$outcome_value <- log1p(log_data$outcome_value)
      log_fit <- stats::lm(
        outcome_value ~ adiposity + age + sex, data = log_data
      )
      log_coefficient <- summary(log_fit)$coefficients["adiposity", , drop = TRUE]
      lgi <- lgi + 1L
      log1p_rows[[lgi]] <- data.frame(
        Analysis = "Baseline OLS",
        Outcome = outcome$label,
        Exposure = exposure$label,
        Scale = "log1p(outcome)",
        N_subjects = length(unique(log_data$id)),
        N_observations = nrow(log_data),
        Estimate_per_1_unit = unname(log_coefficient["Estimate"]),
        SE = unname(log_coefficient["Std. Error"]),
        P = unname(log_coefficient["Pr(>|t|)"]),
        Covariance = "Independent OLS",
        stringsAsFactors = FALSE
      )
    }
  }
}

message("2/4 Longitudinal diagnostics and prespecified sensitivities")
key_loo_outcomes <- c("wbc", "neutrophils", "monocytes", "basophils")
for (exposure_key in c("bmi", "weight")) {
  exposure <- module05_get_adiposity(exposure_key)
  for (outcome_key in names(MODULE05_IMMUNE)) {
    outcome <- module05_get_immune(outcome_key)
    message("  - ", exposure$label, " / ", outcome$label)
    cumulative_data <- module05_prepare_cumulative(dat, outcome_key, exposure_key)
    fixed_formula <- outcome_value ~ visit + delta_adiposity +
      baseline_adiposity + baseline_outcome + sex + age
    main_fit <- module05_fit_with_fallback(
      cumulative_data, fixed_formula, time_var = "visit", reml = TRUE
    )
    main_trend <- module05_emtrends_summary(
      main_fit$fit, specs = ~ 1, var = "delta_adiposity"
    )$table[1L, , drop = FALSE]

    fitted_values <- try(stats::fitted(main_fit$fit), silent = TRUE)
    residual_values <- try(stats::residuals(main_fit$fit), silent = TRUE)
    residual_available <- !inherits(fitted_values, "try-error") &&
      !inherits(residual_values, "try-error") &&
      length(fitted_values) == length(residual_values) &&
      length(residual_values) == nrow(cumulative_data)
    if (residual_available) {
      module05_show_plot(
        module05_residual_plot(
          as.numeric(fitted_values), as.numeric(residual_values), "MMRM residual"
        ),
        paste(exposure$label, outcome$label, "longitudinal residuals")
      )
      module05_show_plot(
        module05_qq_plot(as.numeric(residual_values)),
        paste(exposure$label, outcome$label, "longitudinal Q-Q")
      )
    }

    basis <- module05_add_nonlinear_basis(cumulative_data, "delta_adiposity")
    nonlinear_p <- NA_real_
    spline_covariance <- NA_character_
    if (length(basis$terms)) {
      rhs <- c(
        "visit", "delta_adiposity", basis$terms,
        "baseline_adiposity", "baseline_outcome", "sex", "age"
      )
      spline_formula <- stats::as.formula(
        paste("outcome_value ~", paste(rhs, collapse = " + "))
      )
      spline_fit <- module05_try_mmrm(
        basis$data, spline_formula, time_var = "visit"
      )
      if (!is.null(spline_fit)) {
        nonlinear_p <- module05_joint_wald_p(spline_fit$fit, basis$terms)
        spline_covariance <- spline_fit$covariance
      }
    }

    li <- li + 1L
    longitudinal_audit_rows[[li]] <- data.frame(
      Outcome = outcome$label,
      Exposure = exposure$label,
      N_subjects = length(unique(cumulative_data$id)),
      N_observations = nrow(cumulative_data),
      Main_estimate_per_1_unit = main_trend$estimate,
      Main_SE = main_trend$se,
      Main_P = main_trend$p,
      Main_covariance = main_fit$covariance,
      Residual_diagnostics_available = if (residual_available) "Yes" else "No",
      Natural_spline_nonlin_Wald_P = nonlinear_p,
      Spline_covariance = spline_covariance,
      stringsAsFactors = FALSE
    )

    bes_formula <- outcome_value ~ visit + delta_adiposity +
      baseline_adiposity + baseline_outcome + sex + age + group
    bes_fit <- module05_try_mmrm(cumulative_data, bes_formula, time_var = "visit")
    bes_trend <- module05_try_overall_trend(bes_fit)
    if (!is.null(bes_trend)) {
      bsi <- bsi + 1L
      bes_sensitivity_rows[[bsi]] <- data.frame(
        Outcome = outcome$label,
        Exposure = exposure$label,
        N_subjects = length(unique(cumulative_data$id)),
        N_observations = nrow(cumulative_data),
        Main_estimate_per_1_unit = main_trend$estimate,
        Main_P = main_trend$p,
        BES_adjusted_estimate_per_1_unit = bes_trend$estimate,
        BES_adjusted_SE = bes_trend$se,
        BES_adjusted_P = bes_trend$p,
        Estimate_change = bes_trend$estimate - main_trend$estimate,
        Covariance = bes_fit$covariance,
        stringsAsFactors = FALSE
      )
    }

    if (outcome_key %in% c("eosinophils", "basophils")) {
      log_data <- cumulative_data
      log_data$outcome_value <- log1p(log_data$outcome_value)
      log_data$baseline_outcome <- log1p(log_data$baseline_outcome)
      log_formula <- outcome_value ~ visit + delta_adiposity +
        baseline_adiposity + baseline_outcome + sex + age
      log_fit <- module05_try_mmrm(log_data, log_formula, time_var = "visit")
      log_trend <- module05_try_overall_trend(log_fit)
      if (!is.null(log_trend)) {
        lgi <- lgi + 1L
        log1p_rows[[lgi]] <- data.frame(
          Analysis = "Postoperative cumulative-change MMRM",
          Outcome = outcome$label,
          Exposure = exposure$label,
          Scale = "log1p(outcome and baseline outcome)",
          N_subjects = length(unique(log_data$id)),
          N_observations = nrow(log_data),
          Estimate_per_1_unit = log_trend$estimate,
          SE = log_trend$se,
          P = log_trend$p,
          Covariance = log_fit$covariance,
          stringsAsFactors = FALSE
        )
      }
    }

    if (residual_available) {
      residual_frame <- data.frame(
        id = as.character(cumulative_data$id),
        abs_residual = abs(as.numeric(residual_values)),
        stringsAsFactors = FALSE
      )
      residual_score <- stats::aggregate(
        abs_residual ~ id, data = residual_frame, FUN = mean
      )
      residual_score <- residual_score[
        order(-residual_score$abs_residual), , drop = FALSE
      ]
      candidate_ids <- utils::head(residual_score$id, 5L)
      for (deleted_id in candidate_ids) {
        deleted_data <- cumulative_data[
          as.character(cumulative_data$id) != deleted_id, , drop = FALSE
        ]
        deleted_data$id <- droplevels(deleted_data$id)
        deleted_fit <- module05_try_mmrm(
          deleted_data, fixed_formula, time_var = "visit"
        )
        deleted_trend <- module05_try_overall_trend(deleted_fit)
        if (!is.null(deleted_trend)) {
          tdi <- tdi + 1L
          targeted_deletion_rows[[tdi]] <- data.frame(
            Outcome = outcome$label,
            Exposure = exposure$label,
            Deleted_participant = deleted_id,
            Residual_influence_rank = match(deleted_id, residual_score$id),
            Mean_absolute_residual = residual_score$abs_residual[
              match(deleted_id, residual_score$id)
            ],
            Main_estimate_per_1_unit = main_trend$estimate,
            Deleted_estimate_per_1_unit = deleted_trend$estimate,
            Estimate_change = deleted_trend$estimate - main_trend$estimate,
            Main_P = main_trend$p,
            Deleted_P = deleted_trend$p,
            Deleted_covariance = deleted_fit$covariance,
            stringsAsFactors = FALSE
          )
        }
      }
    }

    if (heavy_audit && outcome_key %in% key_loo_outcomes) {
      ids <- unique(as.character(cumulative_data$id))
      message("    Full LOO: ", length(ids), " refits")
      for (k in seq_along(ids)) {
        deleted_id <- ids[k]
        deleted_data <- cumulative_data[
          as.character(cumulative_data$id) != deleted_id, , drop = FALSE
        ]
        deleted_data$id <- droplevels(deleted_data$id)
        deleted_fit <- module05_try_mmrm(
          deleted_data, fixed_formula, time_var = "visit"
        )
        deleted_trend <- module05_try_overall_trend(deleted_fit)
        if (!is.null(deleted_trend)) {
          looi <- looi + 1L
          full_loo_rows[[looi]] <- data.frame(
            Outcome = outcome$label,
            Exposure = exposure$label,
            Deleted_participant = deleted_id,
            Main_estimate_per_1_unit = main_trend$estimate,
            Deleted_estimate_per_1_unit = deleted_trend$estimate,
            Estimate_change = deleted_trend$estimate - main_trend$estimate,
            Main_P = main_trend$p,
            Deleted_P = deleted_trend$p,
            Deleted_covariance = deleted_fit$covariance,
            stringsAsFactors = FALSE
          )
        }
        if (k %% 20L == 0L) message("      ", k, "/", length(ids))
      }
    }
  }
}

message("3/4 Multiplicity audit")
figure3_file <- file.path(MODULE05_PLOT_DATA_DIR, "_fig3_forest_estimates.csv")
if (!file.exists(figure3_file)) {
  stop("Run 01_immune_models_and_figure3.R before the audit.", call. = FALSE)
}
figure3 <- utils::read.csv(
  figure3_file, check.names = FALSE, stringsAsFactors = FALSE
)

adjacent_rows <- list()
adjacent_i <- 0L
for (exposure_key in c("weight", "bmi")) {
  exposure <- module05_get_adiposity(exposure_key)
  for (outcome_key in names(MODULE05_IMMUNE)) {
    outcome <- module05_get_immune(outcome_key)
    interval_data <- module05_prepare_adjacent(dat, outcome_key, exposure_key)
    fixed_formula <- delta_outcome ~ interval * delta_adiposity +
      start_adiposity + start_outcome + sex + age
    fit <- module05_fit_with_fallback(
      interval_data, fixed_formula, time_var = "interval", reml = TRUE
    )
    trends <- module05_emtrends_summary(
      fit$fit, specs = ~ interval, var = "delta_adiposity"
    )$table
    trends$interval <- as.character(trends$interval)
    trends <- trends[
      match(MODULE05_CONFIG$interval_levels, trends$interval), , drop = FALSE
    ]
    for (j in seq_len(nrow(trends))) {
      adjacent_i <- adjacent_i + 1L
      adjacent_rows[[adjacent_i]] <- data.frame(
        Outcome = outcome$label,
        Exposure = exposure$label,
        Detail = trends$interval[j],
        P = trends$p[j],
        stringsAsFactors = FALSE
      )
    }
  }
}
adjacent_p <- do.call(rbind, adjacent_rows)

add_family <- function(frame, analysis, expected_n, detail_col = NULL) {
  rows <- list()
  index <- 0L
  for (exposure in c("BMI", "Weight")) {
    z <- frame[frame$Exposure == exposure, , drop = FALSE]
    if (nrow(z) != expected_n) {
      stop("Unexpected multiplicity family size for ", analysis, " / ", exposure, ".", call. = FALSE)
    }
    index <- index + 1L
    rows[[index]] <- data.frame(
      Analysis = analysis,
      Outcome = z$Outcome,
      Exposure = exposure,
      Detail = if (is.null(detail_col)) {
        rep("Overall", nrow(z))
      } else {
        as.character(z[[detail_col]])
      },
      P = as.numeric(z$P),
      BH_q = stats::p.adjust(as.numeric(z$P), method = "BH"),
      BH_family = paste0(analysis, " / ", exposure, " / n=", expected_n),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

baseline_p <- data.frame(
  Outcome = figure3$outcome[figure3$analysis_type == "baseline"],
  Exposure = figure3$exposure[figure3$analysis_type == "baseline"],
  P = figure3$p[figure3$analysis_type == "baseline"],
  stringsAsFactors = FALSE
)
overall_p <- data.frame(
  Outcome = figure3$outcome[figure3$analysis_type == "overall change"],
  Exposure = figure3$exposure[figure3$analysis_type == "overall change"],
  P = figure3$p[figure3$analysis_type == "overall change"],
  stringsAsFactors = FALSE
)
visit_p <- data.frame(
  Outcome = figure3$outcome[figure3$analysis_type == "visit-specific"],
  Exposure = figure3$exposure[figure3$analysis_type == "visit-specific"],
  Visit = figure3$visit[figure3$analysis_type == "visit-specific"],
  P = figure3$p[figure3$analysis_type == "visit-specific"],
  stringsAsFactors = FALSE
)
names(adjacent_p)[names(adjacent_p) == "Detail"] <- "Interval"

multiplicity_audit <- rbind(
  add_family(baseline_p, "Figure 3 baseline", 6L),
  add_family(overall_p, "Figure 3 overall cumulative change", 6L),
  add_family(visit_p, "Figure 3 visit-specific cumulative slopes", 18L, "Visit"),
  add_family(adjacent_p, "Table S12 adjacent-interval slopes", 18L, "Interval")
)

message("4/4 Audit summary")
baseline_audit <- do.call(rbind, baseline_audit_rows)
baseline_influence <- if (length(baseline_influence_rows)) {
  do.call(rbind, baseline_influence_rows)
} else data.frame()
longitudinal_audit <- do.call(rbind, longitudinal_audit_rows)
bes_sensitivity <- if (length(bes_sensitivity_rows)) {
  do.call(rbind, bes_sensitivity_rows)
} else data.frame()
log1p_sensitivity <- if (length(log1p_rows)) do.call(rbind, log1p_rows) else data.frame()
targeted_deletion <- if (length(targeted_deletion_rows)) {
  do.call(rbind, targeted_deletion_rows)
} else data.frame()
full_loo <- if (length(full_loo_rows)) do.call(rbind, full_loo_rows) else data.frame()

module05_audit_results <- list(
  baseline = baseline_audit,
  baseline_influence = baseline_influence,
  longitudinal = longitudinal_audit,
  bes_adjusted = bes_sensitivity,
  low_count_log1p = log1p_sensitivity,
  targeted_deletion = targeted_deletion,
  full_leave_one_out = full_loo,
  multiplicity = multiplicity_audit,
  plots_displayed = show_plots,
  heavy_audit = heavy_audit
)
assign("module05_audit_results", module05_audit_results, envir = .GlobalEnv)

baseline_flags <- sum(
  (!is.na(baseline_audit$Natural_spline_nonlin_P) &
     baseline_audit$Natural_spline_nonlin_P < 0.05) |
    ((baseline_audit$OLS_P < 0.05) != (baseline_audit$HC3_P < 0.05)),
  na.rm = TRUE
)
longitudinal_flags <- sum(
  !is.na(longitudinal_audit$Natural_spline_nonlin_Wald_P) &
    longitudinal_audit$Natural_spline_nonlin_Wald_P < 0.05,
  na.rm = TRUE
)
targeted_flags <- if (nrow(targeted_deletion)) {
  sum(
    sign(targeted_deletion$Main_estimate_per_1_unit) !=
      sign(targeted_deletion$Deleted_estimate_per_1_unit) |
      ((targeted_deletion$Main_P < 0.05) != (targeted_deletion$Deleted_P < 0.05)),
    na.rm = TRUE
  )
} else 0L

message("[PASS] baseline models audited: ", nrow(baseline_audit))
message("[PASS] longitudinal models audited: ", nrow(longitudinal_audit))
message("[PASS] BES-adjusted sensitivity models: ", nrow(bes_sensitivity))
message("[PASS] low-count log1p sensitivity models: ", nrow(log1p_sensitivity))
message("[PASS] targeted participant-deletion refits: ", nrow(targeted_deletion))
message("[PASS] multiplicity-audit rows: ", nrow(multiplicity_audit))
if (heavy_audit) message("[PASS] full leave-one-participant-out refits: ", nrow(full_loo))
if (baseline_flags > 0L) message("[WARN] baseline diagnostic flags: ", baseline_flags)
if (longitudinal_flags > 0L) message("[WARN] longitudinal nonlinearity flags: ", longitudinal_flags)
if (targeted_flags > 0L) message("[WARN] targeted-deletion conclusion changes: ", targeted_flags)
if (!show_plots) message("[INFO] diagnostic plots were calculated but not displayed in this session")
message("[PASS] audit results stored in the R object: module05_audit_results")
message("[PASS] no diagnostic files were written")
message("[PASS] module 05 internal audit completed")
