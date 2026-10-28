# Bidirectional signed-delta MNAR tipping-point analysis.
# Modes: check (no scientific output), preflight, confirm, and full.
# Checkpoints are retained only while the staged analysis is incomplete.

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
  stop(
    "Cannot locate 00_longitudinal_shared.R. Open the binge-eating-bariatric-analysis project and rerun.",
    call. = FALSE
  )
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
message("Module 04 fileset: ", MODULE04_FILESET_ID)
message("Module 04 MNAR sensitivity analysis version: ", MODULE04_ANALYSIS_VERSION)
message("Inference in every completed-data model: REML + ", MODULE04_CONFIG$df_method)
message("Formal settings: m=", MODULE04_CONFIG$mi_m, ", signed delta in [-", MODULE04_CONFIG$delta_max, ", +", MODULE04_CONFIG$delta_max, "]")

dat <- module04_load_data()
message("Validated standardized input schema: 47 columns; 30 longitudinal outcomes mapped exactly.")

module04_make_wide <- function(dat, outcome_key) {
  value_long <- dat[, c("id", "visit", outcome_key), drop = FALSE]
  names(value_long)[3L] <- "value"
  wide_value <- stats::reshape(value_long, idvar = "id", timevar = "visit", direction = "wide")
  names(wide_value) <- sub("^value\\.", "", names(wide_value))
  for (v in MODULE04_CONFIG$visit_levels) if (!v %in% names(wide_value)) wide_value[[v]] <- NA_real_
  sf <- dat[!duplicated(dat$id), c("id", "group", "sex", "age", "bmi0"), drop = FALSE]
  wide <- merge(sf, wide_value, by = "id", all.x = TRUE, sort = FALSE)
  wide <- wide[match(sf$id, wide$id), , drop = FALSE]
  observed_any <- rowSums(!is.na(wide[MODULE04_CONFIG$visit_levels])) > 0L
  wide <- wide[observed_any, , drop = FALSE]
  wide$.primary_eligible <- !is.na(wide$M0) & rowSums(!is.na(wide[MODULE04_CONFIG$post_visits])) > 0L
  wide$.baseline_eligible <- rowSums(!is.na(wide[MODULE04_CONFIG$visit_levels])) > 0L
  wide$.row_id <- seq_len(nrow(wide))
  wide
}

module04_change_sd <- function(wide) {
  out <- setNames(numeric(length(MODULE04_CONFIG$post_visits)), MODULE04_CONFIG$post_visits)
  for (v in MODULE04_CONFIG$post_visits) {
    ok <- !is.na(wide$M0) & !is.na(wide[[v]])
    if (sum(ok) < 5L) stop("Fewer than five observed baseline-to-", v, " pairs for SD-of-change scaling.", call. = FALSE)
    s <- stats::sd(wide[[v]][ok] - wide$M0[ok])
    if (!is.finite(s) || s <= 0) stop("Non-positive SD of change at ", v, call. = FALSE)
    out[[v]] <- s
  }
  out
}

module04_make_imputations <- function(wide, outcome_key, m, seed) {
  include_bmi <- !(outcome_key %in% MODULE04_BODY_SIZE_OUTCOMES)
  cols <- c("group", "sex", "age", if (include_bmi) "bmi0", MODULE04_CONFIG$visit_levels)
  imp_data <- wide[cols]
  imp_data$group <- factor(imp_data$group, levels = MODULE04_CONFIG$group_levels)
  imp_data$sex <- factor(imp_data$sex)

  method <- rep("", ncol(imp_data)); names(method) <- names(imp_data)
  for (v in MODULE04_CONFIG$visit_levels) if (anyNA(imp_data[[v]])) method[[v]] <- "pmm"
  pred <- matrix(0L, nrow = ncol(imp_data), ncol = ncol(imp_data), dimnames = list(names(imp_data), names(imp_data)))
  auxiliary <- c("group", "sex", "age", if (include_bmi) "bmi0")
  for (target in MODULE04_CONFIG$visit_levels) {
    if (method[[target]] == "") next
    predictors <- setdiff(c(auxiliary, MODULE04_CONFIG$visit_levels), target)
    pred[target, predictors] <- 1L
  }

  mids <- mice::mice(
  imp_data,
  m = m,
  maxit = MODULE04_CONFIG$mi_maxit,
  method = method,
  predictorMatrix = pred,
  donors = 5L,
  maxcor = 0.999,
  eps = if (identical(outcome_key, "basophils")) 1e-6 else 1e-4,
  seed = seed,
  printFlag = FALSE,
  remove.collinear = FALSE,
  remove.constant = FALSE
  )

  # A logged event is not automatically a failed imputation. In mice, events
  # recorded after iteration 0 can represent an automatic, local removal of a
  # linearly dependent predictor from one conditional imputation model. We do
  # not silently ignore such actions. We allow only rare transient removals in
  # which both the target and removed predictor are longitudinal visit values;
  # initialization events, auxiliary-covariate removals, non-PMM fallbacks, or
  # a non-rare event rate remain fatal.
  logged <- mids$loggedEvents
  if (is.null(logged)) {
    logged <- data.frame(
      it = integer(), im = integer(), dep = character(),
      meth = character(), out = character(),
      stringsAsFactors = FALSE
    )
  }
  logged_event_summary <- "None"
  if (nrow(logged) > 0L) {
    required_event_cols <- c("it", "im", "dep", "meth", "out")
    if (!all(required_event_cols %in% names(logged))) {
      stop(
        "MICE returned an unrecognized loggedEvents structure for ",
        MODULE04_OUTCOMES[[outcome_key]]$label, ".",
        call. = FALSE
      )
    }

    out_tokens <- unique(unlist(strsplit(
      gsub("[`'\"]", "", as.character(logged$out)),
      "[,[:space:]]+"
    )))
    out_tokens <- out_tokens[nzchar(out_tokens)]
    n_conditional_models <- max(
      1L,
      as.integer(m) * as.integer(MODULE04_CONFIG$mi_maxit) * sum(method != "")
    )
    event_fraction <- nrow(logged) / n_conditional_models

    allowed_transient <-
      all(suppressWarnings(as.integer(logged$it)) > 0L) &&
      all(as.character(logged$dep) %in% MODULE04_CONFIG$visit_levels) &&
      all(as.character(logged$meth) == "pmm") &&
      length(out_tokens) > 0L &&
      all(out_tokens %in% MODULE04_CONFIG$visit_levels) &&
      event_fraction <= 0.001 + 1e-12

    if (!allowed_transient) {
      stop(
        "MICE logged event(s) requiring model review for ",
        MODULE04_OUTCOMES[[outcome_key]]$label, ": ",
        paste(utils::capture.output(print(logged)), collapse = " | "),
        call. = FALSE
      )
    }

    logged_event_summary <- paste0(
      nrow(logged), " rare transient visit-predictor removal(s) across ",
      n_conditional_models, " conditional imputation fits: ",
      paste(utils::capture.output(print(logged)), collapse = " | ")
    )
    warning(
      "MICE audit for ", MODULE04_OUTCOMES[[outcome_key]]$label, ": ",
      logged_event_summary,
      call. = FALSE
    )
  }

  completed <- lapply(seq_len(m), function(i) {
    x <- mice::complete(mids, action = i)
    x$id <- wide$id
    x$.row_id <- wide$.row_id
    x$.primary_eligible <- wide$.primary_eligible
    x$.baseline_eligible <- wide$.baseline_eligible
    x <- x[order(x$.row_id), , drop = FALSE]
    x
  })
  list(
    mids = mids,
    completed = completed,
    predictor_matrix = pred,
    method = method,
    logged_events = logged,
    logged_event_summary = logged_event_summary
  )
}

module04_missing_masks <- function(wide) {
  lapply(setNames(MODULE04_CONFIG$post_visits, MODULE04_CONFIG$post_visits), function(v) is.na(wide[[v]]))
}

module04_apply_delta <- function(completed, masks, change_sd, delta, scope) {
  if (!scope %in% c("BE-only", "Both-arm symmetric")) stop("Unknown offset scope: ", scope, call. = FALSE)
  x <- completed
  for (v in MODULE04_CONFIG$post_visits) {
    originally_missing <- masks[[v]]
    if (scope == "BE-only") {
      idx_be <- originally_missing & as.character(x$group) == "BE"
      x[[v]][idx_be] <- x[[v]][idx_be] + delta * change_sd[[v]]
    } else {
      idx_be <- originally_missing & as.character(x$group) == "BE"
      idx_non <- originally_missing & as.character(x$group) == "Non-BE"
      x[[v]][idx_be] <- x[[v]][idx_be] + delta * change_sd[[v]]
      x[[v]][idx_non] <- x[[v]][idx_non] - delta * change_sd[[v]]
    }
  }
  x
}

module04_complete_primary_data <- function(x, outcome_key) {
  x <- x[x$.primary_eligible, , drop = FALSE]
  ids <- x$id
  visits <- MODULE04_CONFIG$post_visits
  n <- nrow(x)
  long <- data.frame(
    id = factor(rep(ids, each = length(visits))),
    group = factor(rep(x$group, each = length(visits)), levels = MODULE04_CONFIG$group_levels),
    sex = factor(rep(x$sex, each = length(visits))),
    visit = factor(rep(visits, times = n), levels = visits),
    outcome_value = as.numeric(t(as.matrix(x[visits]))),
    baseline_value = rep(x$M0, each = length(visits)),
    age = rep(x$age, each = length(visits)),
    bmi0 = if ("bmi0" %in% names(x)) rep(x$bmi0, each = length(visits)) else 0,
    stringsAsFactors = FALSE
  )
  long$baseline_c <- long$baseline_value - mean(x$M0)
  long$age_c <- long$age - mean(x$age)
  long$bmi0_c <- if ("bmi0" %in% names(x)) long$bmi0 - mean(x$bmi0) else 0
  long
}

module04_complete_baseline_data <- function(x, outcome_key) {
  x <- x[x$.baseline_eligible, , drop = FALSE]
  ids <- x$id
  visits <- MODULE04_CONFIG$visit_levels
  n <- nrow(x)
  long <- data.frame(
    id = factor(rep(ids, each = length(visits))),
    group = factor(rep(x$group, each = length(visits)), levels = MODULE04_CONFIG$group_levels),
    sex = factor(rep(x$sex, each = length(visits))),
    visit = factor(rep(visits, times = n), levels = visits),
    outcome_value = as.numeric(t(as.matrix(x[visits]))),
    age = rep(x$age, each = length(visits)),
    bmi0 = if ("bmi0" %in% names(x)) rep(x$bmi0, each = length(visits)) else 0,
    stringsAsFactors = FALSE
  )
  long$age_c <- long$age - mean(x$age)
  long$bmi0_c <- if ("bmi0" %in% names(x)) long$bmi0 - mean(x$bmi0) else 0
  long
}

module04_pool_d1 <- function(payloads) {
  m <- length(payloads)
  if (m < 2L) stop("At least two imputations are required for pooling.", call. = FALSE)
  Q <- do.call(rbind, lapply(payloads, function(z) as.numeric(z$q)))
  Ulist <- lapply(payloads, function(z) as.matrix(z$U))
  k <- ncol(Q)
  Ubar <- Reduce(`+`, Ulist) / m
  B <- if (k == 1L) matrix(stats::var(Q[, 1L]), 1L, 1L) else stats::cov(Q)
  qbar <- colMeans(Q)
  invU <- try(solve(Ubar), silent = TRUE)
  if (inherits(invU, "try-error")) stop("Singular average within-imputation covariance matrix.", call. = FALSE)
  r1 <- (1 + 1 / m) * sum(diag(B %*% invU)) / k
  r1 <- max(0, as.numeric(r1))
  statistic <- as.numeric(t(qbar) %*% invU %*% qbar) / (k * (1 + r1))
  v1 <- k * (m - 1)
  if (r1 < 1e-12) {
    df2 <- Inf
  } else if (v1 > 4) {
    df2 <- 4 + (v1 - 4) * (1 + (1 - 2 / v1) / r1)^2
  } else {
    df2 <- v1 * (1 + 1 / k) * (1 + 1 / r1)^2 / 2
  }
  p <- stats::pf(statistic, df1 = k, df2 = df2, lower.tail = FALSE)
  list(qbar = qbar, Ubar = Ubar, B = B, r = r1, statistic = statistic, df1 = k, df2 = df2, p = p)
}

module04_fit_completed_pair <- function(x, outcome_key, primary_covariance, baseline_covariance) {
  pdat <- module04_complete_primary_data(x, outcome_key)
  bdat <- module04_complete_baseline_data(x, outcome_key)
  pfit <- module04_fit_fixed_covariance(pdat, module04_fixed_formula("primary", outcome_key), primary_covariance)
  bfit <- module04_fit_fixed_covariance(bdat, module04_fixed_formula("baseline_included", outcome_key), baseline_covariance)
  pe <- module04_extract_primary_estimands(pfit)
  be <- module04_extract_baseline_estimands(bfit)
  list(
    time = be$payload_time,
    group_0_6 = be$payload_group_average,
    interaction = pe$payload_interaction,
    group_1_6 = pe$payload_group_average,
    dd = be$payload_dd,
    reversal = be$payload_reversal
  )
}

module04_evaluate_delta_factory <- function(completed, masks, change_sd, outcome_key, primary_covariance, baseline_covariance) {
  cache <- new.env(parent = emptyenv())
  function(delta, scope) {
    key <- paste(scope, sprintf("%+.2f", delta), sep = "|")
    if (exists(key, envir = cache, inherits = FALSE)) return(get(key, envir = cache, inherits = FALSE))
    message("    ", scope, ", delta=", sprintf("%+.2f", delta))
    fits <- lapply(seq_along(completed), function(i) {
      adjusted <- module04_apply_delta(completed[[i]], masks, change_sd, delta, scope)
      module04_fit_completed_pair(adjusted, outcome_key, primary_covariance, baseline_covariance)
    })
    pooled <- list(
      time = module04_pool_d1(lapply(fits, `[[`, "time")),
      group_0_6 = module04_pool_d1(lapply(fits, `[[`, "group_0_6")),
      interaction = module04_pool_d1(lapply(fits, `[[`, "interaction")),
      group_1_6 = module04_pool_d1(lapply(fits, `[[`, "group_1_6")),
      dd = module04_pool_d1(lapply(fits, `[[`, "dd")),
      reversal = module04_pool_d1(lapply(fits, `[[`, "reversal"))
    )
    rev_q <- pooled$reversal$qbar
    rev_state <- if (prod(rev_q) < 0) "Yes" else "No"
    pvals <- c(
      time = pooled$time$p,
      group_0_6 = pooled$group_0_6$p,
      interaction = pooled$interaction$p,
      group_1_6 = pooled$group_1_6$p,
      dd = pooled$dd$p
    )
    conclusion <- c(vapply(pvals, module04_conclusion, character(1)), reversal = rev_state)
    out <- list(p = pvals, conclusion = conclusion, reversal_q = rev_q, pooled = pooled)
    assign(key, out, envir = cache)
    out
  }
}

module04_main_reference <- function(dat, outcome_key) {
  pdat <- module04_prepare_primary(dat, outcome_key)
  pform <- module04_fixed_formula("primary", outcome_key)
  pfit <- module04_fit_with_fallback(pdat, pform)
  pe <- module04_extract_primary_estimands(pfit$fit)
  bdat <- module04_prepare_baseline_included(dat, outcome_key)
  bform <- module04_fixed_formula("baseline_included", outcome_key)
  bfit <- module04_fit_with_fallback(bdat, bform)
  be <- module04_extract_baseline_estimands(bfit$fit)
  pvals <- c(time = be$p_time, group_0_6 = be$p_group_average, interaction = pe$p_interaction, group_1_6 = pe$p_group_average, dd = be$p_dd)
  list(
    p = pvals,
    conclusion = c(vapply(pvals, module04_conclusion, character(1)), reversal = be$reversal),
    primary_covariance = pfit$covariance,
    baseline_covariance = bfit$covariance
  )
}

module04_opposite_target <- function(x) {
  switch(x, Sig = "NS", NS = "Sig", Yes = "No", No = "Yes", stop("Unknown conclusion: ", x))
}

module04_search_scope <- function(evaluate, delta0, main_ref, scope) {
  hypotheses <- names(main_ref$conclusion)
  if (!identical(unname(hypotheses), unname(names(delta0$conclusion)))) {
    stop("Main-model and delta=0 hypothesis sets do not align.", call. = FALSE)
  }

  coarse_abs <- seq(
    MODULE04_CONFIG$delta_coarse_step,
    MODULE04_CONFIG$delta_max,
    by = MODULE04_CONFIG$delta_coarse_step
  )

  # Evaluate each signed coarse scenario once; the evaluator caches all results.
  for (a in coarse_abs) {
    evaluate(-a, scope)
    evaluate(+a, scope)
  }

  find_one_direction <- function(h, sign_value, target) {
    coarse_hit <- NA_real_
    for (a in coarse_abs) {
      z <- evaluate(sign_value * a, scope)
      if (identical(unname(z$conclusion[[h]]), target)) {
        coarse_hit <- a
        break
      }
    }
    if (!is.finite(coarse_hit)) return(NULL)

    lower <- max(0, coarse_hit - MODULE04_CONFIG$delta_coarse_step)
    fine_abs <- seq(
      lower + MODULE04_CONFIG$delta_fine_step,
      coarse_hit,
      by = MODULE04_CONFIG$delta_fine_step
    )
    for (a in fine_abs) {
      z <- evaluate(sign_value * a, scope)
      if (identical(unname(z$conclusion[[h]]), target)) {
        return(list(delta = sign_value * a, abs_delta = a, target = target))
      }
    }
    stop("Coarse tipping point could not be recovered on the fine grid.", call. = FALSE)
  }

  details <- setNames(vector("list", length(hypotheses)), hypotheses)
  cells <- setNames(character(length(hypotheses)), hypotheses)

  for (h in hypotheses) {
    main_conclusion <- unname(main_ref$conclusion[[h]])
    delta0_conclusion <- unname(delta0$conclusion[[h]])
    target <- module04_opposite_target(main_conclusion)

    # The manuscript sensitivity question is anchored to the likelihood-based
    # main-model conclusion. If MAR-MI already gives the opposite conclusion at
    # delta=0, the honest tipping point is exactly zero rather than an invented
    # positive MNAR threshold.
    if (identical(delta0_conclusion, target)) {
      details[[h]] <- list(
        delta = 0,
        abs_delta = 0,
        target = target,
        tie = TRUE,
        delta0_discordance = TRUE
      )
      cells[[h]] <- paste0("0.00→", target)
      next
    }

    if (!identical(delta0_conclusion, main_conclusion)) {
      stop(
        "Unexpected delta=0 conclusion for ", h, ": main=", main_conclusion,
        ", MAR-MI=", delta0_conclusion, ".",
        call. = FALSE
      )
    }

    neg <- find_one_direction(h, -1, target)
    pos <- find_one_direction(h, +1, target)

    if (is.null(neg) && is.null(pos)) {
      details[[h]] <- list(
        delta = NA_real_, abs_delta = Inf, target = target,
        tie = FALSE, delta0_discordance = FALSE
      )
      cells[[h]] <- paste0(
        ">|", formatC(MODULE04_CONFIG$delta_max, format = "f", digits = 2),
        "|→", target
      )
    } else if (!is.null(neg) && !is.null(pos) &&
               abs(neg$abs_delta - pos$abs_delta) < 1e-10) {
      details[[h]] <- list(
        delta = NA_real_, abs_delta = neg$abs_delta, target = target,
        tie = TRUE, delta0_discordance = FALSE
      )
      cells[[h]] <- paste0(
        "±", formatC(neg$abs_delta, format = "f", digits = 2), "→", target
      )
    } else {
      best <- if (is.null(neg)) {
        pos
      } else if (is.null(pos)) {
        neg
      } else if (neg$abs_delta < pos$abs_delta) {
        neg
      } else {
        pos
      }
      details[[h]] <- c(best, list(tie = FALSE, delta0_discordance = FALSE))
      cells[[h]] <- paste0(
        if (best$delta > 0) "+" else "-",
        formatC(abs(best$delta), format = "f", digits = 2),
        "→", target
      )
    }
  }

  list(cells = cells, details = details)
}

module04_run_outcome_sensitivity <- function(dat, outcome_key, outcome_index, m, seed_offset = 0L) {
  spec <- MODULE04_OUTCOMES[[outcome_key]]
  message("  Preparing ", spec$label, " with m=", m)
  main_ref <- module04_main_reference(dat, outcome_key)
  wide <- module04_make_wide(dat, outcome_key)
  masks <- module04_missing_masks(wide)
  change_sd <- module04_change_sd(wide)
  seed <- MODULE04_CONFIG$mi_seed + outcome_index * 1000L + seed_offset
  imp <- module04_make_imputations(wide, outcome_key, m = m, seed = seed)
  evaluate <- module04_evaluate_delta_factory(
    imp$completed, masks, change_sd, outcome_key,
    main_ref$primary_covariance, main_ref$baseline_covariance
  )
  delta0 <- evaluate(0, "BE-only")
  be_only <- module04_search_scope(evaluate, delta0, main_ref, "BE-only")
  both <- module04_search_scope(evaluate, delta0, main_ref, "Both-arm symmetric")
  min_abs <- min(c(
    vapply(be_only$details, function(z) z$abs_delta, numeric(1)),
    vapply(both$details, function(z) z$abs_delta, numeric(1))
  ))
  list(
    be_only = be_only,
    both = both,
    min_abs = min_abs,
    m = m,
    main_ref = main_ref,
    delta0 = delta0,
    mice_event_summary = imp$logged_event_summary
  )
}


MODULE04_MNAR_SCRIPT_VERSION <- "1.5.1-preflight-checkpoint"
MODULE04_MNAR_EXECUTION_PATCH <- "1.5.5-main-anchor-final-delta0-audit"
module04_mnar_mode <- tolower(trimws(Sys.getenv("MODULE04_MNAR_MODE", unset = "check")))
if (!module04_mnar_mode %in% c("check", "preflight", "confirm", "full")) {
  stop(
    "MODULE04_MNAR_MODE must be 'check', 'preflight', 'confirm', or 'full'.",
    call. = FALSE
  )
}
message("MNAR execution mode: ", module04_mnar_mode)

module04_mnar_work_dir <- file.path(MODULE04_INTERMEDIATE_DIR, ".module04_mnar_work")
module04_preflight_csv <- file.path(module04_mnar_work_dir, "preflight_summary.csv")
module04_confirmation_csv <- file.path(module04_mnar_work_dir, "confirmation_summary.csv")
module04_audit_csv <- file.path(
  MODULE04_INTERMEDIATE_DIR,
  "_sensitivity_analysis_mnar_delta0_audit.csv"
)
module04_table_s9_path <- file.path(MODULE04_TABLE_DIR, "TableS9_mnar_be_only.xlsx")
module04_table_s10_path <- file.path(MODULE04_TABLE_DIR, "TableS10_mnar_symmetric.xlsx")
module04_data_md5 <- unname(as.character(tools::md5sum(MODULE04_DATA_FILE)))
module04_hypothesis_order <- c("time", "group_0_6", "interaction", "group_1_6", "dd", "reversal")
module04_hypothesis_labels <- c(
  time = "Time effect",
  group_0_6 = "Average group contrast (M0–M6)",
  interaction = "Group × visit (M1–M6)",
  group_1_6 = "Average group contrast (M1–M6)",
  dd = "ΔΔ (M6–M0)",
  reversal = "Reversal (M6 versus M0)"
)

module04_mnar_metadata <- function(outcome_key, outcome_index, m, seed) {
  list(
    fileset_id = MODULE04_FILESET_ID,
    mnar_script_version = MODULE04_MNAR_SCRIPT_VERSION,
    analysis_version = MODULE04_ANALYSIS_VERSION,
    data_md5 = module04_data_md5,
    outcome_key = outcome_key,
    outcome_index = as.integer(outcome_index),
    m = as.integer(m),
    seed = as.integer(seed),
    R_version = as.character(getRversion()),
    mmrm_version = as.character(utils::packageVersion("mmrm")),
    emmeans_version = as.character(utils::packageVersion("emmeans")),
    mice_version = as.character(utils::packageVersion("mice")),
    settings = list(
      df_method = MODULE04_CONFIG$df_method,
      mi_maxit = MODULE04_CONFIG$mi_maxit,
      delta_max = MODULE04_CONFIG$delta_max,
      delta_coarse_step = MODULE04_CONFIG$delta_coarse_step,
      delta_fine_step = MODULE04_CONFIG$delta_fine_step,
      preferred_covariance = MODULE04_CONFIG$preferred_covariance,
      fallback_covariance = MODULE04_CONFIG$fallback_covariance
    )
  )
}

module04_atomic_save_rds <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(object, tmp, version = 3)
  if (file.exists(path)) unlink(path)
  if (!file.rename(tmp, path)) stop("Could not atomically save checkpoint: ", path, call. = FALSE)
  invisible(path)
}

module04_preflight_cache_path <- function(i, label) {
  stub <- gsub("[^A-Za-z0-9]+", "_", label)
  stub <- gsub("^_+|_+$", "", stub)
  file.path(module04_mnar_work_dir, sprintf("preflight_%02d_%s.rds", i, stub))
}

module04_result_cache_path <- function(i, label) {
  stub <- gsub("[^A-Za-z0-9]+", "_", label)
  stub <- gsub("^_+|_+$", "", stub)
  file.path(module04_mnar_work_dir, sprintf("result_%02d_%s.rds", i, stub))
}

module04_cache_is_valid <- function(object, expected_metadata, required_names) {
  is.list(object) &&
    identical(object$metadata, expected_metadata) &&
    all(required_names %in% names(object))
}

module04_make_preflight_cache <- function(outcome_key, outcome_index) {
  spec <- MODULE04_OUTCOMES[[outcome_key]]
  m <- MODULE04_CONFIG$mi_m
  seed <- MODULE04_CONFIG$mi_seed + outcome_index * 1000L
  metadata <- module04_mnar_metadata(outcome_key, outcome_index, m, seed)
  cache_path <- module04_preflight_cache_path(outcome_index, spec$label)

  if (file.exists(cache_path)) {
    cached <- try(readRDS(cache_path), silent = TRUE)
    if (!inherits(cached, "try-error") &&
        module04_cache_is_valid(
          cached,
          metadata,
          c("main_ref", "masks", "change_sd", "completed", "delta0")
        )) {
      message(sprintf("[%02d/%02d] Delta=0 preflight: %s [resume]", outcome_index, length(MODULE04_OUTCOMES), spec$label))
      return(cached)
    }
    stop(
      "Existing MNAR preflight checkpoint is incompatible: ", cache_path,
      ". Remove the hidden .module04_mnar_work directory only after confirming that code/data changed.",
      call. = FALSE
    )
  }

  message(sprintf("[%02d/%02d] Delta=0 preflight: %s", outcome_index, length(MODULE04_OUTCOMES), spec$label))
  main_ref <- module04_main_reference(dat, outcome_key)
  wide <- module04_make_wide(dat, outcome_key)
  masks <- module04_missing_masks(wide)
  change_sd <- module04_change_sd(wide)
  imp <- module04_make_imputations(wide, outcome_key, m = m, seed = seed)
  evaluate <- module04_evaluate_delta_factory(
    imp$completed,
    masks,
    change_sd,
    outcome_key,
    main_ref$primary_covariance,
    main_ref$baseline_covariance
  )
  delta0 <- evaluate(0, "BE-only")

  out <- list(
    metadata = metadata,
    main_ref = main_ref,
    masks = masks,
    change_sd = change_sd,
    completed = imp$completed,
    delta0 = delta0,
    mice_logged_events = imp$logged_events,
    mice_event_summary = imp$logged_event_summary
  )
  module04_atomic_save_rds(out, cache_path)
  out
}

module04_preflight_row <- function(cache, outcome_label) {
  event_count <- if ("mice_logged_events" %in% names(cache)) {
    nrow(cache$mice_logged_events)
  } else {
    0L
  }
  event_summary <- if ("mice_event_summary" %in% names(cache)) {
    cache$mice_event_summary
  } else {
    "None (checkpoint created before logged-event audit patch)"
  }

  out <- data.frame(
    Outcome = outcome_label,
    M = cache$metadata$m,
    Seed = cache$metadata$seed,
    `MICE logged-event count` = event_count,
    `MICE logged-event audit` = event_summary,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  p_hypotheses <- c("time", "group_0_6", "interaction", "group_1_6", "dd")
  for (h in p_hypotheses) {
    label <- module04_hypothesis_labels[[h]]
    out[[paste0(label, ": main p")]] <- unname(cache$main_ref$p[[h]])
    out[[paste0(label, ": MI delta0 p")]] <- unname(cache$delta0$p[[h]])
    out[[paste0(label, ": main conclusion")]] <- unname(cache$main_ref$conclusion[[h]])
    out[[paste0(label, ": MI delta0 conclusion")]] <- unname(cache$delta0$conclusion[[h]])
    out[[paste0(label, ": match")]] <- if (
      identical(unname(cache$main_ref$conclusion[[h]]), unname(cache$delta0$conclusion[[h]]))
    ) "Yes" else "No"
  }

  h <- "reversal"
  label <- module04_hypothesis_labels[[h]]
  out[[paste0(label, ": main conclusion")]] <- unname(cache$main_ref$conclusion[[h]])
  out[[paste0(label, ": MI delta0 conclusion")]] <- unname(cache$delta0$conclusion[[h]])
  out[[paste0(label, ": match")]] <- if (
    identical(unname(cache$main_ref$conclusion[[h]]), unname(cache$delta0$conclusion[[h]]))
  ) "Yes" else "No"

  match_cols <- grep(": match$", names(out), value = TRUE)
  out$All_match <- if (all(unlist(out[match_cols], use.names = FALSE) == "Yes")) "Yes" else "No"
  out
}

module04_audit_rows <- function(main_ref, delta0, outcome_label, phase, m, note = "") {
  rows <- vector("list", length(module04_hypothesis_order))
  for (i in seq_along(module04_hypothesis_order)) {
    h <- module04_hypothesis_order[[i]]
    main_p <- if (h == "reversal") NA_real_ else unname(main_ref$p[[h]])
    mi_p <- if (h == "reversal") NA_real_ else unname(delta0$p[[h]])
    main_conclusion <- unname(main_ref$conclusion[[h]])
    mi_conclusion <- unname(delta0$conclusion[[h]])
    rows[[i]] <- data.frame(
      Outcome = outcome_label,
      Hypothesis = unname(module04_hypothesis_labels[[h]]),
      `Audit phase` = phase,
      m = as.integer(m),
      `Main-model P value` = main_p,
      `MAR-MI P value` = mi_p,
      `Main conclusion` = main_conclusion,
      `MAR-MI conclusion` = mi_conclusion,
      Concordance = if (identical(main_conclusion, mi_conclusion)) "Yes" else "No",
      Note = note,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

module04_order_audit <- function(audit) {
  outcome_levels <- unname(vapply(MODULE04_OUTCOMES, `[[`, character(1), "display_label"))
  phase_levels <- c("preflight", "confirm", "targeted")
  hypothesis_levels <- unname(module04_hypothesis_labels[module04_hypothesis_order])
  audit$.outcome_order <- match(audit$Outcome, outcome_levels)
  audit$.phase_order <- match(audit[["Audit phase"]], phase_levels)
  audit$.hypothesis_order <- match(audit$Hypothesis, hypothesis_levels)
  if (anyNA(audit[c(".outcome_order", ".phase_order", ".hypothesis_order")])) {
    stop("MNAR audit contains an unknown outcome, phase, or hypothesis.", call. = FALSE)
  }
  audit <- audit[order(audit$.outcome_order, audit$.phase_order, audit$.hypothesis_order), , drop = FALSE]
  audit[c(".outcome_order", ".phase_order", ".hypothesis_order")] <- NULL
  rownames(audit) <- NULL
  audit
}

module04_write_audit <- function(audit) {
  audit <- module04_order_audit(audit)
  module04_write_csv(audit, module04_audit_csv)
  invisible(audit)
}

module04_preflight_mismatch_text <- function(preflight) {
  match_cols <- grep(": match$", names(preflight), value = TRUE)
  pieces <- character()
  for (i in seq_len(nrow(preflight))) {
    bad <- match_cols[unlist(preflight[i, match_cols, drop = FALSE], use.names = FALSE) != "Yes"]
    if (length(bad)) {
      hypotheses <- sub(": match$", "", bad)
      pieces <- c(pieces, paste0(preflight$Outcome[i], " [", paste(hypotheses, collapse = "; "), "]"))
    }
  }
  pieces
}

module04_confirmation_cache_path <- function(i, label) {
  stub <- gsub("[^A-Za-z0-9]+", "_", label)
  stub <- gsub("^_+|_+$", "", stub)
  file.path(module04_mnar_work_dir, sprintf("confirm_m100_%02d_%s.rds", i, stub))
}

module04_confirmation_metadata <- function(outcome_key, outcome_index, m, seed) {
  out <- module04_mnar_metadata(outcome_key, outcome_index, m, seed)
  out$confirmation_patch <- MODULE04_MNAR_EXECUTION_PATCH
  out
}

module04_make_confirmation_cache <- function(outcome_key, outcome_index) {
  spec <- MODULE04_OUTCOMES[[outcome_key]]
  m <- MODULE04_CONFIG$mi_boundary_m
  seed <- MODULE04_CONFIG$mi_seed + outcome_index * 1000L + 500000L
  metadata <- module04_confirmation_metadata(outcome_key, outcome_index, m, seed)
  cache_path <- module04_confirmation_cache_path(outcome_index, spec$label)

  if (file.exists(cache_path)) {
    cached <- try(readRDS(cache_path), silent = TRUE)
    if (!inherits(cached, "try-error") &&
        module04_cache_is_valid(
          cached,
          metadata,
          c("main_ref", "delta0", "mice_logged_events", "mice_event_summary")
        )) {
      message(sprintf(
        "[%02d/%02d] Delta=0 m=100 confirmation: %s [resume]",
        outcome_index, length(MODULE04_OUTCOMES), spec$label
      ))
      return(cached)
    }
    stop(
      "Existing m=100 confirmation checkpoint is incompatible: ", cache_path,
      ". Remove only that confirmation checkpoint after confirming that code/data changed.",
      call. = FALSE
    )
  }

  message(sprintf(
    "[%02d/%02d] Delta=0 m=100 confirmation: %s",
    outcome_index, length(MODULE04_OUTCOMES), spec$label
  ))
  main_ref <- module04_main_reference(dat, outcome_key)
  wide <- module04_make_wide(dat, outcome_key)
  masks <- module04_missing_masks(wide)
  change_sd <- module04_change_sd(wide)
  imp <- module04_make_imputations(wide, outcome_key, m = m, seed = seed)
  evaluate <- module04_evaluate_delta_factory(
    imp$completed,
    masks,
    change_sd,
    outcome_key,
    main_ref$primary_covariance,
    main_ref$baseline_covariance
  )
  delta0 <- evaluate(0, "BE-only")

  out <- list(
    metadata = metadata,
    main_ref = main_ref,
    delta0 = delta0,
    mice_logged_events = imp$logged_events,
    mice_event_summary = imp$logged_event_summary
  )
  module04_atomic_save_rds(out, cache_path)
  out
}

module04_main_estimate_diagnostics <- function(outcome_key, main_ref) {
  pdat <- module04_prepare_primary(dat, outcome_key)
  pfit <- module04_fit_fixed_covariance(
    pdat,
    module04_fixed_formula("primary", outcome_key),
    main_ref$primary_covariance
  )
  pe <- module04_extract_primary_estimands(pfit)

  bdat <- module04_prepare_baseline_included(dat, outcome_key)
  bfit <- module04_fit_fixed_covariance(
    bdat,
    module04_fixed_formula("baseline_included", outcome_key),
    main_ref$baseline_covariance
  )
  be <- module04_extract_baseline_estimands(bfit)

  list(
    group_0_6 = unname(be$payload_group_average$q),
    group_1_6 = unname(pe$payload_group_average$q),
    dd = unname(be$payload_dd$q),
    reversal_q = unname(be$payload_reversal$q)
  )
}

module04_scalar_estimate <- function(source, h) {
  if (h %in% c("group_0_6", "group_1_6", "dd")) {
    return(as.numeric(source[[h]]$qbar[1L]))
  }
  NA_real_
}

module04_confirmation_rows <- function(preflight, outcome_index, cache50, cache100) {
  spec <- MODULE04_OUTCOMES[[outcome_index]]
  label <- spec$label
  row50 <- preflight[preflight$Outcome == label, , drop = FALSE]
  if (nrow(row50) != 1L) stop("Expected one preflight row for ", label, call. = FALSE)

  match_cols <- grep(": match$", names(row50), value = TRUE)
  bad_cols <- match_cols[unlist(row50[1L, match_cols, drop = FALSE], use.names = FALSE) != "Yes"]
  bad_labels <- sub(": match$", "", bad_cols)
  h_lookup <- setNames(names(module04_hypothesis_labels), unname(module04_hypothesis_labels))
  bad_h <- unname(h_lookup[bad_labels])
  if (anyNA(bad_h)) stop("Could not map mismatch hypothesis for ", label, call. = FALSE)

  main_diag <- module04_main_estimate_diagnostics(spec$key, cache50$main_ref)
  out <- vector("list", length(bad_h))

  for (j in seq_along(bad_h)) {
    h <- bad_h[[j]]
    hlabel <- module04_hypothesis_labels[[h]]
    main_p <- if (h == "reversal") NA_real_ else unname(cache50$main_ref$p[[h]])
    p50 <- if (h == "reversal") NA_real_ else unname(cache50$delta0$p[[h]])
    p100 <- if (h == "reversal") NA_real_ else unname(cache100$delta0$p[[h]])
    main_conc <- unname(cache50$main_ref$conclusion[[h]])
    conc50 <- unname(cache50$delta0$conclusion[[h]])
    conc100 <- unname(cache100$delta0$conclusion[[h]])

    main_est <- switch(
      h,
      group_0_6 = main_diag$group_0_6,
      group_1_6 = main_diag$group_1_6,
      dd = main_diag$dd,
      NA_real_
    )
    est50 <- module04_scalar_estimate(cache50$delta0$pooled, h)
    est100 <- module04_scalar_estimate(cache100$delta0$pooled, h)

    stable_class <- if (identical(conc100, main_conc)) {
      "m=50-only discordance; m=100 aligns with main"
    } else if (identical(conc100, conc50)) {
      "stable MAR-MI discordance at m=50 and m=100"
    } else {
      "unstable across m=50 and m=100; further review required"
    }

    out[[j]] <- data.frame(
      Outcome = label,
      Hypothesis = hlabel,
      `Main p` = main_p,
      `MI delta0 p (m=50)` = p50,
      `MI delta0 p (m=100)` = p100,
      `Main conclusion` = main_conc,
      `MI conclusion (m=50)` = conc50,
      `MI conclusion (m=100)` = conc100,
      `Main scalar estimate` = main_est,
      `MI scalar estimate (m=50)` = est50,
      `MI scalar estimate (m=100)` = est100,
      `Main reversal D_M0` = if (h == "reversal") main_diag$reversal_q[1L] else NA_real_,
      `Main reversal D_M6` = if (h == "reversal") main_diag$reversal_q[2L] else NA_real_,
      `MI reversal D_M0 (m=50)` = if (h == "reversal") cache50$delta0$reversal_q[1L] else NA_real_,
      `MI reversal D_M6 (m=50)` = if (h == "reversal") cache50$delta0$reversal_q[2L] else NA_real_,
      `MI reversal D_M0 (m=100)` = if (h == "reversal") cache100$delta0$reversal_q[1L] else NA_real_,
      `MI reversal D_M6 (m=100)` = if (h == "reversal") cache100$delta0$reversal_q[2L] else NA_real_,
      `m=100 MICE logged-event count` = nrow(cache100$mice_logged_events),
      `m=100 MICE logged-event audit` = cache100$mice_event_summary,
      Classification = stable_class,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}


if (!identical(module04_mnar_mode, "check")) {
  for (directory in c(MODULE04_TABLE_DIR, MODULE04_INTERMEDIATE_DIR, module04_mnar_work_dir)) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
}

if (identical(module04_mnar_mode, "check")) {
  spec <- MODULE04_OUTCOMES[["total_protein"]]
  main_ref <- module04_main_reference(dat, spec$key)
  wide <- module04_make_wide(dat, spec$key)
  masks <- module04_missing_masks(wide)
  change_sd <- module04_change_sd(wide)
  if (!all(is.finite(change_sd)) || !all(vapply(masks, is.logical, logical(1)))) {
    stop("MNAR smoke-check data preparation failed.", call. = FALSE)
  }
  message("[PASS] MNAR smoke check completed without imputation or file output")
  message("[PASS] representative outcome: ", spec$display_label)
  message(
    "[PASS] main-model covariance: M1–M6=", main_ref$primary_covariance,
    "; M0–M6=", main_ref$baseline_covariance
  )
  message("For formal sensitivity results, run preflight, optional confirm, and full modes.")

} else if (identical(module04_mnar_mode, "preflight")) {
  preflight_rows <- vector("list", length(MODULE04_OUTCOMES))
  audit_rows <- vector("list", length(MODULE04_OUTCOMES))
  for (i in seq_along(MODULE04_OUTCOMES)) {
    spec <- MODULE04_OUTCOMES[[i]]
    cache <- module04_make_preflight_cache(spec$key, i)
    preflight_rows[[i]] <- module04_preflight_row(cache, spec$label)
    audit_rows[[i]] <- module04_audit_rows(
      cache$main_ref,
      cache$delta0,
      spec$display_label,
      "preflight",
      cache$metadata$m,
      note = paste0("delta=0; ", cache$mice_event_summary)
    )
    rm(cache)
    gc(verbose = FALSE)
  }

  preflight <- do.call(rbind, preflight_rows)
  audit <- do.call(rbind, audit_rows)
  rownames(preflight) <- NULL
  rownames(audit) <- NULL
  module04_assert_outcome_order(preflight$Outcome, context = "MNAR delta=0 preflight")
  module04_write_csv(preflight, module04_preflight_csv)
  module04_write_audit(audit)
  mismatch <- module04_preflight_mismatch_text(preflight)

  message("[PASS] delta=0 MAR-MI preflight completed for all 30 outcomes")
  message("[PASS] consolidated delta=0 audit written: ", module04_audit_csv)
  if (length(mismatch)) {
    stop(
      "Delta=0 preflight found conclusion mismatch(es): ",
      paste(mismatch, collapse = " | "),
      ". Run MODULE04_MNAR_MODE='confirm' before full mode.",
      call. = FALSE
    )
  }
  message("[PASS] all preflight conclusions align with the likelihood-based main models")
  message("Next run: MODULE04_MNAR_MODE='full'")

} else if (identical(module04_mnar_mode, "confirm")) {
  if (!file.exists(module04_preflight_csv)) {
    stop(
      "Delta=0 preflight is missing. First run MODULE04_MNAR_MODE='preflight'.",
      call. = FALSE
    )
  }
  preflight <- utils::read.csv(
    module04_preflight_csv,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  module04_assert_outcome_order(preflight$Outcome, context = "MNAR delta=0 preflight")
  mismatch_labels <- preflight$Outcome[preflight$All_match != "Yes"]

  if (!length(mismatch_labels)) {
    message("[PASS] no delta=0 mismatches require m=100 confirmation")
  } else {
    message(
      "Confirming ", length(mismatch_labels),
      " delta=0 mismatch outcome(s) with m=", MODULE04_CONFIG$mi_boundary_m,
      ": ", paste(mismatch_labels, collapse = ", ")
    )
    rows <- vector("list", length(mismatch_labels))
    audit_rows <- vector("list", length(mismatch_labels))
    for (j in seq_along(mismatch_labels)) {
      label <- mismatch_labels[[j]]
      i <- match(label, vapply(MODULE04_OUTCOMES, `[[`, character(1), "label"))
      if (!is.finite(i)) stop("Outcome not found in frozen order: ", label, call. = FALSE)
      spec <- MODULE04_OUTCOMES[[i]]
      path50 <- module04_preflight_cache_path(i, spec$label)
      if (!file.exists(path50)) stop("Missing m=50 preflight cache: ", path50, call. = FALSE)
      cache50 <- readRDS(path50)
      cache100 <- module04_make_confirmation_cache(spec$key, i)
      rows[[j]] <- module04_confirmation_rows(preflight, i, cache50, cache100)
      audit_rows[[j]] <- module04_audit_rows(
        cache100$main_ref,
        cache100$delta0,
        spec$display_label,
        "confirm",
        cache100$metadata$m,
        note = paste0("m=100 confirmation; delta=0; ", cache100$mice_event_summary)
      )
      rm(cache50, cache100)
      gc(verbose = FALSE)
    }
    confirmation <- do.call(rbind, rows)
    confirm_audit <- do.call(rbind, audit_rows)
    rownames(confirmation) <- NULL
    rownames(confirm_audit) <- NULL
    module04_write_csv(confirmation, module04_confirmation_csv)

    existing_audit <- if (file.exists(module04_audit_csv)) {
      utils::read.csv(module04_audit_csv, check.names = FALSE, stringsAsFactors = FALSE)
    } else {
      confirm_audit[0, , drop = FALSE]
    }
    existing_audit <- existing_audit[existing_audit[["Audit phase"]] != "confirm", , drop = FALSE]
    module04_write_audit(rbind(existing_audit, confirm_audit))

    message("[PASS] targeted m=100 delta=0 confirmation completed")
    message("[PASS] consolidated delta=0 audit updated: ", module04_audit_csv)
    print(confirmation[, c(
      "Outcome", "Hypothesis", "Main conclusion",
      "MI conclusion (m=50)", "MI conclusion (m=100)", "Classification"
    ), drop = FALSE], row.names = FALSE)
    message("Review the confirmation results before starting full mode.")
  }

} else {
  if (!file.exists(module04_preflight_csv)) {
    stop(
      "Delta=0 preflight has not been completed. First run MODULE04_MNAR_MODE='preflight'.",
      call. = FALSE
    )
  }

  preflight <- utils::read.csv(
    module04_preflight_csv,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  module04_assert_outcome_order(preflight$Outcome, context = "MNAR delta=0 preflight")
  if (nrow(preflight) != length(MODULE04_OUTCOMES)) {
    stop("Delta=0 preflight is incomplete.", call. = FALSE)
  }

  mismatch_labels <- unique(preflight$Outcome[preflight$All_match != "Yes"])
  m100_labels <- character()
  confirmation <- NULL

  if (length(mismatch_labels)) {
    if (!file.exists(module04_confirmation_csv)) {
      stop(
        "Delta=0 mismatches exist, but the m=100 confirmation is missing. ",
        "Run MODULE04_MNAR_MODE='confirm' first.",
        call. = FALSE
      )
    }
    confirmation <- utils::read.csv(
      module04_confirmation_csv,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    confirmed_labels <- unique(as.character(confirmation$Outcome))
    if (!setequal(mismatch_labels, confirmed_labels)) {
      stop("The m=100 confirmation outcomes do not match the preflight mismatches.", call. = FALSE)
    }
    if (any(grepl("further review required", confirmation$Classification, fixed = TRUE))) {
      stop(
        "At least one delta=0 mismatch remains unstable between m=50 and m=100. Full mode is blocked.",
        call. = FALSE
      )
    }
    m100_labels <- mismatch_labels
  }

  message(
    "Full-mode anchoring: each cell is the smallest signed delta at which the MAR-MI ",
    "conclusion differs from the likelihood-based main-model conclusion."
  )
  message("A delta=0 discordance is reported as 0.00→target.")
  if (length(m100_labels)) {
    message("Full m=100 outcomes: ", paste(m100_labels, collapse = ", "))
  }

  preflight_cache <- vector("list", length(MODULE04_OUTCOMES))
  for (i in seq_along(MODULE04_OUTCOMES)) {
    spec <- MODULE04_OUTCOMES[[i]]
    m <- MODULE04_CONFIG$mi_m
    seed <- MODULE04_CONFIG$mi_seed + i * 1000L
    expected_metadata <- module04_mnar_metadata(spec$key, i, m, seed)
    path <- module04_preflight_cache_path(i, spec$label)
    if (!file.exists(path)) stop("Missing preflight cache: ", path, call. = FALSE)
    obj <- readRDS(path)
    if (!module04_cache_is_valid(
      obj,
      expected_metadata,
      c("main_ref", "masks", "change_sd", "completed", "delta0")
    )) {
      stop("Incompatible preflight cache: ", path, call. = FALSE)
    }
    preflight_cache[[i]] <- path
  }

  unlink(c(module04_table_s9_path, module04_table_s10_path))
  be_rows <- vector("list", length(MODULE04_OUTCOMES))
  both_rows <- vector("list", length(MODULE04_OUTCOMES))
  targeted_audit_rows <- vector("list", length(MODULE04_OUTCOMES))

  for (i in seq_along(MODULE04_OUTCOMES)) {
    spec <- MODULE04_OUTCOMES[[i]]
    result_path <- module04_result_cache_path(i, spec$label)
    ans <- NULL

    if (file.exists(result_path)) {
      cached_result <- try(readRDS(result_path), silent = TRUE)
      if (!inherits(cached_result, "try-error") &&
          is.list(cached_result) &&
          "answer" %in% names(cached_result) &&
          "metadata" %in% names(cached_result)) {
        cached_m <- as.integer(cached_result$answer$m)
        cached_seed_offset <- if (cached_m == MODULE04_CONFIG$mi_boundary_m) 500000L else 0L
        expected_cached_metadata <- module04_mnar_metadata(
          spec$key,
          i,
          cached_m,
          MODULE04_CONFIG$mi_seed + i * 1000L + cached_seed_offset
        )
        if (module04_cache_is_valid(cached_result, expected_cached_metadata, c("answer"))) {
          message(sprintf(
            "[%02d/%02d] MNAR: %s [resume; m=%d]",
            i, length(MODULE04_OUTCOMES), spec$label, cached_m
          ))
          ans <- cached_result$answer
        }
      }
      if (is.null(ans)) {
        stop(
          "Existing MNAR result checkpoint is incompatible: ", result_path,
          ". Remove it only after confirming that code or data changed.",
          call. = FALSE
        )
      }
    }

    if (is.null(ans)) {
      force_m100 <- spec$label %in% m100_labels
      if (force_m100) {
        message(sprintf(
          "[%02d/%02d] MNAR: %s [confirmed m=100]",
          i, length(MODULE04_OUTCOMES), spec$label
        ))
        ans <- module04_run_outcome_sensitivity(
          dat,
          spec$key,
          i,
          MODULE04_CONFIG$mi_boundary_m,
          seed_offset = 500000L
        )
      } else {
        message(sprintf(
          "[%02d/%02d] MNAR: %s [m=%d]",
          i, length(MODULE04_OUTCOMES), spec$label, MODULE04_CONFIG$mi_m
        ))
        cache <- readRDS(preflight_cache[[i]])
        if ("mice_event_summary" %in% names(cache) &&
            !identical(cache$mice_event_summary, "None")) {
          message("  Reusing audited MICE event: ", cache$mice_event_summary)
        }
        evaluate <- module04_evaluate_delta_factory(
          cache$completed,
          cache$masks,
          cache$change_sd,
          spec$key,
          cache$main_ref$primary_covariance,
          cache$main_ref$baseline_covariance
        )
        be_only <- module04_search_scope(evaluate, cache$delta0, cache$main_ref, "BE-only")
        both <- module04_search_scope(evaluate, cache$delta0, cache$main_ref, "Both-arm symmetric")
        min_abs <- min(c(
          vapply(be_only$details, function(z) z$abs_delta, numeric(1)),
          vapply(both$details, function(z) z$abs_delta, numeric(1))
        ))
        ans <- list(
          be_only = be_only,
          both = both,
          min_abs = min_abs,
          m = MODULE04_CONFIG$mi_m,
          main_ref = cache$main_ref,
          delta0 = cache$delta0,
          mice_event_summary = if ("mice_event_summary" %in% names(cache)) {
            cache$mice_event_summary
          } else {
            "Not recorded in early preflight checkpoint"
          }
        )

        if (is.finite(min_abs) &&
            min_abs <= MODULE04_CONFIG$boundary_recheck_threshold) {
          message(
            "  Boundary tipping point (|delta|<=",
            MODULE04_CONFIG$boundary_recheck_threshold,
            ") detected; using a complete m=",
            MODULE04_CONFIG$mi_boundary_m,
            " rerun for the final table."
          )
          ans <- module04_run_outcome_sensitivity(
            dat,
            spec$key,
            i,
            MODULE04_CONFIG$mi_boundary_m,
            seed_offset = 500000L
          )
        }
        rm(cache, evaluate, be_only, both)
      }

      actual_seed_offset <- if (ans$m == MODULE04_CONFIG$mi_boundary_m) 500000L else 0L
      result_metadata <- module04_mnar_metadata(
        spec$key,
        i,
        ans$m,
        MODULE04_CONFIG$mi_seed + i * 1000L + actual_seed_offset
      )
      module04_atomic_save_rds(
        list(metadata = result_metadata, answer = ans),
        result_path
      )
      gc(verbose = FALSE)
    }

    be <- ans$be_only$cells[module04_hypothesis_order]
    bt <- ans$both$cells[module04_hypothesis_order]
    be_rows[[i]] <- data.frame(
      Outcome = spec$display_label,
      Time = be[["time"]],
      Average_group_M0_M6 = be[["group_0_6"]],
      Group_by_visit = be[["interaction"]],
      Average_group_M1_M6 = be[["group_1_6"]],
      Delta_delta = be[["dd"]],
      Reversal = be[["reversal"]],
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    both_rows[[i]] <- data.frame(
      Outcome = spec$display_label,
      Time = bt[["time"]],
      Average_group_M0_M6 = bt[["group_0_6"]],
      Group_by_visit = bt[["interaction"]],
      Average_group_M1_M6 = bt[["group_1_6"]],
      Delta_delta = bt[["dd"]],
      Reversal = bt[["reversal"]],
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    targeted_audit_rows[[i]] <- module04_audit_rows(
      ans$main_ref,
      ans$delta0,
      spec$display_label,
      "targeted",
      ans$m,
      note = paste0("Full-mode delta=0 anchor; ", ans$mice_event_summary)
    )
    rm(ans)
    gc(verbose = FALSE)
  }

  table_s9 <- do.call(rbind, be_rows)
  table_s10 <- do.call(rbind, both_rows)
  targeted_audit <- do.call(rbind, targeted_audit_rows)
  rownames(table_s9) <- NULL
  rownames(table_s10) <- NULL
  rownames(targeted_audit) <- NULL

  module04_assert_outcome_order(table_s9$Outcome, context = "Table S9", with_units = TRUE)
  module04_assert_outcome_order(table_s10$Outcome, context = "Table S10", with_units = TRUE)
  if (anyDuplicated(table_s9$Outcome) || anyDuplicated(table_s10$Outcome)) {
    stop("Duplicate sensitivity-table outcomes.", call. = FALSE)
  }
  if (!identical(table_s9$Outcome, table_s10$Outcome)) {
    stop("Table S9/S10 outcome keys do not align.", call. = FALSE)
  }

  module04_write_mnar_xlsx(table_s9, module04_table_s9_path, "Table S9")
  module04_write_mnar_xlsx(table_s10, module04_table_s10_path, "Table S10")

  existing_audit <- if (file.exists(module04_audit_csv)) {
    utils::read.csv(module04_audit_csv, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    targeted_audit[0, , drop = FALSE]
  }
  existing_audit <- existing_audit[existing_audit[["Audit phase"]] != "targeted", , drop = FALSE]
  module04_write_audit(rbind(existing_audit, targeted_audit))

  unlink(module04_mnar_work_dir, recursive = TRUE, force = TRUE)

  message("[PASS] bidirectional MNAR tipping-point analyses completed")
  message("[PASS] Table S9 written: ", module04_table_s9_path)
  message("[PASS] Table S10 written: ", module04_table_s10_path)
  message("[PASS] consolidated delta=0 audit written: ", module04_audit_csv)
  message("[PASS] temporary MNAR checkpoints removed after successful full mode")
}
