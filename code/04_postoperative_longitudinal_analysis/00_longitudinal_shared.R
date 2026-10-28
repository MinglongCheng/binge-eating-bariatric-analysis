# Shared data, model, table, and trajectory-plot functions for module 04.
# This file is sourced by 01_ and 02_; it is not an analysis entry point.

options(stringsAsFactors = FALSE)

MODULE04_ANALYSIS_VERSION <- "1.0.3-scientific-correction-explicit-kr"
MODULE04_FILESET_ID <- "module04-2026-08-25-display-refactor-v1"
MODULE04_EXPECTED_R_VERSION <- "4.5.1"
MODULE04_REQUIRED_PACKAGES <- c("mmrm", "emmeans", "mice", "ggplot2", "openxlsx", "png", "zip")

MODULE04_CONFIG <- list(
  alpha = 0.05,
  expected_rows = 488L,
  expected_subjects = 122L,
  expected_group_counts = c("Non-BE" = 66L, "BE" = 56L),
  group_levels = c("Non-BE", "BE"),
  visit_levels = c("M0", "M1", "M3", "M6"),
  post_visits = c("M1", "M3", "M6"),
  single_width_cm = 12.0,
  single_height_cm = 8.0,
  figure_dpi = 600,
  colors = c("Non-BE" = "#1B9E9A", "BE" = "#E59886"),
  linetypes = c("Non-BE" = "solid", "BE" = "dashed"),
  shapes = c("Non-BE" = 16L, "BE" = 15L),
  font_family = "Arial",
  preferred_covariance = "us",
  fallback_covariance = "ar1h",
  df_method = "Kenward-Roger",
  mi_m = 50L,
  mi_boundary_m = 100L,
  mi_maxit = 20L,
  mi_seed = 2025L,
  delta_max = 1.00,
  delta_coarse_step = 0.10,
  delta_fine_step = 0.01,
  boundary_recheck_threshold = 0.05,
  strict_delta0_alignment = TRUE
)

MODULE04_DEFAULT_STYLE <- list(
  font_family = "Arial",
  background_color = "white",
  png_dpi = 600,
  single_axis_title_size_pt = 10.0,
  single_tick_size_pt = 9.0,
  single_legend_size_pt = 9.0,
  line_main_single_width_pt = 1.20,
  color_non_be = "#1B9E9A",
  color_be = "#E59886"
)

module04_outcome <- function(key, label, unit, source_column, slug, category, part) {
  list(
    key = key,
    label = label,
    unit = unit,
    display_label = paste0(label, " (", unit, ")"),
    source_column = source_column,
    slug = slug,
    category = category,
    part = as.integer(part)
  )
}

module04_panel_id <- function(index) {
  index <- as.integer(index)
  if (index < 1L) stop("Panel index must be positive.", call. = FALSE)
  out <- character()
  while (index > 0L) {
    index <- index - 1L
    out <- c(letters[index %% 26L + 1L], out)
    index <- index %/% 26L
  }
  paste(out, collapse = "")
}

MODULE04_SOURCE_COLUMNS <- c(
  id = "participant_id",
  group = "bes_group",
  visit = "visit_month",
  sex = "sex",
  age = "age_years",
  bmi0 = "baseline_bmi_kg_m2"
)

MODULE04_OUTCOMES <- list(
  module04_outcome("weight", "Weight", "kg", "weight_kg", "weight", "Anthropometrics", 1L),
  module04_outcome("bmi", "BMI", "kg/m²", "bmi_kg_m2", "bmi", "Anthropometrics", 1L),
  module04_outcome("total_protein", "Total protein", "g/L", "total_protein_g_l", "total_protein", "Nutrition indicators", 1L),
  module04_outcome("calcium", "Ca", "mmol/L", "calcium_mmol_l", "ca", "Nutrition indicators", 1L),
  module04_outcome("vitamin_d_25oh", "25(OH)D", "ng/mL", "vitamin_d_25oh_ng_ml", "25ohd", "Nutrition indicators", 1L),
  module04_outcome("iron", "Fe", "µmol/L", "iron_umol_l", "fe", "Nutrition indicators", 1L),
  module04_outcome("hemoglobin", "Hb", "g/L", "hemoglobin_g_l", "hb", "Nutrition indicators", 1L),
  module04_outcome("rbc", "RBC", "×10¹²/L", "rbc_10e12_l", "rbc", "Nutrition indicators", 1L),
  module04_outcome("folate", "Folate", "ng/mL", "folate_ng_ml", "folate", "Nutrition indicators", 1L),
  module04_outcome("vitamin_b12", "Vitamin B12", "pg/mL", "vitamin_b12_pg_ml", "vitamin_b12", "Nutrition indicators", 1L),
  module04_outcome("alt", "ALT", "U/L", "alt_u_l", "alt", "Metabolic indicators", 2L),
  module04_outcome("ast", "AST", "U/L", "ast_u_l", "ast", "Metabolic indicators", 2L),
  module04_outcome("ggt", "GGT", "U/L", "ggt_u_l", "ggt", "Metabolic indicators", 2L),
  module04_outcome("total_cholesterol", "Total cholesterol", "mmol/L", "total_cholesterol_mmol_l", "total_cholesterol", "Metabolic indicators", 2L),
  module04_outcome("ldl_c", "LDL-C", "mmol/L", "ldl_c_mmol_l", "ldl_c", "Metabolic indicators", 2L),
  module04_outcome("hdl_c", "HDL-C", "mmol/L", "hdl_c_mmol_l", "hdl_c", "Metabolic indicators", 2L),
  module04_outcome("fpg", "FPG", "mmol/L", "fasting_plasma_glucose_mmol_l", "fpg", "Metabolic indicators", 2L),
  module04_outcome("hba1c", "HbA1c", "%", "hba1c_percent", "hba1c", "Metabolic indicators", 2L),
  module04_outcome("urea", "Urea", "mmol/L", "urea_mmol_l", "urea", "Metabolic indicators", 2L),
  module04_outcome("creatinine", "Creatinine", "µmol/L", "creatinine_umol_l", "creatinine", "Metabolic indicators", 2L),
  module04_outcome("uric_acid", "Uric acid", "µmol/L", "uric_acid_umol_l", "uric_acid", "Metabolic indicators", 2L),
  module04_outcome("sodium", "Na", "mmol/L", "sodium_mmol_l", "na", "Metabolic indicators", 2L),
  module04_outcome("potassium", "K", "mmol/L", "potassium_mmol_l", "k", "Metabolic indicators", 2L),
  module04_outcome("chloride", "Cl", "mmol/L", "chloride_mmol_l", "cl", "Metabolic indicators", 2L),
  module04_outcome("wbc", "WBC", "×10⁹/L", "wbc_10e9_l", "wbc", "Immune indicators", 3L),
  module04_outcome("neutrophils", "Neutrophils", "×10⁹/L", "neutrophils_10e9_l", "neutrophils", "Immune indicators", 3L),
  module04_outcome("lymphocytes", "Lymphocytes", "×10⁹/L", "lymphocytes_10e9_l", "lymphocytes", "Immune indicators", 3L),
  module04_outcome("monocytes", "Monocytes", "×10⁹/L", "monocytes_10e9_l", "monocytes", "Immune indicators", 3L),
  module04_outcome("eosinophils", "Eosinophils", "×10⁹/L", "eosinophils_10e9_l", "eosinophils", "Immune indicators", 3L),
  module04_outcome("basophils", "Basophils", "×10⁹/L", "basophils_10e9_l", "basophils", "Immune indicators", 3L)
)
names(MODULE04_OUTCOMES) <- vapply(MODULE04_OUTCOMES, `[[`, character(1), "key")
for (i in seq_along(MODULE04_OUTCOMES)) {
  MODULE04_OUTCOMES[[i]]$display_order <- as.integer(i)
  MODULE04_OUTCOMES[[i]]$panel_id <- module04_panel_id(i)
}
MODULE04_BODY_SIZE_OUTCOMES <- c("weight", "bmi")

module04_assert_outcome_order <- function(observed, context = "output table", with_units = FALSE) {
  field <- if (isTRUE(with_units)) "display_label" else "label"
  expected <- unname(vapply(MODULE04_OUTCOMES, `[[`, character(1), field))
  observed <- unname(as.character(observed))
  same <- length(observed) == length(expected) && !anyNA(observed) && identical(observed, expected)
  if (!same) {
    max_n <- max(length(observed), length(expected))
    obs_pad <- c(observed, rep(NA_character_, max_n - length(observed)))
    exp_pad <- c(expected, rep(NA_character_, max_n - length(expected)))
    bad <- which(is.na(obs_pad) != is.na(exp_pad) |
      (!is.na(obs_pad) & !is.na(exp_pad) & obs_pad != exp_pad))
    preview <- if (length(bad)) {
      paste(vapply(head(bad, 5L), function(i) sprintf(
        "position %d: observed=%s; expected=%s",
        i,
        ifelse(is.na(obs_pad[i]), "<missing>", shQuote(obs_pad[i])),
        ifelse(is.na(exp_pad[i]), "<missing>", shQuote(exp_pad[i]))
      ), character(1)), collapse = " | ")
    } else {
      "length mismatch"
    }
    stop(context, " outcome order mismatch. ", preview, call. = FALSE)
  }
  invisible(TRUE)
}

module04_normalize_token <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT", sub = "")
  gsub("[^a-z0-9]+", "", tolower(x))
}

module04_walk_up <- function(start, max_levels = 12L) {
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

module04_find_module_root <- function() {
  starts <- unique(c(getwd(), dirname(getwd())))
  for (start in starts) {
    for (cur in module04_walk_up(start)) {
      direct <- file.path(cur, "data", "bariatric_longitudinal.csv")
      nested <- file.path(cur, "binge-eating-bariatric-analysis", "data", "bariatric_longitudinal.csv")
      if (file.exists(direct)) return(normalizePath(cur, winslash = "/"))
      if (file.exists(nested)) return(normalizePath(file.path(cur, "binge-eating-bariatric-analysis"), winslash = "/"))
    }
  }
  stop(
    "Cannot locate binge-eating-bariatric-analysis/data/bariatric_longitudinal.csv. ",
    "Open the binge-eating-bariatric-analysis R project (or set the working directory to it) and rerun.",
    call. = FALSE
  )
}

MODULE04_ROOT <- module04_find_module_root()
MODULE04_DATA_FILE <- file.path(MODULE04_ROOT, "data", "bariatric_longitudinal.csv")
MODULE04_OUTPUT_ROOT <- file.path(MODULE04_ROOT, "outputs", "04_postoperative_longitudinal_analysis")
MODULE04_TABLE_DIR <- file.path(MODULE04_OUTPUT_ROOT, "table")
MODULE04_FIGURE_DIR <- file.path(MODULE04_OUTPUT_ROOT, "figure")
MODULE04_INTERMEDIATE_DIR <- file.path(MODULE04_OUTPUT_ROOT, "intermediate")
MODULE04_PLOT_DATA_DIR <- file.path(MODULE04_ROOT, "outputs", "06_final_figures", "_plot_data")
MODULE04_STYLE_FILE <- file.path(
  MODULE04_ROOT, "code", "06_figure_assembly", "00_figure_style_manifest.csv"
)

module04_check_environment <- function() {
  missing <- MODULE04_REQUIRED_PACKAGES[!vapply(
    MODULE04_REQUIRED_PACKAGES, requireNamespace, logical(1), quietly = TRUE
  )]
  if (length(missing)) {
    stop(
      "Missing locked R package(s): ", paste(missing, collapse = ", "), ".\n",
      "Open the binge-eating-bariatric-analysis project and run renv::restore().",
      call. = FALSE
    )
  }

  skip_version <- identical(Sys.getenv("MODULE04_SKIP_VERSION_CHECK"), "1")
  if (!skip_version && nzchar(MODULE04_EXPECTED_R_VERSION) &&
      as.character(getRversion()) != MODULE04_EXPECTED_R_VERSION) {
    stop(
      "Wrong R version: expected ", MODULE04_EXPECTED_R_VERSION,
      ", found ", as.character(getRversion()),
      ". Launch the project with the locked R version.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

module04_load_style <- function() {
  style <- MODULE04_DEFAULT_STYLE
  if (!file.exists(MODULE04_STYLE_FILE)) {
    message("[INFO] shared style manifest not found; using frozen module 04 defaults")
    return(style)
  }
  manifest <- utils::read.csv(
    MODULE04_STYLE_FILE, check.names = FALSE, stringsAsFactors = FALSE
  )
  lookup <- setNames(names(manifest), tolower(trimws(names(manifest))))
  if (!all(c("key", "value") %in% names(lookup))) {
    stop("Style manifest must contain Key and Value columns: ", MODULE04_STYLE_FILE, call. = FALSE)
  }
  values <- setNames(manifest[[lookup[["value"]]]], trimws(as.character(manifest[[lookup[["key"]]]])))
  numeric_keys <- c(
    "png_dpi", "single_axis_title_size_pt", "single_tick_size_pt",
    "single_legend_size_pt", "line_main_single_width_pt"
  )
  for (key in names(style)) {
    if (!key %in% names(values) || is.na(values[[key]]) || !nzchar(trimws(as.character(values[[key]])))) next
    style[[key]] <- if (key %in% numeric_keys) as.numeric(values[[key]]) else as.character(values[[key]])
  }
  style
}

module04_require_font <- function(font_family) {
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
      match <- suppressWarnings(system2(fc_match, c("-f", "%{family}", shQuote(font_family)), stdout = TRUE, stderr = FALSE))
      found <- length(match) > 0L && any(grepl(font_family, match, fixed = TRUE, ignore.case = TRUE))
    }
  }
  if (!found) {
    stop("Required font '", font_family, "' is unavailable. Install it before running.", call. = FALSE)
  }
  message("[PASS] required font available: ", font_family)
  invisible(TRUE)
}

module04_as_numeric_strict <- function(x, role) {
  if (is.numeric(x)) return(as.numeric(x))
  chr <- trimws(as.character(x))
  chr[chr %in% c("", "NA", "N/A", ".", "-")] <- NA_character_
  chr <- gsub(",", "", chr, fixed = TRUE)
  out <- suppressWarnings(as.numeric(chr))
  bad <- !is.na(chr) & is.na(out)
  if (any(bad)) {
    stop("Non-numeric value(s) found in ", role, ": ", paste(unique(chr[bad])[1:min(5L, sum(bad))], collapse = ", "), call. = FALSE)
  }
  out
}

module04_standardize_group <- function(x) {
  raw <- trimws(as.character(x))
  norm <- module04_normalize_token(raw)
  out <- rep(NA_character_, length(norm))
  out[norm %in% c("0", "nonbe", "nonbingeeating", "withoutbe", "control", "no")] <- "Non-BE"
  out[norm %in% c("1", "be", "bingeeating", "withbe", "case", "yes")] <- "BE"
  # Common legacy coding: 1 = non-BE, 2 = BE.
  if (all(unique(norm[!is.na(norm)]) %in% c("1", "2"))) {
    out[norm == "1"] <- "Non-BE"
    out[norm == "2"] <- "BE"
  }
  if (anyNA(out)) {
    stop("Unrecognized BE-group value(s): ", paste(unique(raw[is.na(out)]), collapse = ", "), call. = FALSE)
  }
  factor(out, levels = MODULE04_CONFIG$group_levels)
}

module04_standardize_visit <- function(x) {
  raw <- trimws(as.character(x))
  norm <- module04_normalize_token(raw)
  out <- rep(NA_character_, length(norm))
  out[norm %in% c("0", "m0", "month0", "baseline", "pre", "preoperative")] <- "M0"
  out[norm %in% c("1", "m1", "month1", "1month", "1mo")] <- "M1"
  out[norm %in% c("3", "m3", "month3", "3month", "3mo")] <- "M3"
  out[norm %in% c("6", "m6", "month6", "6month", "6mo")] <- "M6"
  if (anyNA(out)) {
    stop("Unrecognized visit value(s): ", paste(unique(raw[is.na(out)]), collapse = ", "), call. = FALSE)
  }
  factor(out, levels = MODULE04_CONFIG$visit_levels, ordered = FALSE)
}

module04_fill_patient_constant <- function(x, id, role) {
  split_idx <- split(seq_along(id), id)
  out <- x
  for (idx in split_idx) {
    vals <- unique(x[idx][!is.na(x[idx])])
    if (length(vals) > 1L) stop(role, " is not constant within participant ", id[idx[1]], call. = FALSE)
    if (length(vals) == 1L) out[idx] <- vals
  }
  out
}

module04_load_data <- function(path = MODULE04_DATA_FILE) {
  if (!file.exists(path)) stop("Input file not found: ", path, call. = FALSE)
  raw <- utils::read.csv(path, check.names = FALSE, na.strings = c("", "NA", "N/A", "."), fileEncoding = "UTF-8-BOM")
  cn <- names(raw)

  outcome_columns <- vapply(MODULE04_OUTCOMES, `[[`, character(1), "source_column")
  required_columns <- unique(c(unname(MODULE04_SOURCE_COLUMNS), unname(outcome_columns)))
  missing_columns <- setdiff(required_columns, cn)
  if (length(missing_columns)) {
    stop(
      "The standardized bariatric_longitudinal.csv schema is incomplete. Missing exact column(s): ",
      paste(missing_columns, collapse = ", "), ".\n",
      "Do not rename columns manually; use the validated shared data file.",
      call. = FALSE
    )
  }
  if (anyDuplicated(cn)) {
    stop("Duplicate column names detected in bariatric_longitudinal.csv.", call. = FALSE)
  }

  id_col <- unname(MODULE04_SOURCE_COLUMNS[["id"]])
  group_col <- unname(MODULE04_SOURCE_COLUMNS[["group"]])
  visit_col <- unname(MODULE04_SOURCE_COLUMNS[["visit"]])
  sex_col <- unname(MODULE04_SOURCE_COLUMNS[["sex"]])
  age_col <- unname(MODULE04_SOURCE_COLUMNS[["age"]])
  bmi0_col <- unname(MODULE04_SOURCE_COLUMNS[["bmi0"]])

  dat <- data.frame(
    id = as.character(raw[[id_col]]),
    group = module04_standardize_group(raw[[group_col]]),
    visit = module04_standardize_visit(raw[[visit_col]]),
    sex = factor(raw[[sex_col]]),
    age = module04_as_numeric_strict(raw[[age_col]], "age"),
    stringsAsFactors = FALSE
  )
  if (anyNA(dat$id) || any(!nzchar(dat$id))) stop("Missing participant ID detected.", call. = FALSE)
  if (nlevels(dat$sex) < 2L) stop("Sex has fewer than two observed levels.", call. = FALSE)

  for (key in names(MODULE04_OUTCOMES)) {
    dat[[key]] <- module04_as_numeric_strict(raw[[outcome_columns[[key]]]], MODULE04_OUTCOMES[[key]]$label)
  }

  dat$bmi0 <- module04_as_numeric_strict(raw[[bmi0_col]], "baseline BMI")
  dat$bmi0 <- module04_fill_patient_constant(dat$bmi0, dat$id, "baseline BMI")

  dat$group <- factor(module04_fill_patient_constant(as.character(dat$group), dat$id, "group"), levels = MODULE04_CONFIG$group_levels)
  dat$sex <- factor(module04_fill_patient_constant(as.character(dat$sex), dat$id, "sex"))
  dat$age <- module04_fill_patient_constant(dat$age, dat$id, "age")
  dat$bmi0 <- module04_fill_patient_constant(dat$bmi0, dat$id, "baseline BMI")

  if (nrow(dat) != MODULE04_CONFIG$expected_rows) {
    stop("Expected ", MODULE04_CONFIG$expected_rows, " rows; found ", nrow(dat), ".", call. = FALSE)
  }
  if (length(unique(dat$id)) != MODULE04_CONFIG$expected_subjects) {
    stop("Expected ", MODULE04_CONFIG$expected_subjects, " participants; found ", length(unique(dat$id)), ".", call. = FALSE)
  }
  if (anyDuplicated(dat[c("id", "visit")])) stop("Duplicate participant-by-visit rows detected.", call. = FALSE)
  grid_count <- table(dat$id)
  if (any(grid_count != length(MODULE04_CONFIG$visit_levels))) {
    stop("Every participant must have exactly one row for M0, M1, M3 and M6.", call. = FALSE)
  }
  gcount <- table(unique(dat[c("id", "group")])$group)
  expected <- MODULE04_CONFIG$expected_group_counts
  if (!all(as.integer(gcount[names(expected)]) == as.integer(expected))) {
    stop("Unexpected group counts. Found: ", paste(names(gcount), as.integer(gcount), collapse = "; "), call. = FALSE)
  }
  if (anyNA(dat$age) || anyNA(dat$sex) || anyNA(dat$group) || anyNA(dat$bmi0)) {
    stop("Patient-level covariates (group, sex, age, baseline BMI) must be complete.", call. = FALSE)
  }

  attr(dat, "column_mapping") <- c(
    id = id_col, group = group_col, visit = visit_col, sex = sex_col,
    age = age_col, bmi0 = bmi0_col,
    outcome_columns
  )
  dat[order(dat$id, dat$visit), , drop = FALSE]
}

module04_subject_frame <- function(dat, ids) {
  x <- dat[!duplicated(dat$id) & dat$id %in% ids, c("id", "group", "sex", "age", "bmi0")]
  x <- x[match(ids, x$id), , drop = FALSE]
  if (anyNA(x)) stop("Missing subject-level covariate after subject-frame construction.", call. = FALSE)
  x
}

module04_prepare_primary <- function(dat, outcome_key) {
  base <- dat[dat$visit == "M0", c("id", outcome_key), drop = FALSE]
  names(base)[2] <- "baseline_value"
  post <- dat[dat$visit %in% MODULE04_CONFIG$post_visits, c("id", "visit", outcome_key), drop = FALSE]
  names(post)[3] <- "outcome_value"
  post$baseline_value <- base$baseline_value[match(post$id, base$id)]
  post <- post[!is.na(post$outcome_value) & !is.na(post$baseline_value), , drop = FALSE]
  ids <- unique(post$id)
  if (length(ids) < 10L) stop("Too few participants for primary MMRM: ", MODULE04_OUTCOMES[[outcome_key]]$label, call. = FALSE)
  sf <- module04_subject_frame(dat, ids)
  post$group <- sf$group[match(post$id, sf$id)]
  post$sex <- sf$sex[match(post$id, sf$id)]
  post$age_c <- sf$age[match(post$id, sf$id)] - mean(sf$age)
  post$bmi0_c <- sf$bmi0[match(post$id, sf$id)] - mean(sf$bmi0)
  base_by_id <- base$baseline_value[match(sf$id, base$id)]
  post$baseline_c <- post$baseline_value - mean(base_by_id, na.rm = TRUE)
  post$id <- factor(post$id)
  post$group <- factor(post$group, levels = MODULE04_CONFIG$group_levels)
  post$visit <- factor(as.character(post$visit), levels = MODULE04_CONFIG$post_visits)
  post$sex <- factor(post$sex)
  post
}

module04_prepare_baseline_included <- function(dat, outcome_key) {
  x <- dat[, c("id", "group", "visit", "sex", "age", "bmi0", outcome_key), drop = FALSE]
  names(x)[7] <- "outcome_value"
  x <- x[!is.na(x$outcome_value), , drop = FALSE]
  ids <- unique(x$id)
  if (length(ids) < 10L) stop("Too few participants for baseline-included model: ", MODULE04_OUTCOMES[[outcome_key]]$label, call. = FALSE)
  sf <- module04_subject_frame(dat, ids)
  x$age_c <- x$age - mean(sf$age)
  x$bmi0_c <- x$bmi0 - mean(sf$bmi0)
  x$id <- factor(x$id)
  x$group <- factor(x$group, levels = MODULE04_CONFIG$group_levels)
  x$visit <- factor(as.character(x$visit), levels = MODULE04_CONFIG$visit_levels)
  x$sex <- factor(x$sex)
  x
}

module04_fixed_formula <- function(model_type = c("primary", "baseline_included"), outcome_key) {
  model_type <- match.arg(model_type)
  include_bmi <- !(outcome_key %in% MODULE04_BODY_SIZE_OUTCOMES)
  if (model_type == "primary") {
    rhs <- "group * visit + baseline_c * visit + sex + age_c"
  } else {
    rhs <- "group * visit + sex + age_c"
  }
  if (include_bmi) rhs <- paste(rhs, "+ bmi0_c")
  stats::as.formula(paste("outcome_value ~", rhs))
}

module04_check_fixed_rank <- function(fixed_formula, data) {
  mm <- stats::model.matrix(fixed_formula, data = data)
  qr_mm <- qr(mm)
  ok <- identical(qr_mm$rank, ncol(mm))
  if (!ok) {
    dependent <- colnames(mm)[qr_mm$pivot[(qr_mm$rank + 1L):ncol(mm)]]
    stop("Rank-deficient fixed-effects design. Dependent column(s): ", paste(dependent, collapse = ", "), call. = FALSE)
  }
  list(ok = TRUE, rank = qr_mm$rank, columns = ncol(mm))
}

module04_cov_formula <- function(fixed_formula, covariance) {
  rhs <- paste(deparse(fixed_formula[[3L]], width.cutoff = 500L), collapse = " ")
  stats::as.formula(paste("outcome_value ~", rhs, "+", covariance, "(visit | id)"))
}

module04_capture_warnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) structure(list(error = conditionMessage(e)), class = "module04_fit_error")),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = unique(warnings))
}

module04_convergence_value <- function(fit) {
  if ("component" %in% getNamespaceExports("mmrm")) {
    out <- try(mmrm::component(fit, "convergence"), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
  }
  NA
}

module04_convergence_ok <- function(x) {
  if (length(x) == 1L && is.na(x)) return(TRUE)
  if (is.logical(x)) return(all(x))
  if (is.numeric(x)) return(all(is.finite(x)) && all(x == 0))
  if (is.list(x)) {
    candidate <- unlist(x[c("code", "convergence", "status")], recursive = TRUE, use.names = FALSE)
    if (length(candidate)) return(module04_convergence_ok(candidate))
    return(TRUE)
  }
  txt <- tolower(paste(x, collapse = " "))
  !grepl("fail|not converg|error", txt)
}

module04_fit_one_covariance <- function(data, fixed_formula, covariance) {
  module04_check_fixed_rank(fixed_formula, data)
  full_formula <- module04_cov_formula(fixed_formula, covariance)
  control <- mmrm::mmrm_control(method = MODULE04_CONFIG$df_method)
  captured <- module04_capture_warnings(
    mmrm::mmrm(
      formula = full_formula,
      data = data,
      reml = TRUE,
      control = control
    )
  )
  if (inherits(captured$value, "module04_fit_error")) {
    return(list(ok = FALSE, error = captured$value$error, warnings = captured$warnings))
  }
  fit <- captured$value
  beta <- try(stats::coef(fit), silent = TRUE)
  vbeta <- try(stats::vcov(fit), silent = TRUE)
  if (inherits(beta, "try-error") || inherits(vbeta, "try-error") ||
      any(!is.finite(beta)) || any(!is.finite(vbeta))) {
    return(list(ok = FALSE, error = "Non-finite fixed-effect estimate or covariance matrix.", warnings = captured$warnings))
  }
  conv <- module04_convergence_value(fit)
  if (!module04_convergence_ok(conv)) {
    return(list(ok = FALSE, error = paste("Optimizer convergence status:", paste(conv, collapse = " ")), warnings = captured$warnings))
  }
  bad_warning <- any(grepl("failed|not converg|non-positive|not positive|singular", captured$warnings, ignore.case = TRUE))
  if (bad_warning) {
    return(list(ok = FALSE, error = paste(captured$warnings, collapse = " | "), warnings = captured$warnings))
  }
  list(ok = TRUE, fit = fit, warnings = captured$warnings, convergence = conv)
}

module04_fit_with_fallback <- function(data, fixed_formula) {
  first <- module04_fit_one_covariance(data, fixed_formula, MODULE04_CONFIG$preferred_covariance)
  if (isTRUE(first$ok)) {
    return(list(
      fit = first$fit, covariance = "UN", fallback = FALSE,
      warnings = first$warnings, convergence = first$convergence,
      first_error = NA_character_
    ))
  }
  second <- module04_fit_one_covariance(data, fixed_formula, MODULE04_CONFIG$fallback_covariance)
  if (!isTRUE(second$ok)) {
    stop(
      "Both covariance models failed. UN: ", first$error,
      " | ARH(1): ", second$error,
      call. = FALSE
    )
  }
  list(
    fit = second$fit, covariance = "ARH(1)", fallback = TRUE,
    warnings = unique(c(first$warnings, second$warnings)), convergence = second$convergence,
    first_error = first$error
  )
}

module04_fit_fixed_covariance <- function(data, fixed_formula, covariance_label) {
  cov_name <- if (identical(covariance_label, "UN")) "us" else if (identical(covariance_label, "ARH(1)")) "ar1h" else stop("Unknown covariance label: ", covariance_label)
  out <- module04_fit_one_covariance(data, fixed_formula, cov_name)
  if (!isTRUE(out$ok)) stop("Fixed covariance fit failed [", covariance_label, "]: ", out$error, call. = FALSE)
  out$fit
}

module04_emm_bundle <- function(fit) {
  emm <- emmeans::emmeans(fit, specs = ~ group * visit, weights = "equal")
  grid <- as.data.frame(emm)
  if (!all(c("group", "visit", "emmean") %in% names(grid))) stop("Unexpected emmeans grid columns.", call. = FALSE)
  if (anyDuplicated(grid[c("group", "visit")])) stop("Duplicate group-by-visit cells in emmeans grid.", call. = FALSE)
  list(emm = emm, grid = grid)
}

module04_group_difference_matrix <- function(grid, visits) {
  C <- matrix(0, nrow = length(visits), ncol = nrow(grid), dimnames = list(visits, NULL))
  for (v in visits) {
    i_be <- which(as.character(grid$group) == "BE" & as.character(grid$visit) == v)
    i_non <- which(as.character(grid$group) == "Non-BE" & as.character(grid$visit) == v)
    if (length(i_be) != 1L || length(i_non) != 1L) stop("Cannot identify BE and non-BE emmeans at ", v, call. = FALSE)
    C[v, i_be] <- 1
    C[v, i_non] <- -1
  }
  C
}

module04_time_mean_matrix <- function(grid, visits) {
  C <- matrix(0, nrow = length(visits), ncol = nrow(grid), dimnames = list(visits, NULL))
  for (v in visits) {
    idx <- which(as.character(grid$visit) == v)
    if (length(idx) != 2L) stop("Expected two group means at ", v, call. = FALSE)
    C[v, idx] <- 1 / length(idx)
  }
  C
}

module04_make_contrast <- function(emm, C, labels) {
  if (is.null(dim(C))) C <- matrix(C, nrow = 1L)
  if (nrow(C) != length(labels)) stop("Contrast label count mismatch.", call. = FALSE)
  methods <- lapply(seq_len(nrow(C)), function(i) as.numeric(C[i, ]))
  names(methods) <- labels
  emmeans::contrast(emm, method = methods, adjust = "none")
}

module04_contrast_payload <- function(contrast_object) {
  s <- as.data.frame(summary(contrast_object, infer = c(FALSE, FALSE), adjust = "none"))
  if (!"estimate" %in% names(s)) stop("Contrast output lacks estimate.", call. = FALSE)
  list(q = as.numeric(s$estimate), U = as.matrix(stats::vcov(contrast_object)))
}

module04_contrast_p <- function(contrast_object, joint = FALSE) {
  if (joint) {
    s <- as.data.frame(emmeans::test(contrast_object, joint = TRUE))
  } else {
    s <- as.data.frame(summary(contrast_object, infer = c(TRUE, TRUE), adjust = "none"))
  }
  pcol <- intersect(c("p.value", "p.value."), names(s))
  if (!length(pcol)) pcol <- grep("^p\\.?value", names(s), value = TRUE)
  if (!length(pcol)) stop("Cannot find p-value in emmeans test output.", call. = FALSE)
  p <- as.numeric(s[[pcol[1L]]][1L])
  if (!is.finite(p)) stop("Non-finite p-value from emmeans contrast.", call. = FALSE)
  p
}

module04_extract_primary_estimands <- function(fit) {
  b <- module04_emm_bundle(fit)
  D <- module04_group_difference_matrix(b$grid, MODULE04_CONFIG$post_visits)
  C_interaction <- rbind(
    "D_M3_minus_D_M1" = D["M3", ] - D["M1", ],
    "D_M6_minus_D_M1" = D["M6", ] - D["M1", ]
  )
  C_group_average <- matrix(colMeans(D), nrow = 1L)
  interaction_ct <- module04_make_contrast(b$emm, C_interaction, rownames(C_interaction))
  group_ct <- module04_make_contrast(b$emm, C_group_average, "Average_BE_minus_nonBE_M1_M3_M6")
  list(
    p_interaction = module04_contrast_p(interaction_ct, joint = TRUE),
    p_group_average = module04_contrast_p(group_ct, joint = FALSE),
    payload_interaction = module04_contrast_payload(interaction_ct),
    payload_group_average = module04_contrast_payload(group_ct),
    emmeans = b
  )
}

module04_extract_baseline_estimands <- function(fit) {
  b <- module04_emm_bundle(fit)
  visits <- MODULE04_CONFIG$visit_levels
  D <- module04_group_difference_matrix(b$grid, visits)
  Tm <- module04_time_mean_matrix(b$grid, visits)
  C_time <- rbind(
    "M1_minus_M0" = Tm["M1", ] - Tm["M0", ],
    "M3_minus_M0" = Tm["M3", ] - Tm["M0", ],
    "M6_minus_M0" = Tm["M6", ] - Tm["M0", ]
  )
  C_group_average <- matrix(colMeans(D), nrow = 1L)
  C_dd <- matrix(D["M6", ] - D["M0", ], nrow = 1L)
  C_reversal <- rbind("D_M0" = D["M0", ], "D_M6" = D["M6", ])
  time_ct <- module04_make_contrast(b$emm, C_time, rownames(C_time))
  group_ct <- module04_make_contrast(b$emm, C_group_average, "Average_BE_minus_nonBE_M0_M1_M3_M6")
  dd_ct <- module04_make_contrast(b$emm, C_dd, "Difference_in_change_M6_minus_M0")
  reversal_ct <- module04_make_contrast(b$emm, C_reversal, rownames(C_reversal))
  reversal_est <- module04_contrast_payload(reversal_ct)$q
  reversal <- if (prod(reversal_est) < 0) "Yes" else "No"
  list(
    p_time = module04_contrast_p(time_ct, joint = TRUE),
    p_group_average = module04_contrast_p(group_ct, joint = FALSE),
    p_dd = module04_contrast_p(dd_ct, joint = FALSE),
    reversal = reversal,
    reversal_estimates = setNames(reversal_est, c("M0", "M6")),
    payload_time = module04_contrast_payload(time_ct),
    payload_group_average = module04_contrast_payload(group_ct),
    payload_dd = module04_contrast_payload(dd_ct),
    payload_reversal = module04_contrast_payload(reversal_ct),
    emmeans = b
  )
}

module04_plot_data <- function(baseline_estimands, spec) {
  x <- baseline_estimands$emmeans$grid
  lower_name <- intersect(c("lower.CL", "asymp.LCL"), names(x))
  upper_name <- intersect(c("upper.CL", "asymp.UCL"), names(x))
  se_name <- intersect(c("SE", "se"), names(x))
  if (!length(lower_name) || !length(upper_name) || !length(se_name)) {
    stop("Estimate, SE, or confidence-limit columns not found in emmeans output.", call. = FALSE)
  }
  part_label <- c(
    "1" = "Anthropometrics and nutrition",
    "2" = "Metabolic indicators",
    "3" = "Immune indicators"
  )[[as.character(spec$part)]]
  data.frame(
    outcome_key = spec$key,
    outcome = spec$label,
    unit = spec$unit,
    outcome_label = spec$display_label,
    category = spec$category,
    part = spec$part,
    part_label = part_label,
    panel_id = spec$panel_id,
    display_order = spec$display_order,
    group = factor(as.character(x$group), levels = MODULE04_CONFIG$group_levels),
    visit = factor(as.character(x$visit), levels = MODULE04_CONFIG$visit_levels),
    estimate = as.numeric(x$emmean),
    se = as.numeric(x[[se_name[1L]]]),
    lower = as.numeric(x[[lower_name[1L]]]),
    upper = as.numeric(x[[upper_name[1L]]]),
    stringsAsFactors = FALSE
  )
}

module04_pt_to_mm <- function(value) as.numeric(value) / 2.83464567

module04_axis_label <- function(spec) {
  exponent <- if (identical(spec$key, "rbc")) {
    12L
  } else if (spec$key %in% c(
    "wbc", "neutrophils", "lymphocytes", "monocytes", "eosinophils", "basophils"
  )) {
    9L
  } else {
    NA_integer_
  }
  if (is.na(exponent)) return(spec$display_label)
  bquote(paste(.(spec$label), " (", "×", 10^.(exponent), "/L)"))
}

module04_make_plot <- function(plot_data, spec, style, show_legend = TRUE) {
  colors <- c(
    "Non-BE" = as.character(style$color_non_be),
    "BE" = as.character(style$color_be)
  )
  line_width <- module04_pt_to_mm(style$line_main_single_width_pt)
  border_width <- module04_pt_to_mm(0.75)
  tick_size <- as.numeric(style$single_tick_size_pt)
  axis_size <- as.numeric(style$single_axis_title_size_pt)
  legend_size <- as.numeric(style$single_legend_size_pt)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = visit, y = estimate, group = group,
      color = group, fill = group, linetype = group, shape = group
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper),
      color = NA, alpha = 0.16
    ) +
    ggplot2::geom_line(linewidth = line_width) +
    ggplot2::geom_point(size = 1.58, stroke = 0.30) +
    ggplot2::scale_color_manual(
      values = colors, breaks = MODULE04_CONFIG$group_levels, name = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = colors, breaks = MODULE04_CONFIG$group_levels, name = NULL
    ) +
    ggplot2::scale_linetype_manual(
      values = MODULE04_CONFIG$linetypes,
      breaks = MODULE04_CONFIG$group_levels,
      name = NULL
    ) +
    ggplot2::scale_shape_manual(
      values = MODULE04_CONFIG$shapes,
      breaks = MODULE04_CONFIG$group_levels,
      name = NULL
    ) +
    ggplot2::scale_x_discrete(
      labels = c("M0" = "Baseline", "M1" = "M1", "M3" = "M3", "M6" = "M6"),
      drop = FALSE,
      expand = ggplot2::expansion(add = c(0.12, 0.12))
    ) +
    ggplot2::labs(
      title = spec$label,
      x = NULL,
      y = module04_axis_label(spec)
    ) +
    ggplot2::theme_bw(
      base_size = tick_size,
      base_family = as.character(style$font_family)
    ) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        linewidth = border_width, color = "#4D4D4D"
      ),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = tick_size, color = "#222222"),
      axis.title.y = ggplot2::element_text(
        size = axis_size, margin = ggplot2::margin(r = 5)
      ),
      axis.ticks = ggplot2::element_line(linewidth = border_width),
      axis.ticks.length = grid::unit(1.5, "mm"),
      plot.title = ggplot2::element_text(
        size = 10, hjust = 0.5, margin = ggplot2::margin(b = 4)
      ),
      legend.position = if (isTRUE(show_legend)) "top" else "none",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.box.spacing = grid::unit(0, "pt"),
      legend.margin = ggplot2::margin(0, 0, 2, 0),
      legend.key = ggplot2::element_blank(),
      legend.key.width = grid::unit(8, "mm"),
      legend.key.height = grid::unit(4, "mm"),
      legend.text = ggplot2::element_text(size = legend_size),
      plot.margin = ggplot2::margin(t = 5.5, r = 7, b = 5.5, l = 7)
    )
}

module04_safe_filename <- function(index, spec) {
  sprintf("%02d_trajectory_%s.png", as.integer(index), spec$slug)
}

module04_format_p <- function(p) {
  if (!is.finite(p)) return(NA_character_)
  if (p < 0.001) return("<0.001")
  formatC(p, format = "f", digits = 3)
}

module04_model_log_row <- function(outcome_label, model_label, data, fit_result, fixed_formula) {
  mm <- stats::model.matrix(fixed_formula, data = data)
  notes <- fit_result$warnings
  if (isTRUE(fit_result$fallback) && !is.na(fit_result$first_error)) {
    notes <- c(paste0("UN failure: ", fit_result$first_error), notes)
  }
  data.frame(
    Outcome = outcome_label,
    Model = model_label,
    N_subjects = length(unique(data$id)),
    N_observations = nrow(data),
    Covariance = fit_result$covariance,
    Fallback_used = if (fit_result$fallback) "Yes" else "No",
    Fixed_design_rank = qr(mm)$rank,
    Fixed_design_columns = ncol(mm),
    Convergence = "OK",
    REML = "Yes",
    DF_method = MODULE04_CONFIG$df_method,
    Warnings = if (length(notes)) paste(unique(notes), collapse = " | ") else "None",
    stringsAsFactors = FALSE
  )
}

module04_write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  invisible(path)
}

module04_rewrite_rgb_png <- function(
  source, target = source, dpi, background_color = "white",
  expected_width_cm = NULL, expected_height_cm = NULL
) {
  image <- png::readPNG(source)
  dimensions <- dim(image)
  if (length(dimensions) != 3L || !dimensions[3L] %in% c(3L, 4L)) {
    stop("Unexpected PNG channel structure: ", source, call. = FALSE)
  }
  if (dimensions[3L] == 4L) {
    alpha <- image[, , 4L]
    background <- as.numeric(grDevices::col2rgb(background_color)) / 255
    rgb <- image[, , 1:3, drop = FALSE]
    for (channel in seq_len(3L)) {
      rgb[, , channel] <-
        rgb[, , channel] * alpha + background[channel] * (1 - alpha)
    }
  } else {
    rgb <- image
  }
  png::writePNG(rgb, target = target, dpi = as.numeric(dpi))

  written <- png::readPNG(target, info = TRUE)
  written_dimensions <- dim(written)
  if (length(written_dimensions) != 3L || written_dimensions[3L] != 3L) {
    stop("Final PNG is not encoded as RGB: ", target, call. = FALSE)
  }
  if (!is.null(expected_width_cm) && !is.null(expected_height_cm)) {
    expected_height <- as.integer(round(expected_height_cm / 2.54 * dpi))
    expected_width <- as.integer(round(expected_width_cm / 2.54 * dpi))
    if (abs(written_dimensions[1L] - expected_height) > 2L ||
        abs(written_dimensions[2L] - expected_width) > 2L) {
      stop(
        "Unexpected final PNG dimensions: ",
        paste(written_dimensions[1:2], collapse = " × "),
        call. = FALSE
      )
    }
  }
  metadata <- attr(written, "info")
  recorded_dpi <- suppressWarnings(as.numeric(metadata$dpi))
  if (length(recorded_dpi) < 2L || any(!is.finite(recorded_dpi)) ||
      any(abs(recorded_dpi[1:2] - dpi) > 1)) {
    stop("Final PNG does not contain the requested DPI metadata.", call. = FALSE)
  }
  invisible(target)
}

module04_save_rgb_png <- function(
  plot, path, width_cm, height_cm, dpi, background_color = "white"
) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".module04_", fileext = ".png")
  on.exit(unlink(temporary), add = TRUE)
  ggplot2::ggsave(
    filename = temporary, plot = plot,
    width = width_cm, height = height_cm, units = "cm",
    dpi = dpi, bg = background_color, limitsize = FALSE
  )
  module04_rewrite_rgb_png(
    source = temporary, target = path, dpi = dpi,
    background_color = background_color,
    expected_width_cm = width_cm, expected_height_cm = height_cm
  )
}

module04_xml_tags <- function(text, pattern) {
  matches <- gregexpr(pattern, text, perl = TRUE)[[1L]]
  if (identical(matches[1L], -1L)) return(character())
  regmatches(text, list(matches))[[1L]]
}

module04_sanitize_xlsx <- function(path) {
  extraction_root <- tempfile(pattern = ".module04_xlsx_")
  rebuilt <- tempfile(pattern = ".module04_xlsx_", fileext = ".xlsx")
  dir.create(extraction_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(c(extraction_root, rebuilt), recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(path, exdir = extraction_root)

  relationship_files <- list.files(
    extraction_root, pattern = "\\.rels$", recursive = TRUE,
    full.names = TRUE, all.files = TRUE
  )
  for (relationship_file in relationship_files) {
    relative <- substring(
      normalizePath(relationship_file, winslash = "/", mustWork = TRUE),
      nchar(normalizePath(extraction_root, winslash = "/", mustWork = TRUE)) + 2L
    )
    source_directory <- if (identical(relative, "_rels/.rels")) {
      ""
    } else {
      source_part <- sub(
        "/_rels/([^/]+)\\.rels$", "/\\1", relative, perl = TRUE
      )
      directory <- dirname(source_part)
      if (identical(directory, ".")) "" else directory
    }

    text <- paste(readLines(relationship_file, warn = FALSE, encoding = "UTF-8"), collapse = "")
    tags <- module04_xml_tags(text, "<Relationship\\b[^>]*/>")
    for (tag in tags) {
      if (grepl("\\bTargetMode=\"External\"", tag, perl = TRUE)) next
      target <- sub(
        ".*\\bTarget=\"([^\"]+)\".*", "\\1", tag, perl = TRUE
      )
      resolved <- file.path(extraction_root, source_directory, target)
      if (!file.exists(resolved)) text <- gsub(tag, "", text, fixed = TRUE)
    }
    writeLines(text, relationship_file, useBytes = TRUE)
  }

  content_types <- file.path(extraction_root, "[Content_Types].xml")
  content <- paste(readLines(content_types, warn = FALSE, encoding = "UTF-8"), collapse = "")
  overrides <- module04_xml_tags(content, "<Override\\b[^>]*/>")
  for (override in overrides) {
    part_name <- sub(
      ".*\\bPartName=\"/([^\"]+)\".*", "\\1", override, perl = TRUE
    )
    if (!file.exists(file.path(extraction_root, part_name))) {
      content <- gsub(override, "", content, fixed = TRUE)
    }
  }
  writeLines(content, content_types, useBytes = TRUE)

  archive_files <- list.files(
    extraction_root, recursive = TRUE, full.names = FALSE,
    all.files = TRUE, no.. = TRUE, include.dirs = FALSE
  )
  zip::zipr(
    rebuilt,
    files = archive_files,
    root = extraction_root,
    mode = "mirror",
    include_directories = FALSE
  )
  if (!file.copy(rebuilt, path, overwrite = TRUE)) {
    stop("Failed to replace sanitized XLSX file: ", path, call. = FALSE)
  }
  invisible(openxlsx::loadWorkbook(path))
  invisible(path)
}

module04_write_two_level_xlsx <- function(
  body,
  path,
  sheet_name,
  top_headers,
  sub_headers,
  merge_groups,
  widths
) {
  if (ncol(body) != length(top_headers) || ncol(body) != length(sub_headers)) {
    stop("XLSX header and body column counts differ.", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)
  header <- rbind(top_headers, sub_headers)
  openxlsx::writeData(
    wb, sheet_name, header,
    startRow = 1, startCol = 1,
    colNames = FALSE, rowNames = FALSE, keepNA = FALSE
  )
  openxlsx::writeData(
    wb, sheet_name, body,
    startRow = 3, startCol = 1,
    colNames = FALSE, rowNames = FALSE, keepNA = FALSE
  )
  for (group in merge_groups) {
    if (length(group) != 2L) stop("Each header merge group must contain two columns.", call. = FALSE)
    if (group[1L] == group[2L]) {
      openxlsx::mergeCells(wb, sheet_name, cols = group[1L], rows = 1:2)
    } else {
      openxlsx::mergeCells(wb, sheet_name, cols = group[1L]:group[2L], rows = 1)
    }
  }

  header_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 9, textDecoration = "bold",
    halign = "center", valign = "center", wrapText = TRUE,
    border = c("top", "bottom", "left", "right"),
    borderColour = "#BFBFBF", borderStyle = "thin"
  )
  body_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 9,
    halign = "center", valign = "center", wrapText = TRUE,
    border = "bottom", borderColour = "#D9D9D9", borderStyle = "thin"
  )
  first_col_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 9,
    halign = "left", valign = "center", wrapText = TRUE,
    border = "bottom", borderColour = "#D9D9D9", borderStyle = "thin"
  )
  openxlsx::addStyle(
    wb, sheet_name, header_style,
    rows = 1:2, cols = seq_len(ncol(body)), gridExpand = TRUE, stack = TRUE
  )
  if (nrow(body)) {
    body_rows <- 3:(nrow(body) + 2L)
    openxlsx::addStyle(
      wb, sheet_name, body_style,
      rows = body_rows, cols = seq_len(ncol(body)), gridExpand = TRUE, stack = TRUE
    )
    openxlsx::addStyle(
      wb, sheet_name, first_col_style,
      rows = body_rows, cols = 1, gridExpand = TRUE, stack = TRUE
    )
    openxlsx::setRowHeights(wb, sheet_name, rows = body_rows, heights = 18)
  }
  openxlsx::setRowHeights(wb, sheet_name, rows = 1:2, heights = c(28, 38))
  openxlsx::setColWidths(wb, sheet_name, cols = seq_along(widths), widths = widths)
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 3)
  openxlsx::pageSetup(
    wb, sheet_name, orientation = "landscape",
    fitToWidth = 1, fitToHeight = 0, paperSize = 9
  )
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  module04_sanitize_xlsx(path)
  invisible(path)
}

module04_write_table3_xlsx <- function(table3, path) {
  top <- c(
    "Outcome (unit)",
    "Baseline-included model (M0–M6)", "",
    "Baseline-adjusted MMRM (M1–M6)", "",
    "M6 versus M0", ""
  )
  sub <- c(
    "",
    "Time effect P value", "Average group contrast P value",
    "Group × visit P value", "Average group contrast P value",
    "Reversal", "ΔΔ P value"
  )
  module04_write_two_level_xlsx(
    table3, path, "Table 3", top, sub,
    merge_groups = list(c(1L, 1L), c(2L, 3L), c(4L, 5L), c(6L, 7L)),
    widths = c(28, 18, 22, 18, 22, 13, 13)
  )
}

module04_write_mnar_xlsx <- function(table, path, sheet_name) {
  top <- c(
    "Outcome (unit)",
    "Baseline-included model (M0–M6)", "",
    "Baseline-adjusted MMRM (M1–M6)", "",
    "M6 versus M0", ""
  )
  sub <- c(
    "",
    "Time effect", "Average group contrast",
    "Group × visit", "Average group contrast",
    "ΔΔ", "Reversal"
  )
  module04_write_two_level_xlsx(
    table, path, sheet_name, top, sub,
    merge_groups = list(c(1L, 1L), c(2L, 3L), c(4L, 5L), c(6L, 7L)),
    widths = c(28, 18, 22, 18, 22, 16, 18)
  )
}

module04_conclusion <- function(p) if (is.finite(p) && p < MODULE04_CONFIG$alpha) "Sig" else "NS"
