# Shared data, model, table, and figure functions for module 05.
# This file is sourced by 01_ through 04_; it is not an analysis entry point.

options(stringsAsFactors = FALSE)

# Configuration and display metadata
MODULE05_FILESET_ID <- "module05-2026-08-25-display-refactor-v1"
MODULE05_EXPECTED_R_VERSION <- "4.5.1"
MODULE05_EXPECTED_PACKAGE_VERSIONS <- c(
  mmrm = "0.3.15",
  emmeans = "1.11.2-8",
  readr = "2.1.5",
  ggplot2 = "4.0.0"
)
MODULE05_REQUIRED_PACKAGES <- c(
  names(MODULE05_EXPECTED_PACKAGE_VERSIONS),
  "openxlsx", "png", "zip"
)

MODULE05_CONFIG <- list(
  alpha = 0.05,
  expected_rows = 488L,
  expected_subjects = 122L,
  expected_columns = 40L,
  expected_group_counts = c("Non-BE" = 66L, "BE" = 56L),
  visit_values = c(0L, 1L, 3L, 6L),
  visit_levels = c("M0", "M1", "M3", "M6"),
  current_visit_levels = c("Baseline", "M1", "M3", "M6"),
  post_visit_levels = c("M1", "M3", "M6"),
  interval_levels = c("M0-M1", "M1-M3", "M3-M6"),
  interval_labels = c("0-1 months", "1-3 months", "3-6 months"),
  preferred_covariance = "us",
  fallback_covariance = "ar1",
  df_method = "Kenward-Roger",
  forest_baseline_xlim = c(-0.20, 0.20),
  forest_overall_xlim = c(-0.30, 0.30),
  forest_visit_xlim = c(-0.80, 0.80),
  forest_width_cm = 10.0,
  forest_height_cm = 12.0,
  forest_visit_width_cm = 17.0,
  forest_visit_height_cm = 12.0,
  raw_width_cm = 11.0,
  raw_height_cm = 8.0,
  current_visit_shapes = c("Baseline" = 1L, "M1" = 16L, "M3" = 17L, "M6" = 15L),
  visit_shapes = c("M1" = 16L, "M3" = 17L, "M6" = 15L)
)

MODULE05_DEFAULT_STYLE <- list(
  font_family = "Arial",
  background_color = "white",
  png_dpi = 600,
  single_axis_title_size_pt = 10.0,
  single_tick_size_pt = 9.0,
  single_legend_size_pt = 9.0,
  line_main_single_width_pt = 1.20,
  reference_line_color = "grey70"
)

module05_panel_id <- function(index) {
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

module05_immune_spec <- function(key, label, source_column, safe_name, display_order) {
  list(
    key = key,
    label = label,
    unit = "×10⁹/L",
    display_label = paste0(label, " (×10⁹/L)"),
    source_column = source_column,
    safe_name = safe_name,
    display_order = as.integer(display_order)
  )
}

MODULE05_IMMUNE <- list(
  module05_immune_spec("wbc", "WBC", "wbc_10e9_l", "wbc", 1L),
  module05_immune_spec("neutrophils", "Neutrophils", "neutrophils_10e9_l", "neutrophils", 2L),
  module05_immune_spec("lymphocytes", "Lymphocytes", "lymphocytes_10e9_l", "lymphocytes", 3L),
  module05_immune_spec("monocytes", "Monocytes", "monocytes_10e9_l", "monocytes", 4L),
  module05_immune_spec("eosinophils", "Eosinophils", "eosinophils_10e9_l", "eosinophils", 5L),
  module05_immune_spec("basophils", "Basophils", "basophils_10e9_l", "basophils", 6L)
)
names(MODULE05_IMMUNE) <- vapply(MODULE05_IMMUNE, `[[`, character(1), "key")

module05_adiposity_spec <- function(
  key, label, current_column, baseline_column, unit_scale,
  unit, effect_scale, safe_name
) {
  list(
    key = key,
    label = label,
    current_column = current_column,
    baseline_column = baseline_column,
    unit_scale = unit_scale,
    unit = unit,
    display_label = paste0(label, " (", unit, ")"),
    effect_scale = effect_scale,
    safe_name = safe_name
  )
}

MODULE05_ADIPOSITY <- list(
  bmi = module05_adiposity_spec(
    "bmi", "BMI", "bmi_kg_m2", "baseline_bmi_kg_m2",
    2, "kg/m²", "per 2 kg/m²", "bmi"
  ),
  weight = module05_adiposity_spec(
    "weight", "Weight", "weight_kg", "baseline_weight_kg",
    5, "kg", "per 5 kg", "weight"
  )
)

MODULE05_RAW_PANEL_MAP <- data.frame(
  panel_order = seq_len(12L),
  panel_id = vapply(seq_len(12L), module05_panel_id, character(1)),
  exposure_key = rep(c("weight", "bmi"), each = 6L),
  outcome_key = rep(names(MODULE05_IMMUNE), times = 2L),
  stringsAsFactors = FALSE
)

MODULE05_REQUIRED_SOURCE_COLUMNS <- unique(c(
  "participant_id", "sex", "age_years", "bes_group", "visit_month",
  vapply(MODULE05_ADIPOSITY, `[[`, character(1), "current_column"),
  vapply(MODULE05_ADIPOSITY, `[[`, character(1), "baseline_column"),
  vapply(MODULE05_IMMUNE, `[[`, character(1), "source_column")
))

# Project paths and environment checks
module05_walk_up <- function(start, max_levels = 12L) {
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

module05_find_root <- function() {
  starts <- unique(c(getwd(), dirname(getwd())))
  for (start in starts) {
    for (cur in module05_walk_up(start)) {
      direct <- file.path(cur, "data", "bariatric_longitudinal.csv")
      nested <- file.path(cur, "binge-eating-bariatric-analysis", "data", "bariatric_longitudinal.csv")
      if (file.exists(direct)) return(normalizePath(cur, winslash = "/"))
      if (file.exists(nested)) {
        return(normalizePath(file.path(cur, "binge-eating-bariatric-analysis"), winslash = "/"))
      }
    }
  }
  stop(
    "Cannot locate binge-eating-bariatric-analysis/data/bariatric_longitudinal.csv. ",
    "Open the binge-eating-bariatric-analysis RStudio Project and rerun.",
    call. = FALSE
  )
}

MODULE05_ROOT <- module05_find_root()
MODULE05_DATA_FILE <- file.path(MODULE05_ROOT, "data", "bariatric_longitudinal.csv")
MODULE05_OUTPUT_ROOT <- file.path(MODULE05_ROOT, "outputs", "05_immune_cell_analysis")
MODULE05_TABLE_DIR <- file.path(MODULE05_OUTPUT_ROOT, "table")
MODULE05_FIGURE_ROOT <- file.path(MODULE05_OUTPUT_ROOT, "figure")
MODULE05_FIG3_DIR <- file.path(MODULE05_FIGURE_ROOT, "Fig3_forest")
MODULE05_FIGS4_DIR <- file.path(MODULE05_FIGURE_ROOT, "FigS4_baseline_scatter")
MODULE05_FIGS5_DIR <- file.path(MODULE05_FIGURE_ROOT, "FigS5_current_value_paths")
MODULE05_FIGS6_DIR <- file.path(MODULE05_FIGURE_ROOT, "FigS6_change_from_baseline_paths")
MODULE05_INTERMEDIATE_DIR <- file.path(MODULE05_OUTPUT_ROOT, "intermediate")
MODULE05_MODEL_LOG_FILE <- file.path(
  MODULE05_INTERMEDIATE_DIR, "_immune_cell_analysis_model_run_log.csv"
)
MODULE05_PLOT_DATA_DIR <- file.path(MODULE05_ROOT, "outputs", "06_final_figures", "_plot_data")
MODULE05_STYLE_FILE <- file.path(
  MODULE05_ROOT, "code", "06_figure_assembly", "00_figure_style_manifest.csv"
)

module05_check_environment <- function() {
  missing <- MODULE05_REQUIRED_PACKAGES[
    !vapply(MODULE05_REQUIRED_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop(
      "Missing locked R package(s): ", paste(missing, collapse = ", "),
      ". Open the binge-eating-bariatric-analysis project and run renv::restore().",
      call. = FALSE
    )
  }

  skip_versions <- identical(Sys.getenv("MODULE05_SKIP_VERSION_CHECK"), "1")
  if (!skip_versions && as.character(getRversion()) != MODULE05_EXPECTED_R_VERSION) {
    stop(
      "Wrong R version: expected ", MODULE05_EXPECTED_R_VERSION,
      ", found ", as.character(getRversion()), ".",
      call. = FALSE
    )
  }
  if (!skip_versions) {
    for (pkg in names(MODULE05_EXPECTED_PACKAGE_VERSIONS)) {
      expected <- unname(MODULE05_EXPECTED_PACKAGE_VERSIONS[[pkg]])
      actual <- as.character(utils::packageVersion(pkg))
      if (utils::compareVersion(actual, expected) != 0L) {
        stop(
          "Package version mismatch for ", pkg, ": expected ", expected,
          ", found ", actual, ". Run renv::restore().",
          call. = FALSE
        )
      }
    }
  }
  invisible(TRUE)
}

module05_load_style <- function() {
  style <- MODULE05_DEFAULT_STYLE
  if (!file.exists(MODULE05_STYLE_FILE)) {
    message("[INFO] shared style manifest not found; using frozen module 05 defaults")
    return(style)
  }
  manifest <- utils::read.csv(
    MODULE05_STYLE_FILE, check.names = FALSE, stringsAsFactors = FALSE
  )
  lookup <- setNames(names(manifest), tolower(trimws(names(manifest))))
  if (!all(c("key", "value") %in% names(lookup))) {
    stop("Style manifest must contain Key and Value columns.", call. = FALSE)
  }
  values <- setNames(
    manifest[[lookup[["value"]]]],
    trimws(as.character(manifest[[lookup[["key"]]]]))
  )
  numeric_keys <- c(
    "png_dpi", "single_axis_title_size_pt", "single_tick_size_pt",
    "single_legend_size_pt", "line_main_single_width_pt"
  )
  for (key in names(style)) {
    if (!key %in% names(values) || is.na(values[[key]]) ||
        !nzchar(trimws(as.character(values[[key]])))) next
    style[[key]] <- if (key %in% numeric_keys) {
      as.numeric(values[[key]])
    } else {
      as.character(values[[key]])
    }
  }
  style
}

module05_require_font <- function(font_family) {
  system_name <- Sys.info()[["sysname"]]
  found <- FALSE
  if (identical(system_name, "Darwin")) {
    roots <- c("/System/Library/Fonts", "/Library/Fonts", path.expand("~/Library/Fonts"))
    files <- unlist(lapply(roots[dir.exists(roots)], function(root) {
      list.files(
        root, pattern = "\\.(ttf|ttc|otf)$", recursive = TRUE,
        full.names = TRUE, ignore.case = TRUE
      )
    }), use.names = FALSE)
    found <- any(grepl("arial", basename(files), ignore.case = TRUE))
  } else if (identical(system_name, "Windows")) {
    files <- list.files(file.path(Sys.getenv("WINDIR", "C:/Windows"), "Fonts"), full.names = TRUE)
    found <- any(grepl("^arial", basename(files), ignore.case = TRUE))
  } else {
    fc_match <- Sys.which("fc-match")
    if (nzchar(fc_match)) {
      match <- suppressWarnings(system2(
        fc_match, c("-f", "%{family}", shQuote(font_family)),
        stdout = TRUE, stderr = FALSE
      ))
      found <- length(match) > 0L && any(grepl(font_family, match, fixed = TRUE, ignore.case = TRUE))
    }
  }
  if (!found) {
    stop("Required font '", font_family, "' is unavailable.", call. = FALSE)
  }
  message("[PASS] required font available: ", font_family)
  invisible(TRUE)
}

module05_cleanup_legacy_outputs <- function() {
  legacy <- c(
    file.path(MODULE05_OUTPUT_ROOT, "figures_main_forest"),
    file.path(MODULE05_OUTPUT_ROOT, "figures_raw_baseline"),
    file.path(MODULE05_OUTPUT_ROOT, "figures_raw_current_paths"),
    file.path(MODULE05_OUTPUT_ROOT, "figures_raw_change_paths"),
    file.path(MODULE05_OUTPUT_ROOT, "tables"),
    file.path(MODULE05_OUTPUT_ROOT, "_audit"),
    file.path(MODULE05_OUTPUT_ROOT, "_model_run_log.csv"),
    file.path(MODULE05_OUTPUT_ROOT, "_package_versions.csv")
  )
  existing <- legacy[file.exists(legacy)]
  if (length(existing)) unlink(existing, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

# Frozen input validation and analysis data preparation
module05_as_numeric_strict <- function(x, role) {
  if (is.numeric(x)) return(as.numeric(x))
  chr <- trimws(as.character(x))
  chr[chr %in% c("", "NA", "N/A", ".", "-")] <- NA_character_
  chr <- gsub(",", "", chr, fixed = TRUE)
  out <- suppressWarnings(as.numeric(chr))
  bad <- !is.na(chr) & is.na(out)
  if (any(bad)) {
    stop(
      "Non-numeric value(s) found in ", role, ": ",
      paste(utils::head(unique(chr[bad]), 5L), collapse = ", "),
      call. = FALSE
    )
  }
  out
}

module05_standardize_sex <- function(x) {
  raw <- trimws(as.character(x))
  norm <- gsub("[^a-z0-9]+", "", tolower(raw))
  out <- rep(NA_character_, length(norm))
  out[norm %in% c("female", "f", "0", "2")] <- "Female"
  out[norm %in% c("male", "m", "1")] <- "Male"
  if (anyNA(out)) {
    stop("Unrecognized sex value(s): ", paste(unique(raw[is.na(out)]), collapse = ", "), call. = FALSE)
  }
  factor(out, levels = c("Female", "Male"))
}

module05_standardize_group <- function(x) {
  raw <- trimws(as.character(x))
  norm <- gsub("[^a-z0-9]+", "", tolower(raw))
  out <- rep(NA_character_, length(norm))
  out[norm %in% c("nonbe", "nonbingeeating", "control", "no", "0")] <- "Non-BE"
  out[norm %in% c("be", "bingeeating", "case", "yes", "1")] <- "BE"
  if (all(unique(norm[!is.na(norm)]) %in% c("1", "2"))) {
    out[norm == "1"] <- "Non-BE"
    out[norm == "2"] <- "BE"
  }
  if (anyNA(out)) {
    stop("Unrecognized BES-group value(s): ", paste(unique(raw[is.na(out)]), collapse = ", "), call. = FALSE)
  }
  factor(out, levels = c("Non-BE", "BE"))
}

module05_assert_patient_constant <- function(values, ids, role, tolerance = 1e-10) {
  split_values <- split(values, ids)
  bad <- vapply(split_values, function(z) {
    z <- z[!is.na(z)]
    if (!length(z)) return(TRUE)
    if (is.numeric(z)) return(max(z) - min(z) > tolerance)
    length(unique(as.character(z))) > 1L
  }, logical(1))
  if (any(bad)) {
    stop(role, " is not constant within participant for ID(s): ", paste(utils::head(names(bad)[bad], 10L), collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

module05_load_data <- function() {
  raw <- readr::read_csv(MODULE05_DATA_FILE, show_col_types = FALSE, guess_max = 100000L)
  missing <- setdiff(MODULE05_REQUIRED_SOURCE_COLUMNS, names(raw))
  if (length(missing)) {
    stop("Required standardized column(s) missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (ncol(raw) != MODULE05_CONFIG$expected_columns) {
    stop("Expected ", MODULE05_CONFIG$expected_columns, " columns; found ", ncol(raw), ".", call. = FALSE)
  }

  dat <- data.frame(
    id = trimws(as.character(raw$participant_id)),
    sex = module05_standardize_sex(raw$sex),
    age = module05_as_numeric_strict(raw$age_years, "age_years"),
    group = module05_standardize_group(raw$bes_group),
    visit_month = as.integer(module05_as_numeric_strict(raw$visit_month, "visit_month")),
    baseline_weight = module05_as_numeric_strict(raw$baseline_weight_kg, "baseline_weight_kg"),
    baseline_bmi = module05_as_numeric_strict(raw$baseline_bmi_kg_m2, "baseline_bmi_kg_m2"),
    weight = module05_as_numeric_strict(raw$weight_kg, "weight_kg"),
    bmi = module05_as_numeric_strict(raw$bmi_kg_m2, "bmi_kg_m2"),
    stringsAsFactors = FALSE
  )

  for (spec in MODULE05_IMMUNE) {
    dat[[spec$key]] <- module05_as_numeric_strict(raw[[spec$source_column]], spec$source_column)
  }

  if (nrow(dat) != MODULE05_CONFIG$expected_rows) {
    stop("Expected ", MODULE05_CONFIG$expected_rows, " rows; found ", nrow(dat), ".", call. = FALSE)
  }
  if (anyNA(dat$id) || any(!nzchar(dat$id))) stop("Missing participant ID detected.", call. = FALSE)
  if (length(unique(dat$id)) != MODULE05_CONFIG$expected_subjects) {
    stop("Expected ", MODULE05_CONFIG$expected_subjects, " participants; found ", length(unique(dat$id)), ".", call. = FALSE)
  }
  if (!setequal(unique(dat$visit_month), MODULE05_CONFIG$visit_values)) {
    stop("Unexpected visit_month values: ", paste(sort(unique(dat$visit_month)), collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(dat[c("id", "visit_month")])) {
    stop("Duplicate participant-by-visit rows detected.", call. = FALSE)
  }

  visit_grid <- table(dat$id, factor(dat$visit_month, levels = MODULE05_CONFIG$visit_values))
  if (any(visit_grid != 1L)) stop("Each participant must have exactly one row for M0, M1, M3 and M6.", call. = FALSE)

  module05_assert_patient_constant(dat$age, dat$id, "Age")
  module05_assert_patient_constant(as.character(dat$sex), dat$id, "Sex")
  module05_assert_patient_constant(as.character(dat$group), dat$id, "BES group")
  module05_assert_patient_constant(dat$baseline_weight, dat$id, "Baseline weight")
  module05_assert_patient_constant(dat$baseline_bmi, dat$id, "Baseline BMI")

  baseline <- dat[dat$visit_month == 0L, , drop = FALSE]
  group_counts <- table(baseline$group)
  expected_counts <- MODULE05_CONFIG$expected_group_counts
  if (!identical(as.integer(group_counts[names(expected_counts)]), as.integer(expected_counts))) {
    stop(
      "Unexpected baseline BES-group counts: ",
      paste(names(group_counts), as.integer(group_counts), collapse = "; "),
      call. = FALSE
    )
  }
  if (any(abs(baseline$weight - baseline$baseline_weight) > 1e-8, na.rm = TRUE)) {
    stop("M0 weight does not match baseline_weight_kg.", call. = FALSE)
  }
  if (any(abs(baseline$bmi - baseline$baseline_bmi) > 1e-8, na.rm = TRUE)) {
    stop("M0 BMI does not match baseline_bmi_kg_m2.", call. = FALSE)
  }
  if (any(dat$weight <= 0, na.rm = TRUE) || any(dat$bmi <= 0, na.rm = TRUE)) {
    stop("Non-positive Weight/BMI value detected.", call. = FALSE)
  }
  for (spec in MODULE05_IMMUNE) {
    if (any(dat[[spec$key]] < 0, na.rm = TRUE)) {
      stop("Negative immune-cell count detected for ", spec$label, ".", call. = FALSE)
    }
    if (anyNA(baseline[[spec$key]])) {
      stop("Baseline immune-cell value is missing for ", spec$label, "; the frozen standardization requires M0 values.", call. = FALSE)
    }
  }

  dat$visit <- factor(
    paste0("M", dat$visit_month),
    levels = MODULE05_CONFIG$visit_levels,
    ordered = FALSE
  )
  dat <- dat[order(dat$id, dat$visit_month), , drop = FALSE]
  rownames(dat) <- NULL
  dat
}

module05_get_immune <- function(key) {
  if (!key %in% names(MODULE05_IMMUNE)) stop("Unknown immune outcome: ", key, call. = FALSE)
  MODULE05_IMMUNE[[key]]
}

module05_get_adiposity <- function(key) {
  if (!key %in% names(MODULE05_ADIPOSITY)) stop("Unknown adiposity exposure: ", key, call. = FALSE)
  MODULE05_ADIPOSITY[[key]]
}

module05_baseline_sd <- function(dat, outcome_key) {
  z <- dat[[outcome_key]][dat$visit_month == 0L]
  out <- stats::sd(z, na.rm = TRUE)
  if (!is.finite(out) || out <= 0) stop("Invalid baseline SD for ", outcome_key, ".", call. = FALSE)
  out
}

module05_prepare_baseline <- function(dat, outcome_key, exposure_key) {
  x <- dat[dat$visit_month == 0L, c("id", "age", "sex", "group", outcome_key, exposure_key), drop = FALSE]
  names(x)[names(x) == outcome_key] <- "outcome_value"
  names(x)[names(x) == exposure_key] <- "adiposity"
  x <- x[stats::complete.cases(x[c("outcome_value", "adiposity", "age", "sex")]), , drop = FALSE]
  x$id <- factor(x$id)
  x$sex <- factor(x$sex, levels = c("Female", "Male"))
  x$group <- factor(x$group, levels = c("Non-BE", "BE"))
  if (nrow(x) < 20L) stop("Too few complete baseline records for ", outcome_key, " ~ ", exposure_key, ".", call. = FALSE)
  x
}

module05_prepare_cumulative <- function(dat, outcome_key, exposure_key) {
  baseline <- dat[dat$visit_month == 0L, c("id", outcome_key, exposure_key), drop = FALSE]
  names(baseline) <- c("id", "baseline_outcome", "baseline_adiposity")
  post <- dat[dat$visit_month %in% c(1L, 3L, 6L), c("id", "visit_month", "visit", "age", "sex", "group", outcome_key, exposure_key), drop = FALSE]
  names(post)[names(post) == outcome_key] <- "outcome_value"
  names(post)[names(post) == exposure_key] <- "adiposity_value"
  post$baseline_outcome <- baseline$baseline_outcome[match(post$id, baseline$id)]
  post$baseline_adiposity <- baseline$baseline_adiposity[match(post$id, baseline$id)]
  post$delta_adiposity <- post$adiposity_value - post$baseline_adiposity
  keep <- stats::complete.cases(post[c(
    "outcome_value", "delta_adiposity", "baseline_adiposity",
    "baseline_outcome", "age", "sex", "visit"
  )])
  post <- post[keep, , drop = FALSE]
  post$id <- factor(post$id)
  post$visit <- factor(as.character(post$visit), levels = MODULE05_CONFIG$post_visit_levels)
  post$sex <- factor(post$sex, levels = c("Female", "Male"))
  post$group <- factor(post$group, levels = c("Non-BE", "BE"))
  post <- post[order(post$id, post$visit_month), , drop = FALSE]
  rownames(post) <- NULL
  if (nrow(post) < 20L || length(unique(post$id)) < 10L) {
    stop("Too few postoperative records for ", outcome_key, " ~ change in ", exposure_key, ".", call. = FALSE)
  }
  post
}

module05_prepare_adjacent <- function(dat, outcome_key, exposure_key) {
  interval_def <- data.frame(
    interval = MODULE05_CONFIG$interval_levels,
    start_month = c(0L, 1L, 3L),
    end_month = c(1L, 3L, 6L),
    stringsAsFactors = FALSE
  )
  pieces <- vector("list", nrow(interval_def))
  for (i in seq_len(nrow(interval_def))) {
    start <- dat[dat$visit_month == interval_def$start_month[i], c("id", "age", "sex", "group", outcome_key, exposure_key), drop = FALSE]
    end <- dat[dat$visit_month == interval_def$end_month[i], c("id", outcome_key, exposure_key), drop = FALSE]
    names(start)[names(start) == outcome_key] <- "start_outcome"
    names(start)[names(start) == exposure_key] <- "start_adiposity"
    names(end)[names(end) == outcome_key] <- "end_outcome"
    names(end)[names(end) == exposure_key] <- "end_adiposity"
    z <- merge(start, end, by = "id", all = FALSE, sort = FALSE)
    z$interval <- interval_def$interval[i]
    z$delta_outcome <- z$end_outcome - z$start_outcome
    z$delta_adiposity <- z$end_adiposity - z$start_adiposity
    z <- z[stats::complete.cases(z[c(
      "delta_outcome", "delta_adiposity", "start_outcome",
      "start_adiposity", "age", "sex"
    )]), , drop = FALSE]
    pieces[[i]] <- z
  }
  out <- do.call(rbind, pieces)
  out$interval <- factor(out$interval, levels = MODULE05_CONFIG$interval_levels)
  out$id <- factor(out$id)
  out$sex <- factor(out$sex, levels = c("Female", "Male"))
  out$group <- factor(out$group, levels = c("Non-BE", "BE"))
  out <- out[order(out$id, out$interval), , drop = FALSE]
  rownames(out) <- NULL
  if (nrow(out) < 20L || length(unique(out$id)) < 10L) {
    stop("Too few adjacent-interval records for ", outcome_key, " ~ ", exposure_key, ".", call. = FALSE)
  }
  out
}

module05_prepare_baseline_raw <- function(dat, outcome_key, exposure_key) {
  x <- dat[dat$visit_month == 0L, c("id", outcome_key, exposure_key), drop = FALSE]
  names(x)[names(x) == outcome_key] <- "y"
  names(x)[names(x) == exposure_key] <- "x"
  x[stats::complete.cases(x[c("x", "y")]), , drop = FALSE]
}

module05_prepare_current_paths <- function(dat, outcome_key, exposure_key) {
  columns <- c("id", "visit_month", "visit", outcome_key, exposure_key)
  post <- dat[dat$visit_month %in% c(1L, 3L, 6L), columns, drop = FALSE]
  names(post)[names(post) == outcome_key] <- "y"
  names(post)[names(post) == exposure_key] <- "x"
  post <- post[stats::complete.cases(post[c("x", "y", "visit")]), , drop = FALSE]

  baseline <- dat[
    dat$visit_month == 0L & dat$id %in% unique(post$id),
    columns,
    drop = FALSE
  ]
  names(baseline)[names(baseline) == outcome_key] <- "y"
  names(baseline)[names(baseline) == exposure_key] <- "x"
  baseline <- baseline[stats::complete.cases(baseline[c("x", "y", "visit")]), , drop = FALSE]

  x <- rbind(baseline, post)
  x$visit <- ifelse(x$visit_month == 0L, "Baseline", as.character(x$visit))
  x$visit <- factor(x$visit, levels = MODULE05_CONFIG$current_visit_levels)
  x$id <- factor(x$id)
  x <- x[order(x$id, x$visit_month), , drop = FALSE]
  rownames(x) <- NULL
  x
}

module05_prepare_change_paths <- function(dat, outcome_key, exposure_key) {
  x <- module05_prepare_cumulative(dat, outcome_key, exposure_key)
  out <- data.frame(
    id = x$id,
    visit_month = x$visit_month,
    visit = x$visit,
    x = x$delta_adiposity,
    y = x$outcome_value - x$baseline_outcome,
    stringsAsFactors = FALSE
  )
  out$id <- factor(out$id)
  out$visit <- factor(as.character(out$visit), levels = MODULE05_CONFIG$post_visit_levels)
  out <- out[order(out$id, out$visit_month), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Frozen model-fitting and estimand extraction functions
module05_check_fixed_rank <- function(fixed_formula, data) {
  mm <- stats::model.matrix(fixed_formula, data = data)
  qr_mm <- qr(mm)
  if (qr_mm$rank < ncol(mm)) {
    stop(
      "Fixed-effect design matrix is rank deficient: rank ", qr_mm$rank,
      " of ", ncol(mm), ". Formula: ", paste(deparse(fixed_formula), collapse = " "),
      call. = FALSE
    )
  }
  list(rank = qr_mm$rank, columns = ncol(mm))
}

module05_cov_formula <- function(fixed_formula, covariance, time_var, id_var = "id") {
  lhs <- paste(deparse(fixed_formula[[2L]], width.cutoff = 500L), collapse = " ")
  rhs <- paste(deparse(fixed_formula[[3L]], width.cutoff = 500L), collapse = " ")
  stats::as.formula(paste(lhs, "~", rhs, "+", covariance, "(", time_var, "|", id_var, ")"))
}

module05_capture_warnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) structure(list(error = conditionMessage(e)), class = "module05_fit_error")),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = unique(warnings))
}

module05_convergence_value <- function(fit) {
  if ("component" %in% getNamespaceExports("mmrm")) {
    out <- try(mmrm::component(fit, "convergence"), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
  }
  NA
}

module05_convergence_ok <- function(x) {
  if (length(x) == 1L && is.na(x)) return(TRUE)
  if (is.logical(x)) return(all(x))
  if (is.numeric(x)) return(all(is.finite(x)) && all(x == 0))
  if (is.list(x)) {
    candidate <- unlist(x[c("code", "convergence", "status")], recursive = TRUE, use.names = FALSE)
    if (length(candidate)) return(module05_convergence_ok(candidate))
    return(TRUE)
  }
  txt <- tolower(paste(x, collapse = " "))
  !grepl("fail|not converg|error", txt)
}

module05_fit_one_covariance <- function(data, fixed_formula, covariance, time_var, reml = TRUE) {
  rank_info <- module05_check_fixed_rank(fixed_formula, data)
  full_formula <- module05_cov_formula(fixed_formula, covariance, time_var)
  control <- mmrm::mmrm_control(method = MODULE05_CONFIG$df_method)
  captured <- module05_capture_warnings(
    mmrm::mmrm(
      formula = full_formula,
      data = data,
      reml = reml,
      control = control
    )
  )
  if (inherits(captured$value, "module05_fit_error")) {
    return(list(ok = FALSE, error = captured$value$error, warnings = captured$warnings, rank_info = rank_info))
  }
  fit <- captured$value
  beta <- try(stats::coef(fit), silent = TRUE)
  vbeta <- try(stats::vcov(fit), silent = TRUE)
  if (inherits(beta, "try-error") || inherits(vbeta, "try-error") ||
      any(!is.finite(beta)) || any(!is.finite(vbeta))) {
    return(list(
      ok = FALSE,
      error = "Non-finite fixed-effect estimate or covariance matrix.",
      warnings = captured$warnings,
      rank_info = rank_info
    ))
  }
  conv <- module05_convergence_value(fit)
  if (!module05_convergence_ok(conv)) {
    return(list(
      ok = FALSE,
      error = paste("Optimizer convergence status:", paste(conv, collapse = " ")),
      warnings = captured$warnings,
      rank_info = rank_info
    ))
  }
  bad_warning <- any(grepl("failed|not converg|non-positive|not positive|singular", captured$warnings, ignore.case = TRUE))
  if (bad_warning) {
    return(list(
      ok = FALSE,
      error = paste(captured$warnings, collapse = " | "),
      warnings = captured$warnings,
      rank_info = rank_info
    ))
  }
  list(ok = TRUE, fit = fit, warnings = captured$warnings, convergence = conv, rank_info = rank_info)
}

module05_fit_with_fallback <- function(data, fixed_formula, time_var, reml = TRUE) {
  first <- module05_fit_one_covariance(
    data, fixed_formula, MODULE05_CONFIG$preferred_covariance, time_var, reml = reml
  )
  if (isTRUE(first$ok)) {
    return(list(
      fit = first$fit,
      covariance = "UN",
      fallback = FALSE,
      warnings = first$warnings,
      convergence = first$convergence,
      first_error = NA_character_,
      rank_info = first$rank_info
    ))
  }
  second <- module05_fit_one_covariance(
    data, fixed_formula, MODULE05_CONFIG$fallback_covariance, time_var, reml = reml
  )
  if (!isTRUE(second$ok)) {
    stop(
      "Both covariance models failed. UN: ", first$error,
      " | AR(1): ", second$error,
      call. = FALSE
    )
  }
  list(
    fit = second$fit,
    covariance = "AR(1)",
    fallback = TRUE,
    warnings = unique(c(first$warnings, second$warnings)),
    convergence = second$convergence,
    first_error = first$error,
    rank_info = second$rank_info
  )
}

module05_find_p_column <- function(x) {
  candidates <- intersect(c("p.value", "p.value."), names(x))
  if (!length(candidates)) candidates <- grep("^p\\.?value", names(x), value = TRUE)
  if (!length(candidates)) stop("Cannot find p-value column.", call. = FALSE)
  candidates[1L]
}

module05_find_trend_column <- function(x) {
  candidates <- grep("trend$", names(x), value = TRUE)
  if (length(candidates) != 1L) {
    stop("Expected exactly one emtrends estimate column; found: ", paste(candidates, collapse = ", "), call. = FALSE)
  }
  candidates[1L]
}

module05_find_ci_columns <- function(x) {
  lower <- intersect(c("lower.CL", "asymp.LCL", "lower.HPD"), names(x))
  upper <- intersect(c("upper.CL", "asymp.UCL", "upper.HPD"), names(x))
  if (!length(lower) || !length(upper)) stop("Cannot find confidence-limit columns in emmeans output.", call. = FALSE)
  c(lower = lower[1L], upper = upper[1L])
}

module05_emtrends_summary <- function(fit, specs, var = "delta_adiposity") {
  object <- emmeans::emtrends(fit, specs = specs, var = var)
  sm <- as.data.frame(summary(object, infer = c(TRUE, TRUE), adjust = "none"))
  trend_col <- module05_find_trend_column(sm)
  p_col <- module05_find_p_column(sm)
  ci_cols <- module05_find_ci_columns(sm)
  df_col <- if ("df" %in% names(sm)) "df" else NA_character_
  out <- sm
  out$estimate <- as.numeric(out[[trend_col]])
  out$se <- as.numeric(out$SE)
  out$df_value <- if (!is.na(df_col)) as.numeric(out[[df_col]]) else NA_real_
  out$lcl <- as.numeric(out[[ci_cols["lower"]]])
  out$ucl <- as.numeric(out[[ci_cols["upper"]]])
  out$p <- as.numeric(out[[p_col]])
  list(object = object, table = out)
}

module05_joint_slope_heterogeneity_p <- function(trend_object, labels) {
  if (length(labels) != 3L) stop("The frozen heterogeneity test requires exactly three slopes.", call. = FALSE)
  ct <- emmeans::contrast(
    trend_object,
    method = list(
      second_minus_first = c(-1, 1, 0),
      third_minus_first = c(-1, 0, 1)
    ),
    adjust = "none"
  )
  jt <- as.data.frame(emmeans::test(ct, joint = TRUE))
  p_col <- module05_find_p_column(jt)
  p <- as.numeric(jt[[p_col]][1L])
  if (!is.finite(p)) stop("Non-finite joint slope-heterogeneity p-value.", call. = FALSE)
  p
}

module05_scale_effect <- function(estimate, lcl, ucl, unit_scale, baseline_sd) {
  data.frame(
    estimate_sd = estimate * unit_scale / baseline_sd,
    lcl_sd = lcl * unit_scale / baseline_sd,
    ucl_sd = ucl * unit_scale / baseline_sd,
    stringsAsFactors = FALSE
  )
}

module05_fit_baseline_ols <- function(data) {
  fixed_formula <- outcome_value ~ adiposity + age + sex
  rank_info <- module05_check_fixed_rank(fixed_formula, data)
  fit <- stats::lm(fixed_formula, data = data)
  coef_table <- summary(fit)$coefficients
  if (!"adiposity" %in% rownames(coef_table)) stop("Baseline OLS did not return the adiposity coefficient.", call. = FALSE)
  ci <- stats::confint(fit, parm = "adiposity", level = 0.95)
  list(
    fit = fit,
    estimate = unname(coef_table["adiposity", "Estimate"]),
    se = unname(coef_table["adiposity", "Std. Error"]),
    df = unname(fit$df.residual),
    lcl = unname(ci[1L]),
    ucl = unname(ci[2L]),
    p = unname(coef_table["adiposity", "Pr(>|t|)"]),
    rank_info = rank_info
  )
}


# Article-output and model-log helpers
module05_model_log_row <- function(
  outcome_label, exposure_label, analysis_type, model_label,
  data, fit_result, fixed_formula, time_var
) {
  rank_info <- module05_check_fixed_rank(fixed_formula, data)
  data.frame(
    Outcome = outcome_label,
    Exposure = exposure_label,
    analysis_type = analysis_type,
    Model = model_label,
    N_subjects = length(unique(data$id)),
    N_observations = nrow(data),
    Repeated_factor = time_var,
    Covariance = fit_result$covariance,
    Fallback_used = if (isTRUE(fit_result$fallback)) "Yes" else "No",
    Fixed_design_rank = rank_info$rank,
    Fixed_design_columns = rank_info$columns,
    Convergence = "OK",
    REML = "Yes",
    DF_method = MODULE05_CONFIG$df_method,
    First_UN_error = ifelse(is.na(fit_result$first_error), "None", fit_result$first_error),
    Warnings = if (length(fit_result$warnings)) {
      paste(fit_result$warnings, collapse = " | ")
    } else {
      "None"
    },
    stringsAsFactors = FALSE
  )
}

module05_ols_log_row <- function(
  outcome_label, exposure_label, data, fit_result
) {
  data.frame(
    Outcome = outcome_label,
    Exposure = exposure_label,
    analysis_type = "baseline",
    Model = "Baseline age/sex-adjusted OLS",
    N_subjects = length(unique(data$id)),
    N_observations = nrow(data),
    Repeated_factor = "None",
    Covariance = "Independent OLS",
    Fallback_used = "No",
    Fixed_design_rank = fit_result$rank_info$rank,
    Fixed_design_columns = fit_result$rank_info$columns,
    Convergence = "OK",
    REML = "Not applicable",
    DF_method = "Residual t",
    First_UN_error = "None",
    Warnings = "None",
    stringsAsFactors = FALSE
  )
}

module05_log_columns <- function() {
  c(
    "Outcome", "Exposure", "analysis_type", "Model", "N_subjects",
    "N_observations", "Repeated_factor", "Covariance", "Fallback_used",
    "Fixed_design_rank", "Fixed_design_columns", "Convergence", "REML",
    "DF_method", "First_UN_error", "Warnings"
  )
}

module05_update_model_log <- function(rows, replace_analysis_types) {
  required <- module05_log_columns()
  if (!identical(names(rows), required)) {
    stop("Model-log columns do not match the frozen schema.", call. = FALSE)
  }
  existing <- data.frame()
  if (file.exists(MODULE05_MODEL_LOG_FILE)) {
    candidate <- utils::read.csv(
      MODULE05_MODEL_LOG_FILE, check.names = FALSE,
      stringsAsFactors = FALSE, na.strings = character()
    )
    if (identical(names(candidate), required)) existing <- candidate
  }
  if (nrow(existing)) {
    existing <- existing[!existing$analysis_type %in% replace_analysis_types, , drop = FALSE]
  }
  combined <- rbind(existing, rows)
  analysis_order <- c("baseline", "overall change", "visit-specific", "adjacent interval")
  exposure_order <- c("BMI", "Weight")
  outcome_order <- module05_forest_order()
  combined$analysis_type <- factor(combined$analysis_type, levels = analysis_order)
  combined$Exposure <- factor(combined$Exposure, levels = exposure_order)
  combined$Outcome <- factor(combined$Outcome, levels = outcome_order)
  combined <- combined[order(
    combined$analysis_type, combined$Exposure, combined$Outcome
  ), , drop = FALSE]
  combined$analysis_type <- as.character(combined$analysis_type)
  combined$Exposure <- as.character(combined$Exposure)
  combined$Outcome <- as.character(combined$Outcome)
  rownames(combined) <- NULL
  module05_write_csv(combined, MODULE05_MODEL_LOG_FILE)
  invisible(combined)
}

module05_write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  invisible(path)
}

module05_format_p <- function(p, digits = 3L) {
  ifelse(
    is.na(p),
    "",
    ifelse(
      p < 10^(-digits),
      paste0("<", formatC(10^(-digits), format = "f", digits = digits)),
      formatC(p, format = "f", digits = digits)
    )
  )
}

module05_format_effect_ci <- function(est, lcl, ucl) {
  clean_zero <- function(x) ifelse(abs(x) < 0.005, 0, x)
  sprintf(
    "%.2f (%.2f to %.2f)",
    clean_zero(est), clean_zero(lcl), clean_zero(ucl)
  )
}

module05_forest_order <- function() {
  vapply(MODULE05_IMMUNE, `[[`, character(1), "label")
}

module05_adiposity_axis_label <- function(exposure, delta = FALSE) {
  prefix <- if (delta) paste0("Δ", exposure$label) else exposure$label
  paste0(prefix, " (", exposure$unit, ")")
}

module05_immune_axis_label <- function(outcome, delta = FALSE) {
  prefix <- if (delta) paste0("Δ", outcome$label) else outcome$label
  bquote(paste(.(prefix), " (", "×", 10^9, "/L)"))
}

# Standalone figure helpers
module05_theme_sci <- function(style, show_legend = FALSE) {
  ggplot2::theme_classic(
    base_size = as.numeric(style$single_tick_size_pt),
    base_family = as.character(style$font_family)
  ) +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        colour = "black", linewidth = 0.27, fill = NA
      ),
      axis.title.x = ggplot2::element_text(
        size = as.numeric(style$single_axis_title_size_pt),
        margin = ggplot2::margin(t = 5)
      ),
      axis.title.y = ggplot2::element_text(
        size = as.numeric(style$single_axis_title_size_pt),
        margin = ggplot2::margin(r = 5)
      ),
      axis.text = ggplot2::element_text(
        size = as.numeric(style$single_tick_size_pt), colour = "black"
      ),
      axis.ticks = ggplot2::element_line(linewidth = 0.27, colour = "black"),
      axis.ticks.length = grid::unit(1.7, "mm"),
      legend.position = if (show_legend) "top" else "none",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        size = as.numeric(style$single_legend_size_pt)
      ),
      legend.key.height = grid::unit(4, "mm"),
      legend.key.width = grid::unit(5, "mm"),
      legend.spacing.x = grid::unit(1.5, "mm"),
      legend.margin = ggplot2::margin(0, 0, 2, 0),
      plot.title = ggplot2::element_text(
        size = 10, hjust = 0.5, face = "plain",
        margin = ggplot2::margin(b = 6)
      ),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 9, face = "plain"),
      panel.spacing = grid::unit(2.0, "mm"),
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    )
}

module05_make_forest <- function(
  df, title_text, x_limits, style, facet_column = NULL
) {
  needed <- c("outcome", "estimate", "lower", "upper")
  missing <- setdiff(needed, names(df))
  if (length(missing)) {
    stop("Forest data missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (any(!is.finite(df$estimate)) || any(!is.finite(df$lower)) ||
      any(!is.finite(df$upper))) {
    stop("Non-finite forest estimate or confidence limit.", call. = FALSE)
  }
  df$outcome <- factor(df$outcome, levels = rev(module05_forest_order()))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = estimate, y = outcome)) +
    ggplot2::geom_vline(
      xintercept = 0, linetype = "dashed",
      colour = as.character(style$reference_line_color), linewidth = 0.22
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = lower, xend = upper, yend = outcome),
      linewidth = 0.35, colour = "black"
    ) +
    ggplot2::geom_point(
      shape = 16, size = 1.60, colour = "black"
    ) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::coord_cartesian(xlim = x_limits, clip = "off") +
    ggplot2::labs(
      x = "Standardized β (95% CI)", y = NULL, title = title_text
    ) +
    module05_theme_sci(style)
  if (!is.null(facet_column)) {
    if (!facet_column %in% names(df)) {
      stop("Facet column not found: ", facet_column, call. = FALSE)
    }
    p <- p + ggplot2::facet_grid(
      stats::as.formula(paste(". ~", facet_column)), drop = FALSE
    )
  }
  p
}

module05_make_baseline_raw_plot <- function(
  data, exposure, outcome, style, x_limits = NULL, y_limits = NULL
) {
  p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(colour = "grey30", size = 0.95, alpha = 0.78) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.03, 0.03))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.04, 0.04))) +
    ggplot2::labs(
      x = module05_adiposity_axis_label(exposure),
      y = module05_immune_axis_label(outcome),
      title = paste0("Baseline: ", outcome$label, " versus ", exposure$label)
    ) +
    module05_theme_sci(style)
  if (!is.null(x_limits) || !is.null(y_limits)) {
    p <- p + ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits, clip = "off")
  }
  p
}

module05_path_line_data <- function(data) {
  counts <- table(data$participant_order)
  keep <- names(counts)[counts >= 2L]
  data[as.character(data$participant_order) %in% keep, , drop = FALSE]
}

module05_make_current_path_plot <- function(
  data, exposure, outcome, style, x_limits = NULL, y_limits = NULL
) {
  line_data <- module05_path_line_data(data)
  p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_path(
      data = line_data,
      ggplot2::aes(group = participant_order),
      colour = "grey82", linewidth = 0.18, alpha = 1, na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = visit), colour = "grey25",
      size = 0.95, alpha = 0.86, stroke = 0.30
    ) +
    ggplot2::scale_shape_manual(
      values = MODULE05_CONFIG$current_visit_shapes,
      breaks = MODULE05_CONFIG$current_visit_levels
    ) +
    ggplot2::guides(
      shape = ggplot2::guide_legend(
        nrow = 1, byrow = TRUE,
        override.aes = list(alpha = 1, size = 2.0)
      )
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.03, 0.03))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.04, 0.04))) +
    ggplot2::labs(
      x = module05_adiposity_axis_label(exposure),
      y = module05_immune_axis_label(outcome), shape = NULL,
      title = paste0("Current values: ", outcome$label, " versus ", exposure$label)
    ) +
    module05_theme_sci(style, show_legend = TRUE)
  if (!is.null(x_limits) || !is.null(y_limits)) {
    p <- p + ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits, clip = "off")
  }
  p
}

module05_make_change_path_plot <- function(
  data, exposure, outcome, style, x_limits = NULL, y_limits = NULL
) {
  line_data <- module05_path_line_data(data)
  p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hline(
      yintercept = 0, linetype = "dashed",
      colour = as.character(style$reference_line_color), linewidth = 0.20
    ) +
    ggplot2::geom_vline(
      xintercept = 0, linetype = "dashed",
      colour = as.character(style$reference_line_color), linewidth = 0.20
    ) +
    ggplot2::geom_path(
      data = line_data,
      ggplot2::aes(group = participant_order),
      colour = "grey82", linewidth = 0.18, alpha = 1, na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = visit), colour = "grey25",
      size = 0.95, alpha = 0.86, stroke = 0.30
    ) +
    ggplot2::scale_shape_manual(
      values = MODULE05_CONFIG$visit_shapes,
      breaks = MODULE05_CONFIG$post_visit_levels
    ) +
    ggplot2::guides(
      shape = ggplot2::guide_legend(
        nrow = 1, byrow = TRUE,
        override.aes = list(alpha = 1, size = 2.0)
      )
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.03, 0.03))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.04, 0.04))) +
    ggplot2::labs(
      x = module05_adiposity_axis_label(exposure, delta = TRUE),
      y = module05_immune_axis_label(outcome, delta = TRUE), shape = NULL,
      title = paste0("Change from baseline: Δ", outcome$label, " versus Δ", exposure$label)
    ) +
    module05_theme_sci(style, show_legend = TRUE)
  if (!is.null(x_limits) || !is.null(y_limits)) {
    p <- p + ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits, clip = "off")
  }
  p
}

module05_rewrite_rgb_png <- function(
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
      stop("Unexpected final PNG dimensions: ", target, call. = FALSE)
    }
  }
  recorded_dpi <- suppressWarnings(as.numeric(attr(written, "info")$dpi))
  if (length(recorded_dpi) < 2L || any(!is.finite(recorded_dpi)) ||
      any(abs(recorded_dpi[1:2] - dpi) > 1)) {
    stop("Final PNG does not contain the requested DPI metadata.", call. = FALSE)
  }
  invisible(target)
}

module05_save_rgb_png <- function(
  plot, path, width_cm, height_cm, style
) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = ".module05_", fileext = ".png")
  on.exit(unlink(temporary), add = TRUE)
  ggplot2::ggsave(
    filename = temporary, plot = plot,
    width = width_cm, height = height_cm, units = "cm",
    dpi = as.numeric(style$png_dpi),
    bg = as.character(style$background_color), limitsize = FALSE
  )
  module05_rewrite_rgb_png(
    source = temporary, target = path,
    dpi = as.numeric(style$png_dpi),
    background_color = as.character(style$background_color),
    expected_width_cm = width_cm, expected_height_cm = height_cm
  )
  message("[PASS] standalone figure written: ", path)
  invisible(path)
}

# XLSX compatibility and immune table exports
module05_xml_tags <- function(text, pattern) {
  matches <- gregexpr(pattern, text, perl = TRUE)[[1L]]
  if (identical(matches[1L], -1L)) return(character())
  regmatches(text, list(matches))[[1L]]
}

module05_sanitize_xlsx <- function(path) {
  extraction_root <- tempfile(pattern = ".module05_xlsx_")
  rebuilt <- tempfile(pattern = ".module05_xlsx_", fileext = ".xlsx")
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
    tags <- module05_xml_tags(text, "<Relationship\\b[^>]*/>")
    for (tag in tags) {
      if (grepl("\\bTargetMode=\"External\"", tag, perl = TRUE)) next
      target <- sub(".*\\bTarget=\"([^\"]+)\".*", "\\1", tag, perl = TRUE)
      resolved <- file.path(extraction_root, source_directory, target)
      if (!file.exists(resolved)) text <- gsub(tag, "", text, fixed = TRUE)
    }
    writeLines(text, relationship_file, useBytes = TRUE)
  }

  content_types <- file.path(extraction_root, "[Content_Types].xml")
  content <- paste(readLines(content_types, warn = FALSE, encoding = "UTF-8"), collapse = "")
  overrides <- module05_xml_tags(content, "<Override\\b[^>]*/>")
  for (override in overrides) {
    part_name <- sub(".*\\bPartName=\"/([^\"]+)\".*", "\\1", override, perl = TRUE)
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
    rebuilt, files = archive_files, root = extraction_root,
    mode = "mirror", include_directories = FALSE
  )
  if (!file.copy(rebuilt, path, overwrite = TRUE)) {
    stop("Failed to replace sanitized XLSX file: ", path, call. = FALSE)
  }
  invisible(openxlsx::loadWorkbook(path))
  invisible(path)
}

module05_write_grouped_immune_table_xlsx <- function(
  body, top_headers, sub_headers, header_groups, exposure_row_groups,
  widths, sheet, path, top_header_height = 28
) {
  if (ncol(body) != length(top_headers) ||
      length(sub_headers) != length(top_headers)) {
    stop("Grouped immune table header/body column mismatch.", call. = FALSE)
  }
  if (length(widths) != ncol(body)) {
    stop("Grouped immune table width specification is invalid.", call. = FALSE)
  }
  if (any(vapply(header_groups, length, integer(1)) != 2L) ||
      any(vapply(exposure_row_groups, length, integer(1)) != 2L)) {
    stop("Grouped immune table merge specifications are invalid.", call. = FALSE)
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)

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
  exposure_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 9, textDecoration = "bold",
    halign = "center", valign = "center", wrapText = TRUE,
    border = "bottom", borderColour = "#D9D9D9", borderStyle = "thin"
  )
  outcome_style <- openxlsx::createStyle(
    fontName = "Arial", fontSize = 9,
    halign = "left", valign = "center", wrapText = TRUE,
    border = "bottom", borderColour = "#D9D9D9", borderStyle = "thin"
  )
  separator_style <- openxlsx::createStyle(
    border = "top", borderColour = "#A6A6A6", borderStyle = "thin"
  )

  openxlsx::writeData(
    wb, sheet, rbind(top_headers, sub_headers),
    startRow = 1L, startCol = 1L, colNames = FALSE, rowNames = FALSE
  )
  for (group in header_groups) {
    if (group[1L] == group[2L]) {
      openxlsx::mergeCells(wb, sheet, cols = group[1L], rows = 1:2)
    } else {
      openxlsx::mergeCells(wb, sheet, cols = group[1L]:group[2L], rows = 1L)
    }
  }

  openxlsx::writeData(
    wb, sheet, body, startRow = 3L, startCol = 1L,
    colNames = FALSE, rowNames = FALSE
  )
  for (group in exposure_row_groups) {
    openxlsx::mergeCells(wb, sheet, cols = 1L, rows = group[1L]:group[2L])
  }

  data_rows <- 3L:(nrow(body) + 2L)
  openxlsx::addStyle(
    wb, sheet, header_style, rows = 1:2, cols = seq_len(ncol(body)),
    gridExpand = TRUE, stack = TRUE
  )
  openxlsx::addStyle(
    wb, sheet, body_style, rows = data_rows, cols = seq_len(ncol(body)),
    gridExpand = TRUE, stack = TRUE
  )
  openxlsx::addStyle(
    wb, sheet, exposure_style, rows = data_rows, cols = 1L,
    gridExpand = TRUE, stack = TRUE
  )
  openxlsx::addStyle(
    wb, sheet, outcome_style, rows = data_rows, cols = 2L,
    gridExpand = TRUE, stack = TRUE
  )
  if (length(exposure_row_groups) > 1L) {
    separator_rows <- vapply(exposure_row_groups[-1L], `[[`, integer(1), 1L)
    openxlsx::addStyle(
      wb, sheet, separator_style, rows = separator_rows,
      cols = seq_len(ncol(body)), gridExpand = TRUE, stack = TRUE
    )
  }

  openxlsx::setColWidths(
    wb, sheet, cols = seq_len(ncol(body)), widths = widths
  )
  openxlsx::setRowHeights(wb, sheet, rows = 1L, heights = top_header_height)
  openxlsx::setRowHeights(wb, sheet, rows = 2L, heights = 34)
  openxlsx::setRowHeights(wb, sheet, rows = data_rows, heights = 18)
  openxlsx::freezePane(wb, sheet, firstActiveRow = 3L)
  openxlsx::pageSetup(
    wb, sheet, orientation = "landscape",
    fitToWidth = 1, fitToHeight = 0, paperSize = 9
  )
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  module05_sanitize_xlsx(path)
  message("[PASS] article table written: ", path)
  invisible(path)
}

module05_write_table_s11_xlsx <- function(
  bmi_body, weight_body, sample_sizes, path
) {
  if (ncol(bmi_body) != 12L || ncol(weight_body) != 12L) {
    stop("Table S11 body must have twelve columns.", call. = FALSE)
  }
  if (nrow(bmi_body) != 6L || nrow(weight_body) != 6L) {
    stop("Table S11 requires six immune outcomes for each exposure.", call. = FALSE)
  }
  required_sizes <- c(
    "baseline", "overall_subjects", "overall_observations", "M1", "M3", "M6"
  )
  if (!all(required_sizes %in% names(sample_sizes)) ||
      any(!is.finite(as.numeric(sample_sizes[required_sizes])))) {
    stop("Table S11 sample-size specification is incomplete.", call. = FALSE)
  }

  top_headers <- c(
    "Exposure", "Outcome",
    paste0("Baseline (n=", sample_sizes[["baseline"]], ")"), "",
    paste0(
      "Overall postoperative change (", sample_sizes[["overall_subjects"]],
      " participants; ", sample_sizes[["overall_observations"]], " observations)"
    ), "",
    paste0("M1 (n=", sample_sizes[["M1"]], ")"), "",
    paste0("M3 (n=", sample_sizes[["M3"]], ")"), "",
    paste0("M6 (n=", sample_sizes[["M6"]], ")"), "",
    "Visit × change P value"
  )
  sub_headers <- c(
    "", "", "β (95% CI)", "P value", "β (95% CI)", "P value",
    "β (95% CI)", "P value", "β (95% CI)", "P value",
    "β (95% CI)", "P value", ""
  )
  bmi_table <- data.frame(
    Exposure = rep("BMI", nrow(bmi_body)), bmi_body,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  weight_table <- data.frame(
    Exposure = rep("Weight", nrow(weight_body)), weight_body,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  body <- rbind(bmi_table, weight_table)
  body$Exposure[c(2:6, 8:12)] <- ""

  module05_write_grouped_immune_table_xlsx(
    body = body,
    top_headers = top_headers,
    sub_headers = sub_headers,
    header_groups = list(
      c(1L, 1L), c(2L, 2L), c(3L, 4L), c(5L, 6L),
      c(7L, 8L), c(9L, 10L), c(11L, 12L), c(13L, 13L)
    ),
    exposure_row_groups = list(c(3L, 8L), c(9L, 14L)),
    widths = c(11, 18, 19, 10, 26, 10, 18, 10, 18, 10, 18, 10, 19),
    sheet = "Table S11",
    path = path,
    top_header_height = 42
  )
}

module05_write_table_s12_xlsx <- function(
  weight_body, bmi_body, interval_n, path
) {
  if (ncol(weight_body) != 8L || ncol(bmi_body) != 8L) {
    stop("Table S12 body must have eight columns.", call. = FALSE)
  }
  if (nrow(weight_body) != 6L || nrow(bmi_body) != 6L) {
    stop("Table S12 requires six immune outcomes for each exposure.", call. = FALSE)
  }

  top_headers <- c(
    "Exposure", "Outcome", paste0("M0→M1 (n=", interval_n[1L], ")"), "",
    paste0("M1→M3 (n=", interval_n[2L], ")"), "",
    paste0("M3→M6 (n=", interval_n[3L], ")"), "",
    "Interval × change P value"
  )
  sub_headers <- c(
    "", "", "β (95% CI)", "P value", "β (95% CI)", "P value",
    "β (95% CI)", "P value", ""
  )
  weight_table <- data.frame(
    Exposure = rep("Weight", nrow(weight_body)), weight_body,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  bmi_table <- data.frame(
    Exposure = rep("BMI", nrow(bmi_body)), bmi_body,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  body <- rbind(weight_table, bmi_table)
  body$Exposure[c(2:6, 8:12)] <- ""

  module05_write_grouped_immune_table_xlsx(
    body = body,
    top_headers = top_headers,
    sub_headers = sub_headers,
    header_groups = list(
      c(1L, 1L), c(2L, 2L), c(3L, 4L), c(5L, 6L),
      c(7L, 8L), c(9L, 9L)
    ),
    exposure_row_groups = list(c(3L, 8L), c(9L, 14L)),
    widths = c(12, 18, 20, 10, 20, 10, 20, 10, 19),
    sheet = "Table S12",
    path = path
  )
}
