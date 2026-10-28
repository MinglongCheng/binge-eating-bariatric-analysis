#!/usr/bin/env bash
# Reproduce the standard analysis outputs in Modules 01-06.
# Run from anywhere: bash /path/to/binge-eating-bariatric-analysis/run_standard.sh
# Optional heavy analyses (full MNAR, full LOO) and interactive plots are excluded.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$REPO_ROOT"

# Verify that this file sits at the repository root.
for required in pyproject.toml uv.lock renv.lock code data; do
  if [[ ! -e "$required" ]]; then
    printf '[ERROR] Missing %s. Place run_standard.sh beside README.md at the repository root.\n' "$required" >&2
    exit 1
  fi
done

for executable in uv Rscript; do
  if ! command -v "$executable" >/dev/null 2>&1; then
    printf '[ERROR] %s is not available on PATH. Set up Python/uv and R/renv first.\n' "$executable" >&2
    exit 1
  fi
done

# The validated reproducibility environment uses R 4.5.1.
R_VERSION="$(Rscript --version 2>&1)"
if [[ "$R_VERSION" != *"version 4.5.1"* ]]; then
  printf '[ERROR] Expected R 4.5.1 but found: %s\n' "$R_VERSION" >&2
  printf '[ERROR] Put the R 4.5.1 executable directory first on PATH before rerunning.\n' >&2
  exit 1
fi

# Enforce standard noninteractive modes.
# Full MNAR, full immune LOO, and interactive plots are disabled.
export MPLBACKEND=Agg
export PYTHONDONTWRITEBYTECODE=1
export MODULE04_MNAR_MODE=check
export MODULE05_RUN_HEAVY_AUDIT=0
export MODULE05_SHOW_DIAGNOSTIC_PLOTS=0

run_step() {
  local number="$1"
  local label="$2"
  shift 2

  printf '\n[%02d/18] %s\n' "$number" "$label"

  if "$@"; then
    printf '[PASS] Step %02d completed\n' "$number"
  else
    local status=$?
    printf '[FAIL] Step %02d failed (exit status %d). Remaining steps not run.\n' \
      "$number" "$status" >&2
    return "$status"
  fi
}

printf 'Repository root: %s\n' "$REPO_ROOT"
printf 'R environment: %s\n' "$R_VERSION"
printf 'Modes: MNAR=check; immune full LOO=off; interactive plots=off\n'

# ============================================================
# Module 01: Follow-up missingness
# ============================================================

run_step 1 '01: Follow-up tables' \
  uv run --frozen python -B code/01_followup_missingness/01_followup_missingness_tables.py

run_step 2 '01: Follow-up figures' \
  uv run --frozen python -B code/01_followup_missingness/02_followup_missingness_figures.py


# ============================================================
# Module 02: Preoperative group comparisons
# ============================================================

run_step 3 '02: Distribution diagnostics (no plots)' \
  uv run --frozen python -B code/02_preoperative_group_comparison/00_preoperative_distribution_diagnostics.py --no-plots

run_step 4 '02: Unadjusted comparisons' \
  uv run --frozen python -B code/02_preoperative_group_comparison/01_preoperative_unadjusted_comparisons.py

run_step 5 '02: Threshold comparisons' \
  uv run --frozen python -B code/02_preoperative_group_comparison/02_preoperative_threshold_comparisons.py

run_step 6 '02: Sex-adjusted comparisons' \
  uv run --frozen python -B code/02_preoperative_group_comparison/03_preoperative_sex_adjusted_comparisons.py

run_step 7 '02: Raincloud plots' \
  uv run --frozen python -B code/02_preoperative_group_comparison/04_preoperative_raincloud_plots.py


# ============================================================
# Module 03: Preoperative machine learning
# ============================================================

run_step 8 '03: Feature ranking' \
  uv run --frozen python -B code/03_preoperative_machine_learning/01_feature_ranking.py

run_step 9 '03: Nested cross-validation models' \
  uv run --frozen python -B code/03_preoperative_machine_learning/02_train_nested_cv_models.py

run_step 10 '03: Model interpretation' \
  uv run --frozen python -B code/03_preoperative_machine_learning/03_model_interpretation.py


# ============================================================
# Module 04: Postoperative longitudinal analysis
# ============================================================

run_step 11 '04: Longitudinal models and figures' \
  Rscript code/04_postoperative_longitudinal_analysis/01_longitudinal_models_and_figures.R

run_step 12 '04: MNAR compatibility check (no multiple imputation)' \
  Rscript code/04_postoperative_longitudinal_analysis/02_mnar_tipping_point.R


# ============================================================
# Module 05: Immune-cell analyses
# ============================================================

run_step 13 '05: Immune models and Figure 3' \
  Rscript code/05_immune_cell_analysis/01_immune_models_and_figure3.R

run_step 14 '05: Adjacent-interval table' \
  Rscript code/05_immune_cell_analysis/02_immune_adjacent_interval_table.R

run_step 15 '05: Descriptive standalone figures' \
  Rscript code/05_immune_cell_analysis/03_immune_descriptive_raw_figures.R

run_step 16 '05: Standard diagnostic audit (no full LOO)' \
  Rscript code/05_immune_cell_analysis/04_immune_audit_diagnostics.R


# ============================================================
# Module 06: Final composite figures
# ============================================================

run_step 17 '06: Python composite figures' \
  uv run --frozen python -B code/06_figure_assembly/01_assemble_python_figures.py

run_step 18 '06: R composite figures' \
  Rscript code/06_figure_assembly/02_assemble_r_figures.R


# ============================================================
# Completion
# ============================================================

printf '\n[PASS] All 18 standard analysis commands completed.\n'
printf 'Advanced MNAR (preflight/confirm/full), full immune LOO, and interactive plots were NOT run.\n'
printf 'Existing output files may have been overwritten by their regenerated standard versions.\n'

