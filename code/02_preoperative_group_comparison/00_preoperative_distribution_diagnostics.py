#!/usr/bin/env python3
"""Display distribution diagnostics for the frozen preoperative tests.

The diagnostics report normality, variance, and mechanical test suggestions,
but never change the endpoint-specific tests used for the article tables. By
default, histogram and Q-Q panels are displayed interactively; no file is
written. Use --no-plots for console output only.
"""

from __future__ import annotations

import argparse
import warnings
from pathlib import Path
from runpy import run_path
from typing import Iterable

import numpy as np
import pandas as pd
from scipy import stats
from statsmodels.stats.diagnostic import lilliefors

_ANALYSIS = run_path(str(Path(__file__).with_name("01_preoperative_unadjusted_comparisons.py")))
ALL_SPECS = _ANALYSIS["ALL_SPECS"]
VariableSpec = _ANALYSIS["VariableSpec"]
load_data = _ANALYSIS["load_data"]
GROUP_ORDER = ("Non-BE", "BE")

ALPHA = 0.05
GROUP_COLOURS = {"Non-BE": "#1B9E9A", "BE": "#E59886"}
TEST_LABELS = {
    "student": "Student's t test",
    "welch": "Welch's t test",
    "mwu": "Mann-Whitney U test",
    "descriptive_mwu": "Descriptive only",
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Display distribution diagnostics for the frozen preoperative "
            "endpoint-specific tests. No output files are written."
        )
    )
    parser.add_argument(
        "--variables",
        nargs="+",
        metavar="COLUMN",
        help=(
            "Optional column names to review. When omitted, all continuous "
            "endpoints used in Tables 1, 2, S3, and S4 are reviewed."
        ),
    )
    parser.add_argument(
        "--no-plots",
        action="store_true",
        help="Print diagnostics only; do not open histogram/Q-Q figures.",
    )
    parser.add_argument(
        "--list-variables",
        action="store_true",
        help="List available column names and exit.",
    )
    return parser.parse_args()



def choose_specs(requested_columns: list[str] | None) -> tuple[VariableSpec, ...]:
    available = {spec.column: spec for spec in ALL_SPECS}
    if requested_columns is None:
        return ALL_SPECS

    unknown = sorted(set(requested_columns).difference(available))
    if unknown:
        raise ValueError(
            "Unknown diagnostic variable(s): "
            f"{unknown}. Use --list-variables to see valid column names."
        )

    # Preserve the order supplied on the command line while removing duplicates.
    ordered_unique = list(dict.fromkeys(requested_columns))
    return tuple(available[column] for column in ordered_unique)


def normality_diagnostic(values: np.ndarray) -> dict[str, float | str | int]:
    clean = np.asarray(values, dtype=float)
    clean = clean[np.isfinite(clean)]
    n = int(clean.size)
    if n < 3:
        return {
            "n": n,
            "normality_test": "insufficient n",
            "normality_statistic": np.nan,
            "normality_p": np.nan,
            "skewness": np.nan,
            "excess_kurtosis": np.nan,
        }

    if np.allclose(clean, clean[0]):
        return {
            "n": n,
            "normality_test": "constant values",
            "normality_statistic": np.nan,
            "normality_p": np.nan,
            "skewness": np.nan,
            "excess_kurtosis": np.nan,
        }

    if n <= 50:
        statistic, p_value = stats.shapiro(clean)
        test_name = "Shapiro-Wilk"
    else:
        statistic, p_value = lilliefors(clean, dist="norm", pvalmethod="table")
        test_name = "Lilliefors-corrected K-S"

    return {
        "n": n,
        "normality_test": test_name,
        "normality_statistic": float(statistic),
        "normality_p": float(p_value),
        "skewness": float(stats.skew(clean, bias=False)),
        "excess_kurtosis": float(stats.kurtosis(clean, fisher=True, bias=False)),
    }


def levene_diagnostic(nonbe: np.ndarray, be: np.ndarray) -> tuple[float, float]:
    nonbe_clean = np.asarray(nonbe, dtype=float)
    be_clean = np.asarray(be, dtype=float)
    nonbe_clean = nonbe_clean[np.isfinite(nonbe_clean)]
    be_clean = be_clean[np.isfinite(be_clean)]

    if nonbe_clean.size < 2 or be_clean.size < 2:
        return np.nan, np.nan

    with warnings.catch_warnings():
        warnings.simplefilter("ignore", RuntimeWarning)
        statistic, p_value = stats.levene(nonbe_clean, be_clean, center="mean")
    return float(statistic), float(p_value)


def mechanical_test_recommendation(
    nonbe_normality_p: float,
    be_normality_p: float,
    levene_p: float,
) -> str:
    p_values = (nonbe_normality_p, be_normality_p, levene_p)
    if not all(np.isfinite(value) for value in p_values):
        return "Undetermined"
    if nonbe_normality_p >= ALPHA and be_normality_p >= ALPHA:
        return "Student's t test" if levene_p >= ALPHA else "Welch's t test"
    return "Mann-Whitney U test"


def build_diagnostics(df: pd.DataFrame, specs: Iterable[VariableSpec]) -> pd.DataFrame:
    rows: list[dict[str, object]] = []
    for spec in specs:
        group_values = {
            group: df.loc[df["bes_group"].eq(group), spec.column].dropna().to_numpy(dtype=float)
            for group in GROUP_ORDER
        }
        nonbe_diag = normality_diagnostic(group_values["Non-BE"])
        be_diag = normality_diagnostic(group_values["BE"])
        levene_statistic, levene_p = levene_diagnostic(
            group_values["Non-BE"], group_values["BE"]
        )
        suggested = mechanical_test_recommendation(
            float(nonbe_diag["normality_p"]),
            float(be_diag["normality_p"]),
            levene_p,
        )
        frozen = TEST_LABELS[spec.test]
        if spec.test == "descriptive_mwu":
            status = "Group-defining variable; no inferential test"
        else:
            status = "Consistent" if suggested == frozen else "Frozen mapping differs"

        rows.append(
            {
                "column": spec.column,
                "indicator": spec.label,
                "nonbe_n": int(nonbe_diag["n"]),
                "nonbe_normality_test": nonbe_diag["normality_test"],
                "nonbe_normality_p": float(nonbe_diag["normality_p"]),
                "nonbe_skewness": float(nonbe_diag["skewness"]),
                "nonbe_excess_kurtosis": float(nonbe_diag["excess_kurtosis"]),
                "be_n": int(be_diag["n"]),
                "be_normality_test": be_diag["normality_test"],
                "be_normality_p": float(be_diag["normality_p"]),
                "be_skewness": float(be_diag["skewness"]),
                "be_excess_kurtosis": float(be_diag["excess_kurtosis"]),
                "levene_statistic": levene_statistic,
                "levene_p": levene_p,
                "mechanical_rule": suggested,
                "frozen_test": frozen,
                "status": status,
            }
        )
    return pd.DataFrame(rows)


def format_p(value: object) -> str:
    try:
        numeric = float(value)
    except (TypeError, ValueError):
        return "—"
    if not np.isfinite(numeric):
        return "—"
    return "<0.001" if numeric < 0.001 else f"{numeric:.3f}"


def print_diagnostics(diagnostics: pd.DataFrame) -> None:
    display = diagnostics[
        [
            "column",
            "indicator",
            "nonbe_n",
            "nonbe_normality_test",
            "nonbe_normality_p",
            "be_n",
            "be_normality_test",
            "be_normality_p",
            "levene_p",
            "mechanical_rule",
            "frozen_test",
            "status",
        ]
    ].copy()
    for column in ("nonbe_normality_p", "be_normality_p", "levene_p"):
        display[column] = display[column].map(format_p)

    with pd.option_context(
        "display.max_rows",
        None,
        "display.max_columns",
        None,
        "display.width",
        240,
        "display.max_colwidth",
        45,
    ):
        print("\nDistribution diagnostics (display only; no file is written):")
        print(display.to_string(index=False))


def common_histogram_edges(nonbe: np.ndarray, be: np.ndarray) -> np.ndarray:
    pooled = np.concatenate([nonbe, be]).astype(float)
    pooled = pooled[np.isfinite(pooled)]
    if pooled.size == 0:
        return np.array([0.0, 1.0])
    if np.unique(pooled).size == 1:
        value = float(pooled[0])
        width = max(abs(value) * 0.05, 0.5)
        return np.array([value - width, value + width])

    edges = np.histogram_bin_edges(pooled, bins="auto")
    if edges.size < 6:
        edges = np.linspace(float(pooled.min()), float(pooled.max()), 6)
    return edges


def plot_endpoint(df: pd.DataFrame, spec: VariableSpec, diagnostic_row: pd.Series) -> None:
    import matplotlib as mpl
    import matplotlib.font_manager as font_manager
    import matplotlib.pyplot as plt

    try:
        font_manager.findfont("Arial", fallback_to_default=False)
    except ValueError as exc:
        raise RuntimeError(
            "Required font 'Arial' is not available for diagnostic figures."
        ) from exc
    mpl.rcParams.update({"font.family": "Arial"})

    values = {
        group: df.loc[df["bes_group"].eq(group), spec.column].dropna().to_numpy(dtype=float)
        for group in GROUP_ORDER
    }
    edges = common_histogram_edges(values["Non-BE"], values["BE"])

    fig, axes = plt.subplots(2, 2, figsize=(10.5, 7.2), constrained_layout=True)
    for column_index, group in enumerate(GROUP_ORDER):
        colour = GROUP_COLOURS[group]
        group_values = values[group]
        prefix = "nonbe" if group == "Non-BE" else "be"
        test_name = str(diagnostic_row[f"{prefix}_normality_test"])
        p_text = format_p(diagnostic_row[f"{prefix}_normality_p"])

        hist_ax = axes[0, column_index]
        hist_ax.hist(group_values, bins=edges, alpha=0.72, edgecolor="white", color=colour)
        hist_ax.axvline(np.mean(group_values), linestyle="--", linewidth=1.0, color="black", label="Mean")
        hist_ax.axvline(np.median(group_values), linestyle=":", linewidth=1.2, color="black", label="Median")
        hist_ax.set_title(f"{group}: histogram (n={len(group_values)})")
        hist_ax.set_xlabel(spec.label)
        hist_ax.set_ylabel("Count")
        hist_ax.legend(frameon=False, fontsize=8)

        qq_ax = axes[1, column_index]
        stats.probplot(group_values, dist="norm", plot=qq_ax)
        qq_ax.get_lines()[0].set_markerfacecolor(colour)
        qq_ax.get_lines()[0].set_markeredgecolor(colour)
        qq_ax.get_lines()[0].set_markersize(4)
        qq_ax.get_lines()[1].set_color("black")
        qq_ax.set_title(f"{group}: Q-Q plot\n{test_name}, P={p_text}")

    frozen = str(diagnostic_row["frozen_test"])
    mechanical = str(diagnostic_row["mechanical_rule"])
    levene_text = format_p(diagnostic_row["levene_p"])
    fig.suptitle(
        f"{spec.label}\nFrozen test: {frozen} | Mechanical rule: {mechanical} | "
        f"Levene P={levene_text}",
        fontsize=12,
    )
    print(f"Displaying {spec.column}. Close the figure window to continue.")
    plt.show(block=True)
    plt.close(fig)


def main() -> None:
    args = parse_args()
    if args.list_variables:
        print("Available variables:")
        for spec in ALL_SPECS:
            print(f"  {spec.column}: {spec.label} [{TEST_LABELS[spec.test]}]")
        return

    specs = choose_specs(args.variables)
    df = load_data()
    diagnostics = build_diagnostics(df, specs)
    print_diagnostics(diagnostics)

    if args.no_plots:
        print("\nPlots were suppressed by --no-plots. No output file was written.")
        return

    for spec in specs:
        row = diagnostics.loc[diagnostics["column"].eq(spec.column)].iloc[0]
        plot_endpoint(df, spec, row)

    print("\nFinished. Diagnostic figures were displayed only and were not saved.")


if __name__ == "__main__":
    main()
