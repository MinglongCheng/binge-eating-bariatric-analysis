#!/usr/bin/env python3
"""Create the standalone panels and plot-ready data for Supplementary Figure S1.

Input:
    data/followup_attendance.csv
Outputs:
    outputs/01_followup_missingness/figure/FigS1a_followup_availability_matrix.png
    outputs/01_followup_missingness/figure/FigS1b_bes_score_distribution.png
    outputs/06_final_figures/_plot_data/_figs1_followup_availability.csv
    outputs/06_final_figures/_plot_data/_figs1_bes_distribution.csv
"""

from __future__ import annotations

from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib as mpl
import matplotlib.font_manager as font_manager
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.colors import ListedColormap, TwoSlopeNorm
from matplotlib.patches import Rectangle
from PIL import Image

# Configuration
PROJECT_DIR = Path(__file__).resolve().parents[2]
DATA_FILE = PROJECT_DIR / "data" / "followup_attendance.csv"
FIGURE_DIR = PROJECT_DIR / "outputs" / "01_followup_missingness" / "figure"
PLOT_DATA_DIR = PROJECT_DIR / "outputs" / "06_final_figures" / "_plot_data"
STYLE_MANIFEST = (
    PROJECT_DIR
    / "code"
    / "06_figure_assembly"
    / "00_figure_style_manifest.csv"
)

REQUIRED_COLUMNS = (
    "participant_id",
    "bes_score",
    "bes_group",
    "baseline_available",
    "month_1_attended",
    "month_3_attended",
    "month_6_attended",
)
AVAILABILITY_COLUMNS = (
    "baseline_available",
    "month_1_attended",
    "month_3_attended",
    "month_6_attended",
)

DEFAULT_STYLE: dict[str, object] = {
    "font_family": "Arial",
    "background_color": "white",
    "png_dpi": 600,
    "single_axis_title_size_pt": 10.0,
    "single_tick_size_pt": 9.0,
    "single_legend_size_pt": 9.0,
    "reference_line_color": "#D9D9D9",
}

GROUP_COLORS = {"Non-BE": "#AFDDDC", "BE": "#F6DBD5"}


def cm_to_inches(value_cm: float) -> float:
    return value_cm / 2.54


def load_style() -> dict[str, object]:
    """Read the shared style manifest when available, otherwise use frozen defaults."""
    style = DEFAULT_STYLE.copy()
    if not STYLE_MANIFEST.exists():
        print("[INFO] shared style manifest not found; using frozen module 01 defaults")
        return style

    manifest = pd.read_csv(STYLE_MANIFEST)
    required = {"Key", "Value"}
    if not required.issubset(manifest.columns):
        raise ValueError(
            f"Style manifest must contain columns {sorted(required)}: {STYLE_MANIFEST}"
        )

    values = dict(zip(manifest["Key"].astype(str), manifest["Value"]))
    numeric_keys = {
        "png_dpi",
        "single_axis_title_size_pt",
        "single_tick_size_pt",
        "single_legend_size_pt",
    }
    for key in style:
        if key not in values or pd.isna(values[key]):
            continue
        style[key] = float(values[key]) if key in numeric_keys else str(values[key])
    return style


def configure_matplotlib(style: dict[str, object]) -> None:
    """Apply the frozen standalone-figure typography and require Arial."""
    font_family = str(style["font_family"])
    try:
        font_manager.findfont(font_family, fallback_to_default=False)
    except ValueError as exc:
        raise RuntimeError(
            f"Required font '{font_family}' is not available. Install or enable Arial "
            "before generating manuscript figures."
        ) from exc

    mpl.rcParams.update(
        {
            "font.family": font_family,
            "font.size": float(style["single_tick_size_pt"]),
            "axes.titlesize": float(style["single_axis_title_size_pt"]),
            "axes.labelsize": float(style["single_axis_title_size_pt"]),
            "xtick.labelsize": float(style["single_tick_size_pt"]),
            "ytick.labelsize": float(style["single_tick_size_pt"]),
            "legend.fontsize": float(style["single_legend_size_pt"]),
            "axes.facecolor": str(style["background_color"]),
            "figure.facecolor": str(style["background_color"]),
            "savefig.facecolor": str(style["background_color"]),
            "savefig.transparent": False,
        }
    )
    print(f"[PASS] required font available: {font_family}")


# Data validation and plot-ready export

def normalise_group(series: pd.Series) -> pd.Series:
    values = series.astype("string").str.strip().str.lower()
    mapped = values.map(
        {"non-be": "Non-BE", "non_be": "Non-BE", "nonbe": "Non-BE", "be": "BE"}
    )
    unknown = sorted(values[mapped.isna() & values.notna()].dropna().unique().tolist())
    if unknown:
        raise ValueError(f"Unrecognised bes_group value(s): {unknown}")
    return mapped.astype("string")


def load_data() -> pd.DataFrame:
    """Load, validate, and freeze the participant order used by Figure S1."""
    if not DATA_FILE.exists():
        raise FileNotFoundError(f"Input file not found: {DATA_FILE}")

    df = pd.read_csv(DATA_FILE)
    df.columns = [str(column).strip() for column in df.columns]
    missing = [column for column in REQUIRED_COLUMNS if column not in df.columns]
    if missing:
        raise ValueError(f"Missing required column(s): {missing}")
    df = df[list(REQUIRED_COLUMNS)].copy()

    df["participant_id"] = pd.to_numeric(df["participant_id"], errors="raise")
    df["bes_score"] = pd.to_numeric(df["bes_score"], errors="raise")
    df["bes_group"] = normalise_group(df["bes_group"])

    for column in AVAILABILITY_COLUMNS:
        df[column] = pd.to_numeric(df[column], errors="raise").astype(int)
        if not set(df[column].unique()).issubset({0, 1}):
            raise ValueError(f"{column} must be coded 0/1.")

    group_order = df["bes_group"].map({"Non-BE": 0, "BE": 1})
    df = (
        df.assign(_group_order=group_order)
        .sort_values(["_group_order", "bes_score", "participant_id"], kind="mergesort")
        .drop(columns="_group_order")
        .reset_index(drop=True)
    )

    # Validate the scoring domain, without requiring specific observed extrema.
    bes = df["bes_score"].to_numpy(dtype=float)
    if (not np.isfinite(bes).all() or not np.equal(bes, np.floor(bes)).all()
            or np.any((bes < 0) | (bes > 46))):
        raise ValueError("BES scores must be finite integers in 0–46.")
    expected_group = pd.Series(np.where(bes > 17, "BE", "Non-BE"),
                               index=df.index, dtype="string")
    if df["bes_group"].isna().any() or not df["bes_group"].eq(expected_group).all():
        raise ValueError("BES group must equal BE iff BES > 17.")

    expected = {
        "rows": 122,
        "groups": {"Non-BE": 66, "BE": 56},
        "attendance": {
            "baseline_available": 122,
            "month_1_attended": 77,
            "month_3_attended": 87,
            "month_6_attended": 69,
        },
    }
    observed = {
        "rows": len(df),
        "groups": df["bes_group"].value_counts().to_dict(),
        "attendance": {column: int(df[column].sum()) for column in AVAILABILITY_COLUMNS},
    }
    if observed != expected:
        raise AssertionError(f"Figure data do not match the frozen sample: {observed}")

    print("[PASS] sample check: 122 participants; Non-BE=66; BE=56")
    print("[PASS] attendance check: M1=77; M3=87; M6=69")
    return df


def write_plot_ready_data(df: pd.DataFrame) -> tuple[Path, Path]:
    """Write minimal de-identified data used by standalone and combined Figure S1."""
    PLOT_DATA_DIR.mkdir(parents=True, exist_ok=True)
    participant_order = np.arange(1, len(df) + 1, dtype=int)

    availability = pd.DataFrame(
        {
            "participant_order": participant_order,
            "bes_group": df["bes_group"].astype(str),
            "bes_score": df["bes_score"].astype(int),
            **{column: df[column].astype(int) for column in AVAILABILITY_COLUMNS},
        }
    )
    distribution = availability[["participant_order", "bes_group", "bes_score"]].copy()

    availability_path = PLOT_DATA_DIR / "_figs1_followup_availability.csv"
    distribution_path = PLOT_DATA_DIR / "_figs1_bes_distribution.csv"
    availability.to_csv(availability_path, index=False, encoding="utf-8")
    distribution.to_csv(distribution_path, index=False, encoding="utf-8")

    print(f"[PASS] plot-ready data written: {availability_path}")
    print(f"[PASS] plot-ready data written: {distribution_path}")
    return availability_path, distribution_path


# Figure generation

def save_rgb_png(fig: plt.Figure, output_path: Path, dpi: int) -> None:
    """Save a fixed-size opaque PNG and enforce RGB colour mode."""
    fig.savefig(output_path, dpi=dpi, facecolor="white", transparent=False)
    plt.close(fig)

    with Image.open(output_path) as image:
        if image.mode != "RGB":
            image.convert("RGB").save(output_path, dpi=(dpi, dpi))

    with Image.open(output_path) as image:
        if image.mode != "RGB":
            raise AssertionError(f"Expected RGB PNG, found {image.mode}: {output_path}")
    print(f"[PASS] standalone figure written: {output_path}")


def plot_followup_matrix(
    plot_data: pd.DataFrame, output_path: Path, style: dict[str, object]
) -> None:
    """Draw the participant-level follow-up matrix with complete legends."""
    group_encoded = (
        plot_data["bes_group"]
        .map({"Non-BE": 0, "BE": 1})
        .astype(int)
        .to_numpy()
        .reshape(-1, 1)
    )
    bes = plot_data["bes_score"].to_numpy(dtype=float).reshape(-1, 1)
    availability = plot_data[list(AVAILABILITY_COLUMNS)].to_numpy(dtype=int)

    n = len(plot_data)
    widths = (0.36, 0.84, 1.0, 1.0, 1.0, 1.0)
    x_left = np.concatenate(([0.0], np.cumsum(widths[:-1]))).tolist()
    total_width = float(sum(widths))

    fig = plt.figure(figsize=(cm_to_inches(16), cm_to_inches(10)))
    grid = fig.add_gridspec(
        nrows=1,
        ncols=2,
        width_ratios=(5.30, 1.20),
        left=0.055,
        right=0.985,
        top=0.90,
        bottom=0.12,
        wspace=0.04,
    )
    ax = fig.add_subplot(grid[0, 0])
    legend_ax = fig.add_subplot(grid[0, 1])
    legend_ax.axis("off")

    group_cmap = ListedColormap([GROUP_COLORS["Non-BE"], GROUP_COLORS["BE"]])
    bes_cmap = plt.colormaps["Purples"]
    availability_cmap = ListedColormap(["#E0E0E0", "#303030"])
    bes_norm = TwoSlopeNorm(vmin=0, vcenter=17, vmax=46)

    ax.imshow(
        group_encoded,
        extent=[x_left[0], x_left[0] + widths[0], 0, n],
        cmap=group_cmap,
        vmin=0,
        vmax=1,
        origin="upper",
        interpolation="none",
        aspect="auto",
    )
    ax.imshow(
        bes,
        extent=[x_left[1], x_left[1] + widths[1], 0, n],
        cmap=bes_cmap,
        norm=bes_norm,
        origin="upper",
        interpolation="none",
        aspect="auto",
    )
    for index in range(2, 6):
        ax.imshow(
            availability[:, [index - 2]],
            extent=[x_left[index], x_left[index] + widths[index], 0, n],
            cmap=availability_cmap,
            vmin=0,
            vmax=1,
            origin="upper",
            interpolation="none",
            aspect="auto",
        )

    ax.set_xlim(0, total_width)
    ax.set_ylim(0, n)
    ax.set_xticks([x_left[index] + widths[index] / 2 for index in range(6)])
    ax.set_xticklabels(["Group", "BES score", "Baseline status", "M1 status", "M3 status", "M6 status"])
    ax.set_yticks([])
    ax.set_title("Follow-up availability", pad=7)

    for x_position in [*x_left, total_width]:
        ax.axvline(x_position, color="white", linewidth=0.9)
    for spine in ax.spines.values():
        spine.set_visible(True)
        spine.set_linewidth(0.75)
        spine.set_color("black")

    legend_items = (
        (GROUP_COLORS["Non-BE"], "Non-BE"),
        (GROUP_COLORS["BE"], "BE"),
        ("#303030", "Available"),
        ("#E0E0E0", "Missing"),
    )
    square_size = 0.095
    legend_step = 0.145
    x0 = 0.04
    first_center = 0.91
    for index, (colour, label) in enumerate(legend_items):
        center = first_center - index * legend_step
        y = center - square_size / 2
        legend_ax.add_patch(
            Rectangle(
                (x0, y),
                square_size,
                square_size,
                facecolor=colour,
                edgecolor="black",
                linewidth=0.7,
                transform=legend_ax.transAxes,
            )
        )
        legend_ax.text(
            x0 + square_size + 0.07,
            center,
            label,
            va="center",
            transform=legend_ax.transAxes,
        )

    bes_title_y = first_center - len(legend_items) * legend_step
    legend_ax.text(
        x0,
        bes_title_y,
        "BES score",
        va="center",
        transform=legend_ax.transAxes,
    )
    colourbar_ax = legend_ax.inset_axes([x0, 0.055, square_size, 0.245])
    colourbar = fig.colorbar(
        mpl.cm.ScalarMappable(norm=bes_norm, cmap=bes_cmap),
        cax=colourbar_ax,
        orientation="vertical",
    )
    colourbar.set_ticks([0, 17, 46])
    colourbar.set_ticklabels(["0", "17", "46"])
    colourbar.outline.set_linewidth(0.7)

    save_rgb_png(fig, output_path, int(style["png_dpi"]))


def plot_bes_histogram(
    plot_data: pd.DataFrame, output_path: Path, style: dict[str, object]
) -> None:
    """Draw the complete BES score distribution, including the score of zero."""
    fig, ax = plt.subplots(figsize=(cm_to_inches(14), cm_to_inches(9)))

    max_bes = int(plot_data["bes_score"].max())
    bins = np.arange(-0.5, max_bes + 1.5, 1.0)
    counts, _, _ = ax.hist(
        plot_data["bes_score"],
        bins=bins,
        color="#B7D7E8",
        edgecolor="black",
        linewidth=0.75,
        align="mid",
    )

    plotted_n = int(np.sum(counts))
    if plotted_n != len(plot_data):
        raise AssertionError(
            f"Histogram includes {plotted_n} observations; expected {len(plot_data)}."
        )

    ax.set_title("BES score distribution", pad=7)
    ax.set_xlabel("BES score")
    ax.set_ylabel("Number of participants")
    ax.set_xticks(range(0, max_bes + 1, 2))
    ax.set_xlim(-0.5, max_bes + 0.5)
    ax.set_ylim(0, int(np.nanmax(counts)) + 1)
    ax.grid(
        axis="y",
        color=str(style["reference_line_color"]),
        linewidth=0.55,
        linestyle="--",
    )
    ax.set_axisbelow(True)

    for spine in ax.spines.values():
        spine.set_visible(True)
        spine.set_linewidth(0.75)
        spine.set_color("black")

    fig.subplots_adjust(left=0.13, right=0.98, top=0.88, bottom=0.17)
    save_rgb_png(fig, output_path, int(style["png_dpi"]))


def main() -> None:
    style = load_style()
    configure_matplotlib(style)
    FIGURE_DIR.mkdir(parents=True, exist_ok=True)

    source = load_data()
    availability_path, distribution_path = write_plot_ready_data(source)

    availability = pd.read_csv(availability_path)
    distribution = pd.read_csv(distribution_path)
    plot_followup_matrix(
        availability,
        FIGURE_DIR / "FigS1a_followup_availability_matrix.png",
        style,
    )
    plot_bes_histogram(
        distribution,
        FIGURE_DIR / "FigS1b_bes_score_distribution.png",
        style,
    )
    print("[PASS] module 01 figure workflow completed")


if __name__ == "__main__":
    main()
