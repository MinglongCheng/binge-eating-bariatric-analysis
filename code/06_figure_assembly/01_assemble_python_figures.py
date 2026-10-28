"""Assemble final Python figures from frozen plot-ready data."""

from __future__ import annotations

import math
import warnings
from pathlib import Path
from typing import Iterable, Mapping, Sequence

import matplotlib as mpl
import matplotlib.font_manager as font_manager
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import shap
from matplotlib.cm import ScalarMappable
from matplotlib.colors import ListedColormap, Normalize, TwoSlopeNorm, to_hex, to_rgb
from matplotlib.lines import Line2D
from matplotlib.patches import Patch, Rectangle
from matplotlib.ticker import MaxNLocator
from PIL import Image

SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parents[1]
PLOT_DATA_DIR = PROJECT_ROOT / "outputs" / "06_final_figures" / "_plot_data"
OUTPUT_ROOT = PROJECT_ROOT / "outputs" / "06_final_figures"
EPS_DIR = OUTPUT_ROOT / "eps"
SVG_DIR = OUTPUT_ROOT / "svg"
PNG_DIR = OUTPUT_ROOT / "png_600dpi"
STYLE_FILE = SCRIPT_DIR / "00_figure_style_manifest.csv"

GROUP_ORDER = ("Non-BE", "BE")
RANKING_SERIES = (
    ("fdr_rank", "FDR rank", "#EF476F", "o", 18),
    ("l1_rank", "L1 rank", "#F1D64A", "^", 20),
    ("xgboost_rank", "XGBoost rank", "#2FB171", "s", 18),
    ("overall_rank", "Overall rank", "#B0009B", "*", 23),
)
RANKING_ALPHA = 0.78
RANKING_ZORDER = {
    "xgboost_rank": 2,
    "fdr_rank": 3,
    "l1_rank": 4,
    "overall_rank": 5,
}
RAIN_JITTER_SEEDS = {1: 52, 2: 42, 3: 62}
SHAP_RANDOM_SEED = 42


def cm_to_inches(value_cm: float) -> float:
    return float(value_cm) / 2.54


def blend_with_white(colour: str, white_fraction: float) -> str:
    rgb = np.asarray(to_rgb(colour), dtype=float)
    return to_hex(np.clip(rgb * (1.0 - white_fraction) + white_fraction, 0.0, 1.0))


def load_style() -> dict[str, object]:
    frame = pd.read_csv(STYLE_FILE, dtype=str).fillna("")
    if not {"Key", "Value"}.issubset(frame.columns):
        raise ValueError("Style manifest must contain Key and Value columns.")
    values = {
        str(row.Key).strip(): str(row.Value).strip()
        for row in frame.itertuples(index=False)
        if str(row.Key).strip()
    }
    for key in (
        "png_dpi",
        "combined_width_cm",
        "panel_letter_size_pt",
        "combined_axis_title_size_pt",
        "combined_tick_size_pt",
        "combined_legend_size_pt",
        "single_axis_title_size_pt",
        "single_tick_size_pt",
        "single_legend_size_pt",
        "line_main_width_pt",
        "line_main_single_width_pt",
        "reference_line_width_pt",
    ):
        values[key] = float(values[key])
    return values


def require_font(font_family: str) -> None:
    try:
        path = font_manager.findfont(font_family, fallback_to_default=False)
    except ValueError as exc:
        raise RuntimeError(f"Required font '{font_family}' is unavailable.") from exc
    if not path:
        raise RuntimeError(f"Required font '{font_family}' is unavailable.")
    print(f"[PASS] required font available: {font_family}")


def configure_matplotlib(style: Mapping[str, object]) -> None:
    font = str(style["font_family"])
    common = float(style["combined_tick_size_pt"])
    mpl.rcParams.update(
        {
            "font.family": font,
            "font.sans-serif": [font],
            "mathtext.fontset": "custom",
            "mathtext.rm": font,
            "mathtext.it": f"{font}:italic",
            "mathtext.bf": f"{font}:bold",
            "axes.unicode_minus": False,
            "axes.labelsize": common,
            "axes.titlesize": common,
            "xtick.labelsize": common,
            "ytick.labelsize": common,
            "legend.fontsize": common,
            "font.weight": "normal",
            "axes.titleweight": "normal",
            "axes.labelweight": "normal",
            "svg.fonttype": "none",
            "ps.fonttype": 42,
            "pdf.fonttype": 42,
            "savefig.facecolor": str(style["background_color"]),
            "savefig.bbox": None,
            "savefig.pad_inches": 0,
            "figure.autolayout": False,
            "figure.constrained_layout.use": False,
            "figure.facecolor": str(style["background_color"]),
        }
    )


def read_csv(name: str, required_columns: Iterable[str], expected_rows: int | None = None) -> pd.DataFrame:
    path = PLOT_DATA_DIR / name
    if not path.exists():
        raise FileNotFoundError(f"Missing plot-ready file: {path}")
    frame = pd.read_csv(path)
    missing = sorted(set(required_columns).difference(frame.columns))
    if missing:
        raise ValueError(f"{name} is missing column(s): {', '.join(missing)}")
    if expected_rows is not None and len(frame) != expected_rows:
        raise ValueError(f"{name} has {len(frame)} rows; expected {expected_rows}.")
    return frame


def read_npz(name: str, required_keys: Iterable[str]) -> dict[str, np.ndarray]:
    path = PLOT_DATA_DIR / name
    if not path.exists():
        raise FileNotFoundError(f"Missing plot-ready file: {path}")
    archive = np.load(path, allow_pickle=True)
    missing = sorted(set(required_keys).difference(archive.files))
    if missing:
        raise ValueError(f"{name} is missing key(s): {', '.join(missing)}")
    return {key: archive[key] for key in archive.files}


def save_figure(fig: plt.Figure, stem: str, width_cm: float, height_cm: float, style: Mapping[str, object]) -> None:
    for directory in (EPS_DIR, SVG_DIR, PNG_DIR):
        directory.mkdir(parents=True, exist_ok=True)
    width_in = cm_to_inches(width_cm)
    height_in = cm_to_inches(height_cm)
    dpi = int(round(float(style["png_dpi"])))
    background = str(style["background_color"])
    fig.set_size_inches(width_in, height_in, forward=True)
    fig.set_dpi(dpi)
    fig.patch.set_facecolor(background)
    eps_path = EPS_DIR / f"{stem}.eps"
    svg_path = SVG_DIR / f"{stem}.svg"
    png_path = PNG_DIR / f"{stem}.png"
    temporary_png = PNG_DIR / f".{stem}.temporary.png"
    options = {"facecolor": background, "transparent": False, "bbox_inches": None, "pad_inches": 0}
    with mpl.rc_context({"savefig.bbox": None, "savefig.pad_inches": 0}):
        with warnings.catch_warnings():
            warnings.filterwarnings("ignore", message="The PostScript backend does not support transparency.*")
            fig.savefig(eps_path, format="eps", dpi=dpi, **options)
        fig.savefig(svg_path, format="svg", **options)
        fig.savefig(temporary_png, format="png", dpi=dpi, **options)
    plt.close(fig)
    expected_size = (round(width_in * dpi), round(height_in * dpi))
    with Image.open(temporary_png) as image:
        rgb = image.convert("RGB")
        if any(abs(observed - expected) > 12 for observed, expected in zip(rgb.size, expected_size)):
            raise AssertionError(f"Unexpected PNG size for {stem}: {rgb.size}; expected {expected_size}.")
        if rgb.size != expected_size:
            source_left = max((rgb.width - expected_size[0]) // 2, 0)
            source_top = max((rgb.height - expected_size[1]) // 2, 0)
            cropped = rgb.crop(
                (
                    source_left,
                    source_top,
                    source_left + min(rgb.width, expected_size[0]),
                    source_top + min(rgb.height, expected_size[1]),
                )
            )
            canvas = Image.new("RGB", expected_size, background)
            canvas.paste(
                cropped,
                (
                    max((expected_size[0] - cropped.width) // 2, 0),
                    max((expected_size[1] - cropped.height) // 2, 0),
                ),
            )
            rgb = canvas
        rgb.save(png_path, format="PNG", dpi=(dpi, dpi))
    temporary_png.unlink(missing_ok=True)
    with Image.open(png_path) as image:
        if image.mode != "RGB" or image.size != expected_size:
            raise AssertionError(f"Final PNG validation failed: {png_path}")
        recorded_dpi = image.info.get("dpi")
        if not recorded_dpi or any(abs(float(value) - dpi) > 1 for value in recorded_dpi[:2]):
            raise AssertionError(f"Final PNG lacks {dpi}-dpi metadata: {png_path}")
    for path in (eps_path, svg_path):
        if not path.exists() or path.stat().st_size == 0:
            raise AssertionError(f"Final vector figure was not written: {path}")
    print(f"[PASS] combined figure written: {stem} ({width_cm:g} × {height_cm:g} cm)")


def style_axis(ax: plt.Axes, grid_axis: str | None = None) -> None:
    for spine in ax.spines.values():
        spine.set_visible(True)
        spine.set_linewidth(0.65)
        spine.set_color("0.25")
    ax.tick_params(length=2.3, width=0.65)
    if grid_axis:
        ax.grid(axis=grid_axis, linestyle="--", linewidth=0.42, color="0.88")
        ax.set_axisbelow(True)
    else:
        ax.grid(False)


def cm_rect(fig_width_cm: float, fig_height_cm: float, x_cm: float, y_cm: float, width_cm: float, height_cm: float) -> list[float]:
    return [
        x_cm / fig_width_cm,
        y_cm / fig_height_cm,
        width_cm / fig_width_cm,
        height_cm / fig_height_cm,
    ]


def add_axes_cm(
    fig: plt.Figure,
    fig_width_cm: float,
    fig_height_cm: float,
    x_cm: float,
    y_cm: float,
    width_cm: float,
    height_cm: float,
    *,
    projection: str | None = None,
) -> plt.Axes:
    return fig.add_axes(
        cm_rect(fig_width_cm, fig_height_cm, x_cm, y_cm, width_cm, height_cm),
        projection=projection,
    )


def add_text_cm(
    fig: plt.Figure,
    fig_width_cm: float,
    fig_height_cm: float,
    x_cm: float,
    y_cm: float,
    text: str,
    **kwargs: object,
) -> mpl.text.Text:
    return fig.text(x_cm / fig_width_cm, y_cm / fig_height_cm, text, **kwargs)


def raincloud_panel(ax: plt.Axes, frame: pd.DataFrame, style: Mapping[str, object], panel_order: int) -> None:
    subset = frame.loc[frame["panel_order"].eq(panel_order)].copy()
    indicator = str(subset["indicator"].iloc[0])
    unit = str(subset["unit"].iloc[0])
    colours = {"Non-BE": str(style["color_non_be"]), "BE": str(style["color_be"])}
    positions = np.arange(2, dtype=float)
    for group_index, group in enumerate(GROUP_ORDER):
        values = subset.loc[subset["bes_group"].eq(group), "value"].dropna().to_numpy(float)
        colour = colours[group]
        fill = blend_with_white(colour, 0.84)
        box_position = positions[group_index]
        violin_position = positions[group_index]
        ax.boxplot(
            values,
            positions=[box_position],
            widths=0.16,
            patch_artist=True,
            showfliers=False,
            notch=True,
            medianprops={"color": "black", "linewidth": 1.15},
            boxprops={"facecolor": fill, "edgecolor": colour, "linewidth": 0.72},
            whiskerprops={"color": colour, "linewidth": 0.72},
            capprops={"color": colour, "linewidth": 0.72},
        )
        violin = ax.violinplot(values, positions=[violin_position], widths=0.50, showmeans=False, showmedians=False, showextrema=False)
        for body in violin["bodies"]:
            body.set_facecolor(fill)
            body.set_edgecolor(colour)
            body.set_linewidth(0.62)
            body.set_alpha(1.0)
            vertices = body.get_paths()[0].vertices
            vertices[:, 0] = np.where(vertices[:, 0] > violin_position, vertices[:, 0], violin_position)
        rng = np.random.default_rng(RAIN_JITTER_SEEDS[panel_order] + group_index)
        ax.scatter(
            rng.normal(positions[group_index] - 0.16, 0.035, len(values)),
            values,
            s=4.4,
            color=blend_with_white(colour, 0.18),
            edgecolors="white",
            linewidths=0.20,
            zorder=3,
        )
    ax.set_xticks(positions)
    ax.set_xticklabels(GROUP_ORDER)
    ax.set_ylabel(f"{indicator} ({unit})", labelpad=2)
    ax.set_xlim(-0.52, 1.52)
    ax.tick_params(axis="y", pad=1.5)
    style_axis(ax)


def ranking_panel(ax: plt.Axes, frame: pd.DataFrame) -> list[Line2D]:
    ordered = frame.sort_values("display_order").reset_index(drop=True)
    y_positions = np.arange(len(ordered))
    handles: list[Line2D] = []
    for column, label, colour, marker, size in RANKING_SERIES:
        ax.scatter(
            ordered[column],
            y_positions,
            s=size,
            edgecolor="black",
            linewidth=0.40,
            facecolor=colour,
            marker=marker,
            alpha=RANKING_ALPHA,
            zorder=RANKING_ZORDER[column],
        )
        handles.append(
            Line2D(
                [],
                [],
                marker=marker,
                linestyle="none",
                markerfacecolor=colour,
                markeredgecolor="black",
                markeredgewidth=0.40,
                markersize=4.4 if marker != "*" else 4.7,
                alpha=RANKING_ALPHA,
                label=label,
            )
        )
    ax.set_yticks(y_positions)
    ax.set_yticklabels(ordered["feature_name"].tolist())
    ax.invert_yaxis()
    ax.set_xlabel("Rank (lower is better)")
    maximum = int(ordered[[item[0] for item in RANKING_SERIES]].to_numpy().max())
    tick_max = maximum if maximum % 2 else maximum + 1
    ax.set_xticks(np.arange(1, tick_max + 1, 2))
    ax.set_xlim(0.5, tick_max + 0.7)
    ax.tick_params(axis="y", pad=2.5)
    style_axis(ax, "x")
    return handles


def radar_panel(
    ax: plt.Axes,
    frame: pd.DataFrame,
    *,
    radial_label_angle: float = 55.0,
) -> list[mpl.artist.Artist]:
    ordered = frame.sort_values("display_order").reset_index(drop=True)
    importance = ordered["normalized_cross_model_mean_abs_shap"].to_numpy(float)
    direction = ordered["directional_consistency_abs_mean_sign"].to_numpy(float)
    theta = np.linspace(0, 2 * np.pi, len(ordered), endpoint=False)
    common = float(mpl.rcParams["xtick.labelsize"])
    ax.set_theta_offset(np.pi / 2)
    ax.set_theta_direction(-1)
    ax.spines["polar"].set_visible(False)
    circular_theta = np.linspace(0, 2 * np.pi, 512)
    for radius in (0.25, 0.50, 0.75):
        ax.plot(circular_theta, np.full_like(circular_theta, radius), color="0.82", linewidth=0.50)
    ax.plot(circular_theta, np.ones_like(circular_theta), color="0.25", linewidth=0.60)
    for angle in theta:
        ax.plot([angle, angle], [0, 1], color="0.78", linewidth=0.40, linestyle="--")
    ax.bar(
        theta,
        np.clip(importance, 0, 1),
        width=(2 * np.pi / len(ordered)) * 0.28,
        bottom=0,
        color="#B69AC9",
        edgecolor="#7B3294",
        linewidth=0.55,
        zorder=3,
    )
    ax.plot(np.r_[theta, theta[0]], np.r_[direction, direction[0]], color="#008EC4", linewidth=1.00, zorder=5)
    ax.set_ylim(0, 1.13)
    ax.set_xticks([])
    ax.set_yticks([0.25, 0.50, 0.75, 1.00])
    ax.set_yticklabels(["0.25", "0.50", "0.75", "1.00"], fontsize=common, color="0.45")
    ax.set_rlabel_position(radial_label_angle)
    ax.tick_params(axis="y", pad=-6)
    for angle, row in zip(theta, ordered.itertuples(index=False)):
        degrees = np.degrees(angle)
        rotation = 180 - degrees
        if 90 < rotation % 360 < 270:
            rotation -= 180
        ax.text(
            angle,
            1.095,
            f"{row.display_name} ({row.direction_symbol})",
            rotation=rotation,
            rotation_mode="anchor",
            ha="center",
            va="center",
            fontsize=common,
            fontweight="normal",
        )
    return [
        Patch(facecolor="#B69AC9", edgecolor="#7B3294", label="Normalized mean |SHAP|"),
        Line2D([], [], color="#008EC4", linewidth=1.00, label="Directional consistency"),
    ]


def build_figure1(style: Mapping[str, object]) -> plt.Figure:
    raincloud = read_csv("_fig1_raincloud.csv", {"bes_group", "indicator", "unit", "value", "panel_order"}, 366)
    ranking = read_csv(
        "_fig1_feature_ranking.csv",
        {"feature_name", "overall_rank", "fdr_rank", "l1_rank", "xgboost_rank", "display_order"},
        8,
    )
    radar_all = read_csv(
        "_fig1_radar_all_features_top8.csv",
        {"display_name", "direction_symbol", "normalized_cross_model_mean_abs_shap", "directional_consistency_abs_mean_sign", "display_order"},
        8,
    )
    radar_top4 = read_csv(
        "_fig1_radar_fixed_top4.csv",
        {"display_name", "direction_symbol", "normalized_cross_model_mean_abs_shap", "directional_consistency_abs_mean_sign", "display_order"},
        4,
    )

    width_cm, height_cm = 17.37, 9.75
    fig = plt.figure()
    unit_starts = (0.30, 5.985, 11.67)
    frame_x_offset = 1.13
    frame_width = 4.27
    top_y, top_height = 6.18, 3.06
    lower_y, lower_height = 1.35, 4.02
    tag_gap = 0.14

    for index, unit_x in enumerate(unit_starts):
        frame_x = unit_x + frame_x_offset
        ax = add_axes_cm(fig, width_cm, height_cm, frame_x, top_y, frame_width, top_height)
        raincloud_panel(ax, raincloud, style, index + 1)
        add_text_cm(
            fig, width_cm, height_cm, unit_x, top_y + top_height + tag_gap, "abc"[index],
            ha="left", va="bottom", fontsize=float(style["panel_letter_size_pt"]), fontweight="bold",
        )

    ranking_x = unit_starts[0] + 1.55
    ranking_width = 3.85
    ranking_ax = add_axes_cm(fig, width_cm, height_cm, ranking_x, lower_y, ranking_width, lower_height)
    ranking_handles = ranking_panel(ranking_ax, ranking)

    radar_diameter = 4.45 * (13.0 / 12.0) * (15.0 / 16.0)
    radar_y = lower_y + lower_height - radar_diameter
    radar_x_offset = (5.40 - radar_diameter) / 2
    radar_all_x = unit_starts[1] + radar_x_offset
    radar_top4_x = unit_starts[2] + radar_x_offset
    radar_all_ax = add_axes_cm(fig, width_cm, height_cm, radar_all_x, radar_y, radar_diameter, radar_diameter, projection="polar")
    radar_handles = radar_panel(radar_all_ax, radar_all, radial_label_angle=125.0)
    radar_top4_ax = add_axes_cm(fig, width_cm, height_cm, radar_top4_x, radar_y, radar_diameter, radar_diameter, projection="polar")
    radar_panel(radar_top4_ax, radar_top4)

    tag_y = lower_y + lower_height + tag_gap
    for tag, tag_x in zip("def", unit_starts):
        add_text_cm(
            fig, width_cm, height_cm, tag_x, tag_y, tag,
            ha="left", va="bottom", fontsize=float(style["panel_letter_size_pt"]), fontweight="bold",
        )

    ranking_legend_width = 6.75
    ranking_legend_ax = add_axes_cm(fig, width_cm, height_cm, 0.30, 0.12, ranking_legend_width, 0.56)
    ranking_legend_ax.axis("off")
    ranking_legend_ax.legend(
        handles=ranking_handles, loc="center", bbox_to_anchor=((ranking_x + ranking_width / 2 - 0.30) / ranking_legend_width, 0.50),
        ncol=4, frameon=False, handletextpad=0.12, columnspacing=0.35, borderaxespad=0.0,
        fontsize=float(style["combined_legend_size_pt"]),
    )

    radar_legend_ax = add_axes_cm(fig, width_cm, height_cm, 7.05, 0.12, 10.02, 0.56)
    radar_legend_ax.axis("off")
    radar_legend_ax.legend(
        handles=radar_handles, loc="center", ncol=2, frameon=False,
        handlelength=1.20, handletextpad=0.12, columnspacing=0.35, borderaxespad=0.0,
        fontsize=float(style["combined_legend_size_pt"]),
    )
    return fig

def followup_matrix_panel(ax: plt.Axes, legend_ax: plt.Axes, frame: pd.DataFrame, style: Mapping[str, object]) -> None:
    group_encoded = frame["bes_group"].map({"Non-BE": 0, "BE": 1}).astype(int).to_numpy()[:, None]
    bes = frame["bes_score"].to_numpy(float)[:, None]
    availability = frame[["baseline_available", "month_1_attended", "month_3_attended", "month_6_attended"]].to_numpy(int)
    widths = (0.655, 0.655, 1.31, 1.31, 1.31, 1.31)
    x_left = np.r_[0.0, np.cumsum(widths[:-1])]
    total_width = float(sum(widths))
    n = len(frame)
    base_colours = {"Non-BE": str(style["color_non_be"]), "BE": str(style["color_be"])}
    group_colours = [blend_with_white(base_colours["Non-BE"], 0.65), blend_with_white(base_colours["BE"], 0.65)]
    group_cmap = ListedColormap(group_colours)
    bes_cmap = plt.colormaps["Purples"]
    availability_cmap = ListedColormap(["#E0E0E0", "#303030"])
    bes_norm = TwoSlopeNorm(vmin=0, vcenter=17, vmax=46)
    ax.imshow(group_encoded, extent=[x_left[0], x_left[0] + widths[0], 0, n], cmap=group_cmap, vmin=0, vmax=1, origin="upper", interpolation="none", aspect="auto")
    ax.imshow(bes, extent=[x_left[1], x_left[1] + widths[1], 0, n], cmap=bes_cmap, norm=bes_norm, origin="upper", interpolation="none", aspect="auto")
    for index in range(2, 6):
        ax.imshow(availability[:, [index - 2]], extent=[x_left[index], x_left[index] + widths[index], 0, n], cmap=availability_cmap, vmin=0, vmax=1, origin="upper", interpolation="none", aspect="auto")
    ax.set_xlim(0, total_width)
    ax.set_ylim(0, n)
    ax.set_xticks([x_left[index] + widths[index] / 2 for index in range(6)])
    ax.set_xticklabels(["Group", "BES score", "Baseline status", "M1 status", "M3 status", "M6 status"], rotation=45, ha="right", rotation_mode="anchor")
    ax.tick_params(axis="x", pad=2)
    ax.set_yticks([])
    for x_position in [*x_left, total_width]:
        ax.axvline(x_position, color="white", linewidth=0.70)
    style_axis(ax)
    legend_ax.axis("off")
    items = ((group_colours[0], "Non-BE"), (group_colours[1], "BE"), ("#303030", "Available"), ("#E0E0E0", "Missing"))
    common = float(style["combined_tick_size_pt"])
    x0, box_size, step, top_center = 0.02, 0.085, 0.145, 0.92
    for index, (colour, label) in enumerate(items):
        center = top_center - index * step
        legend_ax.add_patch(Rectangle((x0, center - box_size / 2), box_size, box_size, transform=legend_ax.transAxes, facecolor=colour, edgecolor="black", linewidth=0.50))
        legend_ax.text(x0 + box_size + 0.045, center, label, va="center", fontsize=common)
    legend_ax.text(x0, 0.27, "BES score", va="center", fontsize=common)
    colourbar_ax = legend_ax.inset_axes([x0, 0.035, box_size, 0.185])
    colourbar = plt.colorbar(mpl.cm.ScalarMappable(norm=bes_norm, cmap=bes_cmap), cax=colourbar_ax, orientation="vertical")
    colourbar.set_ticks([0, 17, 46])
    colourbar.set_ticklabels(["0", "17", "46"])
    colourbar.ax.tick_params(width=0.50, length=2.3, labelsize=common)
    colourbar.outline.set_linewidth(0.50)


def bes_histogram_panel(ax: plt.Axes, frame: pd.DataFrame) -> None:
    maximum = int(frame["bes_score"].max())
    counts, _, _ = ax.hist(frame["bes_score"], bins=np.arange(-0.5, maximum + 1.5, 1.0), color="#B7D7E8", edgecolor="black", linewidth=0.50)
    if int(counts.sum()) != len(frame):
        raise AssertionError("BES histogram does not include all participants.")
    ax.set_xlabel("BES score")
    ax.set_ylabel("Number of participants")
    ax.set_xticks(np.arange(0, maximum + 1, 4))
    ax.set_xlim(-0.5, maximum + 0.5)
    ax.set_ylim(0, int(np.nanmax(counts)) + 1)
    ax.yaxis.set_major_locator(MaxNLocator(integer=True))
    style_axis(ax, "y")


def build_figure_s1(style: Mapping[str, object]) -> plt.Figure:
    availability = read_csv(
        "_figs1_followup_availability.csv",
        {"bes_group", "bes_score", "baseline_available", "month_1_attended", "month_3_attended", "month_6_attended"},
        122,
    )
    distribution = read_csv("_figs1_bes_distribution.csv", {"bes_score", "bes_group"}, 122)

    width_cm, height_cm = 17.37, 6.10
    fig = plt.figure()
    panel_a_x, panel_b_x = 0.30, 8.885
    frame_y, frame_height = 1.55, 3.66

    panel_a_content_shift = 0.25
    matrix_ax = add_axes_cm(fig, width_cm, height_cm, panel_a_x + panel_a_content_shift, frame_y, 6.55, frame_height)
    matrix_legend_ax = add_axes_cm(fig, width_cm, height_cm, panel_a_x + panel_a_content_shift + 6.70, frame_y, 1.485, frame_height)
    histogram_x = panel_b_x + 1.03
    histogram_ax = add_axes_cm(fig, width_cm, height_cm, histogram_x, frame_y, 7.155, frame_height)

    followup_matrix_panel(matrix_ax, matrix_legend_ax, availability, style)
    bes_histogram_panel(histogram_ax, distribution)

    tag_y = frame_y + frame_height + 0.14
    add_text_cm(
        fig, width_cm, height_cm, panel_a_x, tag_y, "a",
        ha="left", va="bottom", fontsize=float(style["panel_letter_size_pt"]), fontweight="bold",
    )
    add_text_cm(
        fig, width_cm, height_cm, panel_b_x, tag_y, "b",
        ha="left", va="bottom", fontsize=float(style["panel_letter_size_pt"]), fontweight="bold",
    )
    return fig

def nice_symmetric_limit(values: np.ndarray, minimum_step: float = 0.05) -> tuple[float, float]:
    finite = np.asarray(values, dtype=float)
    finite = finite[np.isfinite(finite)]
    maximum = float(np.max(np.abs(finite))) if finite.size else minimum_step
    step = minimum_step if maximum <= 0.5 else (0.1 if maximum <= 1.0 else 0.25)
    bound = math.ceil((maximum * 1.06) / step) * step
    return -bound, bound


def permutation_beeswarm_offsets(values: np.ndarray, step: float = 0.05, max_push: float = 0.25) -> np.ndarray:
    values = np.asarray(values, dtype=float)
    order = np.argsort(values)
    offsets = np.zeros_like(values, dtype=float)
    slots: list[tuple[float, float]] = []
    for index in order:
        x_value = values[index]
        y_value = 0.0
        attempt = 0
        tried = {0.0}
        placed = False
        while abs(y_value) <= max_push + 1e-9:
            conflict = any(abs(y_value - prior_y) < step and abs(x_value - prior_x) < 0.015 for prior_y, prior_x in slots)
            if not conflict:
                placed = True
                break
            attempt += 1
            candidate = (attempt // 2 + 1) * step * (1 if attempt % 2 else -1)
            while candidate in tried:
                attempt += 1
                candidate = (attempt // 2 + 1) * step * (1 if attempt % 2 else -1)
            y_value = candidate
            tried.add(y_value)
        offsets[index] = y_value if placed else 0.0
        slots.append((offsets[index], x_value))
    return offsets


def permutation_panel(ax: plt.Axes, frame: pd.DataFrame, xlim: tuple[float, float]) -> None:
    ordered = frame[["feature", "display_name", "display_order"]].drop_duplicates().sort_values("display_order")
    line_colour, band_colour = "#4C78A8", "#DCE6F1"
    for row_index, row in enumerate(ordered.itertuples(index=False)):
        rows = frame.loc[frame["feature"].eq(row.feature)]
        values = rows["delta_auroc"].to_numpy(float)
        mean = float(rows["mean_delta_auroc"].iloc[0])
        low = float(rows["interval_low"].iloc[0])
        high = float(rows["interval_high"].iloc[0])
        ax.fill_betweenx([row_index - 0.25, row_index + 0.25], low, high, color=band_colour, zorder=1)
        ax.hlines(row_index, low, high, color=line_colour, linewidth=1.45, zorder=3)
        ax.plot([mean, mean], [row_index - 0.18, row_index + 0.18], color=line_colour, linewidth=1.10, solid_capstyle="round", zorder=4)
        ax.scatter(values, row_index + permutation_beeswarm_offsets(values), s=4.2, facecolor=line_colour, edgecolor="none", zorder=2)
    ax.set_yticks(np.arange(len(ordered)))
    ax.set_yticklabels(ordered["display_name"].tolist())
    ax.invert_yaxis()
    ax.set_xlim(xlim)
    ax.set_xlabel("ΔAUROC (baseline − permuted)", labelpad=3)
    ax.axvline(0, color="0.35", linewidth=0.60, linestyle=(0, (2, 2)), zorder=1)
    ax.tick_params(axis="y", length=0, pad=2.5)
    style_axis(ax, "x")


def shap_xlim(archives: Sequence[Mapping[str, np.ndarray]]) -> tuple[float, float]:
    values: list[np.ndarray] = []
    for archive in archives:
        names = [str(value) for value in archive["feature_names"].tolist()]
        lookup = {name: position for position, name in enumerate(names)}
        display = [str(value) for value in archive["display_features_top_to_bottom"].tolist()]
        values.append(archive["shap_values"][:, [lookup[name] for name in display]].ravel())
    return nice_symmetric_limit(np.concatenate(values))


def shap_panel(swarm_ax: plt.Axes, bar_ax: plt.Axes, archive: Mapping[str, np.ndarray], xlim: tuple[float, float]) -> None:
    features = [str(value) for value in archive["feature_names"].tolist()]
    display_names = [str(value) for value in archive["display_names"].tolist()]
    display_lookup = dict(zip(features, display_names))
    top_to_bottom = [str(value) for value in archive["display_features_top_to_bottom"].tolist()]
    bottom_to_top = list(reversed(top_to_bottom))
    feature_index = {feature: index for index, feature in enumerate(features)}
    indices = [feature_index[feature] for feature in bottom_to_top]
    shap_values = np.asarray(archive["shap_values"], dtype=float)
    raw_values = np.asarray(archive["feature_values"], dtype=float)
    feature_names = [display_lookup[feature] for feature in bottom_to_top]
    shap_frame = pd.DataFrame(shap_values[:, indices], columns=bottom_to_top)
    raw_frame = pd.DataFrame(raw_values[:, indices], columns=bottom_to_top)
    np.random.seed(SHAP_RANDOM_SEED)
    plt.sca(swarm_ax)
    with warnings.catch_warnings():
        warnings.filterwarnings("ignore", message="The NumPy global RNG was seeded.*", category=FutureWarning)
        warnings.filterwarnings("ignore", message="This figure includes Axes that are not compatible with tight_layout.*", category=UserWarning)
        shap.summary_plot(
            shap_frame.iloc[:, ::-1].to_numpy(dtype=float),
            features=raw_frame.iloc[:, ::-1],
            feature_names=list(reversed(feature_names)),
            plot_type="dot",
            cmap="coolwarm",
            color_bar=False,
            show=False,
            sort=False,
            max_display=len(bottom_to_top),
            plot_size=None,
        )
    for collection in swarm_ax.collections:
        if hasattr(collection, "get_offsets") and hasattr(collection, "set_sizes"):
            offsets = collection.get_offsets()
            if len(offsets):
                collection.set_sizes(np.full(len(offsets), 5.5))
    swarm_ax.set_xlim(xlim)
    swarm_ax.set_xlabel("SHAP value (impact on model output)", labelpad=3, fontsize=float(mpl.rcParams["axes.labelsize"]))
    swarm_ax.axvline(0, color="0.55", linewidth=0.60, linestyle=(0, (2, 2)), zorder=1)
    swarm_ax.set_yticks(np.arange(len(feature_names)))
    swarm_ax.set_yticklabels(feature_names, fontsize=float(mpl.rcParams["ytick.labelsize"]))
    swarm_ax.tick_params(axis="x", labelsize=float(mpl.rcParams["xtick.labelsize"]))
    style_axis(swarm_ax)
    swarm_ax.yaxis.set_ticks_position("left")
    swarm_ax.tick_params(axis="y", which="major", left=True, right=False, length=3.0, width=0.65, direction="out", pad=2)
    swarm_ax.spines["right"].set_visible(False)
    mean_abs = np.mean(np.abs(shap_values), axis=0)[indices]
    y_positions = np.arange(len(feature_names))
    bar_ax.barh(y_positions, mean_abs, height=0.60, facecolor="none", edgecolor="black", linewidth=0.70, zorder=2)
    maximum = max(float(np.max(mean_abs)), 1e-12)
    gap = maximum * 0.045
    bar_ax.set_xlim(0, maximum * 1.35)
    bar_ax.set_ylim(swarm_ax.get_ylim())
    bar_ax.set_xticks([])
    bar_ax.tick_params(axis="y", left=False, labelleft=False)
    for spine in bar_ax.spines.values():
        spine.set_visible(False)
    bar_ax.spines["left"].set_visible(True)
    bar_ax.spines["left"].set_linewidth(0.65)
    for y_value, value in enumerate(mean_abs):
        bar_ax.text(value + gap, y_value, f"{value:.2f}", ha="left", va="center", fontsize=float(mpl.rcParams["ytick.labelsize"]))


def build_figure_s2(style: Mapping[str, object]) -> plt.Figure:
    permutation = read_csv(
        "_figs2_permutation_importance.csv",
        {"panel_id", "algorithm", "feature_set", "feature", "display_name", "display_order", "delta_auroc", "mean_delta_auroc", "interval_low", "interval_high"},
        1200,
    )
    required_npz = {"panel_id", "algorithm", "feature_set", "shap_values", "feature_values", "feature_names", "display_names", "display_features_top_to_bottom"}
    archives = [
        read_npz("_figs2_shap_lightgbm_all_features.npz", required_npz),
        read_npz("_figs2_shap_catboost_all_features.npz", required_npz),
        read_npz("_figs2_shap_lightgbm_fixed_top4.npz", required_npz),
        read_npz("_figs2_shap_catboost_fixed_top4.npz", required_npz),
    ]
    archive_by_panel = {str(archive["panel_id"].item()): archive for archive in archives}
    all_xlim = shap_xlim([archive_by_panel["b"], archive_by_panel["d"]])
    top4_xlim = shap_xlim([archive_by_panel["f"], archive_by_panel["h"]])
    permutation_xlim = nice_symmetric_limit(permutation["delta_auroc"].to_numpy(float))

    width_cm, height_cm = 17.37, 15.5
    fig = plt.figure()
    model_label_x = 0.575
    left_unit_x = 1.00
    right_unit_x = 9.05
    perm_x, perm_width = left_unit_x + 1.45, 4.75 * (10.0 / 9.0)
    original_swarm_x, original_swarm_width = right_unit_x + 1.25, 3.90
    swarm_width = original_swarm_width * (9.0 / 8.0)
    swarm_x = original_swarm_x - (swarm_width - original_swarm_width)
    bar_x, bar_width = right_unit_x + 5.15, 1.15
    mean_label_x = right_unit_x + 6.70
    colour_group_shift = 0.18
    colour_x, colour_width = right_unit_x + 6.95 + colour_group_shift, 0.20
    feature_label_x = right_unit_x + 7.32 + colour_group_shift

    row_specs = (
        ("a", "b", "LightGBM\nall features", 11.40, 3.20),
        ("c", "d", "CatBoost\nall features", 7.00, 3.20),
        ("e", "f", "LightGBM\nfixed Top-4", 4.00, 1.60),
        ("g", "h", "CatBoost\nfixed Top-4", 1.00, 1.60),
    )
    common = float(style["combined_tick_size_pt"])
    tag_gap = 0.14

    for perm_id, shap_id, row_label, frame_y, frame_height in row_specs:
        add_text_cm(
            fig, width_cm, height_cm, model_label_x, frame_y + frame_height / 2, row_label,
            rotation=90, ha="center", va="center", fontsize=common, fontweight="normal", linespacing=1.25,
        )

        perm_ax = add_axes_cm(fig, width_cm, height_cm, perm_x, frame_y, perm_width, frame_height)
        perm_frame = permutation.loc[permutation["panel_id"].eq(perm_id)].copy()
        permutation_panel(perm_ax, perm_frame, permutation_xlim)

        swarm_ax = add_axes_cm(fig, width_cm, height_cm, swarm_x, frame_y, swarm_width, frame_height)
        bar_ax = add_axes_cm(fig, width_cm, height_cm, bar_x, frame_y, bar_width, frame_height)
        shap_panel(
            swarm_ax, bar_ax, archive_by_panel[shap_id],
            all_xlim if shap_id in {"b", "d"} else top4_xlim,
        )
        swarm_ax.set_position(cm_rect(width_cm, height_cm, swarm_x, frame_y, swarm_width, frame_height))
        bar_ax.set_position(cm_rect(width_cm, height_cm, bar_x, frame_y, bar_width, frame_height))

        add_text_cm(
            fig, width_cm, height_cm, mean_label_x, frame_y + frame_height / 2, "mean |SHAP|",
            rotation=-90, ha="center", va="center", fontsize=common, fontweight="normal",
        )

        colour_height = frame_height * (0.65 if shap_id in {"b", "d"} else 1.0833333333)
        colour_y = frame_y + (frame_height - colour_height) / 2
        colour_ax = add_axes_cm(fig, width_cm, height_cm, colour_x, colour_y, colour_width, colour_height)
        colourbar = fig.colorbar(
            ScalarMappable(norm=Normalize(0, 1), cmap="coolwarm"),
            cax=colour_ax, orientation="vertical",
        )
        colourbar.outline.set_visible(False)
        colourbar.set_ticks([])
        add_text_cm(fig, width_cm, height_cm, colour_x + colour_width / 2, colour_y + colour_height + 0.10, "High", ha="center", va="bottom", fontsize=common)
        add_text_cm(fig, width_cm, height_cm, colour_x + colour_width / 2, colour_y - 0.10, "Low", ha="center", va="top", fontsize=common)
        add_text_cm(fig, width_cm, height_cm, feature_label_x, frame_y + frame_height / 2, "Feature value", rotation=90, ha="center", va="center", fontsize=common)

        add_text_cm(
            fig, width_cm, height_cm, left_unit_x, frame_y + frame_height + tag_gap, perm_id,
            ha="left", va="bottom", fontsize=float(style["panel_letter_size_pt"]), fontweight="bold",
        )
        shap_tag_x = right_unit_x - (swarm_width - original_swarm_width)
        add_text_cm(
            fig, width_cm, height_cm, shap_tag_x, frame_y + frame_height + tag_gap, shap_id,
            ha="left", va="bottom", fontsize=float(style["panel_letter_size_pt"]), fontweight="bold",
        )
    return fig

def main() -> None:
    style = load_style()
    require_font(str(style["font_family"]))
    configure_matplotlib(style)
    combined_width = float(style["combined_width_cm"])
    save_figure(build_figure1(style), "Fig1_preoperative_biomarkers_ml", combined_width, 9.75, style)
    save_figure(build_figure_s1(style), "FigS1_followup_bes", combined_width, 6.10, style)
    save_figure(build_figure_s2(style), "FigS2_permutation_shap", combined_width, 15.5, style)
    print("[PASS] Python final-figure assembly completed")


if __name__ == "__main__":
    main()
