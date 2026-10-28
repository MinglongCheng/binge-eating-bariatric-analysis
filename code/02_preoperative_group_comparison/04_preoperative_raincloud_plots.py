#!/usr/bin/env python3
"""Create standalone raincloud panels and plot-ready data for Figure 1.

Input:
    data/preoperative_group_comparison.csv
Outputs:
    outputs/02_preoperative_group_comparison/figure/Fig1a_raincloud_total_protein.png
    outputs/02_preoperative_group_comparison/figure/Fig1b_raincloud_ca.png
    outputs/02_preoperative_group_comparison/figure/Fig1c_raincloud_folate.png
    outputs/06_final_figures/_plot_data/_fig1_raincloud.csv
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib as mpl
import matplotlib.font_manager as font_manager
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np
import pandas as pd
from matplotlib.colors import to_rgb
from PIL import Image

PROJECT_DIR = Path(__file__).resolve().parents[2]
DATA_FILE = PROJECT_DIR / "data" / "preoperative_group_comparison.csv"
FIGURE_DIR = PROJECT_DIR / "outputs" / "02_preoperative_group_comparison" / "figure"
PLOT_DATA_DIR = PROJECT_DIR / "outputs" / "06_final_figures" / "_plot_data"
STYLE_MANIFEST = PROJECT_DIR / "code" / "06_figure_assembly" / "00_figure_style_manifest.csv"

GROUP_ORDER = ("Non-BE", "BE")
DEFAULT_STYLE: dict[str, object] = {
    "font_family": "Arial",
    "background_color": "white",
    "png_dpi": 600,
    "single_axis_title_size_pt": 10.0,
    "single_tick_size_pt": 9.0,
    "color_non_be": "#1B9E9A",
    "color_be": "#E59886",
}


@dataclass(frozen=True)
class PlotSpec:
    column: str
    indicator: str
    unit: str
    output_filename: str
    panel_order: int
    jitter_seed: int

    @property
    def y_label(self) -> str:
        return f"{self.indicator} ({self.unit})"


PLOT_SPECS: tuple[PlotSpec, ...] = (
    PlotSpec(
        "total_protein_g_l",
        "Total protein",
        "g/L",
        "Fig1a_raincloud_total_protein.png",
        1,
        52,
    ),
    PlotSpec(
        "calcium_mmol_l",
        "Ca",
        "mmol/L",
        "Fig1b_raincloud_ca.png",
        2,
        42,
    ),
    PlotSpec(
        "folate_ng_ml",
        "Folate",
        "ng/mL",
        "Fig1c_raincloud_folate.png",
        3,
        62,
    ),
)


def cm_to_inches(value_cm: float) -> float:
    return value_cm / 2.54


def load_style() -> dict[str, object]:
    style = DEFAULT_STYLE.copy()
    if not STYLE_MANIFEST.exists():
        print("[INFO] shared style manifest not found; using frozen module 02 defaults")
        return style

    manifest = pd.read_csv(STYLE_MANIFEST)
    if not {"Key", "Value"}.issubset(manifest.columns):
        raise ValueError(f"Style manifest must contain Key and Value columns: {STYLE_MANIFEST}")
    values = dict(zip(manifest["Key"].astype(str), manifest["Value"]))
    numeric_keys = {"png_dpi", "single_axis_title_size_pt", "single_tick_size_pt"}
    for key in style:
        if key not in values or pd.isna(values[key]):
            continue
        style[key] = float(values[key]) if key in numeric_keys else str(values[key])
    return style


def configure_matplotlib(style: dict[str, object]) -> None:
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
            "axes.labelsize": float(style["single_axis_title_size_pt"]),
            "xtick.labelsize": float(style["single_tick_size_pt"]),
            "ytick.labelsize": float(style["single_tick_size_pt"]),
            "axes.facecolor": str(style["background_color"]),
            "figure.facecolor": str(style["background_color"]),
            "savefig.facecolor": str(style["background_color"]),
            "savefig.transparent": False,
        }
    )
    print(f"[PASS] required font available: {font_family}")


def blend_with_white(colour: str, white_fraction: float = 0.84) -> str:
    rgb = np.asarray(to_rgb(colour), dtype=float)
    blended = rgb * (1.0 - white_fraction) + white_fraction
    return "#" + "".join(f"{round(channel * 255):02X}" for channel in blended)


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
    if not DATA_FILE.exists():
        raise FileNotFoundError(f"Input file not found: {DATA_FILE}")

    df = pd.read_csv(DATA_FILE)
    df.columns = [str(column).strip() for column in df.columns]
    required = {"participant_id", "bes_group", *(spec.column for spec in PLOT_SPECS)}
    missing = sorted(required.difference(df.columns))
    if missing:
        raise ValueError(f"Missing required column(s): {missing}")

    df["participant_id"] = pd.to_numeric(df["participant_id"], errors="coerce")
    if df["participant_id"].isna().any() or df["participant_id"].duplicated().any():
        raise ValueError("participant_id must be complete, numeric, and unique.")
    df["bes_group"] = normalise_group(df["bes_group"])
    for spec in PLOT_SPECS:
        df[spec.column] = pd.to_numeric(df[spec.column], errors="coerce")

    observed_groups = df["bes_group"].value_counts().to_dict()
    if len(df) != 122 or observed_groups != {"Non-BE": 66, "BE": 56}:
        raise AssertionError(
            f"Expected 122 participants (Non-BE=66, BE=56); observed {len(df)} and {observed_groups}."
        )
    return df.sort_values("participant_id", kind="mergesort").reset_index(drop=True)


def write_plot_ready_data(df: pd.DataFrame) -> pd.DataFrame:
    PLOT_DATA_DIR.mkdir(parents=True, exist_ok=True)
    participant_order = np.arange(1, len(df) + 1, dtype=int)
    frames = [
        pd.DataFrame(
            {
                "participant_order": participant_order,
                "bes_group": df["bes_group"].astype(str),
                "indicator": spec.indicator,
                "unit": spec.unit,
                "value": df[spec.column].astype(float),
                "panel_order": spec.panel_order,
            }
        )
        for spec in PLOT_SPECS
    ]
    plot_data = pd.concat(frames, ignore_index=True)
    output_path = PLOT_DATA_DIR / "_fig1_raincloud.csv"
    plot_data.to_csv(output_path, index=False, encoding="utf-8")
    print(f"[PASS] plot-ready data written: {output_path}")
    return plot_data


def save_rgb_png(
    fig: plt.Figure,
    output_path: Path,
    *,
    dpi: int,
    background_color: str,
    width_cm: float,
    height_cm: float,
) -> None:
    fig.savefig(output_path, dpi=dpi, facecolor=background_color, transparent=False)
    plt.close(fig)

    with Image.open(output_path) as image:
        if image.mode != "RGB":
            image.convert("RGB").save(output_path, dpi=(dpi, dpi))

    expected_size = (
        round(cm_to_inches(width_cm) * dpi),
        round(cm_to_inches(height_cm) * dpi),
    )
    with Image.open(output_path) as image:
        if image.mode != "RGB":
            raise AssertionError(f"Expected RGB PNG, found {image.mode}: {output_path}")
        if any(abs(observed - expected) > 1 for observed, expected in zip(image.size, expected_size)):
            raise AssertionError(
                f"Expected approximately {expected_size} pixels, found {image.size}: {output_path}"
            )
    print(f"[PASS] standalone figure written: {output_path}")


def plot_endpoint(
    plot_data: pd.DataFrame,
    spec: PlotSpec,
    style: dict[str, object],
) -> Path:
    positions = np.arange(len(GROUP_ORDER), dtype=float)
    box_width = 0.15
    violin_width = 0.50
    colours = {
        "Non-BE": str(style["color_non_be"]),
        "BE": str(style["color_be"]),
    }

    fig, ax = plt.subplots(
        figsize=(cm_to_inches(11.0), cm_to_inches(8.5)),
        constrained_layout=False,
    )
    fig.subplots_adjust(left=0.18, right=0.97, bottom=0.16, top=0.96)
    panel_data = plot_data.loc[plot_data["panel_order"].eq(spec.panel_order)]

    for group_index, group in enumerate(GROUP_ORDER):
        values = (
            panel_data.loc[panel_data["bes_group"].eq(group), "value"]
            .dropna()
            .to_numpy(dtype=float)
        )
        if len(values) < 2:
            raise ValueError(f"Insufficient values for {spec.column} in group {group}.")

        colour = colours[group]
        fill_colour = blend_with_white(colour)
        box_position = positions[group_index] - box_width / 100.0
        violin_position = positions[group_index] + box_width / 50.0

        ax.boxplot(
            values,
            positions=[box_position],
            widths=box_width,
            patch_artist=True,
            showfliers=False,
            notch=True,
            medianprops={"color": "black", "linewidth": 2.0},
            boxprops={"facecolor": fill_colour, "edgecolor": colour, "linewidth": 1.05},
            whiskerprops={"color": colour, "linewidth": 1.05},
            capprops={"color": colour, "linewidth": 1.05},
        )

        violin = ax.violinplot(
            values,
            positions=[violin_position],
            widths=violin_width,
            showmeans=False,
            showmedians=False,
            showextrema=False,
        )
        for body in violin["bodies"]:
            body.set_facecolor(fill_colour)
            body.set_edgecolor(colour)
            body.set_linewidth(0.90)
            body.set_alpha(1.0)
            vertices = body.get_paths()[0].vertices
            vertices[:, 0] = np.where(
                vertices[:, 0] > violin_position,
                vertices[:, 0],
                violin_position,
            )

        rng = np.random.default_rng(spec.jitter_seed + group_index)
        jittered_x = rng.normal(
            loc=positions[group_index] - box_width,
            scale=0.04,
            size=len(values),
        )
        ax.scatter(
            jittered_x,
            values,
            s=8,
            color=colour,
            alpha=0.82,
            edgecolors="white",
            linewidths=0.35,
            zorder=3,
        )

    ax.set_xticks(positions)
    ax.set_xticklabels(GROUP_ORDER)
    ax.set_ylabel(spec.y_label)
    ax.yaxis.set_major_formatter(ticker.FormatStrFormatter("%.1f"))
    ax.grid(False)
    ax.margins(x=0.08)

    for spine in ax.spines.values():
        spine.set_visible(True)
        spine.set_linewidth(0.8)
        spine.set_color("#333333")

    FIGURE_DIR.mkdir(parents=True, exist_ok=True)
    output_path = FIGURE_DIR / spec.output_filename
    save_rgb_png(
        fig,
        output_path,
        dpi=int(style["png_dpi"]),
        background_color=str(style["background_color"]),
        width_cm=11.0,
        height_cm=8.5,
    )
    return output_path


def main() -> None:
    style = load_style()
    configure_matplotlib(style)
    df = load_data()
    plot_data = write_plot_ready_data(df)

    print("[PASS] sample check: 122 participants; Non-BE=66; BE=56")
    for spec in PLOT_SPECS:
        plot_endpoint(plot_data, spec, style)
    print("[PASS] deterministic endpoint-specific jitter retained")
    print("[PASS] module 02 raincloud workflow completed")


if __name__ == "__main__":
    main()
