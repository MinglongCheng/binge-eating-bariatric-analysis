#!/usr/bin/env python3
"""Create held-out permutation, SHAP, and directional-consistency outputs.

The saved-model interpretation methods are unchanged: each model is matched to
its true outer-test fold, each feature is permuted 30 times per fold, SHAP is
computed only for held-out participant-repeat observations, and directional
consistency uses the Spearman-sign rule described in METHODS.md.
"""

from __future__ import annotations

from pathlib import Path
import warnings

import lightgbm as lgb
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib import font_manager
from matplotlib.cm import ScalarMappable
from matplotlib.colors import Normalize
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
import numpy as np
import pandas as pd
from PIL import Image
import shap
from catboost import CatBoostClassifier, Pool
from scipy.stats import spearmanr
from sklearn.metrics import roc_auc_score

PROJECT_DIR = Path(__file__).resolve().parents[2]
DATA_FILE = PROJECT_DIR / "data" / "preoperative_group_comparison.csv"
OUTPUT_DIR = PROJECT_DIR / "outputs" / "03_preoperative_machine_learning"
SPLIT_FILE = OUTPUT_DIR / "splits" / "_outer_cv_splits.csv"
TABLE_S7_FILE = OUTPUT_DIR / "table" / "TableS7_feature_ranking.xlsx"
LGB_MODEL_DIR = OUTPUT_DIR / "models" / "lightgbm"
CAT_MODEL_DIR = OUTPUT_DIR / "models" / "catboost"
FIGURE_DIR = OUTPUT_DIR / "figure"
PLOT_DATA_DIR = PROJECT_DIR / "outputs" / "06_final_figures" / "_plot_data"
STYLE_MANIFEST = PROJECT_DIR / "code" / "06_figure_assembly" / "00_figure_style_manifest.csv"

OUTER_SPLITS = 5
OUTER_REPEATS = 10
RANDOM_SEED = 42
PERMUTATION_REPEATS = 30
SIGN_TOLERANCE = 0.05
ALGORITHMS = ("lightgbm", "catboost")

FEATURES = [
    "wbc_10e9_l",
    "neutrophils_10e9_l",
    "lymphocytes_10e9_l",
    "monocytes_10e9_l",
    "eosinophils_10e9_l",
    "basophils_10e9_l",
    "rbc_10e12_l",
    "hemoglobin_g_l",
    "fasting_plasma_glucose_mmol_l",
    "hba1c_percent",
    "total_protein_g_l",
    "alt_u_l",
    "ast_u_l",
    "ggt_u_l",
    "urea_mmol_l",
    "creatinine_umol_l",
    "uric_acid_umol_l",
    "total_cholesterol_mmol_l",
    "hdl_c_mmol_l",
    "ldl_c_mmol_l",
    "potassium_mmol_l",
    "sodium_mmol_l",
    "chloride_mmol_l",
    "calcium_mmol_l",
    "iron_umol_l",
    "folate_ng_ml",
    "vitamin_b12_pg_ml",
    "vitamin_d_25oh_ng_ml",
]
DISPLAY_NAME = {
    "wbc_10e9_l": "WBC",
    "neutrophils_10e9_l": "Neutrophils",
    "lymphocytes_10e9_l": "Lymphocytes",
    "monocytes_10e9_l": "Monocytes",
    "eosinophils_10e9_l": "Eosinophils",
    "basophils_10e9_l": "Basophils",
    "rbc_10e12_l": "RBC",
    "hemoglobin_g_l": "Hb",
    "fasting_plasma_glucose_mmol_l": "FPG",
    "hba1c_percent": "HbA1c",
    "total_protein_g_l": "Total protein",
    "alt_u_l": "ALT",
    "ast_u_l": "AST",
    "ggt_u_l": "GGT",
    "urea_mmol_l": "Urea",
    "creatinine_umol_l": "Creatinine",
    "uric_acid_umol_l": "Uric acid",
    "total_cholesterol_mmol_l": "Total cholesterol",
    "hdl_c_mmol_l": "HDL-C",
    "ldl_c_mmol_l": "LDL-C",
    "potassium_mmol_l": "K",
    "sodium_mmol_l": "Na",
    "chloride_mmol_l": "Cl",
    "calcium_mmol_l": "Ca",
    "iron_umol_l": "Fe",
    "folate_ng_ml": "Folate",
    "vitamin_b12_pg_ml": "Vitamin B12",
    "vitamin_d_25oh_ng_ml": "25(OH)D",
}
DISPLAY_TO_FEATURE = {display: feature for feature, display in DISPLAY_NAME.items()}
FEATURE_ORDER = {feature: index for index, feature in enumerate(FEATURES)}

DEFAULT_STYLE = {
    "font_family": "Arial",
    "background_color": "white",
    "png_dpi": 600,
    "single_axis_title_size_pt": 10.0,
    "single_tick_size_pt": 9.0,
    "single_legend_size_pt": 9.0,
}


def load_style() -> dict[str, object]:
    style = DEFAULT_STYLE.copy()
    if not STYLE_MANIFEST.exists():
        print("[INFO] shared style manifest not found; using frozen module 03 defaults")
        return style

    manifest = pd.read_csv(STYLE_MANIFEST)
    columns = {str(column).strip().lower(): column for column in manifest.columns}
    if not {"key", "value"}.issubset(columns):
        raise ValueError(
            f"Style manifest must contain Key and Value columns: {STYLE_MANIFEST}"
        )
    values = dict(
        zip(
            manifest[columns["key"]].astype(str).str.strip(),
            manifest[columns["value"]],
        )
    )
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


def require_font(font_family: str) -> None:
    try:
        font_manager.findfont(font_family, fallback_to_default=False)
    except ValueError as exc:
        raise RuntimeError(
            f"Required font {font_family!r} is unavailable. Install it before running."
        ) from exc
    print(f"[PASS] required font available: {font_family}")


def save_rgb_png(
    fig: plt.Figure,
    path: Path,
    width_cm: float,
    height_cm: float,
    dpi: int,
    background_color: str = "white",
) -> None:
    fig.set_size_inches(width_cm / 2.54, height_cm / 2.54, forward=True)
    fig.savefig(path, dpi=dpi, facecolor=background_color, transparent=False)
    plt.close(fig)
    with Image.open(path) as image:
        if image.mode != "RGB":
            image.convert("RGB").save(path, dpi=(dpi, dpi))

    expected = (round(width_cm / 2.54 * dpi), round(height_cm / 2.54 * dpi))
    with Image.open(path) as image:
        if image.mode != "RGB" or any(
            abs(observed - target) > 2
            for observed, target in zip(image.size, expected)
        ):
            raise AssertionError(
                f"Unexpected PNG properties for {path}: {image.mode}, {image.size}"
            )
    print(f"[PASS] standalone figure written: {path}")

def normalise_group(series: pd.Series) -> pd.Series:
    values = series.astype("string").str.strip().str.lower()
    mapped = values.map(
        {"non-be": 0, "non_be": 0, "nonbe": 0, "non be": 0, "be": 1}
    )
    unknown = sorted(values[mapped.isna() & values.notna()].dropna().unique().tolist())
    if unknown:
        raise ValueError(f"Unrecognised bes_group value(s): {unknown}")
    return mapped.astype("Int64")


def load_data() -> tuple[pd.DataFrame, pd.DataFrame, pd.Series]:
    if not DATA_FILE.exists():
        raise FileNotFoundError(f"Input file not found: {DATA_FILE}")
    df = pd.read_csv(DATA_FILE)
    df.columns = [str(column).strip() for column in df.columns]
    required = {"participant_id", "bes_group", *FEATURES}
    missing = sorted(required.difference(df.columns))
    if missing:
        raise ValueError(f"Missing required column(s): {missing}")

    df["participant_id"] = pd.to_numeric(df["participant_id"], errors="coerce")
    if df["participant_id"].isna().any() or df["participant_id"].duplicated().any():
        raise ValueError("participant_id must be complete, numeric, and unique.")
    y = normalise_group(df["bes_group"])
    if y.isna().any():
        raise ValueError("bes_group must be complete.")
    X = df[FEATURES].apply(pd.to_numeric, errors="coerce")

    order = np.argsort(df["participant_id"].to_numpy(), kind="mergesort")
    df = df.iloc[order].reset_index(drop=True)
    X = X.iloc[order].reset_index(drop=True)
    y = y.iloc[order].astype(int).reset_index(drop=True)

    counts = y.value_counts().sort_index().to_dict()
    if len(df) != 122 or counts != {0: 66, 1: 56}:
        raise AssertionError(
            f"Expected N=122 with counts {{0: 66, 1: 56}}; observed N={len(df)}, {counts}."
        )
    return df, X, y


def load_splits(participant_ids: pd.Series) -> pd.DataFrame:
    if not SPLIT_FILE.exists():
        raise FileNotFoundError(
            f"Missing split file: {SPLIT_FILE}\nRun 01_feature_ranking.py first."
        )
    splits = pd.read_csv(SPLIT_FILE)
    required = {"participant_id", "repeat_id", "fold_id", "outer_fold_id"}
    missing = required.difference(splits.columns)
    if missing:
        raise ValueError(f"Split file is missing columns: {sorted(missing)}")
    for column in required:
        splits[column] = pd.to_numeric(splits[column], errors="raise").astype(int)
    if len(splits) != len(participant_ids) * OUTER_REPEATS:
        raise AssertionError("Unexpected split-manifest row count.")
    if set(splits["participant_id"]) != set(participant_ids.astype(int)):
        raise AssertionError("Split participants differ from data participants.")
    if splits.groupby(["participant_id", "repeat_id"]).size().ne(1).any():
        raise AssertionError("Each participant must occur once per repeat.")
    return splits


def load_fixed_top4() -> list[str]:
    if not TABLE_S7_FILE.exists():
        raise FileNotFoundError(
            f"Missing Table S7: {TABLE_S7_FILE}\nRun 01_feature_ranking.py first."
        )
    table = pd.read_excel(TABLE_S7_FILE, sheet_name="Table S7")
    if "Feature" not in table.columns or len(table) < 4:
        raise ValueError("Table S7 does not contain a usable Feature column.")
    top4: list[str] = []
    for display in table["Feature"].iloc[:4].astype(str):
        if display not in DISPLAY_TO_FEATURE:
            raise ValueError(f"Cannot map Table S7 feature name: {display!r}")
        top4.append(DISPLAY_TO_FEATURE[display])
    if len(top4) != 4 or len(set(top4)) != 4:
        raise AssertionError("Fixed Top-4 must contain four unique variables.")
    return top4


def test_indices_for_outer_fold(
    df: pd.DataFrame, splits: pd.DataFrame, outer_fold_id: int
) -> tuple[np.ndarray, int, int]:
    rows = splits.loc[splits["outer_fold_id"].eq(outer_fold_id)]
    if rows.empty:
        raise AssertionError(f"No test assignment for outer fold {outer_fold_id}.")
    repeat_values = rows["repeat_id"].unique()
    fold_values = rows["fold_id"].unique()
    if len(repeat_values) != 1 or len(fold_values) != 1:
        raise AssertionError("Each outer_fold_id must map to one repeat and one fold.")
    repeat_id = int(repeat_values[0])
    fold_id = int(fold_values[0])
    test_ids = set(rows["participant_id"].astype(int))
    indices = np.flatnonzero(df["participant_id"].astype(int).isin(test_ids).to_numpy())
    if len(indices) not in {24, 25}:
        raise AssertionError("Unexpected outer-test size.")
    return indices, repeat_id, fold_id


# ---------------------------------------------------------------------------
# Saved-model access
# ---------------------------------------------------------------------------


def model_path(panel: str, algo: str, repeat_id: int, fold_id: int) -> Path:
    if panel not in {"all_features", "fixed_top4"}:
        raise ValueError(panel)
    if algo == "lightgbm":
        return LGB_MODEL_DIR / (
            f"{panel}_lightgbm_repeat{repeat_id:02d}_fold{fold_id:02d}.txt"
        )
    if algo == "catboost":
        return CAT_MODEL_DIR / (
            f"{panel}_catboost_repeat{repeat_id:02d}_fold{fold_id:02d}.cbm"
        )
    raise ValueError(algo)


def load_model(
    panel: str,
    algo: str,
    repeat_id: int,
    fold_id: int,
    features: list[str],
) -> object:
    path = model_path(panel, algo, repeat_id, fold_id)
    if not path.exists():
        raise FileNotFoundError(
            f"Missing model: {path}\nRun 02_train_nested_cv_models.py first."
        )
    if algo == "lightgbm":
        model = lgb.Booster(model_file=str(path))
        observed = list(model.feature_name())
    else:
        model = CatBoostClassifier()
        model.load_model(str(path), format="cbm")
        observed = list(model.feature_names_)
    if observed != features:
        raise AssertionError(
            f"Feature order in {path.name} differs from the expected {panel} panel.\n"
            f"Expected: {features}\nObserved: {observed}"
        )
    return model


def predict_probability(
    algo: str, model: object, X: pd.DataFrame, features: list[str]
) -> np.ndarray:
    if algo == "lightgbm":
        return np.asarray(model.predict(X[features]), dtype=float)
    return np.asarray(model.predict_proba(X[features])[:, 1], dtype=float)


def native_shap_values(
    algo: str, model: object, X: pd.DataFrame, features: list[str]
) -> np.ndarray:
    if algo == "lightgbm":
        values = np.asarray(model.predict(X[features], pred_contrib=True), dtype=float)
    else:
        values = np.asarray(
            model.get_feature_importance(Pool(X[features]), type="ShapValues"),
            dtype=float,
        )
    expected = (len(X), len(features) + 1)
    if values.shape != expected:
        raise AssertionError(
            f"Unexpected SHAP matrix shape for {algo}/{features}: "
            f"{values.shape}; expected {expected}."
        )
    return values[:, :-1]


# ---------------------------------------------------------------------------
# Interpretation calculations
# ---------------------------------------------------------------------------


def calculate_panel_interpretation(
    panel: str,
    features: list[str],
    df: pd.DataFrame,
    X: pd.DataFrame,
    y: pd.Series,
    splits: pd.DataFrame,
) -> tuple[
    dict[str, dict[str, list[float]]],
    dict[str, np.ndarray],
    dict[str, np.ndarray],
    dict[str, dict[str, list[float]]],
    pd.DataFrame,
    pd.DataFrame,
]:
    permutation = {
        algo: {feature: [] for feature in features} for algo in ALGORITHMS
    }
    shap_chunks = {algo: [] for algo in ALGORITHMS}
    raw_chunks = {algo: [] for algo in ALGORITHMS}
    direction_signs = {
        algo: {feature: [] for feature in features} for algo in ALGORITHMS
    }
    heldout_rows: list[dict[str, int]] = []
    fold_rows: list[dict[str, int]] = []

    expected_folds = OUTER_SPLITS * OUTER_REPEATS
    for outer_fold_id in range(expected_folds):
        test_indices, repeat_id, fold_id = test_indices_for_outer_fold(
            df, splits, outer_fold_id
        )
        X_test = X.iloc[test_indices].copy()
        y_test = y.iloc[test_indices].to_numpy(dtype=int)
        fold_rows.append(
            {"outer_fold_id": outer_fold_id, "repeat_id": repeat_id, "fold_id": fold_id}
        )
        for participant_id in df.iloc[test_indices]["participant_id"].astype(int):
            heldout_rows.append(
                {
                    "participant_id": int(participant_id),
                    "repeat_id": repeat_id,
                    "fold_id": fold_id,
                    "outer_fold_id": outer_fold_id,
                }
            )

        for algo_index, algo in enumerate(ALGORITHMS):
            model = load_model(panel, algo, repeat_id, fold_id, features)
            baseline_probability = predict_probability(algo, model, X_test, features)
            baseline_auc = float(roc_auc_score(y_test, baseline_probability))

            for feature_index, feature in enumerate(features):
                deltas = np.empty(PERMUTATION_REPEATS, dtype=float)
                for permutation_id in range(PERMUTATION_REPEATS):
                    rng = np.random.default_rng(
                        RANDOM_SEED * 10_000_000
                        + (0 if panel == "all_features" else 1) * 1_000_000
                        + algo_index * 100_000
                        + outer_fold_id * 1_000
                        + feature_index * PERMUTATION_REPEATS
                        + permutation_id
                    )
                    X_permuted = X_test.copy()
                    X_permuted[feature] = rng.permutation(
                        X_permuted[feature].to_numpy(copy=True)
                    )
                    permuted_probability = predict_probability(
                        algo, model, X_permuted, features
                    )
                    deltas[permutation_id] = baseline_auc - float(
                        roc_auc_score(y_test, permuted_probability)
                    )
                permutation[algo][feature].append(float(np.mean(deltas)))

            shap_values = native_shap_values(algo, model, X_test, features)
            shap_chunks[algo].append(shap_values)
            raw_chunks[algo].append(X_test[features].to_numpy(dtype=float))

            for feature_index, feature in enumerate(features):
                raw = X_test[feature].to_numpy(dtype=float)
                shap_feature = shap_values[:, feature_index]
                valid = np.isfinite(raw) & np.isfinite(shap_feature)
                if (
                    valid.sum() < 3
                    or np.unique(raw[valid]).size < 2
                    or np.unique(shap_feature[valid]).size < 2
                ):
                    direction_signs[algo][feature].append(np.nan)
                    continue
                rho = float(spearmanr(raw[valid], shap_feature[valid]).statistic)
                if not np.isfinite(rho):
                    direction_signs[algo][feature].append(np.nan)
                elif abs(rho) < SIGN_TOLERANCE:
                    direction_signs[algo][feature].append(0.0)
                else:
                    direction_signs[algo][feature].append(float(np.sign(rho)))

            print(
                f"[interpret] {panel} | outer {outer_fold_id + 1:02d}/{expected_folds:02d} | "
                f"repeat {repeat_id}, fold {fold_id} | {algo} | "
                f"baseline AUROC={baseline_auc:.3f}"
            )

    shap_all = {algo: np.vstack(shap_chunks[algo]) for algo in ALGORITHMS}
    raw_all = {algo: np.vstack(raw_chunks[algo]) for algo in ALGORITHMS}
    expected_shape = (len(df) * OUTER_REPEATS, len(features))
    for algo in ALGORITHMS:
        if shap_all[algo].shape != expected_shape:
            raise AssertionError(
                f"Unexpected pooled SHAP shape for {panel}/{algo}: "
                f"{shap_all[algo].shape}; expected {expected_shape}."
            )
        if raw_all[algo].shape != expected_shape:
            raise AssertionError("Raw-feature and SHAP matrices must align.")
        if any(len(permutation[algo][feature]) != expected_folds for feature in features):
            raise AssertionError(
                f"Each permutation feature must have {expected_folds} fold-level values."
            )
    heldout_map = pd.DataFrame(heldout_rows)
    fold_map = pd.DataFrame(fold_rows)
    if len(heldout_map) != len(df) * OUTER_REPEATS or len(fold_map) != expected_folds:
        raise AssertionError("Held-out mapping does not align with interpretation arrays.")
    return permutation, shap_all, raw_all, direction_signs, heldout_map, fold_map

def summarize_shap_and_direction(
    features: list[str],
    shap_all: dict[str, np.ndarray],
    direction_signs: dict[str, dict[str, list[float]]],
) -> tuple[
    dict[str, dict[str, float]],
    dict[str, float],
    dict[str, float],
    dict[str, float],
    dict[str, dict[str, float]],
    dict[str, dict[str, int]],
]:
    mean_abs_by_algo: dict[str, dict[str, float]] = {}
    normalized_by_algo: dict[str, dict[str, float]] = {}
    direction_by_algo: dict[str, dict[str, float]] = {}
    valid_fold_counts: dict[str, dict[str, int]] = {}

    for algo in ALGORITHMS:
        mean_abs = np.mean(np.abs(shap_all[algo]), axis=0)
        mean_abs_by_algo[algo] = {
            feature: float(mean_abs[index]) for index, feature in enumerate(features)
        }
        maximum = float(np.max(mean_abs))
        normalized_by_algo[algo] = {
            feature: (mean_abs_by_algo[algo][feature] / maximum if maximum > 0 else 0.0)
            for feature in features
        }
        direction_by_algo[algo] = {}
        valid_fold_counts[algo] = {}
        for feature in features:
            values = np.asarray(direction_signs[algo][feature], dtype=float)
            valid = values[np.isfinite(values)]
            valid_fold_counts[algo][feature] = int(valid.size)
            direction_by_algo[algo][feature] = (
                float(np.mean(valid)) if valid.size else np.nan
            )

    cross_importance = {
        feature: float(
            np.mean([normalized_by_algo[algo][feature] for algo in ALGORITHMS])
        )
        for feature in features
    }
    overall_direction = {
        feature: float(
            np.mean([direction_by_algo[algo][feature] for algo in ALGORITHMS])
        )
        for feature in features
    }
    absolute_direction = {
        feature: abs(overall_direction[feature]) for feature in features
    }
    return mean_abs_by_algo, cross_importance, overall_direction, absolute_direction, direction_by_algo, valid_fold_counts


def permutation_beeswarm_offsets(
    x: np.ndarray, step: float = 0.05, max_push: float = 0.25
) -> np.ndarray:
    values = np.asarray(x, dtype=float)
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
            conflict = any(
                abs(y_value - prior_y) < step and abs(x_value - prior_x) < 0.015
                for prior_y, prior_x in slots
            )
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


def nice_xlim(values: np.ndarray, pad: float, step: float, symmetric: bool) -> tuple[float, float]:
    finite = np.asarray(values, dtype=float)
    finite = finite[np.isfinite(finite)]
    if finite.size == 0:
        finite = np.asarray([-0.1, 0.1])
    low, high = float(np.min(finite)), float(np.max(finite))
    if symmetric:
        maximum = np.ceil((max(abs(low), abs(high)) + pad) / step) * step
        return -float(maximum), float(maximum)
    return (
        float(np.floor((min(low, 0.0) - pad) / step) * step),
        float(np.ceil((max(high, 0.0) + pad) / step) * step),
    )


def permutation_summary(values: list[float]) -> tuple[float, float, float]:
    array = np.asarray(values, dtype=float)
    return (
        float(np.mean(array)),
        float(np.quantile(array, 0.025)),
        float(np.quantile(array, 0.975)),
    )


def ordered_permutation_features(
    fold_values: dict[str, list[float]], features: list[str]
) -> list[str]:
    return sorted(
        features,
        key=lambda feature: (
            -abs(permutation_summary(fold_values[feature])[0]),
            FEATURE_ORDER[feature],
        ),
    )


def plot_permutation_panel(
    fold_values: dict[str, list[float]],
    ordered_features: list[str],
    output: Path,
    title: str,
    xlim: tuple[float, float],
    width_cm: float,
    height_cm: float,
    style: dict[str, object],
) -> None:
    font = str(style["font_family"])
    dpi = int(float(style["png_dpi"]))
    axis_size = float(style["single_axis_title_size_pt"])
    tick_size = float(style["single_tick_size_pt"])
    plt.rcParams.update(
        {
            "font.family": font,
            "axes.labelsize": axis_size,
            "xtick.labelsize": tick_size,
            "ytick.labelsize": tick_size,
        }
    )
    fig, ax = plt.subplots()
    line_colour = "#4C78A8"
    band_colour = "#DCE6F1"
    for row_index, feature in enumerate(ordered_features):
        values = np.asarray(fold_values[feature], dtype=float)
        mean, low, high = permutation_summary(values.tolist())
        ax.fill_betweenx(
            [row_index - 0.25, row_index + 0.25],
            low,
            high,
            color=band_colour,
            zorder=1,
        )
        ax.hlines(row_index, low, high, color=line_colour, linewidth=2.6, zorder=3)
        ax.plot(
            [mean, mean],
            [row_index - 0.18, row_index + 0.18],
            color=line_colour,
            linewidth=2.0,
            solid_capstyle="round",
            zorder=4,
        )
        offsets = permutation_beeswarm_offsets(values)
        ax.scatter(
            values,
            row_index + offsets,
            s=16,
            facecolor=line_colour,
            edgecolor="none",
            zorder=2,
        )

    ax.set_yticks(np.arange(len(ordered_features)))
    ax.set_yticklabels([DISPLAY_NAME[feature] for feature in ordered_features])
    ax.invert_yaxis()
    ax.set_xlim(xlim)
    ax.set_xlabel("ΔAUROC (baseline − permuted)", labelpad=5)
    ax.axvline(0, color="0.35", linewidth=0.8, linestyle=(0, (2, 2)), zorder=1)
    ax.grid(axis="x", linestyle="--", linewidth=0.6, color="0.88")
    ax.tick_params(axis="y", length=0, pad=4)
    for spine in ax.spines.values():
        spine.set_linewidth(0.75)
        spine.set_color("0.25")

    left_cm, right_cm = 2.70, 0.70
    bottom_cm, plot_top_gap_cm = 1.20, 1.55
    ax.set_position(
        [
            left_cm / width_cm,
            bottom_cm / height_cm,
            (width_cm - left_cm - right_cm) / width_cm,
            (height_cm - plot_top_gap_cm - bottom_cm) / height_cm,
        ]
    )
    fig.text(
        0.5,
        (height_cm - 0.40) / height_cm,
        title,
        ha="center",
        va="center",
        fontsize=10.0,
    )
    fig.text(
        0.5,
        (height_cm - 0.92) / height_cm,
        "Outer-fold means with empirical 95% intervals",
        ha="center",
        va="center",
        fontsize=9.0,
    )
    save_rgb_png(fig, output, width_cm, height_cm, dpi)

def shap_display_features(
    shap_values: np.ndarray, panel_features: list[str], candidates: list[str]
) -> list[str]:
    mean_abs = pd.Series(np.mean(np.abs(shap_values), axis=0), index=panel_features)
    return sorted(
        candidates,
        key=lambda feature: (-float(mean_abs[feature]), FEATURE_ORDER[feature]),
    )


def shared_shap_xlim(specs: list[tuple[np.ndarray, list[str], list[str]]]) -> tuple[float, float]:
    values: list[np.ndarray] = []
    for shap_values, panel_features, display_features in specs:
        index = {feature: i for i, feature in enumerate(panel_features)}
        values.append(shap_values[:, [index[feature] for feature in display_features]].ravel())
    combined = np.concatenate(values)
    finite = combined[np.isfinite(combined)]
    maximum = float(np.max(np.abs(finite))) if finite.size else 0.1
    step = 0.05 if maximum <= 0.5 else (0.1 if maximum <= 1.0 else 0.25)
    return nice_xlim(finite, pad=0.04 * max(maximum, step), step=step, symmetric=True)


def plot_shap_panel(
    shap_values: np.ndarray,
    raw_values: np.ndarray,
    panel_features: list[str],
    display_features_top_to_bottom: list[str],
    output: Path,
    title: str,
    xlim: tuple[float, float],
    width_cm: float,
    height_cm: float,
    style: dict[str, object],
) -> None:
    feature_index = {feature: index for index, feature in enumerate(panel_features)}
    bottom_to_top = list(reversed(display_features_top_to_bottom))
    indices = [feature_index[feature] for feature in bottom_to_top]
    shap_frame = pd.DataFrame(shap_values[:, indices], columns=bottom_to_top)
    raw_frame = pd.DataFrame(raw_values[:, indices], columns=bottom_to_top)
    mean_abs_all = pd.Series(np.mean(np.abs(shap_values), axis=0), index=panel_features)
    mean_abs = mean_abs_all.loc[bottom_to_top].to_numpy(dtype=float)
    feature_names = [DISPLAY_NAME[feature] for feature in bottom_to_top]

    font = str(style["font_family"])
    dpi = int(float(style["png_dpi"]))
    axis_size = float(style["single_axis_title_size_pt"])
    tick_size = float(style["single_tick_size_pt"])
    plt.rcParams.update(
        {
            "font.family": font,
            "axes.labelsize": axis_size,
            "xtick.labelsize": tick_size,
            "ytick.labelsize": tick_size,
        }
    )

    fig = plt.figure()
    left_cm, swarm_width_cm = 3.15, 9.20
    panel_gap_cm, bar_width_cm = 0.0, 2.10
    right_cm = width_cm - left_cm - swarm_width_cm - panel_gap_cm - bar_width_cm
    bottom_cm, top_gap_cm = 1.20, 1.90
    plot_height_cm = height_cm - bottom_cm - top_gap_cm
    swarm_axis = fig.add_axes(
        [
            left_cm / width_cm,
            bottom_cm / height_cm,
            swarm_width_cm / width_cm,
            plot_height_cm / height_cm,
        ]
    )
    bar_axis = fig.add_axes(
        [
            (left_cm + swarm_width_cm + panel_gap_cm) / width_cm,
            bottom_cm / height_cm,
            bar_width_cm / width_cm,
            plot_height_cm / height_cm,
        ],
        sharey=swarm_axis,
    )

    np.random.seed(RANDOM_SEED)
    plt.sca(swarm_axis)
    with warnings.catch_warnings():
        warnings.filterwarnings(
            "ignore",
            message="The NumPy global RNG was seeded.*",
            category=FutureWarning,
        )
        warnings.filterwarnings(
            "ignore",
            message="This figure includes Axes that are not compatible with tight_layout.*",
            category=UserWarning,
        )
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

    swarm_axis.set_xlim(xlim)
    swarm_axis.set_xlabel("SHAP value (impact on model output)", labelpad=5)
    swarm_axis.axvline(0, color="0.55", linewidth=0.8, linestyle=(0, (2, 2)), zorder=1)
    swarm_axis.set_yticks(np.arange(len(feature_names)))
    swarm_axis.set_yticklabels(feature_names, fontsize=tick_size)
    swarm_axis.tick_params(axis="y", length=0, pad=6)
    for spine in swarm_axis.spines.values():
        spine.set_visible(True)
        spine.set_linewidth(0.75)
        spine.set_color("0.25")
    swarm_axis.spines["right"].set_visible(False)

    y_positions = np.arange(len(feature_names))
    bar_axis.barh(
        y_positions,
        mean_abs,
        height=0.60,
        facecolor="none",
        edgecolor="black",
        linewidth=1.0,
        zorder=2,
    )
    maximum = float(np.max(mean_abs)) if mean_abs.size else 1.0
    maximum = maximum if maximum > 0 else 1.0
    number_gap = maximum * 0.045
    bar_axis.set_xlim(0, maximum * 1.28)
    bar_axis.set_xticks([])
    bar_axis.tick_params(axis="y", left=False, labelleft=False)
    for spine in bar_axis.spines.values():
        spine.set_visible(False)
    bar_axis.spines["left"].set_visible(True)
    bar_axis.spines["left"].set_linewidth(0.75)
    for y_value, value in enumerate(mean_abs):
        bar_axis.text(
            value + number_gap,
            y_value,
            f"{value:.2f}",
            ha="left",
            va="center",
            fontsize=max(8.0, tick_size - 0.5),
            clip_on=False,
        )

    title_y_cm = height_cm - 0.35
    feature_title_y_cm = height_cm - 0.86
    colour_bar_center_cm = height_cm - 1.28
    colour_bar_height_cm = 0.22
    colour_left_cm = left_cm + 0.60
    colour_width_cm = swarm_width_cm - 1.20
    colour_axis = fig.add_axes(
        [
            colour_left_cm / width_cm,
            (colour_bar_center_cm - colour_bar_height_cm / 2) / height_cm,
            colour_width_cm / width_cm,
            colour_bar_height_cm / height_cm,
        ]
    )
    scalar = ScalarMappable(norm=Normalize(0, 1), cmap="coolwarm")
    scalar.set_array([])
    colour_bar = fig.colorbar(scalar, cax=colour_axis, orientation="horizontal")
    colour_bar.outline.set_visible(False)
    colour_bar.set_ticks([])

    header_center_x = (colour_left_cm + colour_width_cm / 2) / width_cm
    fig.text(
        header_center_x,
        title_y_cm / height_cm,
        title,
        ha="center",
        va="center",
        fontsize=10.0,
    )
    fig.text(
        header_center_x,
        feature_title_y_cm / height_cm,
        "Feature value",
        ha="center",
        va="center",
        fontsize=axis_size,
    )
    fig.text(
        (colour_left_cm - 0.45) / width_cm,
        colour_bar_center_cm / height_cm,
        "Low",
        ha="right",
        va="center",
        fontsize=tick_size,
    )
    fig.text(
        (colour_left_cm + colour_width_cm + 0.45) / width_cm,
        colour_bar_center_cm / height_cm,
        "High",
        ha="left",
        va="center",
        fontsize=tick_size,
    )
    bar_right_cm = left_cm + swarm_width_cm + panel_gap_cm + bar_width_cm
    fig.text(
        (bar_right_cm + 0.52) / width_cm,
        (bottom_cm + plot_height_cm / 2) / height_cm,
        "mean |SHAP value|",
        rotation=-90,
        ha="center",
        va="center",
        fontsize=axis_size,
    )

    if right_cm <= 0:
        raise AssertionError("SHAP layout exceeds the target canvas width.")
    save_rgb_png(fig, output, width_cm, height_cm, dpi)

def sign_label(value: float) -> str:
    if not np.isfinite(value):
        return "NA"
    if value > 1e-12:
        return "+"
    if value < -1e-12:
        return "−"
    return "0"


def radar_frame(
    features: list[str],
    cross_importance: dict[str, float],
    overall_direction: dict[str, float],
    absolute_direction: dict[str, float],
) -> pd.DataFrame:
    ordered = sorted(features, key=lambda feature: (-cross_importance[feature], FEATURE_ORDER[feature]))
    return pd.DataFrame({
        "feature": ordered,
        "display_name": [DISPLAY_NAME[feature] for feature in ordered],
        "direction_symbol": [sign_label(overall_direction[feature]) for feature in ordered],
        "normalized_cross_model_mean_abs_shap": [cross_importance[feature] for feature in ordered],
        "directional_consistency_abs_mean_sign": [absolute_direction[feature] for feature in ordered],
        "signed_mean_direction": [overall_direction[feature] for feature in ordered],
        "display_order": np.arange(1, len(ordered) + 1, dtype=int),
    })


def plot_radar(
    frame: pd.DataFrame,
    output: Path,
    title: str,
    width_cm: float,
    height_cm: float,
    style: dict[str, object],
    radial_label_angle: float = 67.0,
) -> None:
    ordered = frame["feature"].tolist()
    importance = frame["normalized_cross_model_mean_abs_shap"].to_numpy(dtype=float)
    direction = frame["directional_consistency_abs_mean_sign"].to_numpy(dtype=float)
    theta = np.linspace(0, 2 * np.pi, len(ordered), endpoint=False)
    font = str(style["font_family"])
    dpi = int(float(style["png_dpi"]))
    tick_size = float(style["single_tick_size_pt"])
    legend_size = float(style["single_legend_size_pt"])
    plt.rcParams.update(
        {
            "font.family": font,
            "xtick.labelsize": tick_size,
            "ytick.labelsize": tick_size,
            "legend.fontsize": legend_size,
        }
    )

    fig = plt.figure()
    group_height_cm = 10.0
    group_offset_cm = max((height_cm - group_height_cm) / 2, 0.0)
    radar_diameter_cm = 6.6
    radar_left_cm = (width_cm - radar_diameter_cm) / 2
    radar_bottom_cm = group_offset_cm + 1.80
    legend_center_cm = group_offset_cm + 0.62
    title_center_cm = group_offset_cm + 9.45
    axis = fig.add_axes(
        [
            radar_left_cm / width_cm,
            radar_bottom_cm / height_cm,
            radar_diameter_cm / width_cm,
            radar_diameter_cm / height_cm,
        ],
        projection="polar",
    )
    axis.set_theta_offset(np.pi / 2)
    axis.set_theta_direction(-1)
    axis.spines["polar"].set_visible(False)
    circular_theta = np.linspace(0, 2 * np.pi, 512)
    for radius in (0.25, 0.50, 0.75):
        axis.plot(
            circular_theta,
            np.full_like(circular_theta, radius),
            color="0.82",
            linewidth=0.65,
        )
    axis.plot(circular_theta, np.ones_like(circular_theta), color="0.25", linewidth=0.75)
    for angle in theta:
        axis.plot([angle, angle], [0, 1], color="0.76", linewidth=0.55, linestyle="--")
    width = (2 * np.pi / len(ordered)) * 0.28
    axis.bar(
        theta,
        np.clip(importance, 0, 1),
        width=width,
        bottom=0,
        color="#B69AC9",
        edgecolor="#7B3294",
        linewidth=0.7,
        zorder=3,
    )
    closed_theta = np.r_[theta, theta[0]]
    closed_direction = np.r_[direction, direction[0]]
    axis.plot(closed_theta, closed_direction, color="#008EC4", linewidth=1.5, zorder=5)
    axis.set_ylim(0, 1.12)
    axis.set_xticks([])
    axis.set_yticks([0.25, 0.50, 0.75, 1.00])
    axis.set_yticklabels(
        ["0.25", "0.50", "0.75", "1.00"],
        fontsize=max(8.0, tick_size - 0.5),
        color="0.45",
    )
    axis.set_rlabel_position(radial_label_angle)
    for angle, row in zip(theta, frame.itertuples(index=False)):
        degrees = np.degrees(angle)
        rotation = 180 - degrees
        if 90 < rotation % 360 < 270:
            rotation -= 180
        axis.text(
            angle,
            1.09,
            f"{row.display_name} ({row.direction_symbol})",
            rotation=rotation,
            rotation_mode="anchor",
            ha="center",
            va="center",
            fontsize=tick_size,
        )

    handles = [
        Patch(
            facecolor="#B69AC9",
            edgecolor="#7B3294",
            label="Normalized cross-model mean |SHAP|",
        ),
        Line2D(
            [],
            [],
            color="#008EC4",
            linewidth=1.5,
            label="Directional consistency (|mean sign|)",
        ),
    ]
    fig.legend(
        handles=handles,
        loc="center",
        ncol=1,
        frameon=False,
        bbox_to_anchor=(0.5, legend_center_cm / height_cm),
        handlelength=2.3,
        handletextpad=0.55,
        labelspacing=0.35,
        borderaxespad=0,
    )
    fig.text(
        0.5,
        title_center_cm / height_cm,
        title,
        ha="center",
        va="center",
        fontsize=10.0,
    )
    save_rgb_png(fig, output, width_cm, height_cm, dpi)

def write_radar_plot_data(
    frame: pd.DataFrame,
    filename: str,
    panel_id: str,
    feature_set: str,
) -> Path:
    output = PLOT_DATA_DIR / filename
    plot_data = frame.copy()
    plot_data.insert(0, "feature_set", feature_set)
    plot_data.insert(0, "panel_id", panel_id)
    plot_data.to_csv(output, index=False, encoding="utf-8", na_rep="NA")
    return output


def write_permutation_plot_data(
    specs: list[dict[str, object]],
    fold_map: pd.DataFrame,
) -> Path:
    rows: list[dict[str, object]] = []
    for spec in specs:
        fold_values = spec["fold_values"]
        for display_order, feature in enumerate(spec["ordered_features"], start=1):
            values = np.asarray(fold_values[feature], dtype=float)
            mean, low, high = permutation_summary(values.tolist())
            for fold_index, value in enumerate(values):
                meta = fold_map.iloc[fold_index]
                rows.append({
                    "panel_id": spec["panel_id"],
                    "panel_order": spec["panel_order"],
                    "algorithm": spec["algorithm_label"],
                    "feature_set": spec["feature_set_label"],
                    "feature": feature,
                    "display_name": DISPLAY_NAME[feature],
                    "display_order": display_order,
                    "outer_fold_id": int(meta["outer_fold_id"]),
                    "repeat_id": int(meta["repeat_id"]),
                    "fold_id": int(meta["fold_id"]),
                    "delta_auroc": float(value),
                    "mean_delta_auroc": mean,
                    "interval_low": low,
                    "interval_high": high,
                })
    output = PLOT_DATA_DIR / "_figs2_permutation_importance.csv"
    pd.DataFrame(rows).to_csv(output, index=False, encoding="utf-8")
    return output


def write_shap_npz(
    filename: str,
    panel_id: str,
    algorithm: str,
    feature_set: str,
    shap_values: np.ndarray,
    raw_values: np.ndarray,
    features: list[str],
    display_features: list[str],
    heldout_map: pd.DataFrame,
) -> Path:
    output = PLOT_DATA_DIR / filename
    unique_ids = sorted(heldout_map["participant_id"].astype(int).unique().tolist())
    participant_order = heldout_map["participant_id"].map(
        {participant_id: order for order, participant_id in enumerate(unique_ids, start=1)}
    )
    np.savez_compressed(
        output,
        panel_id=np.asarray(panel_id),
        algorithm=np.asarray(algorithm),
        feature_set=np.asarray(feature_set),
        shap_values=shap_values,
        feature_values=raw_values,
        feature_names=np.asarray(features, dtype="U64"),
        display_names=np.asarray([DISPLAY_NAME[feature] for feature in features], dtype="U64"),
        display_features_top_to_bottom=np.asarray(display_features, dtype="U64"),
        participant_order=participant_order.to_numpy(dtype=np.int16),
        repeat_id=heldout_map["repeat_id"].to_numpy(dtype=np.int16),
        fold_id=heldout_map["fold_id"].to_numpy(dtype=np.int16),
        outer_fold_id=heldout_map["outer_fold_id"].to_numpy(dtype=np.int16),
    )
    return output


def assert_model_inventory() -> None:
    expected = OUTER_SPLITS * OUTER_REPEATS
    checks = {
        "all-feature LightGBM": len(list(LGB_MODEL_DIR.glob("all_features_lightgbm_repeat*_fold*.txt"))),
        "fixed-Top-4 LightGBM": len(list(LGB_MODEL_DIR.glob("fixed_top4_lightgbm_repeat*_fold*.txt"))),
        "all-feature CatBoost": len(list(CAT_MODEL_DIR.glob("all_features_catboost_repeat*_fold*.cbm"))),
        "fixed-Top-4 CatBoost": len(list(CAT_MODEL_DIR.glob("fixed_top4_catboost_repeat*_fold*.cbm"))),
    }
    bad = {name: count for name, count in checks.items() if count != expected}
    if bad:
        raise FileNotFoundError(
            "Expected 50 models in each all-feature/fixed-Top-4 algorithm set; "
            f"observed {bad}. Run 02_train_nested_cv_models.py first."
        )


def main() -> None:
    style = load_style()
    require_font(str(style["font_family"]))
    for directory in (FIGURE_DIR, PLOT_DATA_DIR):
        directory.mkdir(parents=True, exist_ok=True)

    df, X, y = load_data()
    splits = load_splits(df["participant_id"])
    fixed_top4 = load_fixed_top4()
    assert_model_inventory()

    all_result = calculate_panel_interpretation("all_features", FEATURES, df, X, y, splits)
    top4_result = calculate_panel_interpretation("fixed_top4", fixed_top4, df, X, y, splits)
    all_perm, all_shap, all_raw, all_signs, all_map, all_fold_map = all_result
    top4_perm, top4_shap, top4_raw, top4_signs, top4_map, top4_fold_map = top4_result
    if not all_map.equals(top4_map) or not all_fold_map.equals(top4_fold_map):
        raise AssertionError("Held-out mappings differ between all-feature and fixed-Top-4 panels.")

    _, all_cross, all_direction, all_abs_direction, all_algo_direction, all_valid_counts = summarize_shap_and_direction(
        FEATURES, all_shap, all_signs
    )
    _, top4_cross, top4_direction, top4_abs_direction, top4_algo_direction, top4_valid_counts = summarize_shap_and_direction(
        fixed_top4, top4_shap, top4_signs
    )
    audit_rows: list[dict[str, object]] = []
    for panel, features, scores, counts, overall, absolute in (
        ("All features", FEATURES, all_algo_direction, all_valid_counts, all_direction, all_abs_direction),
        ("Fixed Top-4", fixed_top4, top4_algo_direction, top4_valid_counts, top4_direction, top4_abs_direction),
    ):
        for feature in features:
            audit_rows.append({
                "feature_set": panel,
                "feature": DISPLAY_NAME[feature],
                "lightgbm_valid_folds": counts["lightgbm"][feature],
                "lightgbm_direction_score": scores["lightgbm"][feature],
                "catboost_valid_folds": counts["catboost"][feature],
                "catboost_direction_score": scores["catboost"][feature],
                "overall_signed_direction": overall[feature],
                "absolute_directional_consistency": absolute[feature],
            })
    print("\nDirectional consistency audit:")
    print(pd.DataFrame(audit_rows).to_string(index=False, na_rep="NA", float_format=lambda value: f"{value:.6f}"))
    all_radar_features = sorted(FEATURES, key=lambda feature: (-all_cross[feature], FEATURE_ORDER[feature]))[:8]
    all_radar = radar_frame(all_radar_features, all_cross, all_direction, all_abs_direction)
    top4_radar = radar_frame(fixed_top4, top4_cross, top4_direction, top4_abs_direction)

    permutation_specs: list[dict[str, object]] = []
    for panel_id, panel_order, algo, values, candidates, feature_set_label, filename, title, size in (
        ("a", 1, "lightgbm", all_perm["lightgbm"], FEATURES, "All features", "FigS2a_permutation_lightgbm_all_features_top8.png", "LightGBM, all features", (14.0, 9.0)),
        ("c", 3, "catboost", all_perm["catboost"], FEATURES, "All features", "FigS2c_permutation_catboost_all_features_top8.png", "CatBoost, all features", (14.0, 9.0)),
        ("e", 5, "lightgbm", top4_perm["lightgbm"], fixed_top4, "Fixed Top-4", "FigS2e_permutation_lightgbm_fixed_top4.png", "LightGBM, fixed Top-4", (13.0, 7.5)),
        ("g", 7, "catboost", top4_perm["catboost"], fixed_top4, "Fixed Top-4", "FigS2g_permutation_catboost_fixed_top4.png", "CatBoost, fixed Top-4", (13.0, 7.5)),
    ):
        ordered = ordered_permutation_features(values, candidates)
        if feature_set_label == "All features":
            ordered = ordered[:8]
        permutation_specs.append({
            "panel_id": panel_id,
            "panel_order": panel_order,
            "algorithm": algo,
            "algorithm_label": "LightGBM" if algo == "lightgbm" else "CatBoost",
            "feature_set_label": feature_set_label,
            "fold_values": values,
            "ordered_features": ordered,
            "filename": filename,
            "title": title,
            "size": size,
        })
    x_values: list[float] = []
    for spec in permutation_specs:
        for feature in spec["ordered_features"]:
            values = spec["fold_values"][feature]
            mean, low, high = permutation_summary(values)
            x_values.extend(values)
            x_values.extend([mean, low, high])
    permutation_xlim = nice_xlim(np.asarray(x_values), pad=0.02, step=0.05, symmetric=False)
    for spec in permutation_specs:
        width_cm, height_cm = spec["size"]
        plot_permutation_panel(
            spec["fold_values"], spec["ordered_features"], FIGURE_DIR / spec["filename"],
            spec["title"], permutation_xlim, width_cm, height_cm, style,
        )

    all_display = {
        algo: shap_display_features(all_shap[algo], FEATURES, FEATURES)[:8]
        for algo in ALGORITHMS
    }
    top4_display = {
        algo: shap_display_features(top4_shap[algo], fixed_top4, fixed_top4)
        for algo in ALGORITHMS
    }
    all_xlim = shared_shap_xlim([
        (all_shap[algo], FEATURES, all_display[algo]) for algo in ALGORITHMS
    ])
    top4_xlim = shared_shap_xlim([
        (top4_shap[algo], fixed_top4, top4_display[algo]) for algo in ALGORITHMS
    ])
    shap_specs = (
        ("b", "lightgbm", "LightGBM", "All features", all_shap, all_raw, FEATURES, all_display, "FigS2b_shap_lightgbm_all_features_top8.png", "LightGBM, all features", all_xlim, 16.5, 9.5, "_figs2_shap_lightgbm_all_features.npz", all_map),
        ("d", "catboost", "CatBoost", "All features", all_shap, all_raw, FEATURES, all_display, "FigS2d_shap_catboost_all_features_top8.png", "CatBoost, all features", all_xlim, 16.5, 9.5, "_figs2_shap_catboost_all_features.npz", all_map),
        ("f", "lightgbm", "LightGBM", "Fixed Top-4", top4_shap, top4_raw, fixed_top4, top4_display, "FigS2f_shap_lightgbm_fixed_top4.png", "LightGBM, fixed Top-4", top4_xlim, 16.5, 7.5, "_figs2_shap_lightgbm_fixed_top4.npz", top4_map),
        ("h", "catboost", "CatBoost", "Fixed Top-4", top4_shap, top4_raw, fixed_top4, top4_display, "FigS2h_shap_catboost_fixed_top4.png", "CatBoost, fixed Top-4", top4_xlim, 16.5, 7.5, "_figs2_shap_catboost_fixed_top4.npz", top4_map),
    )
    shap_data_paths: list[Path] = []
    for panel_id, algo, algorithm_label, feature_set_label, shap_dict, raw_dict, features, display_dict, filename, title, xlim, width_cm, height_cm, npz_name, mapping in shap_specs:
        plot_shap_panel(
            shap_dict[algo], raw_dict[algo], features, display_dict[algo],
            FIGURE_DIR / filename, title, xlim, width_cm, height_cm, style,
        )
        shap_data_paths.append(
            write_shap_npz(
                npz_name,
                panel_id,
                algorithm_label,
                feature_set_label,
                shap_dict[algo],
                raw_dict[algo],
                features,
                display_dict[algo],
                mapping,
            )
        )

    plot_radar(all_radar, FIGURE_DIR / "Fig1e_radar_all_features_top8.png", "All-feature model: Top-8 SHAP features", 12.0, 12.0, style, radial_label_angle=70.0)
    plot_radar(top4_radar, FIGURE_DIR / "Fig1f_radar_fixed_top4.png", "Fixed Top-4 model", 12.0, 12.0, style)
    radar_all_path = write_radar_plot_data(
        all_radar,
        "_fig1_radar_all_features_top8.csv",
        panel_id="e",
        feature_set="All features",
    )
    radar_top4_path = write_radar_plot_data(
        top4_radar,
        "_fig1_radar_fixed_top4.csv",
        panel_id="f",
        feature_set="Fixed Top-4",
    )
    permutation_path = write_permutation_plot_data(permutation_specs, all_fold_map)

    print(f"[PASS] sample check: 122 participants; Non-BE=66; BE=56")
    print("[PASS] model inventory: LightGBM=100; CatBoost=100")
    print("[PASS] directional consistency audit displayed in terminal")
    print(f"[PASS] permutation plot-ready data written: {permutation_path}")
    print(f"[PASS] radar plot-ready data written: {radar_all_path}")
    print(f"[PASS] radar plot-ready data written: {radar_top4_path}")
    for path in shap_data_paths:
        print(f"[PASS] SHAP plot-ready data written: {path}")
    print(f"[PASS] standalone figures written: {FIGURE_DIR}")
    print("[PASS] module 03 interpretation workflow completed")


if __name__ == "__main__":
    main()
