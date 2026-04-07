#!/usr/bin/env python3
import os
import sys
import gc
import math
import argparse

import numpy as np
import uproot
import awkward as ak
import matplotlib.pyplot as plt
import joblib
import torch
from torch import nn

try:
    import bk_eff_selection as bg
except Exception as e:
    sys.stderr.write(f"[warn] Could not import bk_eff_selection (background module): {e}\n")
    bg = None

# ===== SHELL-PLOT NOTE START =====
# This script uses the same ANN/BDT feature ordering as write_roc_overlay_all3_fixed_order.py.
# It does NOT subtract binned histograms numerically after the fact.
# Instead, for stability, it builds the nested retention masks directly and then plots the shell:
#   shell(8e-5 -> 4e-5) = pass(8e-5) AND NOT pass(4e-5)
#   shell(4e-5 -> 2e-5) = pass(4e-5) AND NOT pass(2e-5)
# This is equivalent to subtracting the retained populations when the tighter selection is nested.
#
# It makes 34 plots per shell range, using the union of the ANN feature set.  For variables that are
# not present in the BDT feature set (the two L1 isolation significances), the BDT line is omitted.
#
# The default interpretation of the requested numbers is FRACTIONAL background retention:
#   0.00008, 0.00004, 0.00002
# not percent units.  If you truly meant percent units, use:
#   --retentions 8e-7 4e-7 2e-7
# ===== SHELL-PLOT NOTE END =====


class ANNClassifier(nn.Module):
    def __init__(self, in_features: int):
        super().__init__()
        self.net = nn.Sequential(
            nn.Linear(in_features, 264, bias=False),
            nn.BatchNorm1d(264),
            nn.LeakyReLU(),
            nn.Linear(264, 264, bias=False),
            nn.BatchNorm1d(264),
            nn.LeakyReLU(),
            nn.Linear(264, 128, bias=False),
            nn.BatchNorm1d(128),
            nn.LeakyReLU(),
            nn.Linear(128, 128, bias=False),
            nn.BatchNorm1d(128),
            nn.LeakyReLU(),
            nn.Linear(128, 64, bias=False),
            nn.BatchNorm1d(64),
            nn.LeakyReLU(),
            nn.Linear(64, 1),
        )

    def forward(self, x):
        return self.net(x)


ANN_FEATURE_NAMES = [
    "vertex_pos_x",
    "vertex_pos_y",
    "vertex_pos_z",
    "psum",
    "ele_track_n_hits",
    "ele_track_d0",
    "ele_track_phi0",
    "ele_track_z0",
    "ele_track_tan_lambda",
    "ele_track_px",
    "ele_track_py",
    "ele_track_pz",
    "ele_track_chi2",
    "ele_track_x_at_ecal",
    "ele_track_y_at_ecal",
    "ele_track_z_at_ecal",
    "pos_track_n_hits",
    "pos_track_d0",
    "pos_track_phi0",
    "pos_track_z0",
    "pos_track_tan_lambda",
    "pos_track_px",
    "pos_track_py",
    "pos_track_pz",
    "pos_track_chi2",
    "pos_track_x_at_ecal",
    "pos_track_y_at_ecal",
    "pos_track_z_at_ecal",
    "vertex_chi2",
    "vtx_proj_sig",
    "vtx_proj_x_sig",
    "vtx_proj_y_sig",
    "ele_L1_iso_significance",
    "pos_L1_iso_significance",
]

BDT_FEATURE_NAMES = [
    "psum",
    "vertex_pos_x",
    "vertex_pos_y",
    "vertex_pos_z",
    "ele_track_n_hits",
    "ele_track_d0",
    "ele_track_phi0",
    "ele_track_z0",
    "ele_track_tan_lambda",
    "ele_track_px",
    "ele_track_py",
    "ele_track_pz",
    "ele_track_chi2",
    "ele_track_x_at_ecal",
    "ele_track_y_at_ecal",
    "ele_track_z_at_ecal",
    "pos_track_n_hits",
    "pos_track_d0",
    "pos_track_phi0",
    "pos_track_z0",
    "pos_track_tan_lambda",
    "pos_track_px",
    "pos_track_py",
    "pos_track_pz",
    "pos_track_chi2",
    "pos_track_x_at_ecal",
    "pos_track_y_at_ecal",
    "pos_track_z_at_ecal",
    "vertex_chi2",
    "vtx_proj_sig",
    "vtx_proj_x_sig",
    "vtx_proj_y_sig",
]


def _as_float32_col(arr):
    return np.asarray(arr, dtype=np.float32).reshape(-1, 1)


def ann_predict_score(model, scaler_mean, scaler_scale, X, batch_size=100000):
    n = X.shape[0]
    scores = np.empty(n, dtype=np.float32)
    for start in range(0, n, batch_size):
        end = min(start + batch_size, n)
        X_chunk = np.nan_to_num(X[start:end], nan=0.0, posinf=0.0, neginf=0.0)
        X_scaled = (X_chunk.astype(np.float32) - scaler_mean) / scaler_scale
        X_tensor = torch.from_numpy(X_scaled)
        with torch.no_grad():
            scores[start:end] = torch.sigmoid(model(X_tensor)).cpu().numpy().ravel()
    return scores


def bdt_predict_score_batched(model, X, batch_size=100000):
    n = X.shape[0]
    scores = np.empty(n, dtype=np.float32)
    for start in range(0, n, batch_size):
        end = min(start + batch_size, n)
        X_chunk = np.nan_to_num(X[start:end], nan=0.0, posinf=0.0, neginf=0.0)
        scores[start:end] = model.predict_proba(X_chunk)[:, 1].astype(np.float32, copy=False)
    return scores


def load_models(whichmass):
    ann_model_path = f"/sdf/group/hps/users/rodwyer1/run/reach_curves/annstuff/classifier_adv_2021_v9_pass5_run42QualCuts_{int(whichmass)}.pt"
    ann_scaler_path = f"/sdf/group/hps/users/rodwyer1/run/reach_curves/annstuff/scaler_arrays_{int(whichmass)}.npz"
    bdt_model_path = "/sdf/group/hps/users/rodwyer1/run/reach_curves/optimization/bdt_trainer_2426/bdt_model.joblib"

    sys.modules['numpy._core'] = np.core
    ann_scaler = np.load(ann_scaler_path)
    ann_scaler_mean = ann_scaler["mean"].astype(np.float32)
    ann_scaler_scale = ann_scaler["scale"].astype(np.float32)

    ann_model = ANNClassifier(in_features=34)
    ann_state = torch.load(ann_model_path, map_location="cpu")
    ann_model.load_state_dict(ann_state)
    ann_model.eval()

    bdt_model = joblib.load(bdt_model_path)
    return ann_model, ann_scaler_mean, ann_scaler_scale, bdt_model


def common_branch_list():
    return [
        "vertex.invM_", "psum", "vertex.pos_",
        "ele.track_.n_hits_", "ele.track_.d0_", "ele.track_.phi0_",
        "ele.track_.z0_", "ele.track_.tan_lambda_", "ele.track_.px_",
        "ele.track_.py_", "ele.track_.pz_", "ele.track_.chi2_",
        "ele.track_.x_at_ecal_", "ele.track_.y_at_ecal_", "ele.track_.z_at_ecal_",
        "pos.track_.n_hits_", "pos.track_.d0_", "pos.track_.phi0_",
        "pos.track_.z0_", "pos.track_.tan_lambda_", "pos.track_.px_",
        "pos.track_.py_", "pos.track_.pz_", "pos.track_.chi2_",
        "pos.track_.x_at_ecal_", "pos.track_.y_at_ecal_", "pos.track_.z_at_ecal_",
        "vertex.chi2_", "vertex.invMerr_",
        "vtx_proj_sig", "vtx_proj_x_sig", "vtx_proj_y_sig",
        "ele.track_.hit_layers_", "pos.track_.hit_layers_",
        "ele_L1_iso_significance", "pos_L1_iso_significance",
    ]


def derive_l1l1_from_ak(hit_layers):
    if hit_layers is None:
        return None
    hasL0 = ak.to_numpy(ak.any(hit_layers == 0, axis=-1))
    hasL1 = ak.to_numpy(ak.any(hit_layers == 1, axis=-1))
    return np.asarray(hasL0 & hasL1, dtype=bool)


def load_background_arrays():
    if bg is None or not hasattr(bg, "BACKGROUND_PATH"):
        raise RuntimeError("Background module/path unavailable.")

    bg_path = bg.BACKGROUND_PATH
    with uproot.open(bg_path) as f:
        if hasattr(bg, "_open_first_tree"):
            tree = bg._open_first_tree(f)
        else:
            keys = [k for k in f.keys() if ";" in k]
            tree = f[keys[0]] if keys else None
        if tree is None:
            raise RuntimeError("No TTree found in background file.")
        arrays = tree.arrays(common_branch_list(), library="ak", how=dict)
    return arrays


def build_feature_value_dict_from_bg_arrays(arrays):
    vertex_pos = ak.to_numpy(arrays["vertex.pos_"])
    out = {
        "vertex_pos_x": np.asarray(vertex_pos["fX"]),
        "vertex_pos_y": np.asarray(vertex_pos["fY"]),
        "vertex_pos_z": np.asarray(vertex_pos["fZ"]),
        "psum": np.asarray(ak.to_numpy(arrays["psum"])),
        "ele_track_n_hits": np.asarray(ak.to_numpy(arrays["ele.track_.n_hits_"])),
        "ele_track_d0": np.asarray(ak.to_numpy(arrays["ele.track_.d0_"])),
        "ele_track_phi0": np.asarray(ak.to_numpy(arrays["ele.track_.phi0_"])),
        "ele_track_z0": np.asarray(ak.to_numpy(arrays["ele.track_.z0_"])),
        "ele_track_tan_lambda": np.asarray(ak.to_numpy(arrays["ele.track_.tan_lambda_"])),
        "ele_track_px": np.asarray(ak.to_numpy(arrays["ele.track_.px_"])),
        "ele_track_py": np.asarray(ak.to_numpy(arrays["ele.track_.py_"])),
        "ele_track_pz": np.asarray(ak.to_numpy(arrays["ele.track_.pz_"])),
        "ele_track_chi2": np.asarray(ak.to_numpy(arrays["ele.track_.chi2_"])),
        "ele_track_x_at_ecal": np.asarray(ak.to_numpy(arrays["ele.track_.x_at_ecal_"])),
        "ele_track_y_at_ecal": np.asarray(ak.to_numpy(arrays["ele.track_.y_at_ecal_"])),
        "ele_track_z_at_ecal": np.asarray(ak.to_numpy(arrays["ele.track_.z_at_ecal_"])),
        "pos_track_n_hits": np.asarray(ak.to_numpy(arrays["pos.track_.n_hits_"])),
        "pos_track_d0": np.asarray(ak.to_numpy(arrays["pos.track_.d0_"])),
        "pos_track_phi0": np.asarray(ak.to_numpy(arrays["pos.track_.phi0_"])),
        "pos_track_z0": np.asarray(ak.to_numpy(arrays["pos.track_.z0_"])),
        "pos_track_tan_lambda": np.asarray(ak.to_numpy(arrays["pos.track_.tan_lambda_"])),
        "pos_track_px": np.asarray(ak.to_numpy(arrays["pos.track_.px_"])),
        "pos_track_py": np.asarray(ak.to_numpy(arrays["pos.track_.py_"])),
        "pos_track_pz": np.asarray(ak.to_numpy(arrays["pos.track_.pz_"])),
        "pos_track_chi2": np.asarray(ak.to_numpy(arrays["pos.track_.chi2_"])),
        "pos_track_x_at_ecal": np.asarray(ak.to_numpy(arrays["pos.track_.x_at_ecal_"])),
        "pos_track_y_at_ecal": np.asarray(ak.to_numpy(arrays["pos.track_.y_at_ecal_"])),
        "pos_track_z_at_ecal": np.asarray(ak.to_numpy(arrays["pos.track_.z_at_ecal_"])),
        "vertex_chi2": np.asarray(ak.to_numpy(arrays["vertex.chi2_"])),
        "vtx_proj_sig": np.asarray(ak.to_numpy(arrays["vtx_proj_sig"])),
        "vtx_proj_x_sig": np.asarray(ak.to_numpy(arrays["vtx_proj_x_sig"])),
        "vtx_proj_y_sig": np.asarray(ak.to_numpy(arrays["vtx_proj_y_sig"])),
        "ele_L1_iso_significance": np.asarray(ak.to_numpy(arrays["ele_L1_iso_significance"])),
        "pos_L1_iso_significance": np.asarray(ak.to_numpy(arrays["pos_L1_iso_significance"])),
    }
    return out


def build_ann_matrix_from_bg_arrays(arrays):
    vals = build_feature_value_dict_from_bg_arrays(arrays)
    feats = [_as_float32_col(vals[name]) for name in ANN_FEATURE_NAMES]
    return np.hstack(feats)


def build_bdt_matrix_from_bg_arrays(arrays):
    vals = build_feature_value_dict_from_bg_arrays(arrays)
    feats = [_as_float32_col(vals[name]) for name in BDT_FEATURE_NAMES]
    return np.hstack(feats)


def build_common_mask_from_bg_arrays(arrays, proj_fixed):
    invM = ak.to_numpy(arrays["vertex.invM_"])
    psum = ak.to_numpy(arrays["psum"])
    proj_sig = ak.to_numpy(arrays["vtx_proj_sig"])

    ele_l1l1 = derive_l1l1_from_ak(arrays.get("ele.track_.hit_layers_"))
    pos_l1l1 = derive_l1l1_from_ak(arrays.get("pos.track_.hit_layers_"))
    if ele_l1l1 is None:
        ele_l1l1 = np.ones_like(invM, dtype=bool)
    if pos_l1l1 is None:
        pos_l1l1 = np.ones_like(invM, dtype=bool)
    l1l1_mask = ele_l1l1 & pos_l1l1

    psum_mask = (psum >= 1.5) & (psum <= 3.0)
    proj_mask = (proj_sig < proj_fixed)
    return psum_mask & l1l1_mask & proj_mask


def score_threshold_for_retention(scores, base_mask, retention):
    sel_scores = np.asarray(scores[base_mask], dtype=np.float64)
    if sel_scores.size == 0:
        raise RuntimeError("No background events survived the common preselection.")
    if retention <= 0.0 or retention >= 1.0:
        raise ValueError(f"Retention must be between 0 and 1, got {retention}")

    n = sel_scores.size
    n_keep = max(1, int(np.ceil(retention * n)))
    order = np.sort(sel_scores)
    threshold = order[-n_keep]
    realized = np.count_nonzero(sel_scores >= threshold) / float(n)
    return float(threshold), float(realized), int(n_keep), int(n)


def shell_mask(scores, base_mask, thr_looser, thr_tighter):
    pass_looser = base_mask & (scores >= thr_looser)
    pass_tighter = base_mask & (scores >= thr_tighter)
    return pass_looser & (~pass_tighter)


def robust_edges(x, bins):
    x = np.asarray(x)
    x = x[np.isfinite(x)]
    if x.size == 0:
        return np.linspace(0.0, 1.0, bins + 1)
    xmin = np.min(x)
    xmax = np.max(x)
    if xmin == xmax:
        pad = 0.5 if xmin == 0 else 0.05 * abs(xmin)
        return np.linspace(xmin - pad, xmax + pad, bins + 1)
    q1 = np.quantile(x, 0.01)
    q99 = np.quantile(x, 0.99)
    if np.isfinite(q1) and np.isfinite(q99) and q99 > q1:
        return np.linspace(q1, q99, bins + 1)
    return np.linspace(xmin, xmax, bins + 1)


def sanitize_filename(name):
    return ''.join(c if c.isalnum() or c in ('_', '-', '.') else '_' for c in name)


def plot_shell_histogram(var_name, values, ann_mask, bdt_mask, ann_meta, bdt_meta, outpath, bins=60, density=True):
    ann_vals = np.asarray(values[ann_mask])
    bdt_vals = np.asarray(values[bdt_mask])
    combined = np.concatenate([ann_vals[np.isfinite(ann_vals)], bdt_vals[np.isfinite(bdt_vals)]]) if (ann_vals.size + bdt_vals.size) > 0 else np.array([])
    edges = robust_edges(combined, bins)

    fig, ax = plt.subplots(figsize=(7.5, 5.5))
    any_plotted = False

    if ann_vals.size > 0:
        ax.hist(ann_vals, bins=edges, histtype='step', linewidth=2.0, density=density,
                label=(f"ANN shell, target {ann_meta['lo_target']:.1e}->{ann_meta['hi_target']:.1e}; "
                       f"realized {ann_meta['lo_realized']:.3e}->{ann_meta['hi_realized']:.3e}; "
                       f"N={ann_vals.size}"))
        any_plotted = True

    if bdt_mask is not None and bdt_vals.size > 0:
        ax.hist(bdt_vals, bins=edges, histtype='step', linewidth=2.0, density=density,
                label=(f"BDT shell, target {bdt_meta['lo_target']:.1e}->{bdt_meta['hi_target']:.1e}; "
                       f"realized {bdt_meta['lo_realized']:.3e}->{bdt_meta['hi_realized']:.3e}; "
                       f"N={bdt_vals.size}"))
        any_plotted = True

    ax.set_title(f"Background shell comparison: {var_name}")
    ax.set_xlabel(var_name)
    ax.set_ylabel("Density" if density else "Entries")
    ax.grid(True, alpha=0.3)
    if any_plotted:
        ax.legend(fontsize=8)
    fig.tight_layout()
    plt.savefig(outpath, dpi=180)
    plt.close(fig)


def main():
    ap = argparse.ArgumentParser(description="Plot ANN-vs-BDT background shell histograms between tiny retention bands.")
    ap.add_argument("--mass", type=float, default=150.0)
    ap.add_argument("--base-module", type=str, default="decayLength8sel")
    ap.add_argument("--proj-fixed", type=float, default=1.0e9)
    ap.add_argument("--retentions", nargs=3, type=float, default=[8e-5, 4e-5, 2e-5],
                    help="Three nested background retention targets, e.g. 8e-5 4e-5 2e-5")
    ap.add_argument("--outdir", type=str, default="bg_shell_feature_plots_m150")
    ap.add_argument("--bins", type=int, default=60)
    ap.add_argument("--counts", action="store_true",
                    help="Plot counts instead of density-normalized histograms")
    ap.add_argument("--debug", action="store_true")
    args = ap.parse_args()

    try:
        base = __import__(args.base_module)
    except Exception as e:
        sys.stderr.write(f"[error] Could not import signal base module '{args.base_module}': {e}\n")
        sys.exit(1)

    if len(args.retentions) != 3:
        raise ValueError("Please provide exactly three retentions, e.g. --retentions 8e-5 4e-5 2e-5")
    r_loose, r_mid, r_tight = args.retentions
    if not (r_loose > r_mid > r_tight > 0.0):
        raise ValueError("Retentions must be strictly descending and positive, e.g. 8e-5 > 4e-5 > 2e-5")

    whichmass = base._mass_key(1.8 * args.mass / 3.0)
    ann_model, ann_scaler_mean, ann_scaler_scale, bdt_model = load_models(whichmass)

    arrays = load_background_arrays()
    common_mask = build_common_mask_from_bg_arrays(arrays, args.proj_fixed)
    values = build_feature_value_dict_from_bg_arrays(arrays)

    X_ann = build_ann_matrix_from_bg_arrays(arrays)
    X_bdt = build_bdt_matrix_from_bg_arrays(arrays)

    ann_scores = ann_predict_score(ann_model, ann_scaler_mean, ann_scaler_scale, X_ann)
    bdt_scores = bdt_predict_score_batched(bdt_model, X_bdt)

    del X_ann, X_bdt
    gc.collect()

    ann_thr_loose, ann_real_loose, ann_keep_loose, ann_nbase = score_threshold_for_retention(ann_scores, common_mask, r_loose)
    ann_thr_mid, ann_real_mid, ann_keep_mid, _ = score_threshold_for_retention(ann_scores, common_mask, r_mid)
    ann_thr_tight, ann_real_tight, ann_keep_tight, _ = score_threshold_for_retention(ann_scores, common_mask, r_tight)

    bdt_thr_loose, bdt_real_loose, bdt_keep_loose, bdt_nbase = score_threshold_for_retention(bdt_scores, common_mask, r_loose)
    bdt_thr_mid, bdt_real_mid, bdt_keep_mid, _ = score_threshold_for_retention(bdt_scores, common_mask, r_mid)
    bdt_thr_tight, bdt_real_tight, bdt_keep_tight, _ = score_threshold_for_retention(bdt_scores, common_mask, r_tight)

    ann_shell_loose_mid = shell_mask(ann_scores, common_mask, ann_thr_loose, ann_thr_mid)
    ann_shell_mid_tight = shell_mask(ann_scores, common_mask, ann_thr_mid, ann_thr_tight)
    bdt_shell_loose_mid = shell_mask(bdt_scores, common_mask, bdt_thr_loose, bdt_thr_mid)
    bdt_shell_mid_tight = shell_mask(bdt_scores, common_mask, bdt_thr_mid, bdt_thr_tight)

    ranges = [
        (
            f"{r_loose:.1e}_to_{r_mid:.1e}",
            ann_shell_loose_mid,
            bdt_shell_loose_mid,
            {"lo_target": r_loose, "hi_target": r_mid, "lo_realized": ann_real_loose, "hi_realized": ann_real_mid},
            {"lo_target": r_loose, "hi_target": r_mid, "lo_realized": bdt_real_loose, "hi_realized": bdt_real_mid},
        ),
        (
            f"{r_mid:.1e}_to_{r_tight:.1e}",
            ann_shell_mid_tight,
            bdt_shell_mid_tight,
            {"lo_target": r_mid, "hi_target": r_tight, "lo_realized": ann_real_mid, "hi_realized": ann_real_tight},
            {"lo_target": r_mid, "hi_target": r_tight, "lo_realized": bdt_real_mid, "hi_realized": bdt_real_tight},
        ),
    ]

    os.makedirs(args.outdir, exist_ok=True)
    summary_path = os.path.join(args.outdir, "shell_summary.txt")
    with open(summary_path, "w") as sf:
        sf.write("Background shell histogram summary\n")
        sf.write(f"mass = {args.mass}\n")
        sf.write(f"base module = {args.base_module}\n")
        sf.write(f"proj_fixed = {args.proj_fixed}\n")
        sf.write(f"common-preselection survivors = {np.count_nonzero(common_mask)}\n\n")

        sf.write("ANN thresholds\n")
        sf.write(f"  target {r_loose:.3e}: threshold = {ann_thr_loose:.8g}, realized = {ann_real_loose:.8g}, requested keep ~= {ann_keep_loose}/{ann_nbase}\n")
        sf.write(f"  target {r_mid:.3e}: threshold = {ann_thr_mid:.8g}, realized = {ann_real_mid:.8g}, requested keep ~= {ann_keep_mid}/{ann_nbase}\n")
        sf.write(f"  target {r_tight:.3e}: threshold = {ann_thr_tight:.8g}, realized = {ann_real_tight:.8g}, requested keep ~= {ann_keep_tight}/{ann_nbase}\n\n")

        sf.write("BDT thresholds\n")
        sf.write(f"  target {r_loose:.3e}: threshold = {bdt_thr_loose:.8g}, realized = {bdt_real_loose:.8g}, requested keep ~= {bdt_keep_loose}/{bdt_nbase}\n")
        sf.write(f"  target {r_mid:.3e}: threshold = {bdt_thr_mid:.8g}, realized = {bdt_real_mid:.8g}, requested keep ~= {bdt_keep_mid}/{bdt_nbase}\n")
        sf.write(f"  target {r_tight:.3e}: threshold = {bdt_thr_tight:.8g}, realized = {bdt_real_tight:.8g}, requested keep ~= {bdt_keep_tight}/{bdt_nbase}\n\n")

        sf.write("Shell populations\n")
        sf.write(f"  ANN {r_loose:.3e}->{r_mid:.3e}: {np.count_nonzero(ann_shell_loose_mid)}\n")
        sf.write(f"  ANN {r_mid:.3e}->{r_tight:.3e}: {np.count_nonzero(ann_shell_mid_tight)}\n")
        sf.write(f"  BDT {r_loose:.3e}->{r_mid:.3e}: {np.count_nonzero(bdt_shell_loose_mid)}\n")
        sf.write(f"  BDT {r_mid:.3e}->{r_tight:.3e}: {np.count_nonzero(bdt_shell_mid_tight)}\n")

    for range_tag, ann_mask, bdt_mask, ann_meta, bdt_meta in ranges:
        subdir = os.path.join(args.outdir, sanitize_filename(range_tag))
        os.makedirs(subdir, exist_ok=True)
        for var_name in ANN_FEATURE_NAMES:
            outpath = os.path.join(subdir, sanitize_filename(var_name) + ".png")
            # ===== PLOT NOTE =====
            # For the two isolation-significance variables, BDT has no corresponding trained input column.
            # We still show the ANN shell for that variable, and omit the BDT line there.
            bdt_mask_for_plot = bdt_mask if var_name in BDT_FEATURE_NAMES else None
            plot_shell_histogram(
                var_name=var_name,
                values=values[var_name],
                ann_mask=ann_mask,
                bdt_mask=bdt_mask_for_plot,
                ann_meta=ann_meta,
                bdt_meta=bdt_meta,
                outpath=outpath,
                bins=args.bins,
                density=(not args.counts),
            )

    if args.debug:
        print(f"Common-preselection background count: {np.count_nonzero(common_mask)}")
        print(f"ANN realized retentions: {ann_real_loose:.6e}, {ann_real_mid:.6e}, {ann_real_tight:.6e}")
        print(f"BDT realized retentions: {bdt_real_loose:.6e}, {bdt_real_mid:.6e}, {bdt_real_tight:.6e}")
        print(f"ANN shell counts: {np.count_nonzero(ann_shell_loose_mid)}, {np.count_nonzero(ann_shell_mid_tight)}")
        print(f"BDT shell counts: {np.count_nonzero(bdt_shell_loose_mid)}, {np.count_nonzero(bdt_shell_mid_tight)}")
        print(f"Saved plots under: {args.outdir}")
        print(f"Summary file: {summary_path}")


if __name__ == "__main__":
    main()

