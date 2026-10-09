#!/usr/bin/env python3
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
    print(int(whichmass))
    ann_model_path = f"/sdf/group/hps/users/rodwyer1/run/reach_curves/annstuff/classifier_adv_2021_v9_pass5_run42QualCuts_{int(whichmass)}.pt"
    ann_scaler_path = f"/sdf/group/hps/users/rodwyer1/run/reach_curves/annstuff/scaler_arrays_{int(whichmass)}.npz"
    bdt_model_path = f"/sdf/group/hps/users/rodwyer1/run/reach_curves/optimization/bdt_trainer_31026_massdep/bdt_model_{int(whichmass)}.joblib"

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


def build_ann_matrix_from_bg_arrays(arrays):
    psum = ak.to_numpy(arrays["psum"])
    vertex_pos = ak.to_numpy(arrays["vertex.pos_"])

    feats = [
        _as_float32_col(vertex_pos["fX"]),
        _as_float32_col(vertex_pos["fY"]),
        _as_float32_col(vertex_pos["fZ"]),
        _as_float32_col(psum),
    ]

    ele_keys = [
        "n_hits_", "d0_", "phi0_", "z0_", "tan_lambda_",
        "px_", "py_", "pz_", "chi2_",
        "x_at_ecal_", "y_at_ecal_", "z_at_ecal_"
    ]
    for key in ele_keys:
        feats.append(_as_float32_col(ak.to_numpy(arrays[f"ele.track_.{key}"])))
    for key in ele_keys:
        feats.append(_as_float32_col(ak.to_numpy(arrays[f"pos.track_.{key}"])))

    feats.append(_as_float32_col(ak.to_numpy(arrays["vertex.chi2_"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["vtx_proj_sig"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["vtx_proj_x_sig"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["vtx_proj_y_sig"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["ele_L1_iso_significance"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["pos_L1_iso_significance"])))
    return np.hstack(feats)


def build_bdt_matrix_from_bg_arrays(arrays):
    psum = ak.to_numpy(arrays["psum"])
    vertex_pos = ak.to_numpy(arrays["vertex.pos_"])

    feats = [
        _as_float32_col(psum),
        _as_float32_col(vertex_pos["fX"]),
        _as_float32_col(vertex_pos["fY"]),
        _as_float32_col(vertex_pos["fZ"]),
    ]

    ele_keys = [
        "n_hits_", "d0_", "phi0_", "z0_", "tan_lambda_",
        "px_", "py_", "pz_", "chi2_",
        "x_at_ecal_", "y_at_ecal_", "z_at_ecal_"
    ]
    for key in ele_keys:
        feats.append(_as_float32_col(ak.to_numpy(arrays[f"ele.track_.{key}"])))
    for key in ele_keys:
        feats.append(_as_float32_col(ak.to_numpy(arrays[f"pos.track_.{key}"])))

    feats.append(_as_float32_col(ak.to_numpy(arrays["vertex.chi2_"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["vtx_proj_sig"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["vtx_proj_x_sig"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["vtx_proj_y_sig"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["ele_L1_iso_significance"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["pos_L1_iso_significance"])))
    return np.hstack(feats)


def build_ann_matrix_from_sig_events(events):
    feats = [
        _as_float32_col(np.asarray(events["vertex.pos_.fX"])),
        _as_float32_col(np.asarray(events["vertex.pos_.fY"])),
        _as_float32_col(np.asarray(events["vertex.pos_.fZ"])),
        _as_float32_col(np.asarray(events["psum"])),
    ]

    ele_keys = [
        "n_hits_", "d0_", "phi0_", "z0_", "tan_lambda_",
        "px_", "py_", "pz_", "chi2_",
        "x_at_ecal_", "y_at_ecal_", "z_at_ecal_"
    ]
    for key in ele_keys:
        feats.append(_as_float32_col(np.asarray(events[f"ele.track_.{key}"])))
    for key in ele_keys:
        feats.append(_as_float32_col(np.asarray(events[f"pos.track_.{key}"])))

    feats.append(_as_float32_col(np.asarray(events["vertex.chi2_"])))
    feats.append(_as_float32_col(np.asarray(events["vtx_proj_sig"])))
    feats.append(_as_float32_col(np.asarray(events["vtx_proj_x_sig"])))
    feats.append(_as_float32_col(np.asarray(events["vtx_proj_y_sig"])))
    feats.append(_as_float32_col(np.asarray(events["ele_L1_iso_significance"])))
    feats.append(_as_float32_col(np.asarray(events["pos_L1_iso_significance"])))
    return np.hstack(feats)


def build_bdt_matrix_from_sig_events(events):
    feats = [
        _as_float32_col(np.asarray(events["psum"])),
        _as_float32_col(np.asarray(events["vertex.pos_.fX"])),
        _as_float32_col(np.asarray(events["vertex.pos_.fY"])),
        _as_float32_col(np.asarray(events["vertex.pos_.fZ"])),
    ]

    ele_keys = [
        "n_hits_", "d0_", "phi0_", "z0_", "tan_lambda_",
        "px_", "py_", "pz_", "chi2_",
        "x_at_ecal_", "y_at_ecal_", "z_at_ecal_"
    ]
    for key in ele_keys:
        feats.append(_as_float32_col(np.asarray(events[f"ele.track_.{key}"])))
    for key in ele_keys:
        feats.append(_as_float32_col(np.asarray(events[f"pos.track_.{key}"])))

    feats.append(_as_float32_col(np.asarray(events["vertex.chi2_"])))
    feats.append(_as_float32_col(np.asarray(events["vtx_proj_sig"])))
    feats.append(_as_float32_col(np.asarray(events["vtx_proj_x_sig"])))
    feats.append(_as_float32_col(np.asarray(events["vtx_proj_y_sig"])))
    feats.append(_as_float32_col(np.asarray(events["ele_L1_iso_significance"])))
    feats.append(_as_float32_col(np.asarray(events["pos_L1_iso_significance"])))
    return np.hstack(feats)


def derive_l1l1_from_ak(hit_layers):
    if hit_layers is None:
        return None
    hasL0 = ak.to_numpy(ak.any(hit_layers == 0, axis=-1))
    hasL1 = ak.to_numpy(ak.any(hit_layers == 1, axis=-1))
    return np.asarray(hasL0 & hasL1, dtype=bool)


def derive_l1l1_from_events(events, side):
    flag_key = f"{side}.hasL0L1"
    if flag_key in events:
        return np.asarray(events[flag_key], dtype=bool)
    layers_key = f"{side}.track_.hit_layers_"
    if layers_key in events:
        layers_ak = ak.Array(events[layers_key])
        hasL0 = np.asarray((layers_ak == 0).any(axis=1), dtype=bool)
        hasL1 = np.asarray((layers_ak == 1).any(axis=1), dtype=bool)
        return hasL0 & hasL1
    return None


def load_background(base, mass_mev):
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

    invM = ak.to_numpy(arrays["vertex.invM_"])
    psum = ak.to_numpy(arrays["psum"])
    proj_sig = ak.to_numpy(arrays["vtx_proj_sig"])

    ele_L1L1 = derive_l1l1_from_ak(arrays.get("ele.track_.hit_layers_"))
    pos_L1L1 = derive_l1l1_from_ak(arrays.get("pos.track_.hit_layers_"))
    if ele_L1L1 is None:
        ele_L1L1 = np.ones_like(invM, dtype=bool)
    if pos_L1L1 is None:
        pos_L1L1 = np.ones_like(invM, dtype=bool)
    l1l1_mask = ele_L1L1 & pos_L1L1

    X_ann_bg = build_ann_matrix_from_bg_arrays(arrays)
    X_bdt_bg = build_bdt_matrix_from_bg_arrays(arrays)

    return {
        "psum": psum,
        "proj_sig": proj_sig,
        "l1l1_mask": l1l1_mask,
        "X_ann": X_ann_bg,
        "X_bdt": X_bdt_bg,
    }


def load_signal(base):
    events = base._events_cache(base._mass_key(1.8 * args.mass / 3.0))

    s_psum = np.asarray(events["psum"])
    proj_sig = np.asarray(events["vtx_proj_sig"])

    s_e_hasL0L1 = derive_l1l1_from_events(events, "ele")
    s_p_hasL0L1 = derive_l1l1_from_events(events, "pos")
    if s_e_hasL0L1 is None:
        s_e_hasL0L1 = np.ones_like(s_psum, dtype=bool)
    if s_p_hasL0L1 is None:
        s_p_hasL0L1 = np.ones_like(s_psum, dtype=bool)
    l1l1_mask = s_e_hasL0L1 & s_p_hasL0L1

    X_ann_sig = build_ann_matrix_from_sig_events(events)
    X_bdt_sig = build_bdt_matrix_from_sig_events(events)

    return {
        "psum": s_psum,
        "proj_sig": proj_sig,
        "l1l1_mask": l1l1_mask,
        "X_ann": X_ann_sig,
        "X_bdt": X_bdt_sig,
    }


def build_common_mask(psum, l1l1_mask, proj_sig, proj_fixed):
    psum_mask = (psum >= 1.5) & (psum <= 3.0)
    proj_mask = (proj_sig < proj_fixed)
    return psum_mask & l1l1_mask & proj_mask


def main():
    parser = argparse.ArgumentParser(
        description="Make a 2D ANN-vs-BDT score scatter plot on the same events."
    )
    parser.add_argument("--mass", type=float, default=150.0)
    parser.add_argument("--base-module", type=str, default="decayLength8sel")
    parser.add_argument("--proj-fixed", type=float, default=1.0e9)
    parser.add_argument("--dataset", choices=["background", "signal", "both"], default="background")
    parser.add_argument("--outfile", type=str, default="ann_vs_bdt_scatter.png")
    parser.add_argument("--max-points", type=int, default=0,
                        help="Optional cap on number of plotted points per class. 0 means plot all.")
    parser.add_argument("--debug", action="store_true")
    global args
    args = parser.parse_args()

    try:
        base = __import__(args.base_module)
    except Exception as e:
        sys.stderr.write(f"[error] Could not import signal base module '{args.base_module}': {e}\n")
        sys.exit(1)

    whichmass = base._mass_key(1.8 * args.mass / 3.0)
    ann_model, ann_scaler_mean, ann_scaler_scale, bdt_model = load_models(whichmass)

    fig, ax = plt.subplots(figsize=(7, 7))

    rng = np.random.default_rng(12345)

    if args.dataset in ["background", "both"]:
        bg_data = load_background(base, args.mass)
        bg_common = build_common_mask(
            bg_data["psum"],
            bg_data["l1l1_mask"],
            bg_data["proj_sig"],
            args.proj_fixed
        )

        ann_scores_bg = ann_predict_score(
            ann_model, ann_scaler_mean, ann_scaler_scale, bg_data["X_ann"]
        )
        bdt_scores_bg = bdt_predict_score_batched(
            bdt_model, bg_data["X_bdt"]
        )

        x_bg = bdt_scores_bg[bg_common]
        y_bg = ann_scores_bg[bg_common]

        if args.max_points > 0 and len(x_bg) > args.max_points:
            idx = rng.choice(len(x_bg), size=args.max_points, replace=False)
            x_bg = x_bg[idx]
            y_bg = y_bg[idx]

        ax.scatter(
            x_bg,
            y_bg,
            s=3,
            alpha=0.35,
            label=f"Background ({len(x_bg)})"
        )

        del bg_data, ann_scores_bg, bdt_scores_bg
        gc.collect()

    if args.dataset in ["signal", "both"]:
        sig_data = load_signal(base)
        sig_common = build_common_mask(
            sig_data["psum"],
            sig_data["l1l1_mask"],
            sig_data["proj_sig"],
            args.proj_fixed
        )

        ann_scores_sig = ann_predict_score(
            ann_model, ann_scaler_mean, ann_scaler_scale, sig_data["X_ann"]
        )
        bdt_scores_sig = bdt_predict_score_batched(
            bdt_model, sig_data["X_bdt"]
        )

        x_sig = bdt_scores_sig[sig_common]
        y_sig = ann_scores_sig[sig_common]

        if args.max_points > 0 and len(x_sig) > args.max_points:
            idx = rng.choice(len(x_sig), size=args.max_points, replace=False)
            x_sig = x_sig[idx]
            y_sig = y_sig[idx]

        ax.scatter(
            x_sig,
            y_sig,
            s=5,
            alpha=0.45,
            label=f"Signal ({len(x_sig)})"
        )

        del sig_data, ann_scores_sig, bdt_scores_sig
        gc.collect()

    ax.set_xlabel("BDT score")
    ax.set_ylabel("ANN score")
    ax.set_title(f"ANN vs BDT scores on same events, m = {args.mass:.0f} MeV")
    ax.set_xlim(0.0, 1.0)
    ax.set_ylim(0.0, 1.0)
    ax.grid(True, alpha=0.3)
    ax.legend(loc="best")
    fig.tight_layout()
    plt.savefig(args.outfile, dpi=200)

    if args.debug:
        print(f"Saved plot to: {args.outfile}")


if __name__ == "__main__":
    main()
