#!/usr/bin/env python3
import os
import sys
import gc
import math
import argparse
from pathlib import Path

import numpy as np
import uproot
import awkward as ak
import matplotlib.pyplot as plt
import joblib
import torch
from torch import nn
from sklearn.metrics import roc_curve, auc

try:
    import bk_eff_selection as bg
except Exception as e:
    sys.stderr.write(f"[warn] Could not import bk_eff_selection (background module): {e}\n")
    bg = None

HBAR_C = 1.973e-14  # GeV*cm


class ANNClassifier(nn.Module):
    """Architecture matched to classifier_adv_2021_v9_pass5_run42QualCuts_*.pt."""
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
        X_chunk = np.nan_to_num(X[start:end, :32], nan=0.0, posinf=0.0, neginf=0.0)
        scores[start:end] = model.predict_proba(X_chunk)[:, 1].astype(np.float32, copy=False)
    return scores


def beta_func(x, y):
    return (1 + y**2 - x**2 - 2*y) * (1 + y**2 - x**2 + 2*y)


def width_Ap_to_charged(alpha_D, f_pi_D, m_pi_D, m_V_D, m_Ap):
    x = m_pi_D / m_Ap
    y = m_V_D / m_Ap
    Tv = 18.0 - ((3.0/2.0)+(3.0/4.0))
    coeff = alpha_D * Tv / (192.0 * np.power(math.pi, 4))
    return coeff * np.power((m_Ap / m_pi_D), 2) * np.power(m_V_D / m_pi_D, 2) * np.power((m_pi_D / f_pi_D), 4) * m_Ap * np.power(beta_func(x, y), 3 / 2.0)


def width_Ap_to_vector(alpha_D, f_pi_D, m_pi_D, m_V_D, m_Ap, multiplicity=1.0):
    x = m_V_D / m_Ap
    y = m_pi_D / m_Ap
    prefactor = (alpha_D * multiplicity) / (192.0 * (math.pi**4))
    ratio_terms = (m_Ap / m_pi_D)**2 * (m_V_D / m_pi_D)**2 * (m_pi_D / f_pi_D)**4
    return prefactor * ratio_terms * m_Ap * (beta_func(x, y)**1.5)


def width_Ap_to_invis(alpha_D, m_pi_D, m_V_D, m_Ap):
    term1 = 1 - (4.0 * m_pi_D**2) / (m_Ap**2)
    term2 = ((m_V_D**2) / (m_Ap**2 - m_V_D**2))**2
    return ((2.0 * alpha_D) / 3.0) * m_Ap * (term1**1.5) * term2


def rate_2l(alpha_D, f_pi_D, m_pi_D, m_V_D, m_Ap, epsilon, m_l, rho):
    alpha = 1.0 / 137.0
    coeff = (16 * math.pi * alpha_D * alpha * epsilon**2 * f_pi_D**2) / (3 * m_V_D**2)
    term1 = (m_V_D**2 / (m_Ap**2 - m_V_D**2))**2
    term2 = (1 - (4 * m_l**2 / m_V_D**2))**0.5
    term3 = 1 + (2 * m_l**2 / m_V_D**2)
    constant = 1 if not rho else 2
    return coeff * term1 * term2 * term3 * m_V_D * constant


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


def build_feature_matrix_from_bg_arrays(arrays):
    """psum = ak.to_numpy(arrays["psum"])
    proj_sig = ak.to_numpy(arrays["vtx_proj_sig"])
    vertex_pos = ak.to_numpy(arrays["vertex.pos_"])

    feats = [_as_float32_col(psum)]
    for fld in vertex_pos.dtype.names:
        feats.append(_as_float32_col(vertex_pos[fld]))"""
    psum = ak.to_numpy(arrays["psum"])
    proj_sig = ak.to_numpy(arrays["vtx_proj_sig"])
    vertex_pos = ak.to_numpy(arrays["vertex.pos_"])

    feats = []
    for fld in vertex_pos.dtype.names:
        feats.append(_as_float32_col(vertex_pos[fld]))
    feats.append(_as_float32_col(psum))

    ele_keys = ["n_hits_", "d0_", "phi0_", "z0_", "tan_lambda_", "px_", "py_", "pz_", "chi2_", "x_at_ecal_", "y_at_ecal_", "z_at_ecal_"]
    for key in ele_keys:
        feats.append(_as_float32_col(ak.to_numpy(arrays[f"ele.track_.{key}"])))
    for key in ele_keys:
        feats.append(_as_float32_col(ak.to_numpy(arrays[f"pos.track_.{key}"])))

    feats.append(_as_float32_col(ak.to_numpy(arrays["vertex.chi2_"])))
    feats.append(_as_float32_col(proj_sig))
    feats.append(_as_float32_col(ak.to_numpy(arrays["vtx_proj_x_sig"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["vtx_proj_y_sig"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["ele_L1_iso_significance"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["pos_L1_iso_significance"])))
    X = np.hstack(feats)
    return X


def build_feature_matrix_from_sig_events(events):
    """feats = [_as_float32_col(np.asarray(events["psum"]))]
    feats.append(_as_float32_col(np.asarray(events["vertex.pos_.fX"])))
    feats.append(_as_float32_col(np.asarray(events["vertex.pos_.fY"])))
    feats.append(_as_float32_col(np.asarray(events["vertex.pos_.fZ"])))"""

    feats = []
    feats.append(_as_float32_col(np.asarray(events["vertex.pos_.fX"])))
    feats.append(_as_float32_col(np.asarray(events["vertex.pos_.fY"])))
    feats.append(_as_float32_col(np.asarray(events["vertex.pos_.fZ"])))
    feats.append(_as_float32_col(np.asarray(events["psum"])))

    ele_keys = ["n_hits_", "d0_", "phi0_", "z0_", "tan_lambda_", "px_", "py_", "pz_", "chi2_", "x_at_ecal_", "y_at_ecal_", "z_at_ecal_"]
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
    X = np.hstack(feats)
    return X


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
    ele_z0 = ak.to_numpy(arrays["ele.track_.z0_"])
    pos_z0 = ak.to_numpy(arrays["pos.track_.z0_"])
    proj_sig = ak.to_numpy(arrays["vtx_proj_sig"])

    ele_L1L1 = derive_l1l1_from_ak(arrays.get("ele.track_.hit_layers_"))
    pos_L1L1 = derive_l1l1_from_ak(arrays.get("pos.track_.hit_layers_"))
    if ele_L1L1 is None:
        ele_L1L1 = np.ones_like(invM, dtype=bool)
    if pos_L1L1 is None:
        pos_L1L1 = np.ones_like(invM, dtype=bool)
    l1l1_mask = ele_L1L1 & pos_L1L1

    X_bg = build_feature_matrix_from_bg_arrays(arrays)
    return {
        "invM": invM,
        "psum": psum,
        "ele_z0": ele_z0,
        "pos_z0": pos_z0,
        "proj_sig": proj_sig,
        "l1l1_mask": l1l1_mask,
        "X": X_bg,
    }


def load_signal(base, mass_mev, epsilon):
    mkey = base._mass_key(1.8 * mass_mev / 3.0)
    events = base._events_cache(mkey)

    s_psum = np.asarray(events["psum"])
    ele_z0 = np.asarray(events["ele.track_.z0_"])
    pos_z0 = np.asarray(events["pos.track_.z0_"])
    proj_sig = np.asarray(events["vtx_proj_sig"])

    s_e_hasL0L1 = derive_l1l1_from_events(events, "ele")
    s_p_hasL0L1 = derive_l1l1_from_events(events, "pos")
    if s_e_hasL0L1 is None:
        s_e_hasL0L1 = np.ones_like(s_psum, dtype=bool)
    if s_p_hasL0L1 is None:
        s_p_hasL0L1 = np.ones_like(s_psum, dtype=bool)
    l1l1_mask = s_e_hasL0L1 & s_p_hasL0L1

    X_sig = build_feature_matrix_from_sig_events(events)

    alpha_D = 0.01
    m_pi_D = mass_mev / 3.0
    m_V_D = 1.8 * mass_mev / 3.0
    f_pi_D = (mass_mev / 3.0) * (1.0 / (4.0 * math.pi))

    rho_width = width_Ap_to_vector(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, multiplicity=0.75)
    phi_width = width_Ap_to_vector(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, multiplicity=1.5)
    invis_width = width_Ap_to_invis(alpha_D, m_pi_D, m_V_D, mass_mev)
    charged_width = width_Ap_to_charged(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev)
    total_width = rho_width + phi_width + invis_width + charged_width
    rho_fraction = rho_width / total_width
    phi_fraction = phi_width / total_width

    rho_width_vis = rate_2l(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, epsilon, 0.511, True)
    phi_width_vis = rate_2l(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, epsilon, 0.511, False)
    rho_length = (1000 * HBAR_C * 10.0) / rho_width_vis
    phi_length = (1000 * HBAR_C * 10.0) / phi_width_vis

    z_temp = np.asarray(events["true_vd.vtx_z_"], dtype=np.float64)
    psum_temp = np.asarray(events["psum"], dtype=np.float64)
    gamma_temp = 1000 * psum_temp / m_V_D
    z_temp = z_temp * (z_temp >= 0)

    frac_tot = rho_fraction + phi_fraction
    rho_fraction /= frac_tot
    phi_fraction /= frac_tot

    p_accept_temp = rho_fraction * np.exp(-z_temp / (gamma_temp * rho_length)) / (rho_length * gamma_temp)
    p_accept_temp += phi_fraction * np.exp(-z_temp / (gamma_temp * phi_length)) / (phi_length * gamma_temp)
    p_accept_temp /= np.max(p_accept_temp)

    return {
        "psum": s_psum,
        "ele_z0": ele_z0,
        "pos_z0": pos_z0,
        "proj_sig": proj_sig,
        "l1l1_mask": l1l1_mask,
        "X": X_sig,
        "weights": p_accept_temp.astype(np.float64),
    }


def build_common_mask(psum, l1l1_mask, proj_sig, proj_fixed):
    psum_mask = (psum >= 1.5) & (psum <= 3.0)
    proj_mask = (proj_sig < proj_fixed)
    return psum_mask & l1l1_mask & proj_mask


def main():
    ap = argparse.ArgumentParser(description="Overlay ROC curves for cut-based, BDT, and ANN selections.")
    ap.add_argument("--mass", type=float, default=150.0, help="A' mass in MeV. Default: 150")
    ap.add_argument("--epsilon", type=float, default=1e-3, help="Kinetic mixing epsilon. Default: 1e-3")
    ap.add_argument("--base-module", type=str, default="decayLength8sel", help="Signal base module name. Default: decayLength8sel")
    ap.add_argument("--proj-fixed", type=float, default=10.0, help="Fixed proj_sig cut used as common preselection. Default: 1e9")
    ap.add_argument("--outfile", type=str, default="roc_overlay_m150_eps1e-3.png", help="Output plot filename")
    ap.add_argument("--debug", action="store_true", help="Enable debug printing")
    args = ap.parse_args()

    try:
        base = __import__(args.base_module)
    except Exception as e:
        sys.stderr.write(f"[error] Could not import signal base module '{args.base_module}': {e}\n")
        sys.exit(1)

    whichmass = base._mass_key(1.8 * args.mass / 3.0)
    ann_model, ann_scaler_mean, ann_scaler_scale, bdt_model = load_models(whichmass)

    bg_data = load_background(base, args.mass)
    sig_data = load_signal(base, args.mass, args.epsilon)

    bg_common = build_common_mask(bg_data["psum"], bg_data["l1l1_mask"], bg_data["proj_sig"], args.proj_fixed)
    sig_common = build_common_mask(sig_data["psum"], sig_data["l1l1_mask"], sig_data["proj_sig"], args.proj_fixed)

    ann_scores_bg = ann_predict_score(ann_model, ann_scaler_mean, ann_scaler_scale, bg_data["X"])
    ann_scores_sig = ann_predict_score(ann_model, ann_scaler_mean, ann_scaler_scale, sig_data["X"])

    bdt_scores_bg = bdt_predict_score_batched(bdt_model, bg_data["X"])
    bdt_scores_sig = bdt_predict_score_batched(bdt_model, sig_data["X"])

    cut_scores_bg = np.minimum(np.abs(bg_data["ele_z0"]), np.abs(bg_data["pos_z0"])).astype(np.float32)
    cut_scores_sig = np.minimum(np.abs(sig_data["ele_z0"]), np.abs(sig_data["pos_z0"])).astype(np.float32)

    del bg_data["X"], sig_data["X"]
    gc.collect()

    y_true = np.concatenate([
        np.zeros(np.count_nonzero(bg_common), dtype=np.int8),
        np.ones(np.count_nonzero(sig_common), dtype=np.int8),
    ])
    sample_weight = np.concatenate([
        np.ones(np.count_nonzero(bg_common), dtype=np.float64),
        sig_data["weights"][sig_common].astype(np.float64),
    ])

    ann_all = np.concatenate([ann_scores_bg[bg_common], ann_scores_sig[sig_common]])
    bdt_all = np.concatenate([bdt_scores_bg[bg_common], bdt_scores_sig[sig_common]])
    cut_all = np.concatenate([cut_scores_bg[bg_common], cut_scores_sig[sig_common]])

    fpr_ann, tpr_ann, _ = roc_curve(y_true, ann_all, sample_weight=sample_weight)
    fpr_bdt, tpr_bdt, _ = roc_curve(y_true, bdt_all, sample_weight=sample_weight)
    fpr_cut, tpr_cut, _ = roc_curve(y_true, cut_all, sample_weight=sample_weight)

    auc_ann = auc(fpr_ann, tpr_ann)
    auc_bdt = auc(fpr_bdt, tpr_bdt)
    auc_cut = auc(fpr_cut, tpr_cut)

    fig, ax = plt.subplots(figsize=(7, 5.5))
    ax.plot(fpr_ann, tpr_ann, linewidth=2.0, label=f"ANN, AUC = {auc_ann:.4f}")
    ax.plot(fpr_bdt, tpr_bdt, linewidth=2.0, label=f"BDT, AUC = {auc_bdt:.4f}")
    ax.plot(fpr_cut, tpr_cut, linewidth=2.0, label=f"Cut-based min(|z0_e|,|z0_p|), AUC = {auc_cut:.4f}")
    ax.plot([0.0, 1.0], [0.0, 1.0], linestyle="--", linewidth=1.2, label="Random")
    ax.set_xlabel("Background efficiency (FPR)")
    ax.set_ylabel("Signal efficiency (TPR)")
    ax.set_title(f"ROC overlay, m = {args.mass:.0f} MeV, epsilon = {args.epsilon:.1e}")
    ax.set_xlim(0.0, .0001)
    #ax.set_xlim(0.0, 1)
    ax.set_ylim(0.0, 1.0)
    ax.grid(True, alpha=0.3)
    ax.legend(loc="lower right", fontsize=9)
    fig.tight_layout()
    plt.savefig(args.outfile, dpi=200)

    if args.debug:
        print(f"Background events after common preselection: {np.count_nonzero(bg_common)}")
        print(f"Signal events after common preselection: {np.count_nonzero(sig_common)}")
        print(f"ANN AUC: {auc_ann:.6f}")
        print(f"BDT AUC: {auc_bdt:.6f}")
        print(f"Cut AUC: {auc_cut:.6f}")
        print(f"Saved plot to: {args.outfile}")


if __name__ == "__main__":
    main()

