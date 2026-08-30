#!/usr/bin/env python3
import os, sys
import argparse
import numpy as np
import uproot, awkward as ak
import math
import matplotlib.pyplot as plt
import subprocess
from scipy.special import betainc, erfinv
from pathlib import Path
import joblib  # Added to load BDT model
import gc
# ===== ANN CHANGE START =====
import torch
from torch import nn
# ===== ANN CHANGE END =====
print("I GOT HERE 1 31226")
# Try to import background efficiency module for file path and any helpers
try:
    import bk_eff_selection as bg
except Exception as e:
    sys.stderr.write(f"[warn] Could not import bk_eff_selection (background module): {e}\n")
    bg = None

def compile_latex(tex_file):
    tex_path = Path(tex_file).resolve()
    # Run twice for references, etc.
    for _ in range(2):
        subprocess.run(
            ["pdflatex", "-interaction=nonstopmode", tex_path.name],
            cwd=tex_path.parent,
            check=True,
        )
print("I GOT HERE 2 31226")
def _extract_z_from_arrays(arrays):
    """Prefer vertex.pos_.fZ if split; otherwise inspect nested record for fZ/Z/z."""
    for cand in ["vertex.pos_.fZ", "vertex.pos__fZ", "vertex.pos_.Z", "vertex.pos_.z"]:
        if cand in arrays.fields:
            return np.asarray(ak.to_numpy(arrays[cand]))
    if "vertex.pos_" in arrays.fields:
        rec = arrays["vertex.pos_"]
        flds = ak.fields(rec)
        for fn in ["fZ", "Z", "z"]:
            if fn in flds:
                return np.asarray(ak.to_numpy(rec[fn]))
        for sub in ["fCoordinates", "fCoord", "coords", "Coord", "coord"]:
            if sub in flds:
                subrec = rec[sub]
                for fn in ["fZ", "Z", "z"]:
                    if fn in ak.fields(subrec):
                        return np.asarray(ak.to_numpy(subrec[fn]))
    for k in arrays.fields:
        if k.endswith("fZ") or k.endswith(".fZ"):
            return np.asarray(ak.to_numpy(arrays[k]))
    raise KeyError("Could not locate z coordinate from vertex.pos_.")

def zbi_significance(S: float, B: float) -> float:
    """Compute the Zbi significance for signal yield S and background yield B."""
    if B < 0:
        return float('nan')
    if B < 0.5:
        return -1.0
        #return 9.0 if S > 0 else 0.0
    p = betainc(S+B, 1+B, 0.5)
    z = math.sqrt(2.0) * erfinv(1 - 2*p)
    if p < 1e-16:
        z = 9.0
    # Do not allow negative significance (downward fluctuation scenario)
    if z < 0:
        z = 0.0
    return float(z)

# Utility for scientific notation formatting in LaTeX
def format_sci(value: float, prec: int = 2) -> str:
    if value == 0 or not math.isfinite(value):
        return f"{value:.{prec}f}"
    exp = int(math.floor(math.log10(abs(value))))
    base = value / (10**exp)
    # Round base to desired precision
    fmt_base = f"{base:.{prec}f}"
    # Remove trailing zeros and dot if needed
    fmt_base = fmt_base.rstrip('0').rstrip('.')
    return f"{fmt_base} \\times 10^{{{exp}}}"
print("I GOT HERE 3 31226")

# ===== ANN CHANGE START =====
class ANNClassifier(nn.Module):
    """Architecture inferred from classifier_adv_2021_v9_pass5_run42QualCuts.pt."""
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

def ann_predict_score(model, scaler_mean, scaler_scale, X):
    '''X = np.nan_to_num(X, nan=0.0, posinf=0.0, neginf=0.0)
    #X_scaled = scaler.transform(X)
    X_scaled = (X.astype(np.float32) - scaler_mean) / scaler_scale
    X_tensor = torch.from_numpy(X_scaled.astype(np.float32))
    with torch.no_grad():
        logits = model(X_tensor).squeeze(1)
        scores = torch.sigmoid(logits).cpu().numpy()
    return scores'''
    batch_size = 100000
    n = X.shape[0]
    scores = np.empty(n, dtype=np.float32)

    for start in range(0, n, batch_size):
        end = min(start + batch_size, n)

        X_chunk = X[start:end]
        X_chunk = np.nan_to_num(X_chunk)

        X_scaled = (X_chunk.astype(np.float32) - scaler_mean) / scaler_scale
        X_tensor = torch.from_numpy(X_scaled)

        with torch.no_grad():
            chunk_scores = torch.sigmoid(model(X_tensor)).cpu().numpy().ravel()

        scores[start:end] = chunk_scores

    return scores

# ===== LOW-RAM CHANGE START =====
def _as_float32_col(arr):
    return np.asarray(arr, dtype=np.float32).reshape(-1, 1)

def bdt_predict_score_batched(model, X, batch_size=100000):
    n = X.shape[0]
    scores = np.empty(n, dtype=np.float32)
    for start in range(0, n, batch_size):
        end = min(start + batch_size, n)
        X_chunk = np.nan_to_num(X[start:end], nan=0.0, posinf=0.0, neginf=0.0)
        scores[start:end] = model.predict_proba(X_chunk)[:, 1].astype(np.float32, copy=False)
    return scores

# ===== ORDER FIX START =====
# The ANN scaler/model expect the ANN-notebook feature order:
#   vertex_pos_x, vertex_pos_y, vertex_pos_z, psum,
#   ele block, pos block, vertex_chi2,
#   vtx_proj_sig, vtx_proj_x_sig, vtx_proj_y_sig,
#   ele_L1_iso_significance, pos_L1_iso_significance.
#
# The BDT expects the training-script order:
#   psum, vertex_pos_x, vertex_pos_y, vertex_pos_z,
#   ele block, pos block, vertex_chi2,
#   vtx_proj_sig, vtx_proj_x_sig, vtx_proj_y_sig,
#   ele_L1_iso_significance, pos_L1_iso_significance.
#
# So we must build TWO matrices and never feed the ANN matrix into the BDT.
def build_ann_matrix_from_bg_arrays(arrays):
    vertex_pos = ak.to_numpy(arrays["vertex.pos_"])
    feats = [
        _as_float32_col(vertex_pos["fX"]),
        _as_float32_col(vertex_pos["fY"]),
        _as_float32_col(vertex_pos["fZ"]),
        _as_float32_col(ak.to_numpy(arrays["psum"])),
    ]
    ele_keys = ["n_hits_", "d0_", "phi0_", "z0_", "tan_lambda_", "px_", "py_", "pz_", "chi2_", "x_at_ecal_", "y_at_ecal_", "z_at_ecal_"]
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
    vertex_pos = ak.to_numpy(arrays["vertex.pos_"])
    feats = [
        _as_float32_col(ak.to_numpy(arrays["psum"])),
        _as_float32_col(vertex_pos["fX"]),
        _as_float32_col(vertex_pos["fY"]),
        _as_float32_col(vertex_pos["fZ"]),
    ]
    ele_keys = ["n_hits_", "d0_", "phi0_", "z0_", "tan_lambda_", "px_", "py_", "pz_", "chi2_", "x_at_ecal_", "y_at_ecal_", "z_at_ecal_"]
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
    return np.hstack(feats)

def build_bdt_matrix_from_sig_events(events):
    feats = [
        _as_float32_col(np.asarray(events["psum"])),
        _as_float32_col(np.asarray(events["vertex.pos_.fX"])),
        _as_float32_col(np.asarray(events["vertex.pos_.fY"])),
        _as_float32_col(np.asarray(events["vertex.pos_.fZ"])),
    ]
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
    return np.hstack(feats)
# ===== ORDER FIX END =====
# ===== LOW-RAM CHANGE END =====
# ===== ANN CHANGE END =====

# Functions to compute hidden sector decay fractions (using decayLength7sel model equations)
HBAR_C = 1.973e-14  # GeV*cm
def beta_func(x, y):
    return (1 + y**2 - x**2 - 2*y) * (1 + y**2 - x**2 + 2*y)
def width_Ap_to_charged(alpha_D, f_pi_D, m_pi_D, m_V_D, m_Ap):
    x = m_pi_D / m_Ap
    y = m_V_D / m_Ap
    Tv = 18.0 - ((3.0/2.0)+(3.0/4.0))
    coeff = alpha_D * Tv / (192.0 * np.power(math.pi, 4))
    return coeff * np.power((m_Ap / m_pi_D), 2) * np.power(m_V_D / m_pi_D, 2) * np.power((m_pi_D / f_pi_D), 4) * m_Ap * np.power(beta_func(x, y), 3 / 2.0)
def width_Ap_to_vector(alpha_D, f_pi_D, m_pi_D, m_V_D, m_Ap, multiplicity=1.0):
    # Partial width for A' -> V_D + (n_pions) with given multiplicity
    x = m_V_D / m_Ap
    y = m_pi_D / m_Ap
    prefactor = (alpha_D * multiplicity) / (192.0 * (math.pi**4))
    ratio_terms = (m_Ap / m_pi_D)**2 * (m_V_D / m_pi_D)**2 * (m_pi_D / f_pi_D)**4
    return prefactor * ratio_terms * m_Ap * (beta_func(x, y)**1.5)
def width_Ap_to_invis(alpha_D, m_pi_D, m_V_D, m_Ap):
    # Partial width for A' -> pi_D pi_D (invisible mode)
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

def plotRates(outdir):
    mAp = 100
    m_V_D = 1.8*mAp/3.0
    m_pi_D = mAp/3.0
    mpi_over_fpi=[2*(float(t)/100)+4*np.pi*(1.0-float(t)/100) for t in range(100)]
    alpha_D = .01
    rho_width = [width_Ap_to_vector(alpha_D, m_pi_D/mpi_over_fpi[i], m_pi_D, m_V_D, mAp, multiplicity=0.75) for i in range(len(mpi_over_fpi)) ]
    phi_width = [width_Ap_to_vector(alpha_D, m_pi_D/mpi_over_fpi[i], m_pi_D, m_V_D, mAp, multiplicity=1.5) for i in range(len(mpi_over_fpi)) ]
    invis_width = [width_Ap_to_invis(alpha_D, m_pi_D, m_V_D, mAp) for i in range(len(mpi_over_fpi)) ]
    fig, ax = plt.subplots(figsize=(6,4))
    ax.plot(np.linspace(0,2,100), rho_width, label="A'->rho pi", color="red")
    ax.plot(np.linspace(0,2,100), phi_width, label="A'->phi pi", color="blue")
    ax.plot(np.linspace(0,2,100), invis_width, label="A'->pi pi", color="green")
    ax.set_xlabel("mpi_D / f_pi_D")
    ax.set_ylabel("Width [GeV]")
    ax.set_title("Partial widths vs f ratio")
    ax.legend()
    ax.set_yscale("log")
    plt.savefig(outdir+"/rates.png")

##ALL OF THIS HAS BEEN VALIDATED SO FAR
print("I GOT HERE 4 31226")

# ===== MULTISCAN CHANGE START =====
def _suffix_outtxt(path, suffix):
    root, ext = os.path.splitext(path)
    if ext == "":
        ext = ".txt"
    return root + "_" + suffix + ext

def build_bg_cutflow_from_masks(stage_names, stage_masks, total_events, mass_mask, initial_in, initial_out, N_b_massbin):
    bg_cutflow_local = []
    for i, (stage, stage_mask) in enumerate(zip(stage_names, stage_masks)):
        n_in = np.count_nonzero(stage_mask & mass_mask)
        n_out = np.count_nonzero(stage_mask & ~mass_mask)
        frac_in = 100.0 * n_in / total_events if total_events > 0 else 0.0
        frac_out = 100.0 * n_out / total_events if total_events > 0 else 0.0
        if i == 0:
            exp_yield_in = N_b_massbin
            exp_yield_out = N_b_massbin
        else:
            frac_survive = (n_in / initial_in) if initial_in > 0 else 0.0
            frac_survive_out = (n_out / initial_out) if initial_out > 0 else 0.0
            exp_yield_in = N_b_massbin * frac_survive
            exp_yield_out = N_b_massbin * frac_survive_out
        bg_cutflow_local.append((stage, n_in, n_out, frac_in, frac_out, exp_yield_in, exp_yield_out))
    return bg_cutflow_local

def build_sig_cutflow_from_masks(stage_names, stage_masks, acc_yield, vis_yield, total_sig_events, p_accept_temp):
    sig_cutflow_local = []
    baseline_eff = (acc_yield / vis_yield) if vis_yield != 0 else 0.0
    sig_cutflow_local.append(("After acceptance", acc_yield, baseline_eff))
    for stage, stage_mask in zip(stage_names[1:], stage_masks[1:]):
        frac_survive = np.sum(p_accept_temp[stage_mask]) / total_sig_events if total_sig_events > 0 else 0.0
        yield_stage = acc_yield * frac_survive
        sig_cutflow_local.append((stage, yield_stage, frac_survive * 100.0))
    return sig_cutflow_local

def build_sig_table_from_cutflows(sig_cutflow_local, bg_cutflow_local, bg_available):
    sig_table_local = []
    if bg_available:
        bg_yields = {row[0]: row[5] for row in (bg_cutflow_local or [])}
        for stage, S_yield, _eff in sig_cutflow_local:
            if stage == "After acceptance":
                bg_stage = "No cuts"
            elif stage.startswith("After psum+L1L1+proj"):
                bg_stage = "After psum+L1L1+proj"
            elif stage.startswith("After psum+L1L1"):
                bg_stage = "After psum+L1L1"
            elif stage.startswith("After psum"):
                bg_stage = "After psum cut"
            elif stage == "After all cuts":
                bg_stage = "After all cuts"
            else:
                bg_stage = None
            B_yield = bg_yields.get(bg_stage, 0.0) if bg_yields else 0.0
            Zbi_val = zbi_significance(S_yield, B_yield)
            sig_table_local.append((stage, S_yield, B_yield, Zbi_val))
    else:
        for stage, S_yield, _eff in sig_cutflow_local:
            sig_table_local.append((stage, S_yield, 0.0, float('inf') if S_yield>0 else 0.0))
    return sig_table_local

def write_sig_table_to_txt(outtxt_path, mass_mev, epsilon, Val, Val2, sig_table_local):
    if sig_table_local:
        final_stage, final_S, final_B, final_Z = sig_table_local[-1]
    else:
        final_stage, final_S, final_B, final_Z = ("After all cuts", 0.0, 0.0, 0.0)
    with open(outtxt_path, "w") as fout:
        fout.write(f"mass_MeV {mass_mev}\n")
        fout.write(f"epsilon {epsilon}\n")
        fout.write(f"Val {Val}\n")
        fout.write(f"Val2 {Val2}\n")
        fout.write(f"stage {final_stage}\n")
        fout.write(f"S_yield {final_S:.6e}\n")
        fout.write(f"B_yield {final_B:.6e}\n")
        fout.write(f"Zbi {final_Z:.6f}\n")
# ===== MULTISCAN CHANGE END =====

# Main function
def main():
    ap = argparse.ArgumentParser(description="Compute cutflow and final Zbi for signal/background.")
    ap.add_argument("--mass", type=float, required=True, help="A' mass in MeV")
    ap.add_argument("--epsilon", type=float, required=True, help="Kinetic mixing parameter epsilon")
    ap.add_argument("--Val", type=int, default=25, help="Selection parameter (e.g. z0 threshold index).")
    ap.add_argument("--Val2", type=int, default=25, help="Selection parameter (e.g. proj_sig threshold index).")
    ap.add_argument("--base-module", type=str, default="decayLength8sel", help="Signal base module name.")  # updated default base module
    ap.add_argument("--outtxt", type=str, required=True, help="Output text file to store final yields and Zbi.")
    ap.add_argument("--debug", action="store_true", help="Enable debug output")
    # ===== ANN CHANGE START =====
    ap.add_argument("--ann-model", type=str, default="classifier_adv_2021_v9_pass5_run42QualCuts.pt",
                    help="Path to the trained ANN classifier .pt state_dict file.")
    ap.add_argument("--ann-scaler", type=str, default="scaler_2021_v9_pass5_run42_QualCuts.pkl",
                    help="Path to the StandardScaler .pkl file used during ANN training.")
    # ===== ANN CHANGE END =====
    args = ap.parse_args()
    print("I GOT HERE 6 31226")
    mass_mev = args.mass
    epsilon = args.epsilon
    Val = args.Val

    try:
        base = __import__(args.base_module)
    except Exception as e:
        sys.stderr.write(f"[error] Could not import signal base module '{args.base_module}': {e}\n")
        sys.exit(1)

    Val2 = args.Val2
    whichmass = base._mass_key(1.8*mass_mev/3.0)
    # ===== ANN CHANGE START =====
    # Load the trained ANN model and the exact StandardScaler used during training.
    ANN_MODEL_PATH = "/sdf/group/hps/users/rodwyer1/run/reach_curves/annstuff/classifier_adv_2021_v9_pass5_run42QualCuts_"+str(int(whichmass))+".pt"
    #ANN_SCALER_PATH = "/sdf/group/hps/users/rodwyer1/run/reach_curves/annstuff/scaler_2021_v9_pass5_run42_QualCuts_proto4_"+str(int(mass_mev))+".pkl"
    ANN_SCALER_PATH = "/sdf/group/hps/users/rodwyer1/run/reach_curves/annstuff/scaler_arrays_"+str(int(whichmass))+".npz"
    print("I GOT HERE 7 31226")
    try:
        sys.modules['numpy._core'] = np.core
        ann_scaler = np.load(ANN_SCALER_PATH)
        ann_scaler_mean = ann_scaler["mean"].astype(np.float32)
        ann_scaler_scale = ann_scaler["scale"].astype(np.float32)
    except Exception as e:
        sys.stderr.write(f"[error] Could not load ANN scaler '{ANN_SCALER_PATH}': {e}\n")
        sys.exit(1)

    print("I GOT HERE 7.5 31226")
    try:
        ann_model = ANNClassifier(in_features=34)
        ann_state = torch.load(ANN_MODEL_PATH, map_location="cpu")
        ann_model.load_state_dict(ann_state)
        ann_model.eval()
    except Exception as e:
        sys.stderr.write(f"[error] Could not load ANN model '{ANN_MODEL_PATH}': {e}\n")
        sys.exit(1)
    # ===== ANN CHANGE END =====
    print("I GOT HERE 8 31226")

    # ===== MULTISCAN CHANGE START =====
    # ===== MASS-DEPENDENT BDT LOAD FIX START =====
    BDT_MODEL_PATH = f"/sdf/group/hps/users/rodwyer1/run/reach_curves/optimization/bdt_trainer_31026_massdep/bdt_model_{int(whichmass)}.joblib"
    try:
        bdt_model = joblib.load(BDT_MODEL_PATH)
    except Exception as e:
        sys.stderr.write(f"[error] Could not load BDT model '{BDT_MODEL_PATH}': {e}\n")
        sys.exit(1)
    # ===== MASS-DEPENDENT BDT LOAD FIX END =====
    # ===== MULTISCAN CHANGE END =====
    # Import the signal base module (e.g., decayLength8sel.py) dynamically


    # ------------------------------
    # Background events processing
    # ------------------------------
    if bg is None or not hasattr(bg, "BACKGROUND_PATH"):
        sys.stderr.write("[warn] Background module not available, skipping background processing.\n")
        bg_events = None
    else:
        bg_path = bg.BACKGROUND_PATH
        Mkey = base._mass_key(1.8*mass_mev/3.0)
        try:
            # Open background file and get the TTree
            with uproot.open(bg_path) as f:
                # Use helper from module if available to find the tree
                if hasattr(bg, "_open_first_tree"):
                    tree = bg._open_first_tree(f)
                else:
                    # fallback: pick the first tree
                    keys = [k for k in f.keys() if ";" in k]
                    tree = f[keys[0]] if keys else None
                if tree is None:
                    sys.stderr.write("[error] No TTree found in background file.\n")
                    sys.exit(1)
                # Define the branches to load
                desired_branches = [
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
                    "ele_L1_iso_significance","pos_L1_iso_significance",
                    # [HIT CATEGORY] All hit category TTree flags loaded once here.
                    # To add a new category, add its branch name here and in the loops below.
                    "isL1L1", "isL2L2", "isL3L3", "isL1L2", "isL2L3"
                ]
                arrays = tree.arrays(desired_branches, library="ak", how=dict)
        except Exception as e:
            sys.stderr.write(f"[error] Failed to read background file: {e}\n")
            sys.exit(1)
        
        print("I GOT HERE 9 31226")
        # Convert Awkward arrays to numpy for easier masking
        invM = ak.to_numpy(arrays.get("vertex.invM_"))
        psum = ak.to_numpy(arrays.get("psum"))
        ele_z0 = ak.to_numpy(arrays.get("ele.track_.z0_"))
        pos_z0 = ak.to_numpy(arrays.get("pos.track_.z0_"))
        proj_sig = ak.to_numpy(arrays.get("vtx_proj_sig"))
        ele_layers_s = arrays.get("ele.track_.hit_layers_")
        pos_layers_s = arrays.get("pos.track_.hit_layers_")
        # [HIT CATEGORY] Build a mask dict for every hit category loaded above, plus a no-category entry.
        # To add a new category, add its name to HIT_CATEGORIES in both places below.
        # "nocat" = all-True mask (no hit category cut applied).
        HIT_CATEGORIES = ["isL1L1", "isL2L2", "isL3L3", "isL1L2", "isL2L3"]
        bg_hitcat_masks = {"nocat": np.ones(len(invM), dtype=bool)}
        for _cat in HIT_CATEGORIES:
            if _cat in arrays:
                bg_hitcat_masks[_cat] = np.asarray(ak.to_numpy(arrays[_cat]), dtype=bool)
            else:
                bg_hitcat_masks[_cat] = np.ones(len(invM), dtype=bool)
        # Keep L1L1_mask as an alias so existing code below that references it still works.
        L1L1_mask = bg_hitcat_masks["isL1L1"]

        # Define cut masks for background
        psum_mask = (psum >= 1.5) & (psum <= 3.0)

        # ===== ANN CHANGE START =====
        # ===== ORDER FIX START =====
        # Build separate ANN and BDT matrices so each model sees the feature ordering it was trained on.
        X_bg_ann = build_ann_matrix_from_bg_arrays(arrays)
        X_bg_bdt = build_bdt_matrix_from_bg_arrays(arrays)

        #MAYBE FIX RAM DRAW
        del arrays
        gc.collect()
        # ===== ORDER FIX END =====

        print("I GOT HERE 10 31226")
        # Predict ANN score (probability of signal) for each event
        ann_scores_bg = ann_predict_score(ann_model, ann_scaler_mean, ann_scaler_scale, X_bg_ann)
        print(ann_scores_bg)
        threshold_val = 1.0 - ((1.0-float(Val)/25.0)**3.0)
        #1.0 * (float(Val) / 25.0)
        print(threshold_val)
        ann_mask = (ann_scores_bg > threshold_val)
        # ===== MULTISCAN CHANGE START =====
        # ===== LOW-RAM CHANGE NOTE =====
        # Run the BDT in batches directly from X_bg so we do not materialize a second full BDT feature matrix.
        bdt_scores_bg = bdt_predict_score_batched(bdt_model, X_bg_bdt)
        bdt_mask = (bdt_scores_bg > threshold_val)
        z_thr = 0.5 * (float(Val) / 25.0)
        z0_mask = np.logical_and((ele_z0 > z_thr) | (ele_z0 < -z_thr),
                                 (pos_z0 > z_thr) | (pos_z0 < -z_thr))
        # ===== MULTISCAN CHANGE END =====
        # ===== I AM EDITING TO DECREASE RAM HERE START =====
        del X_bg_ann
        del X_bg_bdt
        del ann_scores_bg
        # ===== MULTISCAN CHANGE START =====
        del bdt_scores_bg
        # ===== MULTISCAN CHANGE END =====
        gc.collect()
        # ===== I AM EDITING TO DECREASE RAM HERE END =====


        # ===== ANN CHANGE END =====
        
        print("I GOT HERE 11 31226")
        # Mass window mask (±2 MeV around 1.8m_A/3.0)
        center_geV = float(Mkey)/1000.0
        #1.8*mass_mev / (1000.0 * 3.0)
        mass_mask = (invM > (center_geV - 0.002)) & (invM < (center_geV + 0.002))

        total_events = len(invM)
        initial_in = np.count_nonzero(mass_mask)
        initial_out = total_events - initial_in
        if total_events > 0:
            init_in_frac = 100.0 * initial_in / total_events
            init_out_frac = 100.0 * initial_out / total_events
        else:
            init_in_frac = init_out_frac = 0.0
        if args.debug:
            print(f"Initial in-window fraction: {init_in_frac:.2f}%, out-of-window: {init_out_frac:.2f}%")

        # Sequentially apply cuts and count in/out
        bg_cutflow = []  # will store tuples for table rows
        # ===== MULTISCAN CHANGE START =====
        projval = 50.0*(float(Val2)/10)+3.0
        common_stage_names = ["No cuts", "After psum cut", "After psum+hitcat", "After psum+hitcat+proj", "After all cuts"]

        x_gev = mass_mev / (1000.0)
        poly_num = (-6860.03 + 299358.0*x_gev - 4087220.0*(x_gev**2) +
                    25209900.0*(x_gev**3) - 73485900.0*(x_gev**4) +
                    82579800.0*(x_gev**5))
        m_fraction = poly_num / (82.9268041667 * 1000.0)
        N_B_TOTAL = 3.0e9
        N_b_massbin = N_B_TOTAL * m_fraction * 1.0

        # [HIT CATEGORY] Build cutflows for every category (and nocat).
        # Each entry: bg_cutflows_ann[cat], bg_cutflows_bdt[cat], bg_cutflows_cut[cat].
        # "nocat" gets ann+bdt+cut; named categories get ann+cut only.
        bg_cutflows_ann = {}
        bg_cutflows_bdt = {}
        bg_cutflows_cut = {}
        for _cat, _hitmask in bg_hitcat_masks.items():
            _bg_nocuts  = np.ones(total_events, dtype=bool)
            _bg_psum    = _bg_nocuts & psum_mask
            _bg_hitcat  = _bg_psum & _hitmask
            _bg_proj    = _bg_hitcat & (proj_sig < projval)
            _masks_ann  = [_bg_nocuts, _bg_psum, _bg_hitcat, _bg_proj, _bg_proj & ann_mask]
            _masks_cut  = [_bg_nocuts, _bg_psum, _bg_hitcat, _bg_proj, _bg_proj & z0_mask]
            bg_cutflows_ann[_cat] = build_bg_cutflow_from_masks(common_stage_names, _masks_ann, total_events, mass_mask, initial_in, initial_out, N_b_massbin)
            bg_cutflows_cut[_cat] = build_bg_cutflow_from_masks(common_stage_names, _masks_cut, total_events, mass_mask, initial_in, initial_out, N_b_massbin)
            if _cat == "nocat":
                _masks_bdt = [_bg_nocuts, _bg_psum, _bg_hitcat, _bg_proj, _bg_proj & bdt_mask]
                bg_cutflows_bdt[_cat] = build_bg_cutflow_from_masks(common_stage_names, _masks_bdt, total_events, mass_mask, initial_in, initial_out, N_b_massbin)
        # Legacy aliases so any downstream code referencing the old names still works.
        bg_cutflow_ann = bg_cutflows_ann["nocat"]
        bg_cutflow_bdt = bg_cutflows_bdt["nocat"]
        bg_cutflow_cut = bg_cutflows_cut["nocat"]
        bg_cutflow = bg_cutflow_ann

    ##ALL OF THIS HAS BEEN VALIDATED SO FAR

    # ------------------------------
    # Signal events processing
    # ------------------------------
    # Load signal numerator events (no tight selection applied yet)
    mkey = base._mass_key(1.8*mass_mev/3.0)

    try:
        events = base._events_cache(mkey)
        #THIS PORTION, WHILE LENGTHY, DOES REWEIGHTING BRIEFLY TO FIX CRAP, SHOULD MAKE EVERYTHING WORK RIGHT AWAY DOWNSTREAM
        #print("GOT HERE")
        alpha_D = 0.01  # fixed in decayLength7sel
        m_pi_D = mass_mev / 3.0
        m_V_D = 1.8*mass_mev / 3.0
        # For maximum visible fraction, use f_pi_D such that invisible width is minimal (f_pi as in last rho array entry scenario)
        f_pi_D = (mass_mev / 3.0) * (1.0 / (4.0 * math.pi))
        #print("GOT HERE")
        rho_width = width_Ap_to_vector(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, multiplicity=0.75)
        phi_width = width_Ap_to_vector(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, multiplicity=1.5)
        invis_width = width_Ap_to_invis(alpha_D, m_pi_D, m_V_D, mass_mev)
        charged_width = width_Ap_to_charged(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev)
        total_width = rho_width + phi_width + invis_width + charged_width
        rho_fraction = rho_width/total_width
        phi_fraction = phi_width/total_width
        #print("GOT HERE")

        rho_width = rate_2l(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, epsilon, .511, True)
        phi_width = rate_2l(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, epsilon, .511, False)
        rho_length=(1000*HBAR_C*10.0)/rho_width
        phi_length=(1000*HBAR_C*10.0)/phi_width
        #z_temp = np.asarray(events["vertex.pos_.fZ"], dtype=np.float64)
        z_temp = np.asarray(events["true_vd.vtx_z_"], dtype=np.float64) 
        psum_temp = np.asarray(events["psum"], dtype=np.float64)
        gamma_temp = 1000*psum_temp/m_V_D
        print("z_temp: ")
        print(z_temp)
        z_temp=z_temp*(z_temp>=0)
        print("gamma_temp: ")
        print(gamma_temp)
        print("rho_length: ")
        print(rho_length)
        print("exponand: ")
        print(z_temp/(gamma_temp*rho_length))
        print("rho_fraction: ")
        tot_frac=rho_fraction+phi_fraction
        rho_fraction/=tot_frac
        phi_fraction/=tot_frac
        print(rho_fraction)
        print("phi_fraction: ")
        print(phi_fraction)
        print("probability: ")
        print(rho_fraction*np.exp(-z_temp/(gamma_temp*rho_length))/(rho_length*gamma_temp))
        p_accept_temp = rho_fraction*np.exp(-z_temp/(gamma_temp*rho_length))/(rho_length*gamma_temp)
        p_accept_temp += phi_fraction*np.exp(-z_temp/(gamma_temp*phi_length))/(phi_length*gamma_temp)
        print("max_p_accept_temp: ")
        print(max(p_accept_temp))
        p_accept_temp/= max(p_accept_temp) 

        '''rng = np.random.default_rng(123)            # seed optional, helps reproducibility
        u = rng.random(len(z_temp))
        #print("GOT HERE")
        print("uniform: ")
        print(u)
        print("comparison prob: ")
        print(p_accept_temp)
        mask = (u < p_accept_temp)
        print(mask)
        mask = np.asarray(mask, dtype=bool)
        events = {k: np.asarray(v)[mask] for k, v in events.items()}
        print("The length of events is: "+str(len(events["psum"])))
        #events = events[mask]
        #print("GOT HERE")'''

    except Exception as e:
        sys.stderr.write(f"[error] Could not load signal events for mass {mass_mev}: {e}\n")
        sys.exit(1)
    # Extract needed branches from events (assuming events behaves like a dict)
    try:
        s_vertex_z = np.asarray(events["vertex.pos_.fZ"])
        s_psum = np.asarray(events["psum"])
        ele_z0 = np.asarray(events["ele.track_.z0_"])
        pos_z0 = np.asarray(events["pos.track_.z0_"])
        s_proj = np.asarray(events["vtx_proj_sig"])
    except Exception as e:
        sys.stderr.write(f"[error] Signal events missing required branches: {e}\n")
        sys.exit(1)
    # [HIT CATEGORY] Build a mask dict for every hit category from the cached events dict, plus nocat.
    # "nocat" = all-True (no hit category cut). Named categories use the TTree flag directly.
    # To add a new category, add its name to HIT_CATEGORIES here (must also be in decayLength8sel BRANCHES).
    HIT_CATEGORIES = ["isL1L1", "isL2L2", "isL3L3", "isL1L2", "isL2L3"]
    sig_hitcat_masks = {"nocat": np.ones_like(s_psum, dtype=bool)}
    for _cat in HIT_CATEGORIES:
        if _cat in events:
            sig_hitcat_masks[_cat] = np.asarray(events[_cat], dtype=bool)
        else:
            sig_hitcat_masks[_cat] = np.ones_like(s_psum, dtype=bool)
    # Legacy alias so existing code referencing s_L1L1_mask still works.
    s_L1L1_mask = sig_hitcat_masks["isL1L1"]

    # Define selection masks for signal
    s_psum_mask = (s_psum >= 1.5) & (s_psum <= 3.0)
    # ===== ANN CHANGE START =====
    # ===== ORDER FIX START =====
    # Build separate ANN and BDT matrices so each model sees the feature ordering it was trained on.
    s_threshold_val = .92*(1-float(Val)/25.0)+.9996*(float(Val)/25.0)
    #1.0 - ((1.0-float(Val)/25.0)**6.0)
    #1.0 * (float(Val) / 25.0)
    X_sig_ann = build_ann_matrix_from_sig_events(events)
    X_sig_bdt = build_bdt_matrix_from_sig_events(events)
    gc.collect()
    # ===== ORDER FIX END =====

    ann_scores_sig = ann_predict_score(ann_model, ann_scaler_mean, ann_scaler_scale, X_sig_ann)
    ann_sig_mask = (ann_scores_sig > s_threshold_val)
    s_threshold_val = 1.0 - ((1.0-float(Val)/25.0)**3.0)
    # ===== MULTISCAN CHANGE START =====
    # ===== LOW-RAM CHANGE NOTE =====
    # Run the BDT in batches directly from X_sig so we do not materialize a second full BDT feature matrix.
    bdt_scores_sig = bdt_predict_score_batched(bdt_model, X_sig_bdt)
    bdt_sig_mask = (bdt_scores_sig > s_threshold_val)
    z_thr = 0.5 * (float(Val) / 25.0)
    s_z0_mask = np.logical_and((ele_z0 > z_thr) | (ele_z0 < -z_thr),
                               (pos_z0 > z_thr) | (pos_z0 < -z_thr))
    # ===== MULTISCAN CHANGE END =====
    # ===== I AM EDITING TO DECREASE RAM HERE START =====
    del X_sig_ann
    del X_sig_bdt
    del ann_scores_sig
    # ===== MULTISCAN CHANGE START =====
    del bdt_scores_sig
    # ===== MULTISCAN CHANGE END =====
    gc.collect()
    # ===== I AM EDITING TO DECREASE RAM HERE END =====



    # ===== ANN CHANGE END =====

    projval = 50.0*(float(Val2)/10)+3.0
    #1.0+np.max([0,float(Val2)/10.0-4.0])**3.0
    s_proj_mask = (s_proj < projval)
    total_sig_events = len(s_psum)
    # Count survivors and efficiencies stage by stage
    sig_cutflow = []  # list of tuples (stage, yield, cum_eff%)
    # Compute theoretical yields at key stages:
    # 1. Production yield (no decays yet)
    scale_const = 3.0 * math.pi / (2.0 * 1.0 * (1.0 / 137.0459991))
    try:
        ratio_val = base.ratio(mass_mev)
        print("Used this route")
    except Exception as e:
        sys.stderr.write(f"[error] Failed to compute ratio(mA): {e}\n")
        ratio_val = 0.0
    

    print("scale_const: "+str(scale_const))
    print("ratio_val: "+str(ratio_val))
    print("mass_mev: "+str(mass_mev))
    print("epsilon: "+str(epsilon))

    core = scale_const * ratio_val * mass_mev * (epsilon ** 2)

    N_B_TOTAL = 3.0e9
    x_gev = mass_mev / 1000.0

    poly_num = (-6860.03 + 299358.0*x_gev - 4087220.0*(x_gev**2) +
                25209900.0*(x_gev**3) - 73485900.0*(x_gev**4) +
                82579800.0*(x_gev**5))
    m_fraction = poly_num / (82.9268041667 * 1000.0)  # fraction per 0.1 GeV in this mass bin
    N_b_massbin = N_B_TOTAL * m_fraction * 1.0  # expected events in ±0.002 GeV window if no selection

    prod_yield = N_b_massbin * core  # number of A' produced (per baseline N_B events)
    # 2. Visible yield (multiply by rho fraction last element):
    alpha_D = 0.01  # fixed in decayLength8sel
    m_pi_D = mass_mev / 3.0
    m_V_D = 1.8*mass_mev / 3.0
    f_pi_D = (mass_mev / 3.0) * (1.0 / (4.0 * math.pi))
    rho_width = width_Ap_to_vector(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, multiplicity=0.75)
    phi_width = width_Ap_to_vector(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, multiplicity=1.5)
    invis_width = width_Ap_to_invis(alpha_D, m_pi_D, m_V_D, mass_mev)
    charged_width = width_Ap_to_charged(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev)
    total_width = rho_width + phi_width + invis_width + charged_width
    rho_fraction = rho_width/total_width
    phi_fraction = phi_width/total_width

    # Decay length in mm (lab): (HBAR_C/total_width in cm) * 10 * beta_gamma
    s_gamma = (s_psum *1000.0)/m_V_D
    s_x = s_vertex_z/s_gamma  
    prho=0
    pphi=0

    den_edges, den_vals = base.read_den_hist(m_V_D)
    mk = base._mass_key(m_V_D)
    mask_hist = base.tight_selection(events,Val,Val2)  # using updated tight selection logic upstream
    #zvals  = events["vertex.pos_.fZ"][mask_hist]
    zvals  = events["true_vd.vtx_z_"][mask_hist]
    print(zvals)
    num_vals, _ = np.histogram(zvals, bins=den_edges)
    psum_edges, psum_vals = base.read_psum_hist(m_V_D)
    for I in range(len(den_vals)):
        Ngen = float(den_vals[I])
        Nacc = float(num_vals[I])
        z_cent = .5*(den_edges[I]+den_edges[I+1])
        for J in range(len(psum_vals)):
            psum_val = .5*(psum_edges[J]+psum_edges[J+1])
            gamma = 1000*psum_val/m_V_D
            rho_width = rate_2l(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, epsilon, .511, True)
            phi_width = rate_2l(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, epsilon, .511, False)
            rho_length=(1000*HBAR_C*10.0)/rho_width
            if(z_cent<0):
                z_cent=0
            phi_length=(1000*HBAR_C*10.0)/phi_width
            if Ngen==0.0:
                Ngen=1.0
                Nacc=0.0
            print("Nacc: "+ str(Nacc))
            print("zcent: "+str(z_cent))
            print("gamma times rho_length: "+str(gamma*rho_length))
            print("gamma: "+str(gamma))
            prho += psum_vals[J]*(Nacc/Ngen)*np.exp(-z_cent/(gamma*rho_length))/(rho_length*gamma)
            pphi += psum_vals[J]*(Nacc/Ngen)*np.exp(-z_cent/(gamma*phi_length))/(phi_length*gamma)

    vis_yield = prod_yield*(rho_fraction+phi_fraction)   
    acc_yield = prod_yield*(prho*rho_fraction+pphi*phi_fraction)
    print("Signal processing complete")

    print("m_fraction: "+str(m_fraction))
    print("core: "+str(core))
    print("N_b_massbin: "+str(N_b_massbin))
    print("prho: "+str(prho))
    print("rho_fraction: "+str(rho_fraction))
    print("Prod_yield: "+str(prod_yield))
    print("Vis_yield: "+str(vis_yield))

    # Now selection stages:
    if acc_yield < 0:
        acc_yield = 0.0  # ensure non-negative
        irint(threshold_val)
    sig_cutflow.append(("After acceptance", acc_yield, acc_yield/vis_yield))  # baseline for selection efficiency
    # ===== MULTISCAN CHANGE START =====
    sig_stage_names = ["No cuts", "After psum cut", "After psum+hitcat", "After psum+hitcat+proj", "After all cuts"]
    sig_mask_nocuts = np.ones(total_sig_events, dtype=bool)
    sig_mask_psum   = sig_mask_nocuts & s_psum_mask

    # [HIT CATEGORY] Build signal cutflows and write output files for every category.
    # "nocat" produces ann + bdt + cut output files (3 files).
    # Each named category produces ann + cut output files (2 files each, 5 categories = 10 files).
    # Total: 3 + 10 = 13 output files.
    # To add a new category, add its name to HIT_CATEGORIES in sig_hitcat_masks above; no changes needed here.
    ann_outtxt = _suffix_outtxt(args.outtxt, "ann")
    bdt_outtxt = _suffix_outtxt(args.outtxt, "bdt")
    cut_outtxt = _suffix_outtxt(args.outtxt, "cut")

    try:
        for _cat, _s_hitmask in sig_hitcat_masks.items():
            _sig_hitcat = sig_mask_psum & _s_hitmask
            _sig_proj   = _sig_hitcat & s_proj_mask

            _masks_ann = [sig_mask_nocuts, sig_mask_psum, _sig_hitcat, _sig_proj, _sig_proj & ann_sig_mask]
            _masks_cut = [sig_mask_nocuts, sig_mask_psum, _sig_hitcat, _sig_proj, _sig_proj & s_z0_mask]

            _bg_ann = bg_cutflows_ann.get(_cat) if bg is not None else None
            _bg_cut = bg_cutflows_cut.get(_cat) if bg is not None else None

            _cf_ann = build_sig_cutflow_from_masks(sig_stage_names, _masks_ann, acc_yield, vis_yield, total_sig_events, p_accept_temp)
            _cf_cut = build_sig_cutflow_from_masks(sig_stage_names, _masks_cut, acc_yield, vis_yield, total_sig_events, p_accept_temp)

            _tbl_ann = build_sig_table_from_cutflows(_cf_ann, _bg_ann, bg is not None)
            _tbl_cut = build_sig_table_from_cutflows(_cf_cut, _bg_cut, bg is not None)

            # Output file suffix: "ann" / "cut" for nocat; "<category>_ann" / "<category>_cut" for named categories.
            _sfx_ann = "ann"       if _cat == "nocat" else f"{_cat}_ann"
            _sfx_cut = "cut"       if _cat == "nocat" else f"{_cat}_cut"
            write_sig_table_to_txt(_suffix_outtxt(args.outtxt, _sfx_ann), mass_mev, epsilon, Val, Val2, _tbl_ann)
            write_sig_table_to_txt(_suffix_outtxt(args.outtxt, _sfx_cut), mass_mev, epsilon, Val, Val2, _tbl_cut)

            if _cat == "nocat":
                # BDT only for no-category run
                _masks_bdt = [sig_mask_nocuts, sig_mask_psum, _sig_hitcat, _sig_proj, _sig_proj & bdt_sig_mask]
                _bg_bdt    = bg_cutflows_bdt.get("nocat") if bg is not None else None
                _cf_bdt    = build_sig_cutflow_from_masks(sig_stage_names, _masks_bdt, acc_yield, vis_yield, total_sig_events, p_accept_temp)
                _tbl_bdt   = build_sig_table_from_cutflows(_cf_bdt, _bg_bdt, bg is not None)
                write_sig_table_to_txt(_suffix_outtxt(args.outtxt, "bdt"), mass_mev, epsilon, Val, Val2, _tbl_bdt)

    except Exception as e:
        sys.stderr.write(f"[error] Failed to write output text file set rooted at '{args.outtxt}': {e}\n")
        sys.exit(1)

    # ===== LOW-RAM CHANGE START =====
    del sig_mask_nocuts, sig_mask_psum
    gc.collect()
    # ===== LOW-RAM CHANGE END =====
    # ===== MULTISCAN CHANGE END =====

    if args.debug:
        print(f"[done] Wrote nocat ANN result to {_suffix_outtxt(args.outtxt, 'ann')}")
        print(f"[done] Wrote nocat BDT result to {_suffix_outtxt(args.outtxt, 'bdt')}")
        print(f"[done] Wrote nocat cut result to {_suffix_outtxt(args.outtxt, 'cut')}")
        for _cat in ["isL1L1", "isL2L2", "isL3L3", "isL1L2", "isL2L3"]:
            print(f"[done] Wrote {_cat} ANN result to {_suffix_outtxt(args.outtxt, _cat+'_ann')}")
            print(f"[done] Wrote {_cat} cut result to {_suffix_outtxt(args.outtxt, _cat+'_cut')}")

if __name__ == "__main__":
    print("I GOT HERE 5 31226")
    main()



