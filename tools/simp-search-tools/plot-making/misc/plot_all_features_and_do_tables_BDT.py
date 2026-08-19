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
    if B == 0:
        return 9.0 if S > 0 else 0.0
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

# Functions to compute hidden sector decay fractions (using decayLength7/8sel model equations)
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
    plt.savefig(os.path.join(outdir, "rates.png"))

## ALL OF THIS HAS BEEN VALIDATED SO FAR

# Main function
def main():
    ap = argparse.ArgumentParser(description="Compute cutflow and final Zbi for signal/background, and write LaTeX tables.")
    ap.add_argument("--mass", type=float, required=True, help="A' mass in MeV")
    ap.add_argument("--epsilon", type=float, required=True, help="Kinetic mixing parameter epsilon")
    ap.add_argument("--Val", type=int, default=25, help="Selection parameter (e.g. z0/BDT threshold index).")
    ap.add_argument("--Val2", type=int, default=25, help="Selection parameter (e.g. proj_sig threshold index).")
    ap.add_argument("--outdir", type=str, default="output_plots", help="Directory for output tables/files.")
    ap.add_argument("--bins", type=int, default=80, help="Bins for 1D histograms (unused here, kept for interface).")
    ap.add_argument("--bins2d", type=int, default=80, help="Bins per axis for 2D histograms (unused here, kept for interface).")
    ap.add_argument("--base-module", type=str, default="decayLength8sel", help="Signal base module name.")
    ap.add_argument("--debug", action="store_true", help="Enable debug output")
    args = ap.parse_args()

    mass_mev = args.mass
    epsilon = args.epsilon
    Val = args.Val
    Val2 = args.Val2

    os.makedirs(args.outdir, exist_ok=True)

    # Load the trained BDT model (for replacing z-threshold cuts)
    BDT_MODEL_PATH = "/sdf/group/hps/users/rodwyer1/run/reach_curves/optimization/bdt_trainer/bdt_model.joblib"
    try:
        bdt_model = joblib.load(BDT_MODEL_PATH)
    except Exception as e:
        sys.stderr.write(f"[error] Could not load BDT model: {e}\n")
        sys.exit(1)

    # Import the signal base module (e.g., decayLength8sel.py) dynamically
    try:
        base = __import__(args.base_module)
    except Exception as e:
        sys.stderr.write(f"[error] Could not import signal base module '{args.base_module}': {e}\n")
        sys.exit(1)

    # ------------------------------
    # Background events processing
    # ------------------------------
    if bg is None or not hasattr(bg, "BACKGROUND_PATH"):
        sys.stderr.write("[warn] Background module not available, skipping background processing.\n")
        bg_events = None
        bg_cutflow = None
    else:
        bg_path = bg.BACKGROUND_PATH
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
                # Define the branches to load (extended BDT feature set)
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
                    "ele.track_.hit_layers_", "pos.track_.hit_layers_"
                ]
                arrays = tree.arrays(desired_branches, library="ak", how=dict)
        except Exception as e:
            sys.stderr.write(f"[error] Failed to read background file: {e}\n")
            sys.exit(1)

        # Convert Awkward arrays to numpy for easier masking
        invM = ak.to_numpy(arrays.get("vertex.invM_"))
        psum = ak.to_numpy(arrays.get("psum"))
        ele_z0 = ak.to_numpy(arrays.get("ele.track_.z0_"))
        pos_z0 = ak.to_numpy(arrays.get("pos.track_.z0_"))
        proj_sig = ak.to_numpy(arrays.get("vtx_proj_sig"))
        ele_layers_s = arrays.get("ele.track_.hit_layers_")
        pos_layers_s = arrays.get("pos.track_.hit_layers_")
        if ele_layers_s is not None:
            ele_hasL0_s = ak.to_numpy(ak.any(ele_layers_s == 0, axis=-1))
            ele_hasL1_s = ak.to_numpy(ak.any(ele_layers_s == 1, axis=-1))
            ele_L1L1_s = np.asarray(ele_hasL0_s & ele_hasL1_s, bool)
        else:
            ele_L1L1_s = np.ones_like(invM, bool)
        if pos_layers_s is not None:
            pos_hasL0_s = ak.to_numpy(ak.any(pos_layers_s == 0, axis=-1))
            pos_hasL1_s = ak.to_numpy(ak.any(pos_layers_s == 1, axis=-1))
            pos_L1L1_s = np.asarray(pos_hasL0_s & pos_hasL1_s, bool)
        else:
            pos_L1L1_s = np.ones_like(invM, bool)

        L1L1_mask = np.logical_and(ele_L1L1_s, pos_L1L1_s)

        # Define cut masks for background
        psum_mask = (psum >= 1.5) & (psum <= 3.0)

        # BDT scoring for background events (replaces z-threshold cut)
        # Prepare feature matrix for BDT (same features used in training)
        bdt_features = []
        # vertex.invM_, psum
        bdt_features.append(invM.reshape(-1,1))
        bdt_features.append(psum.reshape(-1,1))
        # vertex.pos_ fields
        vertex_pos = ak.to_numpy(arrays["vertex.pos_"])
        for fld in vertex_pos.dtype.names:
            bdt_features.append(vertex_pos[fld].reshape(-1,1))
        # electron track features
        ele_arr = {
            "n_hits_": ak.to_numpy(arrays["ele.track_.n_hits_"]),
            "d0_":    ak.to_numpy(arrays["ele.track_.d0_"]),
            "phi0_":  ak.to_numpy(arrays["ele.track_.phi0_"]),
            "z0_":    ak.to_numpy(arrays["ele.track_.z0_"]),
            "tan_lambda_": ak.to_numpy(arrays["ele.track_.tan_lambda_"]),
            "px_":    ak.to_numpy(arrays["ele.track_.px_"]),
            "py_":    ak.to_numpy(arrays["ele.track_.py_"]),
            "pz_":    ak.to_numpy(arrays["ele.track_.pz_"]),
            "chi2_":  ak.to_numpy(arrays["ele.track_.chi2_"]),
            "x_at_ecal_": ak.to_numpy(arrays["ele.track_.x_at_ecal_"]),
            "y_at_ecal_": ak.to_numpy(arrays["ele.track_.y_at_ecal_"]),
            "z_at_ecal_": ak.to_numpy(arrays["ele.track_.z_at_ecal_"])
        }
        for arr in ele_arr.values():
            bdt_features.append(arr.reshape(-1,1))
        # positron track features
        pos_arr = {
            "n_hits_": ak.to_numpy(arrays["pos.track_.n_hits_"]),
            "d0_":    ak.to_numpy(arrays["pos.track_.d0_"]),
            "phi0_":  ak.to_numpy(arrays["pos.track_.phi0_"]),
            "z0_":    ak.to_numpy(arrays["pos.track_.z0_"]),
            "tan_lambda_": ak.to_numpy(arrays["pos.track_.tan_lambda_"]),
            "px_":    ak.to_numpy(arrays["pos.track_.px_"]),
            "py_":    ak.to_numpy(arrays["pos.track_.py_"]),
            "pz_":    ak.to_numpy(arrays["pos.track_.pz_"]),
            "chi2_":  ak.to_numpy(arrays["pos.track_.chi2_"]),
            "x_at_ecal_": ak.to_numpy(arrays["pos.track_.x_at_ecal_"]),
            "y_at_ecal_": ak.to_numpy(arrays["pos.track_.y_at_ecal_"]),
            "z_at_ecal_": ak.to_numpy(arrays["pos.track_.z_at_ecal_"])
        }
        for arr in pos_arr.values():
            bdt_features.append(arr.reshape(-1,1))
        # vertex chi2 and invMerr
        bdt_features.append(ak.to_numpy(arrays["vertex.chi2_"]).reshape(-1,1))
        bdt_features.append(ak.to_numpy(arrays["vertex.invMerr_"]).reshape(-1,1))
        # vertex projection features
        bdt_features.append(proj_sig.reshape(-1,1))
        bdt_features.append(ak.to_numpy(arrays["vtx_proj_x_sig"]).reshape(-1,1))
        bdt_features.append(ak.to_numpy(arrays["vtx_proj_y_sig"]).reshape(-1,1))
        # Combine into feature matrix
        X_bg = np.hstack(bdt_features)
        X_bg = np.nan_to_num(X_bg, nan=0.0, posinf=0.0, neginf=0.0)
        # Predict BDT score (probability of signal) for each event
        bdt_scores_bg = bdt_model.predict_proba(X_bg)[:,1]
        threshold_val = 0.5 * (float(Val) / 25.0)
        bdt_mask = (bdt_scores_bg > threshold_val)

        # Mass window mask (±2 MeV around 1.8m_A/3.0)
        center_geV = 1.8*mass_mev / (1000.0 * 3.0)
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
        # Stage 0: no cuts (baseline)
        bg_cutflow.append(("No cuts",
                           initial_in, initial_out,
                           init_in_frac, init_out_frac,
                           None, None))  # yields filled later
        # Apply each cut in order, building on previous mask
        current_mask = np.ones(total_events, dtype=bool)
        # 1. psum
        current_mask &= psum_mask
        n_in = np.count_nonzero(current_mask & mass_mask)
        n_out = np.count_nonzero(current_mask & ~mass_mask)
        frac_in = 100.0 * n_in / total_events if total_events > 0 else 0.0
        frac_out = 100.0 * n_out / total_events if total_events > 0 else 0.0
        bg_cutflow.append(("After psum cut", n_in, n_out, frac_in, frac_out, None, None))
        # 2. L1L1
        current_mask &= L1L1_mask
        n_in = np.count_nonzero(current_mask & mass_mask)
        n_out = np.count_nonzero(current_mask & ~mass_mask)
        frac_in = 100.0 * n_in / total_events if total_events > 0 else 0.0
        frac_out = 100.0 * n_out / total_events if total_events > 0 else 0.0
        bg_cutflow.append(("After psum+L1L1", n_in, n_out, frac_in, frac_out, None, None))
        # 3. proj_sig
        current_mask &= proj_sig < (2*(float(Val2)/10)+1)
        n_in = np.count_nonzero(current_mask & mass_mask)
        n_out = np.count_nonzero(current_mask & ~mass_mask)
        frac_in = 100.0 * n_in / total_events if total_events > 0 else 0.0
        frac_out = 100.0 * n_out / total_events if total_events > 0 else 0.0
        bg_cutflow.append(("After psum+L1L1+proj", n_in, n_out, frac_in, frac_out, None, None))
        # 4. BDT score (replaces z0 cut)
        current_mask &= bdt_mask
        n_in = np.count_nonzero(current_mask & mass_mask)
        n_out = np.count_nonzero(current_mask & ~mass_mask)
        frac_in = 100.0 * n_in / total_events if total_events > 0 else 0.0
        frac_out = 100.0 * n_out / total_events if total_events > 0 else 0.0
        bg_cutflow.append(("After all cuts", n_in, n_out, frac_in, frac_out, None, None))

        # Compute expected background yield in the ±2 MeV window after each stage
        x_gev = mass_mev / 1000.0
        poly_num = (-6860.03 + 299358.0*x_gev - 4087220.0*(x_gev**2) +
                    25209900.0*(x_gev**3) - 73485900.0*(x_gev**4) +
                    82579800.0*(x_gev**5))
        m_fraction = poly_num / (82.9268041667 * 1000.0)  # fraction per 0.1 GeV in this mass bin
        N_B_TOTAL = 3.0e9  # total number of background-triggered events
        N_b_massbin = N_B_TOTAL * m_fraction * 4.0  # expected events in ±0.002 GeV window if no selection

        # Loop through cutflow to fill expected yields
        for i, row in enumerate(bg_cutflow):
            stage, n_in, n_out, pct_in, pct_out, _, _ = row
            if i == 0:
                exp_yield_in = N_b_massbin
                exp_yield_out = N_b_massbin
            else:
                if initial_in > 0:
                    frac_survive = n_in / initial_in
                else:
                    frac_survive = 0.0
                if initial_out > 0:
                    frac_survive_out = n_out / initial_out
                else:
                    frac_survive_out = 0.0
                exp_yield_in = N_b_massbin * frac_survive
                exp_yield_out = N_b_massbin * frac_survive_out
            bg_cutflow[i] = (stage, n_in, n_out, pct_in, pct_out, exp_yield_in, exp_yield_out)

    ## ALL OF THIS HAS BEEN VALIDATED SO FAR

    # ------------------------------
    # Signal events processing
    # ------------------------------
    # Load signal numerator events (no tight selection applied yet)
    mkey = base._mass_key(1.8*mass_mev/3.0)
    try:
        events = base._events_cache(mkey)
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

    # Handle L1L1 for signal
    if "ele.hasL0L1" in events:
        s_e_hasL0L1 = np.asarray(events["ele.hasL0L1"], dtype=bool)
    else:
        s_e_hasL0L1 = None
    if "pos.hasL0L1" in events:
        s_p_hasL0L1 = np.asarray(events["pos.hasL0L1"], dtype=bool)
    else:
        s_p_hasL0L1 = None
    if s_e_hasL0L1 is None or s_p_hasL0L1 is None:
        if "ele.track_.hit_layers_" in events:
            ele_layers = events["ele.track_.hit_layers_"]
            pos_layers = events.get("pos.track_.hit_layers_", None)
            ele_layers_ak = ak.Array(ele_layers)
            hasL0_e = (ele_layers_ak == 0).any(axis=1)
            hasL1_e = (ele_layers_ak == 1).any(axis=1)
            s_e_hasL0L1 = np.logical_and(np.array(hasL0_e, dtype=bool), np.array(hasL1_e, dtype=bool))
            if pos_layers is not None:
                pos_layers_ak = ak.Array(pos_layers)
                hasL0_p = (pos_layers_ak == 0).any(axis=1)
                hasL1_p = (pos_layers_ak == 1).any(axis=1)
                s_p_hasL0L1 = np.logical_and(np.array(hasL0_p, dtype=bool), np.array(hasL1_p, dtype=bool))
            else:
                s_p_hasL0L1 = np.ones_like(s_e_hasL0L1, dtype=bool)
        else:
            s_e_hasL0L1 = np.ones_like(s_psum, dtype=bool)
            s_p_hasL0L1 = np.ones_like(s_psum, dtype=bool)
    s_L1L1_mask = np.logical_and(s_e_hasL0L1, s_p_hasL0L1)

    # Define selection masks for signal
    s_psum_mask = (s_psum >= 1.5) & (s_psum <= 3.0)

    # BDT scoring for signal events (replaces z-threshold cut)
    s_threshold_val = 0.5 * (float(Val) / 25.0)
    s_bdt_features = []
    # invariant mass, psum
    s_bdt_features.append(np.asarray(events["vertex.invM_"]).reshape(-1,1))
    s_bdt_features.append(np.asarray(events["psum"]).reshape(-1,1))
    # vertex.pos_ fields
    s_bdt_features.append(np.asarray(events["vertex.pos_.fX"]).reshape(-1,1))
    s_bdt_features.append(np.asarray(events["vertex.pos_.fY"]).reshape(-1,1))
    s_bdt_features.append(np.asarray(events["vertex.pos_.fZ"]).reshape(-1,1))
    # electron track features
    ele_keys = ["n_hits_", "d0_", "phi0_", "z0_", "tan_lambda_", "px_", "py_", "pz_", "chi2_", "x_at_ecal_", "y_at_ecal_", "z_at_ecal_"]
    for key in ele_keys:
        s_bdt_features.append(np.asarray(events[f"ele.track_.{key}"]).reshape(-1,1))
    # positron track features
    pos_keys = ["n_hits_", "d0_", "phi0_", "z0_", "tan_lambda_", "px_", "py_", "pz_", "chi2_", "x_at_ecal_", "y_at_ecal_", "z_at_ecal_"]
    for key in pos_keys:
        s_bdt_features.append(np.asarray(events[f"pos.track_.{key}"]).reshape(-1,1))
    # vertex chi2 and invMerr
    s_bdt_features.append(np.asarray(events["vertex.chi2_"]).reshape(-1,1))
    s_bdt_features.append(np.asarray(events["vertex.invMerr_"]).reshape(-1,1))
    # vertex projection features
    s_bdt_features.append(np.asarray(events["vtx_proj_sig"]).reshape(-1,1))
    s_bdt_features.append(np.asarray(events["vtx_proj_x_sig"]).reshape(-1,1))
    s_bdt_features.append(np.asarray(events["vtx_proj_y_sig"]).reshape(-1,1))
    X_sig = np.hstack(s_bdt_features)
    X_sig = np.nan_to_num(X_sig, nan=0.0, posinf=0.0, neginf=0.0)
    bdt_scores_sig = bdt_model.predict_proba(X_sig)[:,1]
    bdt_sig_mask = (bdt_scores_sig > s_threshold_val)

    projval = 2*(float(Val2)/10)+1
    s_proj_mask = (s_proj < projval)
    total_sig_events = len(s_psum)

    # Count survivors and efficiencies stage by stage
    sig_cutflow = []  # list of tuples (stage, S_yield, cum_eff%)

    # 1. Production yield (no decays yet)
    scale_const = 3.0 * math.pi / (2.0 * 1.0 * (1.0 / 137.0459991))
    try:
        ratio_val = base.ratio(mass_mev)
    except Exception as e:
        sys.stderr.write(f"[error] Failed to compute ratio(mA): {e}\n")
        ratio_val = 0.0
    core = scale_const * ratio_val * mass_mev * (epsilon ** 2)

    N_B_TOTAL = 3.0e9
    x_gev = mass_mev / 1000.0
    poly_num = (-6860.03 + 299358.0*x_gev - 4087220.0*(x_gev**2) +
                25209900.0*(x_gev**3) - 73485900.0*(x_gev**4) +
                82579800.0*(x_gev**5))
    m_fraction = poly_num / (82.9268041667 * 1000.0)
    N_b_massbin = N_B_TOTAL * m_fraction * 4.0

    prod_yield = N_b_massbin * core  # number of A' produced

    # 2. Visible yield fractions
    alpha_D = 0.01  # fixed
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

    # acceptance integral
    s_gamma = (s_psum *1000.0)/m_V_D
    s_x = s_vertex_z/s_gamma
    prho = 0.0
    pphi = 0.0

    den_edges, den_vals = base.read_den_hist(m_V_D)
    mk = base._mass_key(m_V_D)
    mask_hist = base.tight_selection(events,Val,Val2)  # updated tight selection (BDT-based)
    zvals  = events["vertex.pos_.fZ"][mask_hist]
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
            phi_length=(1000*HBAR_C*10.0)/phi_width
            #print("Psum Val: "+str(psum_vals[J]))
            #print("F(z): "+str(Nacc/Ngen))
            #print("Nacc: "+str(Nacc))
            #print("Ngen: "+str(Ngen))
            #print("rho_len: "+str(rho_length))
            #print("gamma: "+str(gamma))
            #print("z_cent: "+str(z_cent))
            #print("Shifted rho_len: "+str(rho_length*gamma))
            #print("exp val: "+str(np.exp(-z_cent/(gamma*rho_length))))
            #print("Added Prob: "+str(psum_vals[J]*(Nacc/Ngen)*np.exp(-z_cent/(gamma*rho_length))/(rho_length*gamma)))
            #print("Total prob: "+str(prho)+"\n")
            prho += psum_vals[J]*(Nacc/Ngen)*np.exp(-z_cent/(gamma*rho_length))/(rho_length*gamma)
            pphi += psum_vals[J]*(Nacc/Ngen)*np.exp(-z_cent/(gamma*phi_length))/(phi_length*gamma)

    vis_yield = prod_yield*(rho_fraction+phi_fraction)
    acc_yield = prod_yield*(prho*rho_fraction+pphi*phi_fraction)
    print("Signal processing complete")

    # Selection stages:
    if acc_yield < 0:
        acc_yield = 0.0  # ensure non-negative
    sig_cutflow.append(("After acceptance", acc_yield, acc_yield/vis_yield))  # baseline

    current_mask = np.ones(total_sig_events, dtype=bool)
    # psum cut
    current_mask &= s_psum_mask
    frac_survive = np.count_nonzero(current_mask) / total_sig_events if total_sig_events > 0 else 0.0
    yield_psum = acc_yield * frac_survive
    sig_cutflow.append(("After psum cut", yield_psum, frac_survive * 100.0))
    # L1L1 cut
    current_mask &= s_L1L1_mask
    frac_survive = np.count_nonzero(current_mask) / total_sig_events if total_sig_events > 0 else 0.0
    yield_L1 = acc_yield * frac_survive
    sig_cutflow.append(("After psum+L1L1", yield_L1, frac_survive * 100.0))
    # proj_sig cut
    current_mask &= s_proj_mask
    frac_survive = np.count_nonzero(current_mask) / total_sig_events if total_sig_events > 0 else 0.0
    yield_proj = acc_yield * frac_survive
    sig_cutflow.append(("After psum+L1L1+proj", yield_proj, frac_survive * 100.0))
    # BDT score cut (replaces z0 cut)
    current_mask &= bdt_sig_mask
    frac_survive = np.count_nonzero(current_mask) / total_sig_events if total_sig_events > 0 else 0.0
    yield_final = acc_yield * frac_survive
    sig_cutflow.append(("After all cuts", yield_final, frac_survive * 100.0))

    # ------------------------------
    # Significance calculation
    # ------------------------------
    sig_table = []  # will hold (stage, S_yield, B_yield, Zbi)
    if bg is not None and bg_cutflow is not None:
        bg_yields = { row[0]: row[5] for row in (bg_cutflow or []) }  # use in-window yields
        for stage, S_yield, _eff in sig_cutflow:
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
            sig_table.append((stage, S_yield, B_yield, Zbi_val))
    else:
        for stage, S_yield, _eff in sig_cutflow:
            sig_table.append((stage, S_yield, 0.0, float('inf') if S_yield>0 else 0.0))

    # ------------------------------
    # Output: final text and LaTeX tables
    # ------------------------------
    if sig_table:
        final_stage, final_S, final_B, final_Z = sig_table[-1]
    else:
        final_stage, final_S, final_B, final_Z = ("After all cuts", 0.0, 0.0, 0.0)

    # Write a simple text summary in outdir (replacing former --outtxt)
    txt_name = f"final_yields_m{int(mass_mev)}_Val{Val}_Val2{Val2}.txt"
    outtxt_path = os.path.join(args.outdir, txt_name)
    try:
        with open(outtxt_path, "w") as fout:
            fout.write(f"mass_MeV {mass_mev}\n")
            fout.write(f"epsilon {epsilon}\n")
            fout.write(f"Val {Val}\n")
            fout.write(f"Val2 {Val2}\n")
            fout.write(f"stage {final_stage}\n")
            fout.write(f"S_yield {final_S:.6e}\n")
            fout.write(f"B_yield {final_B:.6e}\n")
            fout.write(f"Zbi {final_Z:.6f}\n")
    except Exception as e:
        sys.stderr.write(f"[error] Failed to write output text file '{outtxt_path}': {e}\n")
        sys.exit(1)

    if args.debug:
        print(f"[done] Wrote final yields and significance to {outtxt_path}")

    # ------------------------------
    # LaTeX-formatted tables (same structure as plot_all_features_and_do_tables_3.py)
    # ------------------------------
    tex_path = os.path.join(args.outdir, "latek.tex")
    with open(tex_path, "w") as file1:
        file1.write("\\documentclass{article}\n")
        file1.write("\\usepackage{graphicx} % Required for inserting images\n")
        file1.write("\\usepackage{amsmath}\n")
        file1.write("\\usepackage{xcolor}\n")
        file1.write("\\pagecolor[rgb]{1,1,1}\n")
        file1.write("\\title{TablesForBackgroundRates}\n")
        file1.write("\\author{rodwyer100 }\n")
        file1.write("\\date{November 2025}\n")
        file1.write("\\begin{document}\n")
        file1.write("\\maketitle\n")

        # Summary table with core and yields
        file1.write("\\begin{tabular}{l|r}\n")
        file1.write("Original Core & "+str(core)+"\\\\\n")
        file1.write("Total Num Events & 3e9\\\\\n")
        file1.write("Area Under Curve At Point & "+str(m_fraction * 4.0)+"\\\\\n")
        file1.write("Events At Mass & "+str(format_sci(N_b_massbin))+"\\\\\n")
        file1.write("A Prime Yield &"+str(format_sci(prod_yield))+"\\\\\n")
        file1.write("Rho BR and Phi BR &"+str(np.round(rho_fraction,4))+","+str(np.round(phi_fraction,4))+"\\\\\n")
        file1.write("Visible Yield &"+ str(format_sci(vis_yield))+"\\\\\n")
        file1.write("Rho Acc and Phi Acc &"+str(np.round(prho,6))+","+str(np.round(pphi,6))+"\\\\\n")
        file1.write("Rho Yield and Phi Yield &"+str(format_sci(prod_yield*prho*rho_fraction))+","+str(format_sci(prod_yield*pphi*phi_fraction))+"\\\\\n")
        file1.write("\\end{tabular}\n")
        file1.write("\\textbf{Cut 0}\n")

        # Background cutflow table
        if bg is not None and bg_cutflow is not None:
            file1.write("\n%% Background cutflow table\n")
            file1.write("\\begin{tabular}{l|rr|rr|r}\n")
            file1.write("\\textbf{Cut stage} & $N_{\\text{in}}$ & $N_{\\text{out}}$ & In~(\\% total) & Out~(\\% total) & $B_{\\text{yield}}$ \\\\ \\hline\n")
            for stage, n_in, n_out, pct_in, pct_out, B_yield, _ in bg_cutflow:
                count_in_str = f"{int(n_in)}"
                count_out_str = f"{int(n_out)}"
                pct_in_str = f"{pct_in:.2f}\\%"
                pct_out_str = f"{pct_out:.2f}\\%"
                yield_str = format_sci(B_yield) if B_yield is not None else "--"
                file1.write(f"{stage} & {count_in_str} & {count_out_str} & {pct_in_str} & {pct_out_str} & ${yield_str}$ \\\\\n")
            file1.write("\\end{tabular}\n")

        # Signal yield cutflow table
        file1.write("%% Signal yield table\n")
        file1.write("\\begin{tabular}{l|r@{~}r}\n")
        file1.write("\\textbf{Stage} & $S_{\\text{yield}}$ & (cum.\\,eff\\%) \\\\ \\hline\n")
        prod_str = format_sci(prod_yield)
        vis_str = format_sci(vis_yield)
        vis_frac_pct = rho_fraction * 100.0
        file1.write(f"Production (total) & ${prod_str}$ & (100\\%) \\\\\n")
        file1.write(f"Visible decays & ${vis_str}$ & ({vis_frac_pct:.2f}\\%) \\\\\n")
        for stage, S_yield, eff in sig_cutflow:
            yield_str = format_sci(S_yield)
            file1.write(f"{stage} & ${yield_str}$ & ({eff:.2f}\\%) \\\\\n")
        file1.write("\\end{tabular}\n")

        # Significance per step table
        file1.write("\\hline\n")
        file1.write("%% Significance per selection stage\n")
        file1.write("\\begin{tabular}{l|r r r}\n")
        file1.write("\\textbf{Stage} & $S_{\\text{yield}}$ & $B_{\\text{yield}}$ & $Z_{\\text{Bi}}$ \\\\ \\hline\n")
        for stage, S_yield, B_yield, Zbi_val in sig_table:
            S_str = format_sci(S_yield)
            B_str = format_sci(B_yield)
            Z_str = f"{Zbi_val:.2f}" if math.isfinite(Zbi_val) else "$\\infty$"
            file1.write(f"{stage} & ${S_str}$ & ${B_str}$ & {Z_str} \\\\\n")
        file1.write("\\end{tabular}\n")
        file1.write("\n")
        file1.write("\\end{document}\n")

    # Optionally compile LaTeX (if pdflatex is available)
    try:
        compile_latex(tex_path)
    except Exception as e:
        if args.debug:
            print(f"[warn] pdflatex compilation failed: {e}")

if __name__ == "__main__":
    main()

