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

# Try to import background efficiency module for file path and any helpers
try:
    import bk_eff_selection as bg
except Exception as e:
    sys.stderr.write(f"[warn] Could not import bk_eff_selection (background module): {e}\n")
    bg = None

#DEN_PATH_TMPL_2 = LOCATION+"logger_{mass}.root"
#DEN_HIST_NAME_2 = "h_z_eepair"


#@lru_cache(maxsize=None)
#def _den_cache_2(mass_key):
#    """Cache denominator histogram once per mass."""
#    den_path = DEN_PATH_TMPL_2.format(mass=int(mass_key))
#    with uproot.open(den_path) as f:
#        h = f[DEN_HIST_NAME_2]
#        edges = np.asarray(h.axes[0].edges())
#        vals  = np.asarray(h.values(), dtype=float)
#    return edges, vals

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
    """Compute the Zbi significance for signal yield S and background yield B:contentReference[oaicite:9]{index=9}:contentReference[oaicite:10]{index=10}."""
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
    charged_width = [width_Ap_to_charged(alpha_D, m_pi_D/mpi_over_fpi[i], m_pi_D, m_V_D, mAp) for i in range(len(mpi_over_fpi)) ]
    total_width = [rho_width[i]+phi_width[i]+invis_width[i]+charged_width[i] for i in range(len(mpi_over_fpi)) ]
    rho_width = [rho_width[i]/total_width[i] for i in range(len(mpi_over_fpi)) ]
    phi_width = [phi_width[i]/total_width[i] for i in range(len(mpi_over_fpi)) ]
    invis_width = [invis_width[i]/total_width[i] for i in range(len(mpi_over_fpi)) ]
    charged_width = [charged_width[i]/total_width[i] for i in range(len(mpi_over_fpi)) ]
    fig, ax=plt.subplots()
    ax.plot(mpi_over_fpi,invis_width,"red")
    ax.plot(mpi_over_fpi,rho_width,"yellow")
    ax.plot(mpi_over_fpi,phi_width,"green")
    ax.plot(mpi_over_fpi,[phi_width[i]+rho_width[i] for i in range(len(mpi_over_fpi))],"blue")
    ax.plot(mpi_over_fpi,charged_width,"purple")
    ax.plot(mpi_over_fpi,[phi_width[i]+rho_width[i]+charged_width[i] for i in range(len(mpi_over_fpi))],"black")
    ax.set_yscale("log")
    plt.savefig(outdir+"/contours.png")

##ALL OF THIS HAS BEEN VALIDATED SO FAR

# Main function
def main():
    ap = argparse.ArgumentParser(description="Compute cutflow tables and plots for signal/background.")
    ap.add_argument("--mass", type=float, required=True, help="A' mass in MeV")
    ap.add_argument("--epsilon", type=float, required=True, help="Kinetic mixing parameter epsilon")
    #ap.add_argument("--Val", type=int, default=25, help="Selection parameter (e.g. z0 threshold index).")
    #ap.add_argument("--Val2", type=int, default=25, help="Selection parameter (e.g. proj_sig threshold index).")
    ap.add_argument("--outdir", type=str, default="output_plots", help="Directory for output plots.")
    ap.add_argument("--bins", type=int, default=80, help="Bins for 1D histograms.")
    ap.add_argument("--bins2d", type=int, default=80, help="Bins per axis for 2D histograms.")
    ap.add_argument("--base-module", type=str, default="decayLength7sel", help="Signal base module name.")
    ap.add_argument("--debug", action="store_true", help="Enable debug output")
    ap.add_argument("--normalized", type=int, default=0, help="1 if normalizaed")
    args = ap.parse_args()

    mass_mev = args.mass
    epsilon = args.epsilon
    #Val = args.Val
    Val2 = args.Val2
    normalized = (args.normalized==1)

    os.makedirs(args.outdir, exist_ok=True)

    # Import the signal base module (e.g., decayLength7sel.py) dynamically
    try:
        base = __import__(args.base_module)
    except Exception as e:
        sys.stderr.write(f"[error] Could not import signal base module '{args.base_module}': {e}\n")
        sys.exit(1)

    # ------------------------------
    # Background events processing
    # ------------------------------

    mask_list = [] 
    cutflow_list = []

    for Val in range(25):
        if bg is None or not hasattr(bg, "BACKGROUND_PATH"):
            sys.stderr.write("[warn] Background module not available, skipping background processing.\n")
            bg_events = None
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
                    # Define the branches to load
                    desired_branches = [
                        "psum","vertex.pos_","vertex.invM_","vertex.invMerr_",
                        "ele.track_.n_hits_", "ele.track_.d0_", "ele.track_.phi0_", "ele.track_.z0_", "ele.track_.tan_lambda_",
                        "ele.track_.px_", "ele.track_.py_", "ele.track_.pz_", "ele.track_.chi2_", "ele.track_.x_at_ecal_",
                        "ele.track_.y_at_ecal_", "ele.track_.z_at_ecal_",
                        "pos.track_.n_hits_", "pos.track_.d0_", "pos.track_.phi0_", "pos.track_.z0_", "pos.track_.tan_lambda_",
                        "pos.track_.px_", "pos.track_.py_", "pos.track_.pz_", "pos.track_.chi2_", "pos.track_.x_at_ecal_",
                        "pos.track_.y_at_ecal_", "pos.track_.z_at_ecal_",
                        "vertex.chi2_", "vtx_proj_sig", "vtx_proj_x_sig", "vtx_proj_y_sig",

                        "vertex.invM_", "psum",
                        "ele.track_.z0_", "pos.track_.z0_",
                        "vtx_proj_sig",
                        "ele.track_.hit_layers_", "pos.track_.hit_layers_"
                        #"vertex.pos_.fZ_"
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
                ele_L1L1_s = np.ones_like(invM_sig, bool)
            if pos_layers_s is not None:
                pos_hasL0_s = ak.to_numpy(ak.any(pos_layers_s == 0, axis=-1))
                pos_hasL1_s = ak.to_numpy(ak.any(pos_layers_s == 1, axis=-1))
                pos_L1L1_s = np.asarray(pos_hasL0_s & pos_hasL1_s, bool)
            else:
                pos_L1L1_s = np.ones_like(invM_sig, bool)

            L1L1_mask = np.logical_and(ele_L1L1_s, pos_L1L1_s)

            # Define cut masks for background
            psum_mask = (psum >= 1.5) & (psum <= 3.0)
            z_thr = 0.5 * (float(Val) / 25.0)
            z0_mask = np.logical_and((ele_z0 > z_thr) | (ele_z0 < -z_thr),
                                 (pos_z0 > z_thr) | (pos_z0 < -z_thr))
            #proj_mask = (proj_sig < 50)

            proj_mask = (proj_sig < 20.0*(Val2)+1.0)

            # Mass window mask (±2 MeV around 1.8m_A/3.0)
            center_geV = 1.8*mass_mev / (1000.0 * 3.0)
            mass_mask = (invM>-1.0)#(invM > (center_geV - 0.002)) & (invM < (center_geV + 0.002))

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
                                None))  # yield to be filled after computing expected yields
            # Apply each cut in order, building on previous mask
            current_mask = np.ones(total_events, dtype=bool)
            # 1. psum
            current_mask &= psum_mask
            n_in = np.count_nonzero(current_mask & mass_mask)
            n_out = np.count_nonzero(current_mask & ~mass_mask)
            frac_in = 100.0 * n_in / total_events if total_events > 0 else 0.0
            frac_out = 100.0 * n_out / total_events if total_events > 0 else 0.0
            bg_cutflow.append(("After psum cut", n_in, n_out, frac_in, frac_out, None))
            # 2. L1L1
            current_mask &= L1L1_mask
            n_in = np.count_nonzero(current_mask & mass_mask)
            n_out = np.count_nonzero(current_mask & ~mass_mask)
            frac_in = 100.0 * n_in / total_events if total_events > 0 else 0.0
            frac_out = 100.0 * n_out / total_events if total_events > 0 else 0.0
            bg_cutflow.append(("After psum+L1L1", n_in, n_out, frac_in, frac_out, None))
            # 3. proj_sig
            current_mask &= proj_mask
            n_in = np.count_nonzero(current_mask & mass_mask)
            n_out = np.count_nonzero(current_mask & ~mass_mask)
            frac_in = 100.0 * n_in / total_events if total_events > 0 else 0.0
            frac_out = 100.0 * n_out / total_events if total_events > 0 else 0.0
            bg_cutflow.append(("After psum+L1L1+proj", n_in, n_out, frac_in, frac_out, None))
            # 4. z0
            current_mask &= z0_mask
            n_in = np.count_nonzero(current_mask & mass_mask)
            n_out = np.count_nonzero(current_mask & ~mass_mask)
            frac_in = 100.0 * n_in / total_events if total_events > 0 else 0.0
            frac_out = 100.0 * n_out / total_events if total_events > 0 else 0.0
            bg_cutflow.append(("After all cuts", n_in, n_out, frac_in, frac_out, None))

            # Compute expected background yield in the ±2 MeV window after each stage
            # Use polynomial m(x) fraction for the mass window (0.1 GeV width):contentReference[oaicite:16]{index=16}
            #x_gev = 1.8*mass_mev / (1000.0*3.0)
            x_gev = mass_mev / (1000.0)

            #DIVIDED BY 3.0 HERE TOO AND TIMES 1.8. NVMD I DON'T ACTUALLY THINK THIS IS RIGHT NOW!!

            poly_num = (-6860.03 + 299358.0*x_gev - 4087220.0*(x_gev**2) +
                    25209900.0*(x_gev**3) - 73485900.0*(x_gev**4) + 82579800.0*(x_gev**5))
            m_fraction = poly_num / (82.9268041667 * 1000.0)  # fraction per 0.1 GeV in this mass bin
            N_B_TOTAL = 3.0e9  # total number of background-triggered events (given by user)
            N_b_massbin = N_B_TOTAL * m_fraction * 4.0  # expected events in ±0.005 GeV window if no selection, this is 4 MeV wide
            # Loop through cutflow to fill expected yields for in-window
            for i, row in enumerate(bg_cutflow):
                stage, n_in, n_out, pct_in, pct_out, _ = row
                if i == 0:
                    # initial (no cuts) expected yield in window = N_b_massbin (baseline)
                    exp_yield_in = N_b_massbin
                    exp_yield_out = N_b_massbin
                else:
                    if initial_in > 0:
                        # fraction of in-window events surviving = n_in / initial_in
                        frac_survive = n_in / initial_in
                    else:
                        frac_survive = 0.0
                    if initial_out > 0:
                        # fraction of in-window events surviving = n_in / initial_in
                        frac_survive_out = n_out / initial_out
                    else:
                        frac_survive_out = 0.0
                    exp_yield_in = N_b_massbin * frac_survive
                    exp_yield_out = N_b_massbin * frac_survive_out
                bg_cutflow[i] = (stage, n_in, n_out, pct_in, pct_out, exp_yield_in, exp_yield_out)
            cutflow_list.append(bg_cutflow)
        bg_final_mask = None
        if bg is not None:
            total_events = len(invM)
            bg_final_mask = np.ones(total_events, dtype=bool)
            bg_final_mask &= psum_mask & L1L1_mask & proj_mask & z0_mask & mass_mask  # also ensure in mass window for plotting background
        mask_list.append(bg_final_mask)
    # ------------------------------
    # Plot generation (after final cuts)
    # ------------------------------

    # Choose features to plot: use common features present in both samples
    features_to_plot = ["psum","vertex.pos_.fX_","vertex.pos_.fY_","vertex.pos_.fZ_","vertex.invM_","vertex.invMerr_",
    "ele.track_.n_hits_", "ele.track_.d0_", "ele.track_.phi0_", "ele.track_.z0_", "ele.track_.tan_lambda_",
    "ele.track_.px_", "ele.track_.py_", "ele.track_.pz_", "ele.track_.chi2_", "ele.track_.x_at_ecal_",
    "ele.track_.y_at_ecal_", "ele.track_.z_at_ecal_",
    "pos.track_.n_hits_", "pos.track_.d0_", "pos.track_.phi0_", "pos.track_.z0_", "pos.track_.tan_lambda_",
    "pos.track_.px_", "pos.track_.py_", "pos.track_.pz_", "pos.track_.chi2_", "pos.track_.x_at_ecal_",
    "pos.track_.y_at_ecal_", "pos.track_.z_at_ecal_",
    "vertex.chi2_", "vtx_proj_sig", "vtx_proj_x_sig", "vtx_proj_y_sig"]

    #"vertex.pos_.fZ", "ele.track_.z0_", "pos.track_.z0_", "psum", "vtx_proj_sig"]
    # Ensure those exist in events and arrays
    print(features_to_plot)
    #features_to_plot = [f for f in features_to_plot if (f in events) and (bg is None or f in arrays.keys())]
    #print(features_to_plot)
    # Plot 1D histograms overlay
    for feat in features_to_plot:
        for I in range(25):
            if bg_final_mask is not None and feat in arrays.keys():
                b_data = ak.to_numpy(arrays[feat])[mask_list[I]]
                print("The background for "+str(feat)+" is as follows")
                print(b_data)
            else:
                b_data = np.array([])  # no background
                print("I said there was no background for "+str(feat))
            # Compute S and B yields for annotation (use final yields from above)
            B_final = cutflow_list[I][-1][5] if bg is not None else 0.0
            combined = b_data
            #np.concatenate([b_data]) if b_data.size > 0
            if combined.size > 0:
                q_low, q_high = np.percentile(combined, [0.5, 99.5])
            else:
                q_low, q_high = 0, 1
            # Add a small padding
            pad = 0.05 * (q_high - q_low if q_high > q_low else 1.0)
            hist_range = (q_low - pad, q_high + pad)
            # Bin weights such that total area equals expected yield
            b_w = None

            if(normalized):
                B_final = 1.0
            if b_data.size > 0:
                b_w = np.full(b_data.shape, B_final / b_data.size)
            #plt.figure(figsize=(7.5, 4.5))
            bins = args.bins
            plt.hist(b_data, bins=bins, range=hist_range, weights=b_w, histtype="step", linewidth=1.8, label="Val="+str(25*(I/25.0)))
        title = (f"{feat} distribution after final cuts\n"
                 f"B = {B_final:.2g} "
                 f"Val = {I}")
        plt.title(title)
        plt.xlabel(feat)
        plt.ylabel("Expected events")
        plt.yscale("log")
        plt.legend(loc="best")
        plt.grid(alpha=0.3)
        # Save plot
        fname = feat.replace("/", "_").replace(".", "_")
        outfile = os.path.join(args.outdir, f"plot1D_{fname}_overlaidValues.png")
        plt.tight_layout()
        plt.savefig(outfile, dpi=150)
        plt.close()
        if args.debug:
            print(f"[debug] Saved 1D plot: {outfile}")
    if args.debug:
        print("[done] All tables generated and plots saved.")        


if __name__ == "__main__":
    main()

