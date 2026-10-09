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
    ap.add_argument("--Val", type=int, default=25, help="Selection parameter (e.g. z0 threshold index).")
    ap.add_argument("--Val2", type=int, default=25, help="Selection parameter (e.g. proj_sig threshold index).")
    ap.add_argument("--outdir", type=str, default="output_plots", help="Directory for output plots.")
    ap.add_argument("--bins", type=int, default=80, help="Bins for 1D histograms.")
    ap.add_argument("--bins2d", type=int, default=80, help="Bins per axis for 2D histograms.")
    ap.add_argument("--base-module", type=str, default="decayLength7sel", help="Signal base module name.")
    ap.add_argument("--debug", action="store_true", help="Enable debug output")
    ap.add_argument("--normalized", type=int, default=0, help="1 if normalizaed")
    args = ap.parse_args()

    mass_mev = args.mass
    epsilon = args.epsilon
    Val = args.Val
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
    if bg is None or not hasattr(bg, "BACKGROUND_PATH"):
        sys.stderr.write("[warn] Background module not available, skipping background processing.\n")
        bg_events = None
    else:
        Mkey = base._mass_key(1.8*mass_mev/3.0)
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
        proj_mask = (proj_sig < 1.6)

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
        N_b_massbin = N_B_TOTAL * m_fraction * 1.0  # expected events in ±0.005 GeV window if no selection, this is 4 MeV wide
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

    ##ALL OF THIS HAS BEEN VALIDATED SO FAR
    

    # ------------------------------
    # Signal events processing
    # ------------------------------
    # Load signal numerator events (no tight selection applied yet)
    mkey = base._mass_key(1.8*mass_mev/3.0)

    ##I THINK THIS NEEDS TO BE DIVIDED BY 3.0 and times 1.8

    try:
        events = base._events_cache(mkey)
        ####REWEIGHTING SHIT HAPPENS HERE
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
        print(z_temp) 
        z_temp=z_temp*(z_temp>=0.0)
        print(z_temp)
        #print(gamma_temp)
        #print(z_temp/(gamma_temp*rho_length))
        p_accept_temp = rho_fraction*np.exp(-z_temp/(gamma_temp*rho_length))/(rho_length*gamma_temp)
        p_accept_temp += phi_fraction*np.exp(-z_temp/(gamma_temp*phi_length))/(phi_length*gamma_temp)
        p_accept_temp/= max(p_accept_temp)
 
        '''rng = np.random.default_rng(123)            # seed optional, helps reproducibility
        u = rng.random(len(z_temp))
        #print("GOT HERE")
        print(u)
        print(p_accept_temp)
        mask = (u < p_accept_temp)
        print(mask)
        mask = np.asarray(mask, dtype=bool)
        events = {k: np.asarray(v)[mask] for k, v in events.items()}
        #events = events[mask]
        #print("GOT HERE")'''

    except Exception as e:
        sys.stderr.write(f"[error] Could not load signal events for mass {mass_mev}: {e}\n")
        sys.exit(1)
    # Extract needed branches from events (assuming events behaves like a dict or similar mapping)
    # We'll gather arrays for psum, z0, proj_sig, etc.
    try:
        s_vertex_z = np.asarray(events["vertex.pos_.fZ"])
        s_psum = np.asarray(events["psum"])
        ele_z0 = np.asarray(events["ele.track_.z0_"])
        pos_z0 = np.asarray(events["pos.track_.z0_"])
        s_proj = np.asarray(events["vtx_proj_sig"])
    except Exception as e:
        sys.stderr.write(f"[error] Signal events missing required branches: {e}\n")
        sys.exit(1)
    # Handle L1L1 for signal (use hasL0L1 if present, else derive from hit_layers)
    if "ele.hasL0L1" in events:
        s_e_hasL0L1 = np.asarray(events["ele.hasL0L1"], dtype=bool)
    else:
        s_e_hasL0L1 = None
    if "pos.hasL0L1" in events:
        s_p_hasL0L1 = np.asarray(events["pos.hasL0L1"], dtype=bool)
    else:
        s_p_hasL0L1 = None
    if s_e_hasL0L1 is None or s_p_hasL0L1 is None:
        # Derive from hit_layers if possible
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
    s_z_thr = 0.5 * (float(Val) / 25.0)
    s_z0_mask = np.logical_and((ele_z0 > s_z_thr) | (ele_z0 < -s_z_thr),
                               (pos_z0 > s_z_thr) | (pos_z0 < -s_z_thr))
    
    print(Val2)
    projval=10.0*(float(Val2)/10)+1
    s_proj_mask = (s_proj < projval)
    

    total_sig_events = len(s_psum)
    # Count survivors and efficiencies stage by stage
    sig_cutflow = []  # list of tuples (stage, yield, cum_eff%)
    # Compute theoretical yields at key stages:
    # 1. Production yield (no decays yet)
    # Use formula from base: core = scale_const * ratio(mA) * mA * epsilon^2:contentReference[oaicite:17]{index=17}
    scale_const = 3.0 * math.pi / (2.0 * 1.0 * (1.0 / 137.0459991))
    try:
        ratio_val = base.ratio(mass_mev)
    except Exception as e:
        sys.stderr.write(f"[error] Failed to compute ratio(mA): {e}\n")
        ratio_val = 0.0
    core = scale_const * ratio_val * mass_mev * (epsilon ** 2)
    

    N_B_TOTAL = 3.0e9

    x_gev = mass_mev / (1000.0)

    #DIVIDED BY 3.0 HERE TOO AND TIMES 1.8 NVMND I DONT THINK I NEED TO DO THIS

    poly_num = (-6860.03 + 299358.0*x_gev - 4087220.0*(x_gev**2) +
                    25209900.0*(x_gev**3) - 73485900.0*(x_gev**4) + 82579800.0*(x_gev**5))
    m_fraction = poly_num / (82.9268041667 * 1000.0)  # fraction per 0.1 GeV in this mass bin
    N_b_massbin = N_B_TOTAL * m_fraction * 1.0  # expected events in ±0.002 GeV window if no selection


    prod_yield = N_b_massbin * core  # number of A' produced (per baseline N_B events)
    # 2. Visible yield (multiply by rho fraction last element):contentReference[oaicite:18]{index=18}
    # Compute rho (and phi) branching fractions using hidden sector model
    alpha_D = 0.01  # fixed in decayLength7sel
    m_pi_D = mass_mev / 3.0
    m_V_D = 1.8*mass_mev / 3.0 
    # For maximum visible fraction, use f_pi_D such that invisible width is minimal (f_pi as in last rho array entry scenario)
    f_pi_D = (mass_mev / 3.0) * (1.0 / (4.0 * math.pi))
    rho_width = width_Ap_to_vector(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, multiplicity=0.75)
    phi_width = width_Ap_to_vector(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, multiplicity=1.5)
    invis_width = width_Ap_to_invis(alpha_D, m_pi_D, m_V_D, mass_mev)
    charged_width = width_Ap_to_charged(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev)
    total_width = rho_width + phi_width + invis_width + charged_width
    rho_fraction = rho_width/total_width
    phi_fraction = phi_width/total_width

    ##ALL OF THIS HAS BEEN VALIDATED SO FAR
    
    # Decay length in mm (lab): (HBAR_C/total_width in cm) * 10 * beta_gamma
    #L_total_cm = HBAR_C / total_width
    s_gamma = (s_psum *1000.0)/m_V_D
    s_x = s_vertex_z/s_gamma  
    prho=0
    pphi=0
    
    #print("DO I GET HERE")
    den_edges, den_vals = base.read_den_hist(m_V_D)
    #counts=[1.0 for i in range(len(den_vals))]
    #valuesp=[0.0 for i in range(len(den_vals))]
    #valuesr=[0.0 for i in range(len(den_vals))]
    mk = base._mass_key(m_V_D)
    mask   = base.tight_selection(events,Val,Val2)
    #zvals  = events["vertex.pos_.fZ"][mask]
    zvals  = events["true_vd.vtx_z_"][mask]
    num_vals, _ = np.histogram(zvals, bins=den_edges)
    psum_edges, psum_vals = base.read_psum_hist(m_V_D)
    for I in range(len(den_vals)):
        Ngen = float(den_vals[I])
        Nacc = float(num_vals[I])
        z_cent = .5*(den_edges[I]+den_edges[I+1])
        #print("\n z value: "+str(z_cent))
        #print("Length of psum vals: "+str(len(psum_vals)))
        for J in range(len(psum_vals)):
            psum = .5*(psum_edges[J]+psum_edges[J+1])
            gamma = 1000*psum/m_V_D
            #print("gamma value: "+str(gamma))
            #print("gamma prob: "+str(psum_vals[J]))
            rho_width = rate_2l(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, epsilon, .511, True)
            phi_width = rate_2l(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, epsilon, .511, False)
            rho_length=(1000*HBAR_C*10.0)/rho_width
            phi_length=(1000*HBAR_C*10.0)/phi_width
            print("unboosted decay length: "+str(rho_length))
            print("unboosted decay length: "+str(phi_length))
            #print("boosted decay length: "+str(rho_length*gamma))
            #print("z prob: "+str(np.exp(-z_cent/(gamma*rho_length))/(rho_length*gamma)))
            #print("New prho: "+str(prho))
            if(z_cent<0):
                z_cent=0
            if Ngen==0.0:
                Ngen=1.0
                Nacc=0.0
            prho+=psum_vals[J]*(Nacc/Ngen)*np.exp(-z_cent/(gamma*rho_length))/(rho_length*gamma)
            pphi+=psum_vals[J]*(Nacc/Ngen)*np.exp(-z_cent/(gamma*phi_length))/(phi_length*gamma)

    #for i in range(len(s_x)):
    #    #print("I get to "+str(i))
    #    z = s_vertex_z[i]
    #    #print(z)
    #    if not (den_edges[0] <= z < den_edges[-1]):
    #        continue
    #    mk = base._mass_key(m_V_D)
    #    mask   = base.tight_selection(events,Val)
    #    zvals  = events["vertex.pos_.fZ"][mask]
    #    num_vals, _ = np.histogram(zvals, bins=den_edges)
    #    I = np.searchsorted(den_edges, z, side="right") - 1
    #    #counts[I]+=1.0
    #    Ngen = float(den_vals[I])
    #    Nacc = float(num_vals[I])
    #    #Nacc = Ngen
    #    rho_width = rate_2l(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, epsilon, .511, True)
    #    phi_width = rate_2l(alpha_D, f_pi_D, m_pi_D, m_V_D, mass_mev, epsilon, .511, False)
    #    rho_length=(1000*HBAR_C*10.0)/rho_width
    #    phi_length=(1000*HBAR_C*10.0)/phi_width
    #    valuesr[I]=valuesr[I]+(Nacc/Ngen)*np.exp(-s_x[i]/rho_length)/(rho_length*s_gamma[i])
    #    valuesp[I]=valuesp[I]+(Nacc/Ngen)*np.exp(-s_x[i]/phi_length)/(phi_length*s_gamma[i])
    #    #print(counts)
    #    #print(valuesr)
    #    #print(rho_width)
    #    #print(phi_width)
    #    #print(s_x[i])
    #    #print(s_gamma[i])
    #    #print(rho_length)
    #    #print(phi_length)
    #    #print("Done here: "+str(np.exp(-s_x[i]/rho_length)/(rho_length*s_gamma[i])))
    #    #print(pphi)
    #prho=sum([valuesr[I]/(counts[I]+1.0*(counts[I]==0)) for I in range(len(den_vals))])
    #pphi=sum([valuesp[I]/(counts[I]+1.0*(counts[I]==0)) for I in range(len(den_vals))])
    vis_yield = prod_yield*(rho_fraction+phi_fraction)   
    acc_yield = prod_yield*(prho*rho_fraction+pphi*phi_fraction)   
    print("I DO GET HERE")   

    ####ALL AFTER HAS BEEN CHECKED

    # Now selection stages:
    # Initial (after acceptance, before any tight cuts)
    if acc_yield < 0:
        acc_yield = 0.0  # ensure non-negative
    sig_cutflow.append(("After acceptance", acc_yield, acc_yield/vis_yield))  # baseline for selection efficiency
    # Sequentially apply cuts and compute yield
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
    # z0 cut
    current_mask &= s_z0_mask
    frac_survive = np.count_nonzero(current_mask) / total_sig_events if total_sig_events > 0 else 0.0
    yield_final = acc_yield * frac_survive
    sig_cutflow.append(("After all cuts", yield_final, frac_survive * 100.0))


    # ------------------------------
    # Significance calculation
    # ------------------------------
    # We have expected B_yield from bg_cutflow and S_yield from sig_cutflow for matching stages
    # (We consider "After acceptance" as our baseline stage 0 for significance, corresponding to background "No cuts" stage)
    sig_table = []  # will hold (stage, S_yield, B_yield, Zbi)
    if bg is not None:
        # Map background stages to yields for signal stages:
        # Use "No cuts" background for "After acceptance" signal (assuming acceptance is effectively no tight cuts for background as well)
        # and subsequent cuts correspond in order.


        #ROW FIVE FOR BACKGROUND BASED ON IN, ROW SIX FOR BACKGROUND BASED ON OUT
        bg_yields = { row[0]: row[6] for row in (bg_cutflow or []) }
        for stage, S_yield, _eff in sig_cutflow:
            # Find corresponding background stage (naming must match the way we labeled)
            if stage == "After acceptance":
                bg_stage = "No cuts"
            elif stage.startswith("After psum+L1L1+proj"):
                bg_stage = "After psum+L1L1+proj"
            elif stage.startswith("After psum+L1L1"):
                # Could be with or without +proj, handle above first
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
        # If no background info, just output signal yields and placeholder zeros for background
        for stage, S_yield, _eff in sig_cutflow:
            sig_table.append((stage, S_yield, 0.0, float('inf') if S_yield>0 else 0.0))
    file1 = open(args.outdir+"/latek.tex",'w')
    # ------------------------------
    # Output LaTeX-formatted tables
    # ------------------------------
    # Background cutflow table
    
    file1.write("\\documentclass{article}\n")
    file1.write("\\usepackage{graphicx} % Required for inserting images\n")
    file1.write("\\usepackage{amsmath}\n")
    #file1.write("\\pagecolor{white}\n")
    file1.write("\\usepackage{xcolor}\n")
    file1.write("\\pagecolor[rgb]{1,1,1}\n")
    file1.write("\\title{TablesForBackgroundRates}\n")
    file1.write("\\author{rodwyer100 }\n")
    file1.write("\\date{November 2025}\n")
    file1.write("\\begin{document}\n")
    file1.write("\\maketitle\n")
    file1.write("\\begin{tabular}{l|r}\n")
    file1.write("Original Core & "+str(core)+"\\\\\n")
    file1.write("Total Num Events & 3e9\\\\\n")
    file1.write("Area Under Curve At Point & "+str(m_fraction * 1.0)+"\\\\\n")
    file1.write("Events At Mass & "+str(format_sci(N_b_massbin))+"\\\\\n")
    file1.write("A Prime Yield &"+str(format_sci(prod_yield))+"\\\\\n")
    file1.write("Rho BR and Phi BR &"+str(np.round(rho_fraction,4))+","+str(np.round(phi_fraction,4))+"\\\\\n")
    file1.write("Visible Yield &"+ str(format_sci(vis_yield))+"\\\\\n")
    file1.write("Rho Acc and Phi Acc &"+str(np.round(prho,6))+","+str(np.round(pphi,6))+"\\\\\n")
    file1.write("Rho Yield and Phi Yield &"+str(format_sci(prod_yield*prho*rho_fraction))+","+str(format_sci(prod_yield*pphi*phi_fraction))+"\\\\\n")
    file1.write("\\end{tabular}\n")
    file1.write("\\textbf{Cut 0}\n")
    if bg is not None:
        file1.write("\n%% Background cutflow table\n")
        file1.write("\\begin{tabular}{l|rr|rr|r}\n")
        file1.write("\\textbf{Cut stage} & $N_{\\text{in}}$ & $N_{\\text{out}}$ & In~(\\% total) & Out~(\\% total) & $B_{\\text{yield}}$ \\\\ \\hline\n")
        for stage, n_in, n_out, pct_in, pct_out, B_yield, _ in bg_cutflow:
            # Format counts as integers and yields in scientific notation
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
    # We include production and visible as additional rows for clarity
    prod_str = format_sci(prod_yield)
    vis_str = format_sci(vis_yield)
    # Visible fraction percent = rho_fraction*100
    vis_frac_pct = rho_fraction * 100.0
    file1.write(f"Production (total) & ${prod_str}$ & (100\\%) \\\\\n")
    file1.write(f"Visible decays & ${vis_str}$ & ({vis_frac_pct:.2f}\\%) \\\\\n")
    # Acceptance stage is already in sig_cutflow[0]
    for stage, S_yield, eff in sig_cutflow:
        yield_str = format_sci(S_yield)
        file1.write(f"{stage} & ${yield_str}$ & ({eff:.2f}\\%) \\\\\n")
    file1.write("\\end{tabular}\n")
    # Significance per step table
    file1.write("\hline")
    file1.write("%% Significance per selection stage\n")
    file1.write("\\begin{tabular}{l|r r r}\n")
    file1.write("\\textbf{Stage} & $S_{\\text{yield}}$ & $B_{\\text{yield}}$ & $Z_{\\text{Bi}}$ \\\\ \\hline\n")
    for stage, S_yield, B_yield, Zbi_val in sig_table:
        S_str = format_sci(S_yield)
        B_str = format_sci(B_yield)
        Z_str = f"{Zbi_val:.2f}" if math.isfinite(Zbi_val) else "$\\infty$"
        file1.write(f"{stage} & ${S_str}$ & ${B_str}$ & {Z_str} \\\\\n")
    file1.write("\\end{tabular}\n")
    file1.write("\n")  # blank line after tables
    file1.write("\\end{document}\n")
    file1.close()
    # ------------------------------
    # Plot generation (after final cuts)
    # ------------------------------
    # If background events loaded, apply final mask to get distributions; else only signal.
    # We already have `current_mask` from signal final cut, and for background from above loop (after all cuts).
    # For clarity, recompute final masks:
    sig_final_mask = np.ones(total_sig_events, dtype=bool)
    sig_final_mask &= s_psum_mask & s_L1L1_mask & s_proj_mask & s_z0_mask
    bg_final_mask = None
    if bg is not None:
        total_events = len(invM)
        bg_final_mask = np.ones(total_events, dtype=bool)
        bg_final_mask &= psum_mask & L1L1_mask & proj_mask & z0_mask & mass_mask  # also ensure in mass window for plotting background
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
    features_to_plot = [f for f in features_to_plot if (f in events) and (bg is None or f in arrays.keys())]
    print(features_to_plot)
    # Plot 1D histograms overlay
    for feat in features_to_plot:
        s_data = np.asarray(events[feat])[sig_final_mask]
        reweight = p_accept_temp[sig_final_mask]
        # Background: if available, use bg_final_mask (already includes mass window)
        if bg_final_mask is not None and feat in arrays.keys():
            b_data = ak.to_numpy(arrays[feat])[bg_final_mask]
        else:
            b_data = np.array([])  # no background
        # Compute S and B yields for annotation (use final yields from above)
        S_final = yield_final if 'yield_final' in locals() else (len(s_data))
        B_final = bg_cutflow[-1][5] if bg is not None else 0.0
        Zbi_final = zbi_significance(S_final, B_final)
        # Determine histogram range (0.5% to 99.5% quantile of combined data)
        combined = np.concatenate([s_data, b_data]) if b_data.size > 0 else s_data
        if combined.size > 0:
            q_low, q_high = np.percentile(combined, [0.5, 99.5])
        else:
            q_low, q_high = 0, 1
        # Add a small padding
        pad = 0.05 * (q_high - q_low if q_high > q_low else 1.0)
        hist_range = (q_low - pad, q_high + pad)
        # Bin weights such that total area equals expected yield
        s_w = None; b_w = None
        if(normalized):
            S_final = 1.0
            B_final = 1.0
        if s_data.size > 0:
            s_w = np.full(s_data.shape, S_final / s_data.size)*reweight
        if b_data.size > 0:
            b_w = np.full(b_data.shape, B_final / b_data.size)
        plt.figure(figsize=(7.5, 4.5))
        bins = args.bins
        plt.hist(s_data, bins=bins, range=hist_range, weights=s_w, histtype="step", linewidth=1.8, label="Signal")
        if b_data.size > 0:
            plt.hist(b_data, bins=bins, range=hist_range, weights=b_w, histtype="step", linewidth=1.8, label="Background")
        title = (f"{feat} distribution after final cuts\n"
                 f"S = {S_final:.2g}, B = {B_final:.2g}, Z$_{{Bi}}$ = {Zbi_final:.2f}; "
                 f"mass = {mass_mev:.0f} MeV, $\\epsilon$ = {epsilon}, Val = {Val}")
        plt.title(title)
        plt.xlabel(feat)
        plt.ylabel("Expected events")
        plt.yscale("log")
        plt.legend(loc="best")
        plt.grid(alpha=0.3)
        # Save plot
        fname = feat.replace("/", "_").replace(".", "_")
        outfile = os.path.join(args.outdir, f"plot1D_{fname}_{int(round(mass_mev))}MeV_eps{str(epsilon).replace('.', 'p')}_V{Val}.png")
        plt.tight_layout()
        plt.savefig(outfile, dpi=150)
        plt.close()
        if args.debug:
            print(f"[debug] Saved 1D plot: {outfile}")
    # Plot 2D histograms for each pair of features (for signal and background separately)
    feat_pairs = [(features_to_plot[i], features_to_plot[j]) for i in range(len(features_to_plot)) for j in range(i+1, len(features_to_plot))]
    # Optionally limit number of pairs
    max_pairs = getattr(args, "max_pairs", None)
    if max_pairs:
        feat_pairs = feat_pairs[:int(max_pairs)]
    for fx, fy in feat_pairs:
        # Prepare data
        s_x = np.asarray(events[fx])[sig_final_mask]
        s_y = np.asarray(events[fy])[sig_final_mask]
        if bg_final_mask is not None and fx in arrays.keys() and fy in arrays.keys():
            b_x = ak.to_numpy(arrays[fx])[bg_final_mask]
            b_y = ak.to_numpy(arrays[fy])[bg_final_mask]
        else:
            b_x = np.array([]); b_y = np.array([])
        # Plot for signal
        if s_x.size >= 2 and s_y.size >= 2:
            plt.figure(figsize=(6.5, 5.5))
            H, xedges, yedges = np.histogram2d(s_x, s_y, bins=args.bins2d)
            plt.pcolormesh(xedges, yedges, H.T, cmap='Blues')
            plt.colorbar(label="Signal count")
            plt.xlabel(fx); plt.ylabel(fy)
            plt.title(f"Signal {fx} vs {fy} (after cuts)")
            outfile = os.path.join(args.outdir, f"plot2D_signal_{fx.replace('.', '_')}_vs_{fy.replace('.', '_')}.png")
            plt.tight_layout(); plt.savefig(outfile, dpi=150); plt.close()
        # Plot for background
        if b_x.size >= 2 and b_y.size >= 2:
            plt.figure(figsize=(6.5, 5.5))
            H, xedges, yedges = np.histogram2d(b_x, b_y, bins=args.bins2d)
            plt.pcolormesh(xedges, yedges, H.T, cmap='Oranges')
            plt.colorbar(label="Background count")
            plt.xlabel(fx); plt.ylabel(fy)
            plt.title(f"Background {fx} vs {fy} (after cuts)")
            outfile = os.path.join(args.outdir, f"plot2D_background_{fx.replace('.', '_')}_vs_{fy.replace('.', '_')}.png")
            plt.tight_layout(); plt.savefig(outfile, dpi=150); plt.close()
    

    plotRates(args.outdir)
    if args.debug:
        print("[done] All tables generated and plots saved.")        
    compile_latex(args.outdir+"/latek.tex")


if __name__ == "__main__":
    main()

