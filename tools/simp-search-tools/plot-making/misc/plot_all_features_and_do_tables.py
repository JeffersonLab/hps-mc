#!/usr/bin/env python3
import argparse, sys, os, math
import numpy as np
import uproot
import awkward as ak

from scipy.special import betainc, erfinv

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

def sanitize(name: str) -> str:
    """Sanitize a string to use in file names by replacing special characters with underscore:contentReference[oaicite:11]{index=11}."""
    bad_chars = [" ", "/", "\\", "(", ")", "[", "]", "{", "}", ":", ";", ",", "|", "<", ">", "?", "*", "'", '"', "."]
    out = name
    for b in bad_chars:
        out = out.replace(b, "_")
    while "__" in out:
        out = out.replace("__", "_")
    return out.strip("_")

def main():
    ap = argparse.ArgumentParser(description="Compute yields, significance, and produce plots for signal and background.")
    ap.add_argument("--mass", type=float, required=True, help="A' mass in MeV.")
    ap.add_argument("--epsilon", type=float, required=True, help="Kinetic mixing parameter (epsilon).")
    ap.add_argument("--Val", type=int, default=25, help="Tight selection parameter (e.g., z0 threshold index):contentReference[oaicite:12]{index=12}.")
    ap.add_argument("--outdir", type=str, default="output_plots", help="Output directory for plots and tables:contentReference[oaicite:13]{index=13}.")
    ap.add_argument("--bins", type=int, default=80, help="Number of bins for 1D plots:contentReference[oaicite:14]{index=14}.")
    ap.add_argument("--bins2d", type=int, default=80, help="Number of bins per axis for 2D plots:contentReference[oaicite:15]{index=15}.")
    ap.add_argument("--signal-file", type=str, default=None, help="Path to signal ROOT file for the given mass (preselection events).")
    ap.add_argument("--base-module", type=str, default="decayLength5sel", help="Name of signal base module (default 'decayLength5sel'):contentReference[oaicite:16]{index=16}.")
    ap.add_argument("--debug", action="store_true", help="Enable debug output.")
    args = ap.parse_args()

    # Ensure output directory exists
    os.makedirs(args.outdir, exist_ok=True)

    # Dynamically import the signal base module for physics calculations:contentReference[oaicite:17]{index=17}.
    try:
        base = __import__(args.base_module)
    except ImportError as e:
        sys.stderr.write(f"[error] Unable to import base module '{args.base_module}': {e}\n")
        sys.exit(1)
    # Dynamically import or use background module for selection functions
    try:
        import bk_eff_selection as bg
    except ImportError as e:
        sys.stderr.write(f"[error] Unable to import background selection module: {e}\n")
        sys.exit(1)

    mass_mev = float(args.mass)
    epsilon = float(args.epsilon)
    Val = int(args.Val)
    x_gev = mass_mev / 1000.0
    # Compute polynomial mass fraction m(x) for background yield:contentReference[oaicite:18]{index=18}.
    m_fraction = 0.0
    try:
        # Use poly_m_of_x from combine_zbi if available
        from combine_zbi import poly_m_of_x as _poly_m
        m_fraction = _poly_m(x_gev)
    except Exception:
        # Fallback to polynomial formula:contentReference[oaicite:19]{index=19}
        num = (-6860.03 + 299358.0*x_gev - 4087220.0*(x_gev**2) + 25209900.0*(x_gev**3)
               - 73485900.0*(x_gev**4) + 82579800.0*(x_gev**5))
        m_fraction = num / (82.9268041667*1000.0)
    # Ensure m_fraction is non-negative and zero outside valid range:contentReference[oaicite:20]{index=20}.
    if x_gev < 0.05 or x_gev > 0.25:
        m_fraction = 0.0
    N_B_TOTAL = 3.0e9

    # Load background events from file (preselection tree) using the background module's config.
    bg_file = None
    cand = getattr(bg, "BACKGROUND_PATH", None)
    if cand:
        if isinstance(cand, (list, tuple)):
            bg_paths = [str(p) for p in cand]
        else:
            bg_paths = [str(cand)]
        if bg_paths:
            bg_file = bg_paths[0]
    if not bg_file or not os.path.exists(bg_file):
        sys.stderr.write(f"[error] Background file not found: {bg_file}\n")
        sys.exit(1)
    if args.debug:
        print(f"[debug] Loading background file: {bg_file}")
    with uproot.open(bg_file) as f:
        # Use custom tree opener if available:contentReference[oaicite:23]{index=23}.
        if hasattr(bg, "_open_first_tree"):
            tree = bg._open_first_tree(f)
        else:
            keys = [k for k in f.keys() if ";" in k]
            tree = f[keys[0]] if keys else None
        if tree is None:
            sys.stderr.write("[error] No TTree found in background file.\n")
            sys.exit(1)
        # Read necessary branches into Awkward arrays:contentReference[oaicite:24]{index=24}.
        branches = ["vertex.invM_", "vtx_proj_sig", "ele.track_.z0_", "pos.track_.z0_", "psum"]
        # include hit_layers for L0L1 calculation
        branches += ["ele.track_.hit_layers_", "pos.track_.hit_layers_"]
        arrays = tree.arrays(branches, library="ak", how=dict)
    # Convert Awkward arrays to numpy for convenience
    invM_bg = ak.to_numpy(arrays.get("vertex.invM_"))
    proj_sig_bg = ak.to_numpy(arrays.get("vtx_proj_sig"))
    ele_z0_bg = ak.to_numpy(arrays.get("ele.track_.z0_"))
    pos_z0_bg = ak.to_numpy(arrays.get("pos.track_.z0_"))
    psum_bg = ak.to_numpy(arrays.get("psum"))
    # Determine L0L1 hit flags for each track:contentReference[oaicite:25]{index=25}:contentReference[oaicite:26]{index=26}.
    ele_layers = arrays.get("ele.track_.hit_layers_")
    pos_layers = arrays.get("pos.track_.hit_layers_")
    if ele_layers is not None:
        ele_hasL0 = ak.to_numpy(ak.any(ele_layers == 0, axis=-1))
        ele_hasL1 = ak.to_numpy(ak.any(ele_layers == 1, axis=-1))
        ele_hasL0L1 = np.asarray(ele_hasL0 & ele_hasL1, dtype=bool)
    else:
        ele_hasL0L1 = np.ones_like(invM_bg, dtype=bool)
    if pos_layers is not None:
        pos_hasL0 = ak.to_numpy(ak.any(pos_layers == 0, axis=-1))
        pos_hasL1 = ak.to_numpy(ak.any(pos_layers == 1, axis=-1))
        pos_hasL0L1 = np.asarray(pos_hasL0 & pos_hasL1, dtype=bool)
    else:
        pos_hasL0L1 = np.ones_like(invM_bg, dtype=bool)
    # Compute mass window mask (±10 MeV around m/1.8):contentReference[oaicite:27]{index=27}:contentReference[oaicite:28]{index=28}.
    center = mass_mev / (1000.0 * 1.8)
    low_edge, high_edge = center - 0.01, center + 0.01  # 10 MeV window in GeV:contentReference[oaicite:29]{index=29}.
    mask_mass_bg = (invM_bg > low_edge) & (invM_bg < high_edge)
    # Sequentially apply cuts: psum, L1L1, proj_sig, z0.
    total_events = len(invM_bg)
    # Baseline (no tight cuts, only preselection baseline):
    inside0 = np.sum(mask_mass_bg)
    outside0 = np.sum(~mask_mass_bg)
    # psum cut (1.5 <= psum <= 3.0 GeV):contentReference[oaicite:30]{index=30}.
    mask_psum = (psum_bg >= 1.5) & (psum_bg <= 3.0)
    inside1 = np.sum(mask_mass_bg & mask_psum)
    outside1 = np.sum(~mask_mass_bg & mask_psum)
    # L1L1 cut (both tracks have L0 & L1 hits):contentReference[oaicite:31]{index=31}.
    mask_L1 = ele_hasL0L1 & pos_hasL0L1
    inside2 = np.sum(mask_mass_bg & mask_psum & mask_L1)
    outside2 = np.sum(~mask_mass_bg & mask_psum & mask_L1)
    # proj_sig cut (vertex projection significance < 1.6):contentReference[oaicite:32]{index=32}.
    mask_proj = proj_sig_bg < 1.6
    inside3 = np.sum(mask_mass_bg & mask_psum & mask_L1 & mask_proj)
    outside3 = np.sum(~mask_mass_bg & mask_psum & mask_L1 & mask_proj)
    # z0 cut (both tracks |z0| > zthr):contentReference[oaicite:33]{index=33}:contentReference[oaicite:34]{index=34}.
    zthr = 0.5 * (Val * (1.0/25.0))
    mask_z0 = (np.abs(ele_z0_bg) > zthr) & (np.abs(pos_z0_bg) > zthr)
    inside4 = np.sum(mask_mass_bg & mask_psum & mask_L1 & mask_proj & mask_z0)
    outside4 = np.sum(~mask_mass_bg & mask_psum & mask_L1 & mask_proj & mask_z0)
    # Calculate expected background yields at each stage using polynomial scaling:contentReference[oaicite:35]{index=35}:contentReference[oaicite:36]{index=36}.
    N_b_massbin = N_B_TOTAL * m_fraction * 10.0  # expected BG count in ±0.1 GeV mass bin:contentReference[oaicite:37]{index=37}.
    B0 = N_b_massbin * (inside0 / inside0 if inside0>0 else 0)  # should equal N_b_massbin
    B1 = N_b_massbin * (inside1 / inside0 if inside0>0 else 0)
    B2 = N_b_massbin * (inside2 / inside0 if inside0>0 else 0)
    B3 = N_b_massbin * (inside3 / inside0 if inside0>0 else 0)
    B4 = N_b_massbin * (inside4 / inside0 if inside0>0 else 0)
    # Compute signal yields at various stages.
    # Theoretical production rate core = const * ratio(m) * mA * eps^2:contentReference[oaicite:38]{index=38}:contentReference[oaicite:39]{index=39}.
    scale_const = 3.0 * math.pi / (2.0 * 1.0 * (1.0/137.0459991))  # from decayLength5sel (parallel aprime calculation):contentReference[oaicite:40]{index=40}.
    ratio_val = base.ratio(mass_mev) if hasattr(base, "ratio") else 0.0
    core = scale_const * ratio_val * mass_mev * (epsilon ** 2)
    # Branching ratio to e+e- (visible decays). For m_A above 2*m_mu, include muon channel.
    mA_GeV = mass_mev / 1000.0
    # Expected number of A' produced (no acceptance cuts)
    S_produced = N_B_TOTAL * core
    # Signal acceptance fraction without tight selection (Val=0):contentReference[oaicite:41]{index=41}:contentReference[oaicite:42]{index=42}.
    try:
        sR0, sP0 = base.getSum(epsilon, mass_mev, 0)
    except Exception as e:
        sR0, sP0 = (0.0, 0.0)
        if args.debug:
            sys.stderr.write(f"[debug] getSum with Val=0 failed: {e}\n")
    total_accept_frac = float(sR0 + sP0)
    # Signal acceptance fraction with tight selection (Val):contentReference[oaicite:43]{index=43}:contentReference[oaicite:44]{index=44}.
    try:
        sR, sP = base.getSum(epsilon, mass_mev, Val)
    except Exception as e:
        sys.stderr.write(f"[error] Failed to compute signal acceptance fraction: {e}\n")
        sys.exit(1)
    signal_accept_frac = float(sR + sP)
    # Expected signal yields:
    S_accept = S_produced * total_accept_frac    # after geometric acceptance (pre-selection)
    S_final = S_produced * signal_accept_frac    # after tight selection cuts
    # Compute Zbi significance at each cut stage (using S_accept for stages before z0, and S_final after z0).
    Z0 = zbi_significance(S_accept, B0)
    Z1 = zbi_significance(S_accept, B1)
    Z2 = zbi_significance(S_accept, B2)
    Z3 = zbi_significance(S_accept, B3)
    Z4 = zbi_significance(S_final, B4)
    # Prepare LaTeX tables content.
    tex_lines = []
    tex_lines.append("Entries for mass = %.1f MeV, epsilon = %.2g, Val = %d" % (mass_mev, epsilon, Val))
    # Background cutflow table
    tex_lines.append("\\begin{table}[h]\\centering")
    tex_lines.append("\\begin{tabular}{lccc} \\hline")
    tex_lines.append("Cut stage & In-window & Out-of-window & $B_\\text{exp}$ \\\\ \\hline")
    tex_lines.append(f"No cuts & {inside0} & {outside0} & {B0:.2g} \\\\")
    tex_lines.append(f"Psum cut & {inside1} & {outside1} & {B1:.2g} \\\\")
    tex_lines.append(f"L1L1 cut & {inside2} & {outside2} & {B2:.2g} \\\\")
    tex_lines.append(f"Pointing cut & {inside3} & {outside3} & {B3:.2g} \\\\")
    tex_lines.append(f"Min $|z0|$ cut & {inside4} & {outside4} & {B4:.2g} \\\\ \\hline")
    tex_lines.append("\\end{tabular}")
    tex_lines.append("\\caption{Background cutflow: counts inside/outside mass window and expected yield in signal region.}")
    tex_lines.append("\\end{table}")
    tex_lines.append("")
    # Signal yield table
    tex_lines.append("\\begin{table}[h]\\centering")
    tex_lines.append("\\begin{tabular}{lc} \\hline")
    tex_lines.append("Signal stage & Expected yield \\\\ \\hline")
    tex_lines.append(f"Produced $A'$ & {S_produced:.2g} \\\\")
    tex_lines.append(f"Visible $e^+e^-$ decays & {S_visible:.2g} \\\\")
    tex_lines.append(f"In acceptance (pre-selection) & {S_accept:.2g} \\\\")
    tex_lines.append(f"After tight cuts & {S_final:.2g} \\\\ \\hline")
    tex_lines.append("\\end{tabular}")
    tex_lines.append("\\caption{Expected signal yields at each stage for $m_{A'}=%g$ MeV, $\\epsilon=%g$.}" % (mass_mev, epsilon))
    tex_lines.append("\\end{table}")
    tex_lines.append("")
    # Significance table
    tex_lines.append("\\begin{table}[h]\\centering")
    tex_lines.append("\\begin{tabular}{lc} \\hline")
    tex_lines.append("Selection stage & $Z_{Bi}$ \\\\ \\hline")
    tex_lines.append(f"No selection & {Z0:.2f} \\\\")
    tex_lines.append(f"Psum cut & {Z1:.2f} \\\\")
    tex_lines.append(f"L1L1 cut & {Z2:.2f} \\\\")
    tex_lines.append(f"Pointing cut & {Z3:.2f} \\\\")
    tex_lines.append(f"After $|z0|$ cut & {Z4:.2f} \\\\ \\hline")
    tex_lines.append("\\end{tabular}")
    tex_lines.append("\\caption{Calculated $Z_{Bi}$ significance after each cut stage.}")
    tex_lines.append("\\end{table}")
    # Write LaTeX output to file
    tex_filename = os.path.join(args.outdir, f"results_{int(round(mass_mev))}MeV_V{Val}.tex")
    with open(tex_filename, "w") as fout:
        fout.write("\n".join(tex_lines))
    if args.debug:
        print(f"[debug] Wrote LaTeX tables to {tex_filename}")
    # Now load signal events for plotting if available
    sig_events = {}
    if args.signal_file:
        sig_file_path = args.signal_file
    else:
        sig_file_path = getattr(base, "SIGNAL_PATH", None)
    if sig_file_path:
        if args.debug:
            print(f"[debug] Loading signal file: {sig_file_path}")
        try:
            with uproot.open(sig_file_path) as sf:
                # Use same tree opening logic as background (preselection)
                if hasattr(bg, "_open_first_tree"):
                    tree_s = bg._open_first_tree(sf)
                else:
                    keys = [k for k in sf.keys() if ";" in k]
                    tree_s = sf[keys[0]] if keys else None
                arrays_s = tree_s.arrays(branches, library="ak", how=dict)
            invM_sig = ak.to_numpy(arrays_s.get("vertex.invM_"))
            proj_sig = ak.to_numpy(arrays_s.get("vtx_proj_sig"))
            ele_z0_sig = ak.to_numpy(arrays_s.get("ele.track_.z0_"))
            pos_z0_sig = ak.to_numpy(arrays_s.get("pos.track_.z0_"))
            psum_sig = ak.to_numpy(arrays_s.get("psum"))
            ele_layers_s = arrays_s.get("ele.track_.hit_layers") or arrays_s.get("ele.track_.hit_layers_")
            pos_layers_s = arrays_s.get("pos.track_.hit_layers") or arrays_s.get("pos.track_.hit_layers_")
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
            mask_mass_sig = (invM_sig > low_edge) & (invM_sig < high_edge)
            mask_tight_sig = (psum_sig >= 1.5) & (psum_sig <= 3.0) & ele_L1L1_s & pos_L1L1_s & (proj_sig < 1.6) & ((np.abs(ele_z0_sig) > zthr) & (np.abs(pos_z0_sig) > zthr))
            # Apply selection mask and mass window
            mask_final_sig = mask_mass_sig & mask_tight_sig
            # Build signal events dictionary for features after selection
            for key, arr in arrays_s.items():
                arr_np = ak.to_numpy(arr)
                if arr_np is None or arr_np.ndim != 1:
                    continue
                sig_events[key] = arr_np[mask_final_sig]
        except Exception as e:
            sys.stderr.write(f"[warn] Failed to load or process signal file: {e}\n")
            sig_events = {}
    else:
        if args.debug:
            print("[debug] No signal file provided or configured; skipping signal event plots.")
    # Build background events dict after selection (already applied sequentially above)
    bg_events = {}
    # We have masks for final selection:
    mask_final_bg = mask_mass_bg & mask_psum & mask_L1 & mask_proj & mask_z0
    if total_events and np.sum(mask_final_bg) > 0:
        # Filter numeric arrays
        for arr_name, arr_val in [("vertex.invM_", invM_bg), ("vtx_proj_sig", proj_sig_bg),
                                  ("ele.track_.z0_", ele_z0_bg), ("pos.track_.z0_", pos_z0_bg),
                                  ("psum", psum_bg)]:
            if arr_val is not None and len(arr_val) == total_events:
                bg_events[arr_name] = arr_val[mask_final_bg]
    # Determine common features for plotting:contentReference[oaicite:45]{index=45}:contentReference[oaicite:46]{index=46}.
    features_1d = []
    if sig_events:
        for feat in sig_events.keys():
            if feat in bg_events and sig_events[feat].size >= 2 and bg_events[feat].size >= 2:
                features_1d.append(feat)
        if not features_1d:
            # If no common features (or no background), use all signal features
            features_1d = sorted([k for k,v in sig_events.items() if v.size >= 2])
    elif bg_events:
        features_1d = sorted([k for k,v in bg_events.items() if v.size >= 2])
    else:
        features_1d = []
    # Plot 1D overlaid histograms for each feature:contentReference[oaicite:47]{index=47}:contentReference[oaicite:48]{index=48}.
    for feat in features_1d:
        sig_arr = sig_events.get(feat, np.array([]))
        bg_arr = bg_events.get(feat, np.array([]))
        if sig_arr.size == 0 and bg_arr.size == 0:
            continue
        # Determine histogram range between 0.5% and 99.5% quantiles:contentReference[oaicite:49]{index=49}.
        if bg_arr.size > 0:
            combined = np.concatenate([sig_arr, bg_arr]) if sig_arr.size>0 else bg_arr
        else:
            combined = sig_arr
        q1, q99 = np.quantile(combined, [0.005, 0.995]) if combined.size > 0 else (0, 1)
        pad = 0.05 * (q99 - q1 + 1e-12)
        hist_range = (q1 - pad, q99 + pad)
        # Compute weights so that total area equals expected yields:contentReference[oaicite:50]{index=50}.
        sig_weights = np.full(sig_arr.shape, S_final/ sig_arr.size) if sig_arr.size > 0 else None
        B_final = B4  # expected B yield after final selection
        bg_weights = np.full(bg_arr.shape, B_final/ bg_arr.size) if bg_arr.size > 0 else None
        import matplotlib.pyplot as plt
        fig, ax = plt.subplots(figsize=(7.8, 4.8))
        if sig_arr.size > 0:
            ax.hist(sig_arr, bins=args.bins, range=hist_range, weights=sig_weights, histtype="step", linewidth=1.8, label="signal")
        if bg_arr.size > 0:
            ax.hist(bg_arr, bins=args.bins, range=hist_range, weights=bg_weights, histtype="step", linewidth=1.8, label="background")
        title_line1 = f"{feat}"
        title_line2 = f"S={S_final:.2g}, B={B4:.2g}, Zbi={Z4:.2f}; mass={mass_mev:g} MeV, "
        title_line2 += f"epsilon={epsilon}, Val={Val}"
        ax.set_title(f"{title_line1}\n{title_line2}")
        ax.set_xlabel(feat)
        ax.set_ylabel("Expected events")
        ax.grid(True, alpha=0.3)
        ax.legend()
        ax.set_yscale("log")
        ymin, ymax = ax.get_ylim()
        ax.set_ylim(bottom=max(ymin, 1e-6), top=ymax)
        fname = f"oned_{sanitize(feat)}_{int(round(mass_mev))}MeV"
        eps_str = str(epsilon).replace('.', 'p').replace('-', 'm')
        fname += f"_eps{eps_str}_V{Val}.png"
        out_path = os.path.join(args.outdir, fname)
        fig.tight_layout()
        fig.savefig(out_path, dpi=160, bbox_inches="tight")
        plt.close(fig)
    # Plot 2D histograms for each feature pair (signal and background separately):contentReference[oaicite:51]{index=51}:contentReference[oaicite:52]{index=52}.
    if features_1d:
        feats_for_2d = features_1d
        pairs = [(feats_for_2d[i], feats_for_2d[j]) for i in range(len(feats_for_2d)) for j in range(i+1, len(feats_for_2d))]
        if args.bins2d < len(pairs):
            pairs = pairs[:args.bins2d]
        import matplotlib.pyplot as plt
        for fx, fy in pairs:
            # Signal 2D
            x_sig = sig_events.get(fx, np.array([]))
            y_sig = sig_events.get(fy, np.array([]))
            if x_sig.size >= 10 and y_sig.size >= 10:
                weight_sig = S_final / x_sig.size if x_sig.size>0 else None
                fig, ax = plt.subplots(figsize=(6.6, 5.8))
                rx = np.quantile(x_sig, [0.01, 0.99]); ry = np.quantile(y_sig, [0.01, 0.99])
                pad_x = 0.05 * (rx[1] - rx[0] + 1e-12); pad_y = 0.05 * (ry[1] - ry[0] + 1e-12)
                range2d = [(rx[0]-pad_x, rx[1]+pad_x), (ry[0]-pad_y, ry[1]+pad_y)]
                H, xedges, yedges, img = ax.hist2d(x_sig, y_sig, bins=args.bins2d, range=range2d, weights=(np.full(x_sig.shape, weight_sig) if weight_sig is not None else None))
                cb = fig.colorbar(img, ax=ax)
                cb.set_label("Expected events")
                ax.set_title(f"Signal 2D: {fx} vs {fy}\\nmass={mass_mev:g} MeV, epsilon={epsilon}, Val={Val}")
                ax.set_xlabel(fx); ax.set_ylabel(fy)
                ax.grid(True, alpha=0.15)
                fname2d = f"twoD_signal_{sanitize(fx)}__{sanitize(fy)}_{int(round(mass_mev))}MeV"
                fname2d += f"_eps{eps_str}_V{Val}.png"
                fpath2d = os.path.join(args.outdir, fname2d)
                fig.tight_layout(); fig.savefig(fpath2d, dpi=160, bbox_inches="tight"); plt.close(fig)
            # Background 2D
            x_bg = bg_events.get(fx, np.array([]))
            y_bg = bg_events.get(fy, np.array([]))
            if x_bg.size >= 10 and y_bg.size >= 10:
                weight_bg = B4 / x_bg.size if x_bg.size>0 else None
                fig, ax = plt.subplots(figsize=(6.6, 5.8))
                rx = np.quantile(x_bg, [0.01, 0.99]); ry = np.quantile(y_bg, [0.01, 0.99])
                pad_x = 0.05 * (rx[1] - rx[0] + 1e-12); pad_y = 0.05 * (ry[1] - ry[0] + 1e-12)
                range2d = [(rx[0]-pad_x, rx[1]+pad_x), (ry[0]-pad_y, ry[1]+pad_y)]
                H, xedges, yedges, img = ax.hist2d(x_bg, y_bg, bins=args.bins2d, range=range2d, weights=(np.full(x_bg.shape, weight_bg) if weight_bg is not None else None))
                cb = fig.colorbar(img, ax=ax)
                cb.set_label("Expected events")
                ax.set_title(f"Background 2D: {fx} vs {fy}\\nmass={mass_mev:g} MeV, epsilon={epsilon}, Val={Val}")
                ax.set_xlabel(fx); ax.set_ylabel(fy)
                ax.grid(True, alpha=0.15)
                fname2d = f"twoD_background_{sanitize(fx)}__{sanitize(fy)}_{int(round(mass_mev))}MeV"
                fname2d += f"_eps{eps_str}_V{Val}.png"
                fpath2d = os.path.join(args.outdir, fname2d)
                fig.tight_layout(); fig.savefig(fpath2d, dpi=160, bbox_inches="tight"); plt.close(fig)
    # Print summary of outputs
    print(f"[done] LaTeX tables written to: {tex_filename}")
    # Optionally print some plot file examples
    # (List up to 5 example output files)
    out_images = []
    for root, dirs, files in os.walk(args.outdir):
        for fname in files:
            if fname.endswith(".png"):
                out_images.append(os.path.join(root, fname))
    if out_images:
        print(f"[done] {len(out_images)} plots saved. Example files:")
        for p in out_images[:min(5, len(out_images))]:
            print("  ", p)

if __name__ == "__main__":
    main()

