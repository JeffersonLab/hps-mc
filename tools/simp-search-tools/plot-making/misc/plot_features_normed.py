#!/usr/bin/env python3
import argparse, os, sys, numpy as np
import matplotlib.pyplot as plt

# ---- pull in background helpers like in plot_z0_signal_v_back.py ----
try:
    import bk_eff_selection as bg
except Exception as e:
    sys.stderr.write("ERROR: Could not import bk_eff_selection as bg. Make sure it is on PYTHONPATH or in CWD.\n")
    raise

# ---- safe import for the signal "base" module (default: decayLength5sel) ----
def import_signal_base(module_name="decayLength5sel"):
    try:
        base = __import__(module_name)
        return base
    except SystemExit as se:
        sys.stderr.write(
            "FATAL: Importing '{}' triggered SystemExit, probably from argparse at top level.\n"
            "Please guard the CLI with if __name__ == '__main__'.\n".format(module_name)
        )
        raise
    except Exception:
        import importlib.util
        here = os.path.dirname(os.path.abspath(__file__))
        candidate = os.path.join(here, module_name + ".py")
        if os.path.exists(candidate):
            spec = importlib.util.spec_from_file_location(module_name, candidate)
            mod = importlib.util.module_from_spec(spec)
            try:
                spec.loader.exec_module(mod)
                return mod
            except SystemExit:
                sys.stderr.write(
                    "FATAL: Executing '{}' still triggered argparse at import.\n"
                    "Please guard the CLI with if __name__ == '__main__' in that file.\n".format(candidate)
                )
                raise
        else:
            raise

# ---- import combine_zbi to reuse its normalization formulas ----
try:
    import combine_zbi as cz
except Exception as e:
    sys.stderr.write("ERROR: Could not import combine_zbi.py. Place it next to this script or on PYTHONPATH.\n")
    raise

# ---------- utilities replicated/adapted from plot_z0_signal_v_back.py ----------
def _ensure_dir(d):
    if d and not os.path.isdir(d):
        os.makedirs(d, exist_ok=True)

def _compute_signal_weights(base, z_evt, epsilon, mass_mev, Val):
    """Return per-event weights for signal using base.getProb(z, epsilon, mass, Val)."""
    w = []
    for z in z_evt:
        pr, pp = base.getProb(float(z), float(epsilon), float(mass_mev), int(Val))
        ww = float(pr) + float(pp)
        if not np.isfinite(ww) or ww < 0.0:
            ww = 0.0
        w.append(ww)
    return np.asarray(w, dtype=float)

def _load_background_arrays(mass_mev, Val):
    import uproot
    import awkward as ak
    with uproot.open(bg.BACKGROUND_PATH) as f:
        t = bg._open_first_tree(f)
        arrays = t.arrays(bg.BRANCHES, library="ak")
    invM = np.asarray(ak.to_numpy(arrays["vertex.invM_"]))
    mwin = bg._mass_window_mask(invM, mass_mev)
    events = {}
    events["vertex.pos_.fZ"] = bg._extract_z_from_arrays(arrays)
    events["psum"] = np.asarray(ak.to_numpy(arrays["psum"]))
    events["vtx_proj_sig"] = np.asarray(ak.to_numpy(arrays["vtx_proj_sig"]))
    tight = bg._tight_selection_mask(events, Val)
    mask = mwin & tight
    ele_z0 = np.asarray(ak.to_numpy(arrays["ele.track_.z0_"]))[mask]
    pos_z0 = np.asarray(ak.to_numpy(arrays["pos.track_.z0_"]))[mask]
    return ele_z0, pos_z0

def _load_signal_arrays(base, mass_mev, Val):
    mkey   = base._mass_key(mass_mev)
    events = base._events_cache(mkey)
    mask   = base.tight_selection(events, Val)
    ele_z0 = events["ele.track_.z0_"][mask]
    pos_z0 = events["pos.track_.z0_"][mask]
    z_evt  = events["vertex.pos_.fZ"][mask]
    return ele_z0, pos_z0, z_evt

# ---------- combine_zbi normalization helpers ----------
def _expected_counts_from_cz(II, L, Val):
    """
    Reproduce combine_zbi main calculations to get arrays over epsilon slots:
      S_arr (expected signal counts per eps-slot),
      B_arr (expected background counts per eps-slot),
      Z_arr (zbi per eps-slot).
    """
    mass_mev = (240.0 / float(L)) * float(II)
    x_gev    = mass_mev / 1000.0

    sig_path = cz.SIG_IN_TMPL.format(II=II, Val=Val)
    bg_path  = cz.BG_IN_TMPL.format(II=II, L=L, Val=Val)

    sig_frac = cz.parse_signal_array(sig_path)  # array over epsilon slots
    bg_frac  = cz.read_bg_fraction(bg_path)     # scalar

    m_val = cz.poly_m_of_x(x_gev)
    N_b_massbin = cz.N_B_TOTAL_RECO * m_val * 10.0

    S_arr = cz.N_B_TOTAL_RECO * sig_frac
    B_arr = np.full_like(S_arr, N_b_massbin * bg_frac)
    Z_arr = cz._zbi_wrapper(S_arr, B_arr)
    return mass_mev, S_arr, B_arr, Z_arr

# ---------- plotting ----------
def _counts_hist(ax, data_sig, data_bg, bins, rrange, S_tot, B_tot, title, xlabel, weights_sig=None):
    """
    Plot *count*-scaled histograms so that integrals match S_tot and B_tot.
    """
    data_sig = np.asarray(data_sig)
    data_bg  = np.asarray(data_bg)

    # mask invalids
    if weights_sig is not None:
        weights_sig = np.asarray(weights_sig)
        m_sig = np.isfinite(data_sig) & np.isfinite(weights_sig)
        data_sig = data_sig[m_sig]
        weights_sig = weights_sig[m_sig]
    else:
        m_sig = np.isfinite(data_sig)
        data_sig = data_sig[m_sig]
        weights_sig = np.ones_like(data_sig, dtype=float)

    data_bg = data_bg[np.isfinite(data_bg)]

    # auto-range if needed
    if rrange is None:
        both = np.concatenate([data_sig, data_bg]) if data_sig.size and data_bg.size else (data_sig if data_sig.size else data_bg)
        if both.size:
            q1, q99 = np.quantile(both, [0.005, 0.995])
            pad = 0.05 * (q99 - q1 + 1e-9)
            rrange = (q1 - pad, q99 + pad)
        else:
            rrange = (-10.0, 10.0)

    # raw hists (counts per bin using current weights)
    sig_counts, edges = np.histogram(data_sig, bins=bins, range=rrange, weights=weights_sig)
    bg_counts,  _     = np.histogram(data_bg,  bins=bins, range=rrange)

    # scale so integrals match requested totals
    sig_sum = sig_counts.sum()
    bg_sum  = bg_counts.sum()
    sig_scale = (S_tot / sig_sum) if sig_sum > 0 else 0.0
    bg_scale  = (B_tot / bg_sum)  if bg_sum  > 0 else 0.0

    sig_counts *= sig_scale
    bg_counts  *= bg_scale

    # step plots
    centers = 0.5*(edges[:-1] + edges[1:])
    ax.step(centers, sig_counts, where="mid", linewidth=1.8, label=f"signal (area={S_tot:.3g})")
    ax.step(centers, bg_counts,  where="mid", linewidth=1.8, label=f"background (area={B_tot:.3g})")
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel("expected counts per bin")
    ax.grid(True, alpha=0.30)
    ax.legend()

def main():
    ap = argparse.ArgumentParser(description="Plot feature histograms with combine_zbi normalization (expected S and B).")
    ap.add_argument("--II", type=int, required=True, help="Index II (mass slot) used in reach curves.")
    ap.add_argument("--L",  type=int, required=True, help="L used in reach curves.")
    ap.add_argument("--Val", type=int, default=25, help="Tight selection parameter. Default 25.")
    ap.add_argument("--eps-index", type=int, required=True, help="Index into epsilon grid for normalization (from combine_zbi arrays).")
    ap.add_argument("--epsilon", type=float, default=None, help="Optional epsilon for signal *shape* weighting (base.getProb).")
    ap.add_argument("--base-module", type=str, default="decayLength5sel", help="Signal base module name. Default decayLength5sel.")
    ap.add_argument("--bins", type=int, default=80, help="Number of bins. Default 80.")
    ap.add_argument("--range", type=float, nargs=2, default=None, help="Explicit range: min max (in feature units).")
    ap.add_argument("--outdir", type=str, default=".", help="Output directory.")
    args = ap.parse_args()

    _ensure_dir(args.outdir)

    # expected S,B from combine_zbi logic
    mass_mev, S_arr, B_arr, Z_arr = _expected_counts_from_cz(args.II, args.L, args.Val)
    ei = int(args.eps_index)
    if not (0 <= ei < len(S_arr)):
        raise IndexError(f"--eps-index {ei} is out of bounds for arrays of length {len(S_arr)}")
    S_tot = float(S_arr[ei])
    B_tot = float(B_arr[ei])
    Z_val = float(Z_arr[ei])

    # load data
    base = import_signal_base(args.base_module)
    sig_ele, sig_pos, sig_z = _load_signal_arrays(base, mass_mev, args.Val)
    bg_ele,  bg_pos         = _load_background_arrays(mass_mev, args.Val)

    # build feature arrays
    sig_minabs = np.minimum(np.abs(sig_ele), np.abs(sig_pos))
    bg_minabs  = np.minimum(np.abs(bg_ele),  np.abs(bg_pos))

    # signal per-event weights for shape if epsilon provided
    if args.epsilon is not None:
        weights_sig = _compute_signal_weights(base, sig_z, float(args.epsilon), mass_mev, args.Val)
        eps_lab = f"eps={args.epsilon:g}"
    else:
        weights_sig = None
        eps_lab = "eps=<none>"

    # titles
    head = f"mass={mass_mev:g} MeV, Val={args.Val}, eps-idx={ei}, {eps_lab}\nExpected S={S_tot:.3g}, B={B_tot:.3g}, ZBi={Z_val:.3g}"

    # 1) electron z0_
    fig1, ax1 = plt.subplots(figsize=(7.6, 4.6))
    _counts_hist(ax1, sig_ele, bg_ele, args.bins, tuple(args.range) if args.range else None,
                 S_tot, B_tot, title=head + "\nFeature: electron z0_", xlabel="ele.track_.z0_ [mm]",
                 weights_sig=weights_sig)
    f1 = os.path.join(args.outdir, f"normed_ele_z0_{int(round(mass_mev))}MeV_V{args.Val}_eidx{ei}.png")
    fig1.tight_layout(); fig1.savefig(f1, dpi=160, bbox_inches="tight"); plt.close(fig1)

    # 2) positron z0_
    fig2, ax2 = plt.subplots(figsize=(7.6, 4.6))
    _counts_hist(ax2, sig_pos, bg_pos, args.bins, tuple(args.range) if args.range else None,
                 S_tot, B_tot, title=head + "\nFeature: positron z0_", xlabel="pos.track_.z0_ [mm]",
                 weights_sig=weights_sig)
    f2 = os.path.join(args.outdir, f"normed_pos_z0_{int(round(mass_mev))}MeV_V{args.Val}_eidx{ei}.png")
    fig2.tight_layout(); fig2.savefig(f2, dpi=160, bbox_inches="tight"); plt.close(fig2)

    # 3) min(|z0_e|, |z0_p|)
    fig3, ax3 = plt.subplots(figsize=(7.6, 4.6))
    _counts_hist(ax3, sig_minabs, bg_minabs, args.bins, tuple(args.range) if args.range else None,
                 S_tot, B_tot, title=head + "\nFeature: min(|z0_e|,|z0_p|)", xlabel="min(|ele.z0_|, |pos.z0_|) [mm]",
                 weights_sig=weights_sig)
    f3 = os.path.join(args.outdir, f"normed_minabs_z0_{int(round(mass_mev))}MeV_V{args.Val}_eidx{ei}.png")
    fig3.tight_layout(); fig3.savefig(f3, dpi=160, bbox_inches="tight"); plt.close(fig3)

    print("[ok] wrote:"); print(" ", f1); print(" ", f2); print(" ", f3)
    print(f"[info] S={S_tot:.6g}, B={B_tot:.6g}, ZBi={Z_val:.6g}; arrays length = {len(S_arr)}")

if __name__ == "__main__":
    main()

