#!/usr/bin/env python3
"""
plot_z0_signal_vs_bg.py

Overlay unit-normalized z0_ histograms for signal vs background:
  1) electron track z0_
  2) positron track z0_
  3) min(|ele.track_.z0_|, |pos.track_.z0_|)

Signal is epsilon-weighted using base.getProb(z, epsilon, mass, Val) at each
event's vertex z, then unit-normalized (density=True) for shape comparison.
Background is unit-normalized without epsilon weighting.

Usage:
  python3 plot_z0_signal_vs_bg.py --mass 150 --epsilon 3e-5 --Val 25 --bins 80 --outdir plots/
  # optional: --range -20 20
"""

import argparse
import os
import sys
import numpy as np
import matplotlib.pyplot as plt

# --- import background helpers ---
try:
    import bk_eff_selection as bg
except Exception as e:
    sys.stderr.write("ERROR: Could not import bg_eff_fraction.py. Make sure it is in PYTHONPATH or the working directory.\n")
    raise

# --- import signal base safely ---
def import_signal_base(module_name="decayLength5sel"):
    try:
        base = __import__(module_name)
        return base
    except SystemExit as se:
        sys.stderr.write(
            "FATAL: Importing '{}' triggered SystemExit, probably from argparse at top level.\n"
            "Please wrap the CLI in:\n"
            "    if __name__ == \"__main__\":\n"
            "        sys.exit(main())\n".format(module_name)
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
                    "Please guard the CLI with if __name__ == \"__main__\" in that file.\n".format(candidate)
                )
                raise
        else:
            raise

def _ensure_dir(d):
    if d and not os.path.isdir(d):
        os.makedirs(d, exist_ok=True)

def _compute_signal_weights(base, z_evt, epsilon, mass_mev, Val):
    w = []
    for z in z_evt:
        pr, pp = base.getProb(float(z), float(epsilon), float(mass_mev), int(Val))
        ww = float(pr) + float(pp)
        if not np.isfinite(ww) or ww < 0.0:
            ww = 0.0
        w.append(ww)
    return np.asarray(w, dtype=float)

def _unit_hist(ax, data_sig, data_bg, bins, rrange, title, xlabel, weights_sig=None):
    # Avoid NaNs/Infs
    data_sig = np.asarray(data_sig)
    data_bg  = np.asarray(data_bg)
    if weights_sig is not None:
        weights_sig = np.asarray(weights_sig)
        # align mask
        m_sig = np.isfinite(data_sig) & np.isfinite(weights_sig)
        data_sig = data_sig[m_sig]
        weights_sig = weights_sig[m_sig]
    else:
        m_sig = np.isfinite(data_sig)
        data_sig = data_sig[m_sig]
    data_bg = data_bg[np.isfinite(data_bg)]

    # Auto range if not provided: robust middle 99%
    if rrange is None:
        if data_sig.size and data_bg.size:
            both = np.concatenate([data_sig, data_bg])
        elif data_sig.size:
            both = data_sig
        elif data_bg.size:
            both = data_bg
        else:
            both = np.array([])
        if both.size:
            q1, q99 = np.quantile(both, [0.005, 0.995])
            pad = 0.05 * (q99 - q1 + 1e-9)
            rrange = (q1 - pad, q99 + pad)
        else:
            rrange = (-10.0, 10.0)

    # Plot; density=True makes areas 1 regardless of total weight
    ax.hist(data_sig, bins=bins, range=rrange, histtype="step", linewidth=1.6,
            density=True, label="signal (eps-weighted shape)", weights=weights_sig)
    ax.hist(data_bg,  bins=bins, range=rrange, histtype="step", linewidth=1.6,
            density=True, label="background")
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel("unit-normalized density")
    ax.grid(True, alpha=0.3)
    ax.legend()

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

def main():
    ap = argparse.ArgumentParser(description="Overlay unit-normalized z0_ histograms for epsilon-weighted signal vs background.")
    ap.add_argument("--mass", type=float, required=True, help="A' mass in MeV.")
    ap.add_argument("--epsilon", type=float, required=True, help="epsilon (not epsilon^2). Used for signal weighting.")
    ap.add_argument("--Val",  type=int, default=25, help="Selection parameter for tight selection. Default 25.")
    ap.add_argument("--bins", type=int, default=80, help="Number of bins. Default 80.")
    ap.add_argument("--range", type=float, nargs=2, default=None, help="Explicit z0_ range: min max.")
    ap.add_argument("--outdir", type=str, default=".", help="Output directory. Default current directory.")
    args = ap.parse_args()

    _ensure_dir(args.outdir)

    base = import_signal_base()
    mass_mev = float(args.mass)
    Val = int(args.Val)
    epsilon = float(args.epsilon)

    sig_ele, sig_pos, sig_z = _load_signal_arrays(base, mass_mev, Val)
    bg_ele,  bg_pos         = _load_background_arrays(mass_mev, Val)

    # Build min(|z0_e|, |z0_p|)
    sig_minabs = np.minimum(np.abs(sig_ele), np.abs(sig_pos))
    bg_minabs  = np.minimum(np.abs(bg_ele),  np.abs(bg_pos))

    # Signal weights from epsilon, mass, z
    weights_sig = _compute_signal_weights(base, sig_z, epsilon, mass_mev, Val)

    # 1) electron z0_
    fig1, ax1 = plt.subplots(figsize=(7.2, 4.4))
    _unit_hist(ax1, sig_ele, bg_ele, args.bins, tuple(args.range) if args.range else None,
               title=f"electron z0_ (tight); mass {mass_mev:g} MeV, Val={Val}, eps={epsilon:g}",
               xlabel="ele.track_.z0_ [mm]", weights_sig=weights_sig)
    f1 = os.path.join(args.outdir, f"z0_ele_sig_vs_bg_{int(round(mass_mev))}MeV_V{Val}_eps{str(epsilon).replace('.','p')}.png")
    fig1.tight_layout(); fig1.savefig(f1, dpi=160, bbox_inches="tight"); plt.close(fig1)

    # 2) positron z0_
    fig2, ax2 = plt.subplots(figsize=(7.2, 4.4))
    _unit_hist(ax2, sig_pos, bg_pos, args.bins, tuple(args.range) if args.range else None,
               title=f"positron z0_ (tight); mass {mass_mev:g} MeV, Val={Val}, eps={epsilon:g}",
               xlabel="pos.track_.z0_ [mm]", weights_sig=weights_sig)
    f2 = os.path.join(args.outdir, f"z0_pos_sig_vs_bg_{int(round(mass_mev))}MeV_V{Val}_eps{str(epsilon).replace('.','p')}.png")
    fig2.tight_layout(); fig2.savefig(f2, dpi=160, bbox_inches="tight"); plt.close(fig2)

    # 3) min(|z0_e|, |z0_p|)
    fig3, ax3 = plt.subplots(figsize=(7.2, 4.4))
    _unit_hist(ax3, sig_minabs, bg_minabs, args.bins, tuple(args.range) if args.range else None,
               title=f"min(|z0_e|,|z0_p|) (tight); mass {mass_mev:g} MeV, Val={Val}, eps={epsilon:g}",
               xlabel="min(|ele.z0_|, |pos.z0_|) [mm]", weights_sig=weights_sig)
    f3 = os.path.join(args.outdir, f"z0_minabs_sig_vs_bg_{int(round(mass_mev))}MeV_V{Val}_eps{str(epsilon).replace('.','p')}.png")
    fig3.tight_layout(); fig3.savefig(f3, dpi=160, bbox_inches="tight"); plt.close(fig3)

    print("[ok] wrote:"); print(" ", f1); print(" ", f2); print(" ", f3)
    print(f"[info] counts: sig(ele,pos) = {sig_ele.size}, {sig_pos.size}  |  bg(ele,pos) = {bg_ele.size}, {bg_pos.size}")

if __name__ == "__main__":
    main()

