#!/usr/bin/env python3
"""
plot_z0_weighted.py

Make epsilon-weighted histograms of ele.track_.z0_ and pos.track_.z0_ for events
passing the tight selection, at a given A' mass and epsilon.

Weights per event are w(z) = prho(z) + pphi(z) from getProb(z, epsilon, mA, Val),
which already folds in realistic momentum distributions (via getAEnergy) and the
acceptance fraction F(z) (via getFrac with tight_selection).

USAGE
-----
python3 plot_z0_weighted.py --mass 150 --epsilon 3e-5 --val 25 --out z0_150MeV_e3e-5.png
"""

import argparse
import os
import sys
import numpy as np
import matplotlib.pyplot as plt

# Import Rory's base module (must be in PYTHONPATH or the working directory)
try:
    import decayLength5sel as base
except Exception as e:
    sys.stderr.write("ERROR: Could not import 'decayLength51162025.py'. "
                     "Place this script in the same directory or add it to PYTHONPATH.\n")
    raise

def compute_weights(z_vals, epsilon, mass_mev, Val):
    """
    For each z in z_vals, compute w(z) = prho(z) + pphi(z),
    where prho, pphi are from base.getProb(z, epsilon, mass_mev, Val).
    Returns a numpy array of weights aligned with z_vals.
    """
    # Vectorized via list comprehension to preserve base.getProb behavior
    weights = []
    for z in z_vals:
        pr, pp = base.getProb(float(z), float(epsilon), float(mass_mev), int(Val))
        w = float(pr) + float(pp)
        if not np.isfinite(w) or w < 0.0:
            w = 0.0
        weights.append(w)
    return np.asarray(weights, dtype=float)

def main():
    parser = argparse.ArgumentParser(description="Weighted z0_ histograms for ele/pos after tight selection.")
    parser.add_argument("--mass", type=float, required=True, help="A' mass in MeV (e.g. 150)")
    parser.add_argument("--epsilon", type=float, required=True, help="Kinetic mixing (epsilon), not epsilon^2")
    parser.add_argument("--val", type=int, default=25, help="Selection hyperparameter 'Val' (default=25)")
    parser.add_argument("--bins", type=int, default=80, help="Number of bins for z0_ histograms (default=80)")
    parser.add_argument("--range", type=float, nargs=2, default=None,
                        help="Explicit z0_ range as 'min max' (e.g. -20 20). If omitted, auto from data.")
    parser.add_argument("--out", type=str, default=None, help="Output PNG filename. If omitted, an automatic name is used.")
    args = parser.parse_args()

    # Resolve mass key and load cached events/denominator once
    mkey = base._mass_key(args.mass)
    events = base._events_cache(mkey)

    # Build tight selection mask
    mask = base.tight_selection(events, args.val)

    # Extract arrays
    z_evt   = events["vertex.pos_.fZ"][mask]
    ele_z0  = events.get("ele.track_.z0_")[mask]
    pos_z0  = events.get("pos.track_.z0_")[mask]

    # Guard against NaNs/Infs
    finite_mask = np.isfinite(z_evt) & np.isfinite(ele_z0) & np.isfinite(pos_z0)
    z_evt  = z_evt[finite_mask]
    ele_z0 = ele_z0[finite_mask]
    pos_z0 = pos_z0[finite_mask]

    if z_evt.size == 0:
        sys.stderr.write("No events after selection. Nothing to plot.\n")
        sys.exit(2)

    # Compute epsilon-weighted probability per event at its vertex z
    weights = compute_weights(z_evt, args.epsilon, args.mass, args.val)

    # Histogram settings
    if args.range is not None:
        z0_min, z0_max = float(args.range[0]), float(args.range[1])
    else:
        # Derive a robust range from the middle 99% to avoid extreme tails
        both = np.concatenate([ele_z0, pos_z0])
        q1, q99 = np.quantile(both, [0.005, 0.995])
        pad = 0.05 * (q99 - q1 + 1e-9)
        z0_min, z0_max = q1 - pad, q99 + pad

    # Make weighted histograms for ele and pos separately
    hist_ele, edges = np.histogram(ele_z0, bins=args.bins, range=(z0_min, z0_max), weights=weights)
    hist_pos, _     = np.histogram(pos_z0, bins=args.bins, range=(z0_min, z0_max), weights=weights)
    centers = 0.5 * (edges[:-1] + edges[1:])

    # Plot
    plt.figure(figsize=(8, 4.6))
    plt.step(centers, hist_ele, where="mid", linewidth=1.6, label="electron z0_ (weighted)")
    plt.step(centers, hist_pos, where="mid", linewidth=1.6, linestyle="--", label="positron z0_ (weighted)")
    plt.xlabel("track z0_ [mm]")
    plt.ylabel("weighted counts")
    plt.title(f"Weighted z0_ after tight selection\nmA'≈{base._closest_available_mass(args.mass)} MeV, epsilon={args.epsilon:g}, Val={args.val}")
    plt.grid(True, alpha=0.3)
    plt.legend()

    # Output name
    out_png = args.out
    if not out_png:
        safe_eps = str(args.epsilon).replace(".", "p").replace("-", "m")
        out_png = f"z0_weighted_{int(round(args.mass))}MeV_eps{safe_eps}_Val{args.val}.png"

    plt.tight_layout()
    plt.savefig(out_png, dpi=160, bbox_inches="tight")
    print(f"[ok] wrote {out_png}")
    print(f"[info] events used: {z_evt.size}")
    print(f"[info] ele.hist sum={hist_ele.sum():.6g}  pos.hist sum={hist_pos.sum():.6g}")

if __name__ == "__main__":
    main()

