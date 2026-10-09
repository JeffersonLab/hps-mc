#!/usr/bin/env python3
"""
plot_bkg_obs.py  –  extract and plot n_a_obs/bkg_est quantities from HPS yield JSON(s).

Usage:
    python plot_bkg_obs.py  file1.json [file2.json ...]

Each JSON is expected to be a list (or dict with a list value) of mass-point entries
of the form:
    {
        "mass_GeV": <float>,
        "n_a_obs":  <float>,
        "n_a_obs_err": <float>,
        "bkg_est":  <float>,
        "bkg_est_err": <float>,
        ...
    }

Produces one multi-panel PNG per JSON file.
"""

import sys
import json
import pathlib
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
from scipy.stats import poisson, norm

# ── Look-elsewhere correction ──────────────────────────────────────────────────
N_TRIALS = 20          # number of independent mass bins searched

# ── Significance contour levels (in σ) ────────────────────────────────────────
LOCAL_SIGMAS  = [1, 2, 3]
GLOBAL_SIGMAS = [1, 2, 3]   # global p = local_p * N_TRIALS  →  z_global < z_local

# ── Significance contour colours ──────────────────────────────────────────────
SIG_COLORS = {1: "#4CAF50", 2: "#FF9800", 3: "#F44336"}


def load_mass_points(path: str) -> list:
    """Load JSON and return a list of (region, dict) tuples with the required keys.

    Handles the structure:
        { "meta": {...}, "regions": { "L1L1": [{...}, ...], "L2L2": [{...}, ...] } }
    as well as a bare list or a single dict.
    Returns a list of dicts, each with an extra "region" key.
    """
    with open(path) as f:
        raw = json.load(f)

    REQUIRED = {"mass_GeV", "n_a_obs", "n_a_obs_err", "bkg_est", "bkg_est_err"}

    # Collect (region_name, entry_list) pairs
    region_lists = []  # list of (region_name, [entries])

    if isinstance(raw, dict) and "regions" in raw:
        regions = raw["regions"]
        if isinstance(regions, dict):
            for region_name, entries in regions.items():
                if isinstance(entries, list):
                    region_lists.append((region_name, entries))
        elif isinstance(regions, list):
            region_lists.append(("default", regions))
    elif isinstance(raw, dict):
        # Try first list value, else treat as single entry
        for v in raw.values():
            if isinstance(v, list):
                region_lists.append(("default", v))
                break
        else:
            region_lists.append(("default", [raw]))
    elif isinstance(raw, list):
        region_lists.append(("default", raw))

    points = []
    for region_name, entries in region_lists:
        for entry in entries:
            if not isinstance(entry, dict):
                continue
            if not REQUIRED.issubset(entry.keys()):
                continue
            rec = {k: entry[k] for k in REQUIRED}
            rec["region"] = region_name
            points.append(rec)

    if not points:
        raise ValueError(f"No valid mass-point entries found in {path}")

    points.sort(key=lambda d: (d["region"], d["mass_GeV"]))
    return points


def local_pvalue(n_obs: float, mu_bkg: float,
                sigma_bkg: float = 0.0, sigma_obs: float = 0.0) -> float:
    """
    One-sided Gaussian p-value accounting for uncertainties on both the
    background estimate and the observed count.

    Since n_obs and bkg_est are independent, their difference is Gaussian:

        n_obs - bkg_est ~ N(0, sqrt(sigma_obs^2 + sigma_bkg^2))

    so:
        p = norm.sf( (n_obs - bkg_est) / sqrt(sigma_obs^2 + sigma_bkg^2) )

    sigma_obs = 0 recovers the bkg-uncertainty-only case.
    Both = 0 is a degenerate edge case; returns 0.5 at equality.
    """
    sigma_tot = np.sqrt(sigma_obs**2 + sigma_bkg**2)
    if sigma_tot <= 0.0:
        return 0.5 if n_obs == mu_bkg else (1.0 if n_obs < mu_bkg else 0.0)
    z = (n_obs - mu_bkg) / sigma_tot
    return float(norm.sf(z))


def pval_to_z(p: float) -> float:
    """Convert p-value to one-sided Gaussian significance z."""
    p = np.clip(p, 1e-15, 1.0)
    return float(norm.isf(p))


def ratio_err(n, dn, b, db):
    """Gaussian error propagation for r = n / b."""
    r = n / b
    dr = r * np.sqrt((dn / n) ** 2 + (db / b) ** 2)
    return r, dr


def make_counts_ratio_plot(points: list, out_path: str, title: str = ""):
    """Counts (log y) + ratio panel, HPS-style."""
    from matplotlib.gridspec import GridSpec

    regions = sorted(set(p["region"] for p in points))
    STYLES = [
        {"obs_color": "black",   "bkg_color": "#CC0000", "ratio_color": "#CC0000"},
        {"obs_color": "#1565C0", "bkg_color": "#E65100", "ratio_color": "#E65100"},
        {"obs_color": "#2E7D32", "bkg_color": "#6A1B9A", "ratio_color": "#6A1B9A"},
    ]

    fig = plt.figure(figsize=(8, 7))
    gs  = GridSpec(2, 1, figure=fig, height_ratios=[3, 1.2], hspace=0.06)
    ax_cnt = fig.add_subplot(gs[0])
    ax_rat = fig.add_subplot(gs[1], sharex=ax_cnt)

    all_mass = np.array([p["mass_GeV"] for p in points])
    xlo, xhi = all_mass.min() * 0.995, all_mass.max() * 1.005

    for i, region in enumerate(regions):
        rpts  = [p for p in points if p["region"] == region]
        mass  = np.array([p["mass_GeV"]     for p in rpts])
        n_obs = np.array([p["n_a_obs"]      for p in rpts])
        n_err = np.array([p["n_a_obs_err"]  for p in rpts])
        b_est = np.array([p["bkg_est"]      for p in rpts])
        b_err = np.array([p["bkg_est_err"]  for p in rpts])
        sty   = STYLES[i % len(STYLES)]

        ratio    = n_obs / b_est
        ratio_dr = ratio * np.sqrt((n_err / np.where(n_obs > 0, n_obs, 1))**2
                                 + (b_err / np.where(b_est > 0, b_est, 1))**2)

        ax_cnt.step(mass, b_est, where="mid",
                    color=sty["bkg_color"], lw=1.5, ls="--",
                    label=f"ABCD est. ({region})")
        ax_cnt.fill_between(mass, b_est - b_err, b_est + b_err,
                            step="mid", color=sty["bkg_color"], alpha=0.18)
        ax_cnt.errorbar(mass, n_obs, yerr=n_err,
                        fmt="o", color=sty["obs_color"], ms=4,
                        lw=1.1, capsize=2, capthick=1.1, zorder=5,
                        label=f"Data obs. ({region})")

        ax_rat.errorbar(mass, ratio, yerr=ratio_dr,
                        fmt="o", color=sty["ratio_color"], ms=4,
                        lw=1.1, capsize=2, capthick=1.1)

    # counts cosmetics
    ax_cnt.set_yscale("log")
    ax_cnt.set_ylabel("Events", fontsize=11)
    ax_cnt.legend(fontsize=9, framealpha=0.9, loc="upper right")
    ax_cnt.yaxis.set_major_formatter(ticker.LogFormatterMathtext())
    ax_cnt.xaxis.set_minor_locator(ticker.AutoMinorLocator())
    ax_cnt.grid(True, which="major", ls="--", alpha=0.35)
    ax_cnt.grid(True, which="minor", ls=":",  alpha=0.15)
    ax_cnt.set_xlim(xlo, xhi)
    ax_cnt.text(0.02, 0.97, "HPS Internal", transform=ax_cnt.transAxes,
                fontsize=11, fontweight="bold", va="top")
    ax_cnt.text(0.02, 0.89, r"$L_{\rm int} = 8.7\ \rm pb^{-1}$",
                transform=ax_cnt.transAxes, fontsize=9, va="top")
    if len(regions) == 1:
        ax_cnt.text(0.02, 0.82, regions[0],
                    transform=ax_cnt.transAxes, fontsize=9, va="top")
    plt.setp(ax_cnt.get_xticklabels(), visible=False)

    # ratio cosmetics
    ax_rat.axhline(1.0, color="gray", ls="-", lw=1.0)
    ax_rat.set_ylabel(r"ABCD / Obs.", fontsize=10)
    ax_rat.set_xlabel(r"$m_{e^+e^-}\ \rm [GeV]$", fontsize=11)
    ax_rat.set_ylim(0, 3)
    ax_rat.set_xlim(xlo, xhi)
    ax_rat.xaxis.set_minor_locator(ticker.AutoMinorLocator())
    ax_rat.yaxis.set_minor_locator(ticker.AutoMinorLocator())
    ax_rat.grid(True, which="major", ls="--", alpha=0.35)

    if title:
        fig.suptitle(title, fontsize=12, fontweight="bold", y=1.01)
    fig.savefig(out_path, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved: {out_path}")


def make_pvalue_plot(points: list, out_path: str, title: str = ""):
    """Local p-value curve with local/global sigma threshold lines."""
    regions = sorted(set(p["region"] for p in points))
    STYLES = [
        {"obs_color": "black"},
        {"obs_color": "#1565C0"},
        {"obs_color": "#2E7D32"},
    ]

    fig, ax = plt.subplots(figsize=(8, 5))

    all_mass = np.array([p["mass_GeV"] for p in points])
    xlo, xhi = all_mass.min() * 0.995, all_mass.max() * 1.005

    for i, region in enumerate(regions):
        rpts  = [p for p in points if p["region"] == region]
        mass  = np.array([p["mass_GeV"]     for p in rpts])
        n_obs = np.array([p["n_a_obs"]      for p in rpts])
        n_err = np.array([p["n_a_obs_err"]  for p in rpts])
        b_est = np.array([p["bkg_est"]      for p in rpts])
        b_err = np.array([p["bkg_est_err"]  for p in rpts])
        sty   = STYLES[i % len(STYLES)]

        print(f"  Computing p-values for {region} ({len(rpts)} mass points)...")
        p_local = np.array([local_pvalue(n, b, db, dn)
                            for n, dn, b, db in zip(n_obs, n_err, b_est, b_err)])
        ax.plot(mass, p_local, "o-",
                color=sty["obs_color"], ms=3.5, lw=1.0, zorder=5,
                label=f"Local $p$ ({region})")

        # Report minimum p-value for masses > 70 MeV
        mask = mass * 1e3 > 70.0
        if mask.any():
            idx_min  = np.argmin(p_local[mask])
            m_min    = mass[mask][idx_min]
            p        = p_local[mask][idx_min]          # one p-value from the data
            z_loc    = norm.isf(np.clip(p, 1e-15, 1.0))
            # Global sigma: same LEE scaling as the plot threshold lines.
            # p*N_TRIALS is the trials-corrected p; norm.isf of that gives global sigma.
            # If p*N > 1 the measurement is not globally significant (z_glo <= 0).
            z_glo    = z_loc / N_TRIALS
            print(f"  [{region}] min p-value (m > 70 MeV) at m = {m_min*1e3:.1f} MeV ({m_min:.4f} GeV):")
            print(f"    p = {p:.4e}")
            print(f"    local  significance = {z_loc:.2f} sigma")
            print(f"    global significance = {z_glo:.2f} sigma  [/{N_TRIALS} LEE]")

    # sigma threshold lines — thick
    for sigma in LOCAL_SIGMAS:
        p_loc_line = norm.sf(sigma)
        p_glo_line = norm.sf(sigma) / N_TRIALS   # local p needed for global sigma claim
        ax.axhline(p_loc_line,
                   color=SIG_COLORS[sigma], ls="-", lw=2.8, alpha=0.85,
                   label=fr"Local ${sigma}\sigma$  ($p={p_loc_line:.2e}$)")
        ax.axhline(p_glo_line,
                   color=SIG_COLORS[sigma], ls="--", lw=2.8, alpha=0.85,
                   label=fr"Global ${sigma}\sigma$  ($p={p_glo_line:.2e}$)")

    ax.set_yscale("log")
    ax.set_ylabel("$p$-value", fontsize=11)
    ax.set_xlabel(r"$m_{e^+e^-}\ \rm [GeV]$", fontsize=11)
    ax.set_xlim(xlo, xhi)
    ax.set_title(fr"Poisson $p$-value  (LEE: $N={N_TRIALS}$ trials, Bonferroni)", fontsize=10)
    ax.legend(fontsize=8, ncol=3, framealpha=0.9)
    ax.xaxis.set_minor_locator(ticker.AutoMinorLocator())
    ax.grid(True, which="major", ls="--", alpha=0.35)
    ax.grid(True, which="minor", ls=":",  alpha=0.15)

    if title:
        fig.suptitle(title, fontsize=12, fontweight="bold")
    fig.savefig(out_path, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved: {out_path}")

def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    for json_path in sys.argv[1:]:
        print(f"Processing: {json_path}")
        points = load_mass_points(json_path)
        stem   = pathlib.Path(json_path).stem
        make_counts_ratio_plot(points, f"{stem}_counts_ratio.png", title=stem)
        make_pvalue_plot(points,       f"{stem}_pvalue.png",       title=stem)

if __name__ == "__main__":
    main()
