#!/usr/bin/env python3
"""
make_maxZbi_grid_worker.py
"""

import os
import sys
import glob
import math
import argparse

import numpy as np
import matplotlib.pyplot as plt


ALL_SUFFIXES = [
    "ann",
    "bdt",
    "cut",
    "isL1L1_ann",
    "isL1L1_cut",
    "isL2L2_ann",
    "isL2L2_cut",
    "isL3L3_ann",
    "isL3L3_cut",
    "isL1L2_ann",
    "isL1L2_cut",
    "isL2L3_ann",
    "isL2L3_cut",
]


def parse_result_file(path):
    vals = {}
    with open(path, "r") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            parts = line.split()
            if len(parts) < 2:
                continue
            vals[parts[0]] = parts[1]

    try:
        mass = float(vals["mass_MeV"])
        eps  = float(vals["epsilon"])
        Val  = int(float(vals["Val"]))
        Val2 = int(float(vals["Val2"]))
        if math.isnan(float(vals["S_yield"])):
            S_yield = -1.0
            B_yield = -1.0
            Zbi     = -1.0
        else:
            S_yield = math.floor(float(vals["S_yield"]))
            B_yield = float(vals["B_yield"])
            Zbi     = float(vals["Zbi"])
    except KeyError as e:
        raise RuntimeError(f"Missing key {e} in file {path}")

    return mass, eps, Val, Val2, S_yield, B_yield, Zbi


def collect_best_by_mass_eps(directory, suffix):
    pattern = os.path.join(directory, f"m*_Val*_epsIdx*_projIdx*_{suffix}.txt")
    files   = sorted(glob.glob(pattern))
    if not files:
        raise RuntimeError(f"No files found matching {pattern}")

    best = {}
    bad  = []

    for path in files:
        try:
            mass, eps, Val, Val2, S_yield, B_yield, Zbi = parse_result_file(path)
        except RuntimeError:
            print(f"[{suffix}] WARNING — skipping malformed file: {path}")
            continue

        if (B_yield < 0) or math.isnan(B_yield):
            bad.append((path, B_yield))
            continue
        if B_yield == 0:
            continue
        if S_yield <= 0:
            continue

        key = (mass, eps)
        if key not in best or Zbi > best[key]["Zbi"]:
            best[key] = {"Zbi": Zbi, "Val": Val, "Val2": Val2,
                         "S": S_yield, "B": B_yield}

    if bad:
        print(f"[{suffix}] WARNING — files with negative/NaN B_yield (skipped):")
        for p, bv in bad:
            print(f"  {p}   B_yield={bv}")
    else:
        print(f"[{suffix}] No negative/NaN B_yield files.")

    if not best:
        raise RuntimeError(
            f"[{suffix}] No valid (mass, epsilon) combinations found.")

    return best


def build_grids(best_dict):
    masses   = sorted({m for (m, _e) in best_dict.keys()})
    epsilons = sorted({e for (_m, e) in best_dict.keys()})
    nm, ne   = len(masses), len(epsilons)

    mass_to_idx = {m: i for i, m in enumerate(masses)}
    eps_to_idx  = {e: i for i, e in enumerate(epsilons)}

    Z_grid    = np.full((ne, nm), np.nan)
    Val_grid  = np.full((ne, nm), np.nan)
    Val2_grid = np.full((ne, nm), np.nan)
    S_grid    = np.full((ne, nm), np.nan)
    B_grid    = np.full((ne, nm), np.nan)

    for (m, e), info in best_dict.items():
        i_m = mass_to_idx[m]
        i_e = eps_to_idx[e]
        Z_grid[i_e, i_m]    = info["Zbi"]
        Val_grid[i_e, i_m]  = info["Val"]
        Val2_grid[i_e, i_m] = info["Val2"]
        S_grid[i_e, i_m]    = info["S"]
        B_grid[i_e, i_m]    = info["B"]

    return (np.array(masses), np.array(epsilons),
            Z_grid, Val_grid, Val2_grid, S_grid, B_grid)


def make_bin_edges_from_centers_lin(centers):
    centers = np.asarray(centers)
    if centers.size < 2:
        raise ValueError("Need at least two centers to build edges.")
    edges = np.empty(centers.size + 1)
    edges[1:-1] = 0.5 * (centers[:-1] + centers[1:])
    edges[0]    = centers[0]  - 0.5 * (centers[1] - centers[0])
    edges[-1]   = centers[-1] + 0.5 * (centers[-1] - centers[-2])
    return edges


def make_bin_edges_from_centers_log(centers):
    centers = np.asarray(centers)
    if centers.size < 2:
        raise ValueError("Need at least two centers to build edges.")
    if np.any(centers <= 0):
        raise ValueError("Epsilon centers must be positive for log-scale edges.")
    edges = np.empty(centers.size + 1)
    edges[1:-1] = np.sqrt(centers[:-1] * centers[1:])
    edges[0]    = centers[0]  / np.sqrt(centers[1]  / centers[0])
    edges[-1]   = centers[-1] * np.sqrt(centers[-1] / centers[-2])
    return edges


def plot_2d_grid(masses, eps, grid, outfile, label, title="",
                 fmt="{:.2f}", cmap="cividis"):
    x_edges = make_bin_edges_from_centers_lin(masses)
    y_edges = make_bin_edges_from_centers_log(eps)

    fig, ax = plt.subplots(figsize=(9, 7))
    mesh = ax.pcolormesh(x_edges, y_edges, grid, shading="auto", cmap=cmap)
    ax.set_yscale("log")
    ax.set_ylim(eps.min(), eps.max())
    ax.set_xlabel("mass [MeV]")
    ax.set_ylabel("epsilon")
    if title:
        ax.set_title(title)

    cbar = fig.colorbar(mesh, ax=ax)
    cbar.set_label(label)

    norm    = mesh.norm
    cmap_fn = mesh.cmap
    for i_e, eps_val in enumerate(eps):
        for i_m, mass_val in enumerate(masses):
            val = grid[i_e, i_m]
            if np.isnan(val):
                continue
            rgba        = cmap_fn(norm(val))
            r, g, b, _ = rgba
            luminance   = 0.299*r + 0.587*g + 0.114*b
            ax.text(mass_val, eps_val, fmt.format(val),
                    ha="center", va="center", fontsize=7,
                    color="white" if luminance < 0.5 else "black")

    fig.tight_layout()
    fig.savefig(outfile, dpi=150)
    plt.close(fig)
    print(f"  Saved {outfile}")


def make_plots_for_suffix(indir, outdir, suffix):
    subdir = os.path.join(outdir, suffix)
    os.makedirs(subdir, exist_ok=True)

    best = collect_best_by_mass_eps(indir, suffix)
    masses, epsilons, Z_grid, Val_grid, Val2_grid, S_grid, B_grid = build_grids(best)

    Val_cut_grid  = 0.5 * (Val_grid / 25)
    Val2_cut_grid = 50.0 * (Val2_grid / 10) + 1.0
    Val3_cut_grid = .797*(1-Val_grid/25.0)+.9983*(Val_grid/25.0)

    def out(name):
        return os.path.join(subdir, name)

    plot_2d_grid(masses, epsilons, Z_grid,
                 outfile=out("maxZbi_grid.png"),
                 label="max Zbi", title=f"Max Zbi  [{suffix}]")

    plot_2d_grid(masses, epsilons, Val_cut_grid,
                 outfile=out("ValCut_at_maxZbi_grid.png"),
                 label="z0 cut value")

    plot_2d_grid(masses, epsilons, Val2_cut_grid,
                 outfile=out("Val2Cut_at_maxZbi_grid.png"),
                 label="proj cut")

    plot_2d_grid(masses, epsilons, Val3_cut_grid,
                 outfile=out("Val3Cut_at_maxZbi_grid.png"),
                 label="z0 cut value")

    plot_2d_grid(masses, epsilons, S_grid,
                 outfile=out("Signal_at_maxZbi_grid.png"),
                 label="S_yield")

    plot_2d_grid(masses, epsilons, B_grid,
                 outfile=out("Background_at_maxZbi_grid.png"),
                 label="B_yield")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--task-id", type=int, required=True)
    ap.add_argument("--indir", required=True)
    ap.add_argument("--outdir", required=True)
    args = ap.parse_args()

    suffix = ALL_SUFFIXES[args.task_id]

    make_plots_for_suffix(args.indir, args.outdir, suffix)


if __name__ == "__main__":
    main()
