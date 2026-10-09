#!/usr/bin/env python3
"""
make_maxZbi_grid_onthefly_regions.py

Runs task-ids 4, 10, 6, 12 (isL1L1_cut, isL1L2_cut, isL2L2_cut, isL2L3_cut)
and writes one JSON per suffix.

val2 hard-coded to 7 => proj_sig_cut = 2.8
Two |z0| regions evaluated per (mass, epsilon) point, with boundaries that
are *hit-category specific* (not mass dependent):
    Region lo : min_y0_lo  < |z0| <= min_y0_hi   (bounded band)
    Region hi : min_y0_hi  < |z0|                 (unbounded above)

Per-category boundaries (from optimisation table):
    L1L1 : lo_band=(0.24, 0.32),  hi_band=(0.32, inf)
    L1L2 : lo_band=(0.30, 0.40),  hi_band=(0.40, inf)
    L2L2 : lo_band=(0.20, 0.48),  hi_band=(0.48, inf)
    L2L3 : lo_band=(0.44, 0.50),  hi_band=(0.50, inf)

Data source: streams directly from the 10% preselection ROOT files under
    /sdf/data/hps/physics2021/preselection/v11/data_10pc/
    Filename pattern: merged_hps_0<NNNNN>_job<J>_merge-batch-{1,2}.root
All files with run number <= RUN_CAP (hard-coded 14200) are loaded in a
single pass at startup; every subsequent per-mass computation is pure numpy.

CLI: python make_maxZbi_grid_onthefly_regions.py --outdir /path/to/output
"""

import os
import sys
import math
import json
import glob
import argparse
import importlib

import numpy as np

# ---------------------------------------------------------------------------
# Hard-coded val2
# ---------------------------------------------------------------------------
FIXED_VAL2   = 7
PROJ_SIG_CUT = 4.0 * FIXED_VAL2 / 10.0   # 2.8

# ---------------------------------------------------------------------------
# Data scale factor: original 1% sample used SCALE=100; 10% sample uses 10.
# ---------------------------------------------------------------------------
SIGNAL_DATA_SCALE = 1.0


RUN_CAP = 15200   # only include runs with run number <= this value

BATCH_DATA_DIRS = [
    "/sdf/data/hps/physics2021/preselection/v11/data_10pc",
]

# Branch names as they appear in the ROOT TTrees
_BG_BRANCHES = [
    "vertex.invM_",
    "psum",
    "ele.track_.z0_",
    "pos.track_.z0_",
    "vtx_proj_sig",
    "isL1L1",
    "isL2L2",
    "isL3L3",
    "isL1L2",
    "isL2L3",
]
_HITCAT_BRANCHES = ["isL1L1", "isL2L2", "isL3L3", "isL1L2", "isL2L3"]

# ---------------------------------------------------------------------------
# Task IDs and suffixes
# ---------------------------------------------------------------------------
ALL_SUFFIXES = [
    "ann",          # 0
    "bdt",          # 1
    "cut",          # 2
    "isL1L1_ann",   # 3
    "isL1L1_cut",   # 4  <-- task-id 4
    "isL2L2_ann",   # 5
    "isL2L2_cut",   # 6  <-- task-id 6
    "isL3L3_ann",   # 7
    "isL3L3_cut",   # 8
    "isL1L2_ann",   # 9
    "isL1L2_cut",   # 10 <-- task-id 10
    "isL2L3_ann",   # 11
    "isL2L3_cut",   # 12 <-- task-id 12
]

TASK_IDS = [4, 10, 6, 12]  # isL1L1_cut, isL1L2_cut, isL2L2_cut, isL2L3_cut

# ---------------------------------------------------------------------------
# Per-hit-category |z0| regions.
# Each category gets exactly two exclusive regions:
#   "lo" : min_y0_low_mass < |z0| <= min_y0_high_mass   (bounded band)
#   "hi" : min_y0_high_mass < |z0|                       (unbounded above)
# Boundaries come from the optimisation table and are NOT mass-dependent.
# ---------------------------------------------------------------------------
#   format: (label, lower_bound, upper_bound);  upper_bound=None => unbounded
HITCAT_Z0_REGIONS = {
    "isL1L1": [("lo", 0.24, 0.32), ("hi", 0.32, None)],
    "isL1L2": [("lo", 0.30, 0.40), ("hi", 0.40, None)],
    "isL2L2": [("lo", 0.20, 0.48), ("hi", 0.48, None)],
    "isL2L3": [("lo", 0.44, 0.50), ("hi", 0.50, None)],
    "nocat":  [("lo", 0.24, 0.32), ("hi", 0.32, None)],  # fallback
}

# ---------------------------------------------------------------------------
# Grid definition
# ---------------------------------------------------------------------------
MASSES_MEV = np.arange(50, 350, 25, dtype=float)
EPSILONS   = np.logspace(math.log10(1e-4), math.log10(1e-2), 150)

# ---------------------------------------------------------------------------
# Physics constants
# ---------------------------------------------------------------------------
ALPHA_QED = 1.0 / 137.0459991
HBAR_C    = 1.973e-14   # GeV*cm
ALPHA_D   = 0.01


# ---------------------------------------------------------------------------
# Physics helpers
# ---------------------------------------------------------------------------
def scale_const():
    return 3.0 * math.pi / (2.0 * 1.0 * ALPHA_QED)

def beta_func(x, y):
    return (1 + y**2 - x**2 - 2*y) * (1 + y**2 - x**2 + 2*y)

def width_Ap_to_charged(alpha_D, f_pi_D, m_pi_D, m_V_D, m_Ap):
    x = m_pi_D / m_Ap; y = m_V_D / m_Ap
    Tv = 18.0 - (3.0/2.0 + 3.0/4.0)
    coeff = alpha_D * Tv / (192.0 * math.pi**4)
    return (coeff * (m_Ap/m_pi_D)**2 * (m_V_D/m_pi_D)**2
            * (m_pi_D/f_pi_D)**4 * m_Ap * beta_func(x, y)**1.5)

def width_Ap_to_vector(alpha_D, f_pi_D, m_pi_D, m_V_D, m_Ap, multiplicity=1.0):
    x = m_V_D / m_Ap; y = m_pi_D / m_Ap
    pf = (alpha_D * multiplicity) / (192.0 * math.pi**4)
    rt = (m_Ap/m_pi_D)**2 * (m_V_D/m_pi_D)**2 * (m_pi_D/f_pi_D)**4
    return pf * rt * m_Ap * beta_func(x, y)**1.5

def width_Ap_to_invis(alpha_D, m_pi_D, m_V_D, m_Ap):
    t1 = 1 - (4.0 * m_pi_D**2) / m_Ap**2
    t2 = (m_V_D**2 / (m_Ap**2 - m_V_D**2))**2
    return (2.0 * alpha_D / 3.0) * m_Ap * t1**1.5 * t2

def rate_2l(alpha_D, f_pi_D, m_pi_D, m_V_D, m_Ap, epsilon, m_l, rho):
    alpha = 1.0 / 137.0
    coeff = (16 * math.pi * alpha_D * alpha * epsilon**2 * f_pi_D**2) / (3 * m_V_D**2)
    t1 = (m_V_D**2 / (m_Ap**2 - m_V_D**2))**2
    t2 = (1 - (4 * m_l**2 / m_V_D**2))**0.5
    t3 = 1 + (2 * m_l**2 / m_V_D**2)
    return coeff * t1 * t2 * t3 * m_V_D * (2 if rho else 1)

def dark_masses(mass_mev):
    m_pi_D = mass_mev / 3.0
    m_V_D  = 1.8 * mass_mev / 3.0
    f_pi_D = (mass_mev / 3.0) / (4.0 * math.pi)
    return m_pi_D, m_V_D, f_pi_D


# ---------------------------------------------------------------------------
# Batch file discovery
# ---------------------------------------------------------------------------
def discover_batch_files(run_cap=RUN_CAP):
    """
    Find all 10% preselection ROOT files whose run number is <= run_cap.

    Expected layout (flat directory):
      <data_dir>/merged_hps_0<NNNNN>_job<J>_merge-batch-{1,2}.root
    The run number is the 5-digit field after 'hps_0' in the filename.
    """
    found = []
    for data_dir in BATCH_DATA_DIRS:
        if not os.path.isdir(data_dir):
            sys.stderr.write(f"[warn] data dir not found: {data_dir}\n")
            continue
        pattern = os.path.join(data_dir, "merged_hps_0*_job*_merge-batch-*.root")
        for path in glob.glob(pattern):
            fname = os.path.basename(path)
            # fname: merged_hps_0NNNNN_job..._merge-batch-N.root
            try:
                run_str = fname.split("merged_hps_0")[1].split("_job")[0]
                run_num = int(run_str)
            except (IndexError, ValueError):
                sys.stderr.write(f"[warn] could not parse run number from {fname}, skipping\n")
                continue
            if run_num <= run_cap:
                found.append(path)
    found.sort()
    return found


# ---------------------------------------------------------------------------
# Single-pass loader: reads all needed branches from all batch files once.
# Returns the same bg_arrays dict shape used by the rest of the script.
# Also returns raw invM and psum arrays for N_b_massbin_from_arrays.
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# Single-pass streaming accumulator.
# Streams all batch files once, chunk by chunk, and accumulates the per-mass
# counts needed for signal normalisation — never holding the full dataset in
# RAM.  Peak memory is one uproot chunk (~200k events) at a time.
# ---------------------------------------------------------------------------
def compute_N_b_per_mass(root_files, masses_mev, width_mev=2.0):
    """
    Returns a dict  {mass_mev: N_b}  equivalent to calling the original
    N_b_massbin for every mass, but using O(chunk) memory instead of
    O(full dataset).

    Applies the same cuts as the original N_b_massbin:
        psum > 2.8  AND  invM in (center - width/2, center + width/2)
    with SIGNAL_DATA_SCALE applied.
    """
    import uproot
    import awkward as ak

    # Pre-compute mass windows once
    windows = {}
    for m in masses_mev:
        center = float(m) / 1000.0
        half   = 0.5 * width_mev / 1000.0
        windows[float(m)] = (center - half, center + half)

    counts = {float(m): 0 for m in masses_mev}

    n_files   = len(root_files)
    n_loaded  = 0
    n_skipped = 0

    for i, path in enumerate(root_files):
        if (i % 50) == 0:
            print(f"  [loader] {i}/{n_files} files processed "
                  f"({n_loaded} ok, {n_skipped} skipped)...", flush=True)
        try:
            with uproot.open(path) as f:
                tree = None
                for key in f.keys():
                    if key.split(";")[0] == "preselection":
                        tree = f[key]
                        break
                if tree is None:
                    sys.stderr.write(f"[warn] no preselection tree in {path}\n")
                    n_skipped += 1
                    continue

                for chunk in tree.iterate(["vertex.invM_", "psum"], library="ak",
                                          step_size=200_000, how=dict):
                    invM_c = ak.to_numpy(chunk["vertex.invM_"])
                    psum_c = ak.to_numpy(chunk["psum"])
                    psum_mask = psum_c > 2.8
                    finite_mask = np.isfinite(invM_c)
                    base_mask = psum_mask & finite_mask
                    for m, (lo, hi) in windows.items():
                        counts[m] += int(np.sum(base_mask & (invM_c > lo) & (invM_c < hi)))

            n_loaded += 1
        except Exception as e:
            sys.stderr.write(f"[warn] skipping {path}: {e}\n")
            n_skipped += 1

    print(f"  [loader] done: {n_loaded} files loaded, {n_skipped} skipped.", flush=True)

    if n_loaded == 0:
        raise RuntimeError("No events loaded — check BATCH_DATA_DIRS and RUN_CAP.")

    # Apply scale and width normalisation, matching original N_b_massbin exactly
    return {m: SIGNAL_DATA_SCALE * float(c) / width_mev for m, c in counts.items()}


def N_b_massbin_from_cache(mass_mev, nb_cache):
    """Look up pre-computed N_b for this mass from the cache dict."""
    return nb_cache[float(mass_mev)]




# ---------------------------------------------------------------------------
# Background helpers
# ---------------------------------------------------------------------------
def mass_res(x):
    return 1000.0 * (.0025169 - .0133 * x + .137 * x * x)

def suffix_to_hitcat(suffix):
    for cat in ("isL1L1", "isL2L2", "isL3L3", "isL1L2", "isL2L3"):
        if suffix.startswith(cat + "_"):
            return cat
    return "nocat"

def z0_regions_for_suffix(suffix):
    """Return the two-element region list for this suffix's hit category."""
    hitcat = suffix_to_hitcat(suffix)
    return HITCAT_Z0_REGIONS.get(hitcat, HITCAT_Z0_REGIONS["nocat"])




# ---------------------------------------------------------------------------
# Signal yields for both regions in one pass
# ---------------------------------------------------------------------------
def compute_signal_all_regions(base, mass_mev, epsilon, nb_signal, proj_val, suffix,
                               z0_regions):
    m_pi_D, m_V_D, f_pi_D = dark_masses(mass_mev)
    hitcat = suffix_to_hitcat(suffix)

    try:
        ratio_val = base.ratio(mass_mev)
    except Exception:
        ratio_val = 1.0
    aprime_rate = nb_signal * scale_const() * ratio_val * mass_mev * epsilon**2

    rho_w   = width_Ap_to_vector(ALPHA_D, f_pi_D, m_pi_D, m_V_D, mass_mev, 0.75)
    phi_w   = width_Ap_to_vector(ALPHA_D, f_pi_D, m_pi_D, m_V_D, mass_mev, 1.5)
    invis_w = width_Ap_to_invis(ALPHA_D, m_pi_D, m_V_D, mass_mev)
    charg_w = width_Ap_to_charged(ALPHA_D, f_pi_D, m_pi_D, m_V_D, mass_mev)
    total_w      = rho_w + phi_w + invis_w + charg_w
    rho_fraction = rho_w / total_w
    phi_fraction = phi_w / total_w

    rho_Gamma  = rate_2l(ALPHA_D, f_pi_D, m_pi_D, m_V_D, mass_mev, epsilon, 0.511, True)
    phi_Gamma  = rate_2l(ALPHA_D, f_pi_D, m_pi_D, m_V_D, mass_mev, epsilon, 0.511, False)
    rho_length = (1000 * HBAR_C * 10.0) / rho_Gamma
    phi_length = (1000 * HBAR_C * 10.0) / phi_Gamma

    nan_result = {r: np.nan for r, _, _ in z0_regions}

    mkey = base._mass_key(m_V_D)
    try:
        events = base._events_cache(mkey)
    except Exception as e:
        sys.stderr.write(f"[warn] No MC events for mass {mass_mev} MeV: {e}\n")
        return nan_result

    try:
        den_edges, den_vals   = base.read_den_hist(m_V_D)
        psum_edges, psum_vals = base.read_psum_hist(m_V_D)
        zvals    = np.asarray(events["true_vd.vtx_z_"])
        num_vals, _ = np.histogram(zvals, bins=den_edges)
    except Exception as e:
        sys.stderr.write(f"[warn] Histogram read failed for mass {mass_mev}: {e}\n")
        return nan_result

    prho = pphi = 0.0
    for I in range(len(den_vals)):
        Ngen   = float(den_vals[I]) or 1.0
        Nacc   = float(num_vals[I]) if float(den_vals[I]) > 0 else 0.0
        z_cent = max(0.0, 0.5 * (den_edges[I] + den_edges[I+1]))
        for J in range(len(psum_vals)):
            pv    = 0.5 * (psum_edges[J] + psum_edges[J+1])
            gamma = 1000.0 * pv / m_V_D
            prho += (psum_vals[J] * (Nacc/Ngen)
                     * np.exp(-z_cent / (gamma * rho_length)) / (rho_length * gamma))
            pphi += (psum_vals[J] * (Nacc/Ngen)
                     * np.exp(-z_cent / (gamma * phi_length)) / (phi_length * gamma))

    acc_yield = aprime_rate * (prho * rho_fraction + pphi * phi_fraction)

    try:
        z_ev    = np.asarray(events["true_vd.vtx_z_"], dtype=np.float64)
        psum_ev = np.asarray(events["psum"],            dtype=np.float64)
        ele_z0  = np.asarray(events["ele.track_.z0_"],  dtype=np.float64)
        pos_z0  = np.asarray(events["pos.track_.z0_"],  dtype=np.float64)
        s_proj  = np.asarray(events["vtx_proj_sig"],    dtype=np.float64)
        invM_ev = np.asarray(events["vertex.invM_"],    dtype=np.float64)
    except Exception as e:
        sys.stderr.write(f"[warn] Missing branches for mass {mass_mev}: {e}\n")
        return nan_result

    N = len(psum_ev)
    hitmask = (np.asarray(events[hitcat], dtype=bool)
               if hitcat != "nocat" and hitcat in events
               else np.ones(N, dtype=bool))

    tot_frac     = rho_fraction + phi_fraction
    rho_frac_vis = rho_fraction / tot_frac
    phi_frac_vis = phi_fraction / tot_frac
    gamma_ev     = 1000.0 * psum_ev / m_V_D
    z_clipped    = np.where(z_ev >= 0, z_ev, 0.0)

    p_accept = (
        rho_frac_vis * np.exp(-z_clipped / (gamma_ev * rho_length)) / (rho_length * gamma_ev)
      + phi_frac_vis * np.exp(-z_clipped / (gamma_ev * phi_length)) / (phi_length * gamma_ev)
    )
    p_max = np.max(p_accept) if len(p_accept) > 0 else 1.0
    if p_max > 0:
        p_accept /= p_max

    mask_psum = (psum_ev >= 1.5) & (psum_ev <= 3.0)
    mask_vtxz = z_ev >= -1.1
    mask_hit  = mask_psum & mask_vtxz & hitmask
    mask_proj = mask_hit  & (s_proj < proj_val)

    center_geV  = float(base._mass_key(m_V_D)) / 1000.0
    mass_window = ((invM_ev > center_geV - 1.7 * mass_res(center_geV) / 1000.0) &
               (invM_ev < center_geV + 1.7 * mass_res(center_geV) / 1000.0))
    mask_proj   = mask_proj & mass_window

    abs_ele = np.abs(ele_z0)
    abs_pos = np.abs(pos_z0)

    results = {}
    for reg_label, z0_lo, z0_hi in z0_regions:
        if z0_hi is None:
            z0_mask = (abs_ele > z0_lo) & (abs_pos > z0_lo)
        else:
            z0_mask = ((abs_ele > z0_lo) & (abs_ele <= z0_hi) &
                       (abs_pos > z0_lo) & (abs_pos <= z0_hi))
        mask_final = mask_proj & z0_mask
        results[reg_label] = (acc_yield * np.sum(p_accept[mask_final]) / N
                              if N > 0 else 0.0)
    return results


# ---------------------------------------------------------------------------
# Grid loop for one suffix
# ---------------------------------------------------------------------------
def run_grid(base_module, outdir, task_id, suffix, nb_cache):
    """
    nb_cache: {mass_mev: N_b} pre-computed by compute_N_b_per_mass in main().
    """
    os.makedirs(outdir, exist_ok=True)
    print(f"\n{'='*60}")
    print(f"[{suffix}]  task-id={task_id}  proj_sig_cut={PROJ_SIG_CUT:.4f}")

    try:
        base = importlib.import_module(base_module)
    except Exception as e:
        sys.exit(f"[error] Cannot import '{base_module}': {e}")

    z0_regions = z0_regions_for_suffix(suffix)
    print(f"  z0 regions: { {r: (lo, hi) for r, lo, hi in z0_regions} }", flush=True)

    points = []

    for mass in MASSES_MEV:
        print(f"  mass {mass:.0f} MeV", flush=True)

        try:
            nb_signal = N_b_massbin_from_cache(mass, nb_cache)
        except Exception as e:
            sys.stderr.write(f"  [warn] N_b_massbin failed for {mass} MeV: {e}\n")
            continue
        if nb_signal <= 0 or not math.isfinite(nb_signal):
            sys.stderr.write(f"  [warn] nb_signal={nb_signal} -- skipping.\n")
            continue

        for eps in EPSILONS:
            try:
                sig_per_region = compute_signal_all_regions(
                    base, mass, eps, nb_signal, PROJ_SIG_CUT, suffix, z0_regions)
            except Exception as e:
                sys.stderr.write(f"  [warn] signal failed eps={eps:.3e}: {e}\n")
                continue

            entry = {"mass_MeV": float(mass), "epsilon": float(eps)}
            valid = True
            for reg_label, _, _ in z0_regions:
                S = sig_per_region[reg_label]
                if not math.isfinite(S) or S < 0:
                    valid = False
                    break
                entry[f"signal_region{reg_label}"] = S
                #math.floor(S)

            if valid:
                points.append(entry)

    points.sort(key=lambda d: (d["mass_MeV"], d["epsilon"]))

    z0_meta = {r: {"lo": lo, "hi": hi} for r, lo, hi in z0_regions}
    hitcat  = suffix_to_hitcat(suffix)
    output = {
        "metadata": {
            "suffix":        suffix,
            "task_id":       task_id,
            "hit_category":  hitcat,
            "Val2":          FIXED_VAL2,
            "proj_sig_cut":  PROJ_SIG_CUT,
            "run_cap":       RUN_CAP,
            "z0_regions":    z0_meta,
            "mass_grid_MeV": MASSES_MEV.tolist(),
            "epsilon_grid":  EPSILONS.tolist(),
            "description": (
                f"Signal yields for two |z0| regions (hit-category specific). "
                f"Hit category: {hitcat}. "
                "Cut order: psum in [1.5,3.0], hit-category, "
                f"vtx_proj_sig < {PROJ_SIG_CUT}, then |z0| band. "
                f"Regions: lo=({z0_regions[0][1]},{z0_regions[0][2]}), "
                f"hi=({z0_regions[1][1]},inf). "
                f"Data: batch files run <= {RUN_CAP}. "
                "signal_regionX = S_yield."
            ),
        },
        "n_points": len(points),
        "data":     points,
    }

    outfile = os.path.join(outdir, f"results_{suffix}_projSig_{PROJ_SIG_CUT:.4f}.json")
    with open(outfile, "w") as f:
        json.dump(output, f, indent=2)

    print(f"[{suffix}] Wrote {len(points)} points -> {outfile}")
    return outfile


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------
def main():
    ap = argparse.ArgumentParser(
        description=(
            "Compute signal yields for task-ids 4,10,6,12 with two hit-category-specific "
            "|z0| regions and hard-coded val2=7 (proj_sig_cut=2.8). One JSON per suffix. "
            f"Data source: batch ROOT files with run <= {RUN_CAP} (Option B, single load)."
        )
    )
    ap.add_argument("--outdir", required=True,
                    help="Directory where JSON output files will be written.")
    ap.add_argument("--base-module", default="decayLength8sel",
                    help="Signal MC module to import (default: decayLength8sel).")
    ap.add_argument("--task-id", type=int, default=None,
                    help="Run a single task-id instead of all four (4,10,6,12).")
    args = ap.parse_args()

    # ------------------------------------------------------------------
    # Discover and stream all files once, accumulating per-mass counts.
    # Peak memory: one chunk (~200k events) at a time.
    # ------------------------------------------------------------------
    print(f"[main] Discovering batch files (run <= {RUN_CAP})...", flush=True)
    root_files = discover_batch_files(run_cap=RUN_CAP)
    if not root_files:
        sys.exit(f"[error] No batch files found under {BATCH_DATA_DIRS} with run <= {RUN_CAP}")
    print(f"[main] Found {len(root_files)} ROOT files. Streaming for N_b counts...", flush=True)

    nb_cache = compute_N_b_per_mass(root_files, MASSES_MEV)
    print(f"[main] N_b cache: { {int(m): round(v,1) for m,v in nb_cache.items()} }", flush=True)

    # ------------------------------------------------------------------
    # Run grid for each requested task-id.
    # ------------------------------------------------------------------
    task_ids = [args.task_id] if args.task_id is not None else TASK_IDS
    for task_id in task_ids:
        if task_id >= len(ALL_SUFFIXES):
            sys.stderr.write(f"[warn] task-id {task_id} out of range, skipping\n")
            continue
        suffix = ALL_SUFFIXES[task_id]
        run_grid(args.base_module, args.outdir, task_id, suffix, nb_cache)


if __name__ == "__main__":
    main()
