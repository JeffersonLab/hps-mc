import os
import sys
import math
import json
import glob
import argparse
import importlib
import uproot
import awkward as ak
import numpy as np
import matplotlib.pyplot as plt


# ---------------------------------------------------------------------------
# Physics constants
# ---------------------------------------------------------------------------
ALPHA_QED = 1.0 / 137.0459991
HBAR_C    = 1.973e-14   # GeV*cm
ALPHA_D   = 0.01


# ---------------------------------------------------------------------------
# Hard-coded val2
# ---------------------------------------------------------------------------
FIXED_VAL2   = 7
PROJ_SIG_CUT = 4.0 * FIXED_VAL2 / 10.0   # 2.8


# Smeared mass resolution parameters for simps
# provided by M. Gignac on July 9 2026
SIMP_MASS_RES_PARAMS = {
        "isL1L1": (1.9717, -0.0114, 1.60e-4),  # L1 hit
        "isL2L2": (2.0161, -0.0068, 1.18e-4),  # L2 hit (no L1)
        "isL1L2": (2.3306, -0.0135, 1.58e-4),  # L1+L2 (cross)
}

def simp_mass_res(mass_mev, category):
    """SIMP vertex-mass resolution (MeV) vs. invariant mass (MeV)
    category: one of "isL1L1", "isL2L2", "isL3L3", "isL1L2", "isL2L3"
    always used smeared values
    """
    try:
        p0, p1, p2 = SIMP_MASS_RES_PARAMS[category]
    except KeyError:
        raise ValueError(
            f"Unknown category {category!r}; expected one of {list(SIMP_MASS_RES_PARAMS)}"
        )
    mass_mev = np.asarray(mass_mev, dtype=float)
    return p0 + p1 * mass_mev + p2 * mass_mev**2


# Radiative fraction [Eq. 25]
def f_rad(x):
    x = x / 1000    # MeV -> GeV
    return -0.1461 + 7.8814*x - 121.2812*x**2 + 890.3339*x**3 - 3111.8721*x**4 + 4150.8765*x**5


# Prompt acceptance factor \tilde{N}_rad [Eq. 47]
def Ntilde_rad(x):
    x = x / 1000    # MeV -> GeV
    return 1.821 - 1.163E2*x + 2.976E3*x**2 - 4.089E4*x**3 + 3.456E5 *x**4 - 1.860E6*x**5 + 6.204E6*x**6 - 1.166E7 *x**7 + 9.429E6*x**8

# This calculates \delta N_{bkg} / \delta m for each mass in MASSES_MEV
def compute_N_b_per_mass(root_files, masses_mev, width_mev=2.0, SIGNAL_DATA_SCALE=1.0):
    """
    Returns a dict  {mass_mev: N_b}  equivalent to calling the original
    N_b_massbin for every mass, but using O(chunk) memory instead of
    O(full dataset).

    Applies the same cuts as the original N_b_massbin:
        psum > 2.8  AND  invM in (center - width/2, center + width/2)
    with SIGNAL_DATA_SCALE applied.
    """
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

# Calculates the dark pion mass, dark vector mass, and dark pion decay constant based on A' mass
def dark_masses(mass_mev):
    m_pi_D = mass_mev / 3.0
    m_V_D  = 1.8 * mass_mev / 3.0
    f_pi_D = (mass_mev / 3.0) / (4.0 * math.pi)
    return m_pi_D, m_V_D, f_pi_D

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

# Branches to load from numerator files (based on Rory's BDT backbone)
BRANCHES = [
    "psum","true_vd.pz_","true_vd.px_","true_vd.py_","vertex.pos_","vertex.invM_","vertex.invMerr_",
    "ele.track_.n_hits_", "ele.track_.d0_", "ele.track_.phi0_", "ele.track_.z0_", "ele.track_.tan_lambda_",
    "ele.track_.px_", "ele.track_.py_", "ele.track_.pz_", "ele.track_.chi2_", "ele.track_.x_at_ecal_",
    "ele.track_.y_at_ecal_", "ele.track_.z_at_ecal_",
    "pos.track_.n_hits_", "pos.track_.d0_", "pos.track_.phi0_", "pos.track_.z0_", "pos.track_.tan_lambda_",
    "pos.track_.px_", "pos.track_.py_", "pos.track_.pz_", "pos.track_.chi2_", "pos.track_.x_at_ecal_",
    "pos.track_.y_at_ecal_", "pos.track_.z_at_ecal_",
    "vertex.chi2_", "vtx_proj_sig", "vtx_proj_x_sig", "vtx_proj_y_sig",
    "true_vd.vtx_z_",
    "ele_L1_iso_significance","pos_L1_iso_significance",
    # [HIT CATEGORY] All hit category TTree flags loaded here so they are available in the events cache.
    # The active category used in tight_selection is controlled separately below.
    "isL1L1", "isL2L2", "isL3L3", "isL1L2", "isL2L3"
    #"max_y0err", "pos_y0err", "ele_y0err", "min_y0_vtx", "min_y0_vtx_proj", "vtx_y_at_zero", "delta_y0_vtx_proj", "ele.cluster_.time_", "pos.cluster_.time_"
]

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

def _events_cache(mass_vD, signal_dir):
    """Load once per mass: all requested branches into numpy arrays; keep in memory for fast selections."""
    with uproot.open(signal_dir+'/simp'+str(int(mass_vD))+'v2.root') as f:
        t = f["preselection"]
        arrays = t.arrays(BRANCHES, library="ak")
    events = {}
    # z coordinate
    zvals = _extract_z_from_arrays(arrays)
    zvals = zvals[np.isfinite(zvals)]
    events["vertex.pos_.fZ"] = zvals


    events["ele.track_.z0_"] = np.asarray(ak.to_numpy(arrays["ele.track_.z0_"]))
    events["pos.track_.z0_"] = np.asarray(ak.to_numpy(arrays["pos.track_.z0_"]))

    # --- Derived, per-event reductions from vector-like branches ---
    # Convert jagged vectors into 1D numpy arrays that your tight_selection can use.

    # [HIT CATEGORY] Load all hit category flags from TTree. All are stored so write_final_yields
    # can apply whichever it needs without reloading. To add a new category, add its branch name
    # to BRANCHES above and include it in this loop.
    for _cat in ["isL1L1", "isL2L2", "isL3L3", "isL1L2", "isL2L3"]:
        if _cat in arrays.fields:
            events[_cat] = np.asarray(ak.to_numpy(arrays[_cat]), dtype=bool)

    # ele.track_.hit_layers: does this event have hits on BOTH L0 and L1?
    if "ele.track_.hit_layers_" in arrays.fields:
        ele_layers = arrays["ele.track_.hit_layers_"]       # ak.Array (jagged)
        ele_has0 = ak.any(ele_layers == 0, axis=-1)
        ele_has1 = ak.any(ele_layers == 1, axis=-1)
        # Ensure no Nones and force boolean dtype
        events["ele.hasL0"]   = np.asarray(ak.fill_none(ele_has0, False), dtype=bool)
        events["ele.hasL1"]   = np.asarray(ak.fill_none(ele_has1, False), dtype=bool)
        events["ele.hasL0L1"] = np.asarray(ak.fill_none(ele_has0 & ele_has1, False), dtype=bool)
        #events["ele.track_.hit_layers_"] = arrays["ele.track_.hit_layers_"]

    # (optional) positron side, same pattern:
    if "pos.track_.hit_layers_" in arrays.fields:
        pos_layers = arrays["pos.track_.hit_layers_"]
        pos_has0 = ak.any(pos_layers == 0, axis=-1)
        pos_has1 = ak.any(pos_layers == 1, axis=-1)
        events["pos.hasL0L1"] = np.asarray(ak.fill_none(pos_has0 & pos_has1, False), dtype=bool)
        #events["pos.track_.hit_layers_"] = arrays["pos.track_.hit_layers_"]

    # ele.track_.lambda_kinks_: fixed-length (e.g. 14) vector per event -> reduce to a scalar
    if "ele.track_.lambda_kinks_" in arrays.fields:
        lam_e = arrays["ele.track_.lambda_kinks_"]
        # Example reduction: max absolute kink per event
        events["ele.lambda_kinks_maxabs"] = ak.to_numpy(ak.max(ak.abs(lam_e), axis=-1))

    # pos.track_.lambda_kinks_: same pattern if you need it
    if "pos.track_.lambda_kinks_" in arrays.fields:
        lam_p = arrays["pos.track_.lambda_kinks_"]
        events["pos.lambda_kinks_maxabs"] = ak.to_numpy(ak.max(ak.abs(lam_p), axis=-1))

    # Other branches
    for key in BRANCHES:
        if key not in arrays.fields:
            continue
        a = arrays[key]
        if hasattr(a, "fields") and len(ak.fields(a)) > 0:
            for sub in ak.fields(a):
                try:
                    subarr = ak.to_numpy(a[sub])
                    if np.issubdtype(subarr.dtype, np.number):
                        events[f"{key}.{sub}"] = np.asarray(subarr)
                except Exception:
                    continue
        else:
            try:
                arr = ak.to_numpy(a)
                if np.issubdtype(arr.dtype, np.number):
                    events[key] = np.asarray(arr)
            except Exception:
                pass
    # Consistent length
    lengths = [len(v) for v in events.values() if isinstance(v, np.ndarray)]
    if not lengths:
        raise RuntimeError("No usable branches were loaded from numerator file.")
    N = min(lengths)
    for k in list(events.keys()):
        v = events[k]
        if isinstance(v, np.ndarray) and len(v) != N:
            events[k] = v[:N]
    return events

    """Load once per mass: all requested branches into numpy arrays; keep in memory for fast selections."""
    with uproot.open(signal_dir+'/simp'+str(int(mass_vD))+'v2.root') as f:
        t = f["preselection"]
        arrays = t.arrays(BRANCHES, library="ak")
    events = {}

