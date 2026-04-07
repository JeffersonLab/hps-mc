#!/usr/bin/env python3
# See module docstring below for details.
"""
Compute the fraction of background events that pass a tight selection, per mass bin.

Args (only these three are accepted):
  --II   : integer mass-bin index
  --L    : integer, total number of mass bins
  --Val  : integer, parameter controlling tight selection (e.g., z-threshold)

Behavior:
- Reads a background ROOT file with a 'preselection' tree (same structure as your signal numerator files).
- Applies a mass window around target mass m(II,L) both to numerator and denominator.
- Applies a tight selection (parameterized by Val) only to the numerator.
- Writes the fraction (numerator/denominator) to an output txt file.

Edit BACKGROUND_PATH to point to your background file if necessary.
"""

import argparse
import numpy as np
import uproot
import awkward as ak

BACKGROUND_PATH = "/sdf/group/hps/users/rodwyer1/run/reach_curves/datafiles/pres3/bigpreselectblind.root"
#"/sdf/data/hps/physics2021/preselection/v8/data_1pc_z0_calb_run_by_run/merged_hps_014536_job316.root"
#bigpreselectblind.root"
#"/sdf/group/hps/users/rodwyer1/run/reach_curves/datafiles/pres/bigpreselect.root"  # change if needed
TREE_CANDIDATES = ["preselection", "preselection;1"]

BRANCHES = [
    "psum", "vertex.invM_", "vertex.pos_",
    "ele.track_.n_hits_", "ele.track_.d0_", "ele.track_.phi0_", "ele.track_.z0_", "ele.track_.tan_lambda_",
    "ele.track_.px_", "ele.track_.py_", "ele.track_.pz_", "ele.track_.chi2_", "ele.track_.x_at_ecal_",
    "ele.track_.y_at_ecal_", "ele.track_.z_at_ecal_",
    "pos.track_.n_hits_", "pos.track_.d0_", "pos.track_.phi0_", "pos.track_.z0_", "pos.track_.tan_lambda_",
    "pos.track_.px_", "pos.track_.py_", "pos.track_.pz_", "pos.track_.chi2_", "pos.track_.x_at_ecal_",
    "pos.track_.y_at_ecal_", "pos.track_.z_at_ecal_",
    "vertex.chi2_", "vertex.invMerr_", "vtx_proj_sig", "vtx_proj_x_sig", "vtx_proj_y_sig",
    # [HIT CATEGORY] TTree flag for hit category. Swap "isL1L1" for e.g. "isL2L2", "isL1L2" etc. to change category.
    "isL1L1"
]

VECTOR_BRANCHES = [
    "ele.track_.hit_layers_",
    "pos.track_.hit_layers_",
    #"ele.track_.lambda_kinks_",
    #"pos.track_.lambda_kinks_",
]

OUT_TMPL = "/sdf/group/hps/users/rodwyer1/run/reach_curves/optimization/outputText/bg_eff_output_I{II}_L{L}_V{Val}.txt"

def _open_first_tree(file):
    for name in TREE_CANDIDATES:
        if name in file:
            return file[name]
    for _, obj in file.items():
        try:
            if obj.classname.startswith("TTree"):
                return obj
        except Exception:
            pass
    raise KeyError("No TTree found (expected 'preselection').")

def _extract_z_from_arrays(arrays):
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

def _target_mass_mev(II, L):
    #Altered to turn into mV, hopefully all that is required
    #I dont think we divide for the scripts outside of this script (i.e. the plotting one)
    #This is because the division only affects the mA epsilon/2pi (alpha) measurement. All aspects
    #of the actual getFrac etc don't need it
    #When we do histograms with actual abundance, this will change


    #return (240.0 / float(L)) * float(II)/1.8
    return (240.0 / float(L)) * float(II)

def _mass_window_mask(invM, mass_mev):
    center = mass_mev/1000.0  # GeV
    low    = center - 10.0/1000.0
    high   = center + 10.0/1000.0
    return (invM > low) & (invM < high)

def _tight_selection_mask(events, Val):
    #z = events["vertex.pos_.fZ"]
    elez0 = events.get("ele.track_.z0_")
    posz0 = events.get("pos.track_.z0_")
    proj_sig = events.get("vtx_proj_sig")
    psum = events.get("psum")
    print(proj_sig)
    #zthr = 2.0 * float(Val) * (1/25.0) + 1.5
    zthr=.5*(float(Val)*(1.0/25.0))
    mask = np.isfinite(psum) & ((posz0 > zthr)|(posz0 < -zthr)) & ((elez0 > zthr)|(elez0 < -zthr))
    # [HIT CATEGORY] Use TTree isL1L1 flag directly instead of deriving from hit_layers_.
    # To switch hit category, swap "isL1L1" for e.g. "isL2L2", "isL1L2", etc. (must also update BRANCHES and main2 load above).
    _hitcat = events.get("isL1L1")
    if _hitcat is not None:
        mask &= np.asarray(_hitcat, dtype=bool)
    mask &= (psum>=1.5)&(psum<=3.0)
    mask &= (proj_sig<1.6)
    #(proj_sig<5.0*(float(Val)*(1/25.0)))
    return mask

def main2():
    ap = argparse.ArgumentParser()
    ap.add_argument("--II", type=int, required=True)
    ap.add_argument("--L",  type=int, required=True)
    ap.add_argument("--Val", type=int, required=True)
    args = ap.parse_args()

    mass_mev = _target_mass_mev(args.II, args.L)

    with uproot.open(BACKGROUND_PATH) as f:
        t = _open_first_tree(f)
        arrays = t.arrays(BRANCHES + VECTOR_BRANCHES, library="ak")

    events = {}
    zvals = _extract_z_from_arrays(arrays)
    events["vertex.pos_.fZ"] = zvals
    if "vertex.invM_" not in arrays.fields:
        raise KeyError("vertex.invM_ not found in background file.")
    events["vertex.invM_"] = np.asarray(ak.to_numpy(arrays["vertex.invM_"]))
    events["vtx_proj_sig"] = np.asarray(ak.to_numpy(arrays["vtx_proj_sig"])) 
    events["ele.track_.z0_"] = np.asarray(ak.to_numpy(arrays["ele.track_.z0_"]))
    events["pos.track_.z0_"] = np.asarray(ak.to_numpy(arrays["pos.track_.z0_"]))
    events["psum"] = np.asarray(ak.to_numpy(arrays["psum"]))

    # [HIT CATEGORY] Load isL1L1 directly from TTree branch.
    # To switch hit category, change "isL1L1" here and in BRANCHES above to e.g. "isL2L2", "isL1L2", etc.
    if "isL1L1" in arrays.fields:
        events["isL1L1"] = np.asarray(ak.to_numpy(arrays["isL1L1"]), dtype=bool)

    # ele.track_.hit_layers: does this event have hits on BOTH L0 and L1?
    if "ele.track_.hit_layers" in arrays.fields:
        ele_layers = arrays["ele.track_.hit_layers"]       # ak.Array (jagged)
        ele_has0 = ak.any(ele_layers == 0, axis=-1)
        ele_has1 = ak.any(ele_layers == 1, axis=-1)
        # Ensure no Nones and force boolean dtype
        events["ele.hasL0"]   = np.asarray(ak.fill_none(ele_has0, False), dtype=bool)
        events["ele.hasL1"]   = np.asarray(ak.fill_none(ele_has1, False), dtype=bool)
        events["ele.hasL0L1"] = np.asarray(ak.fill_none(ele_has0 & ele_has1, False), dtype=bool)

    # (optional) positron side, same pattern:
    if "pos.track_.hit_layers" in arrays.fields:
        pos_layers = arrays["pos.track_.hit_layers"]
        pos_has0 = ak.any(pos_layers == 0, axis=-1)
        pos_has1 = ak.any(pos_layers == 1, axis=-1)
        events["pos.hasL0L1"] = np.asarray(ak.fill_none(pos_has0 & pos_has1, False), dtype=bool)

    mask_mass  = _mass_window_mask(events["vertex.invM_"], mass_mev)
    
    denom = int(np.sum(mask_mass))

    mask_tight = _tight_selection_mask(events, args.Val)
    numer = int(np.sum(mask_mass & mask_tight))

    frac = float(numer) / float(denom) if denom > 0 else 0.0

    outfile = OUT_TMPL.format(II=args.II, L=args.L, Val=args.Val)
    with open(outfile, "w") as fo:
        fo.write(f"{frac:.10g}\n")

    print(f"mass(MeV)={mass_mev:.3f}, Val={args.Val}, denom={denom}, numer={numer}, frac={frac:.6g}")
    print(f"Wrote: {outfile}")

if __name__ == "__main__":
    main2()

