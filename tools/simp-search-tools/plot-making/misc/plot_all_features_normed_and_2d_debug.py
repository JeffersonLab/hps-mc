#!/usr/bin/env python3
import os
import sys
import argparse
import itertools
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path

# ---------------------- Robust Position Extractors ----------------------
def _first_present_field(arrays, candidates):
    for name in candidates:
        if name in getattr(arrays, "fields", []):
            try:
                import awkward as ak
                arr = ak.to_numpy(arrays[name])
                return np.asarray(arr)
            except Exception:
                continue
    return None

def _extract_pos_components(arrays):
    # Candidates for each axis in descending preference
    candX = [
        "vertex.pos_.fX","vertex.pos_.X","vertex.pos_.x","vtx_x","vertex.x","pos_x","vtxPosX","vtxX","vtx.pos.x"
    ]
    candY = [
        "vertex.pos_.fY","vertex.pos_.Y","vertex.pos_.y","vtx_y","vertex.y","pos_y","vtxPosY","vtxY","vtx.pos.y"
    ]
    candZ = [
        "vertex.pos_.fZ","vertex.pos_.Z","vertex.pos_.z","vtx_z","vertex.z","pos_z","vtxPosZ","vtxZ","vtx.pos.z"
    ]
    out = {}
    x = _first_present_field(arrays, candX)
    y = _first_present_field(arrays, candY)
    z = _first_present_field(arrays, candZ)
    if x is not None: out["vertex.pos_.fX"] = x
    if y is not None: out["vertex.pos_.fY"] = y
    if z is not None: out["vertex.pos_.fZ"] = z
    return out

# ---------------------- Alias Helpers ----------------------
def _first_numeric_alias(arrays, names):
    import awkward as ak
    for nm in names:
        if nm in getattr(arrays, "fields", []):
            try:
                arr = ak.to_numpy(arrays[nm])
                arr = np.asarray(arr)
                if arr.ndim == 1 and np.issubdtype(arr.dtype, np.number):
                    return arr
            except Exception:
                pass
    return None

# ---------------------- Imports & Base Modules ----------------------
# Background helper module (expected in user's repo)
try:
    import bk_eff_selection as bg
except Exception as e:
    sys.stderr.write("[warn] Could not import bk_eff_selection as bg: {}.\n".format(e))
    bg = None

# Signal base module (decayLength5sel by default)
def import_signal_base(module_name="decayLength7sel"):
    try:
        base = __import__(module_name)
        return base
    except SystemExit:
        sys.stderr.write("[fatal] Importing '{}' triggered SystemExit (argparse at top-level?). Guard CLI with if __name__ == '__main__'.\n".format(module_name))
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
                sys.stderr.write("[fatal] Executing '{}' still triggered argparse at import. Please guard the CLI in that file.\n".format(candidate))
                raise
        else:
            raise

# ---------------------- I/O Utilities ----------------------
def _ensure_dir(d):
    if d and not os.path.isdir(d):
        os.makedirs(d, exist_ok=True)

# ---------------------- Data Loading ----------------------
def load_signal_events(base, mass_mev, Val):
    """Return dict of feature->np.array for signal after tight selection."""
    mkey   = base._mass_key(mass_mev)
    events = base._events_cache(mkey)
    mask   = base.tight_selection(events, Val)
    out = {}
    for k, v in events.items():
        if isinstance(v, np.ndarray) and v.ndim == 1 and len(v) == len(mask):
            vv = v[mask]
            if np.issubdtype(vv.dtype, np.number):
                vv = vv[np.isfinite(vv)]
                out[k] = vv
    if "vertex.pos_.fZ" not in out and "vertex.pos_.z" in out:
        out["vertex.pos_.fZ"] = out["vertex.pos_.z"]
    return out

from pathlib import Path  # make sure this import is at the top of the file

def _map_desired_to_available(avail_names, desired_list):
    """
    Map canonical desired branch names (e.g. 'vertex.invM_') to actual ROOT
    branch names present in the file (e.g. 'vertex./vertex.invM_').
    Strategy:
      1) exact match
      2) suffix match on '/{desired}' or '.{desired}' or '{desired}'
      3) last-token match after '/' then after '.'
    """
    avail = list(avail_names)
    mapping = {}

    def last_token(s):
        part = s.split('/')[-1]
        return part.split('.')[-1]

    for d in desired_list:
        if d in avail:
            mapping[d] = d
            continue
        # suffix match first
        cand = [a for a in avail if a.endswith("/"+d) or a.endswith("."+d) or a.endswith(d)]
        if not cand:
            dtok = last_token(d)
            cand = [a for a in avail if last_token(a) == dtok]
        if cand:
            mapping[d] = sorted(cand, key=len)[0]  # shortest path
    return mapping

def _get_array_by_canonical(arrays, canonical, name_map):
    """Fetch arrays[name_map[canonical]] if mapped, else try suffix match in arrays.fields."""
    real = name_map.get(canonical)
    if real is None and canonical in getattr(arrays, "fields", []):
        real = canonical
    if real is None:
        for f in getattr(arrays, "fields", []):
            if f.endswith("/"+canonical) or f.endswith("."+canonical) or f.endswith(canonical):
                real = f
                break
    if real is None:
        return None
    import awkward as ak
    try:
        return np.asarray(ak.to_numpy(arrays[real]))
    except Exception:
        return None

def load_background_events(mass_mev, Val, desired_keys, debug=False):
    """Return dict feature->np.array for background after tight selection, robust to 'vertex./...' names."""
    if bg is None:
        return {}

    import uproot, awkward as ak, glob

    # Resolve BACKGROUND_PATH -> list of files (file, dir, glob, list are accepted)
    def _resolve_bg_paths(bg_module):
        paths = []
        cand = getattr(bg_module, "BACKGROUND_PATH", None)
        def add(c):
            if c is None: return
            if isinstance(c, (list, tuple)):
                for cc in c: add(cc); return
            c = str(c)
            p = Path(c)
            if any(ch in c for ch in "*?["):
                paths.extend(sorted(glob.glob(c)))
            elif p.is_dir():
                paths.extend(sorted(str(pp) for pp in p.rglob("*.root")))
            elif p.exists():
                paths.append(str(p))
        add(cand)
        return paths

    files = _resolve_bg_paths(bg)
    if not files:
        if debug:
            print("[debug] bk_eff_selection from:", getattr(bg, "__file__", "<unknown>"))
            print("[debug] BACKGROUND_PATH:", getattr(bg, "BACKGROUND_PATH", None))
            print("[debug] resolved background files: 0")
        return {}

    with uproot.open(files[0]) as f:
        if hasattr(bg, "_open_first_tree"):
            t = bg._open_first_tree(f)
        else:
            # choose first TTree-like key
            candidates = [k for k in f.keys() if ";" in k]
            tname = candidates[0] if candidates else None
            if tname is None:
                if debug:
                    print("[debug] no TTree candidates in:", files[0])
                return {}
            t = f[tname]

        try:
            avail = list(t.keys())  # this returns the full branch paths, like 'vertex./vertex.invM_'
        except Exception:
            avail = []

        # Build the request: desired keys + common aux (mass, psum, vtx_proj_sig, vertex.pos_* aliases)
        base_needed = {
            "vertex.invM_", "psum", "vtx_proj_sig", "vertex.pos_",
            "vertex.pos_.fX","vertex.pos_.fY","vertex.pos_.fZ",
            "vertex.pos_.X","vertex.pos_.Y","vertex.pos_.Z",
            "vertex.pos_.x","vertex.pos_.y","vertex.pos_.z",
        }
        req_all = set(desired_keys) | base_needed | set(getattr(bg, "BRANCHES", []))

        # Map canonical requests to available names like 'vertex./vertex.invM_'
        name_map = _map_desired_to_available(avail, sorted(req_all))
        branch_req = sorted(set(name_map.values()))

        arrays = t.arrays(branch_req, library="ak")

    # ----- Build events dict using canonical names -----
    events_bg = {}

    # Position (X/Y/Z): try via your project-specific extractor first, then robust aliases
    if hasattr(bg, "_extract_z_from_arrays"):
        try:
            zvals = bg._extract_z_from_arrays(arrays)
            events_bg["vertex.pos_.fZ"] = np.asarray(zvals, dtype=float)
        except Exception:
            pass

    # Robust XYZ using the actual names we loaded
    def _first_numeric_alias(arrays, names):
        import awkward as ak
        for nm in names:
            if nm in getattr(arrays, "fields", []):
                try:
                    arr = np.asarray(ak.to_numpy(arrays[nm]))
                    if arr.ndim == 1 and np.issubdtype(arr.dtype, np.number):
                        return arr
                except Exception:
                    pass
        return None

    # Z fallback through mapping
    if "vertex.pos_.fZ" not in events_bg:
        z = _get_array_by_canonical(arrays, "vertex.pos_.fZ", name_map)
        if z is None:
            for alias in ["vertex.pos_.Z","vertex.pos_.z"]:
                z = _get_array_by_canonical(arrays, alias, name_map)
                if z is not None: break
        if z is not None:
            events_bg["vertex.pos_.fZ"] = z

    # Bring over any requested desired keys using the mapping
    for k in desired_keys:
        arr = _get_array_by_canonical(arrays, k, name_map)
        if arr is not None and np.issubdtype(arr.dtype, np.number):
            events_bg[k] = arr

    # psum and vtx_proj_sig (with aliases, then mapping)
    if "psum" not in events_bg:
        _tmp = _first_numeric_alias(arrays, ["psum","vertex.psum","p_sum","pSum","sumP"])
        if _tmp is None:
            _tmp = _get_array_by_canonical(arrays, "psum", name_map)
        if _tmp is not None:
            events_bg["psum"] = _tmp

    if "vtx_proj_sig" not in events_bg:
        _tmp = _first_numeric_alias(arrays, ["vtx_proj_sig","vertex.projSig","vtxProjSig","vtx_sigma_proj"])
        if _tmp is None:
            _tmp = _get_array_by_canonical(arrays, "vtx_proj_sig", name_map)
        if _tmp is not None:
            events_bg["vtx_proj_sig"] = _tmp

    # Mass window (use canonical or alias via mapping). If not found, skip mass windowing.
    invM_aliases = ["vertex.invM_", "invM_", "invMass", "m_inv", "mInv", "vtxInvM", "vtx.invM"]
    invM = None
    for nm in invM_aliases:
        invM = _get_array_by_canonical(arrays, nm, name_map)
        if invM is not None:
            break
    if invM is None:
        mwin = None
        if debug:
            print("[debug] invariant mass not found via mapping; skipping mass window.")
    else:
        mwin = bg._mass_window_mask(invM, mass_mev) if hasattr(bg, "_mass_window_mask") else np.ones_like(invM, dtype=bool)

    # Tight selection mask
    tight_mask = None
    if hasattr(bg, "_tight_selection_mask"):
        try:
            tight_mask = bg._tight_selection_mask(events_bg, Val)
        except Exception as e:
            if debug:
                print("[debug] bg._tight_selection_mask failed:", e)
    if tight_mask is None:
        z = events_bg.get("vertex.pos_.fZ")
        tight_mask = np.isfinite(z) if z is not None else np.ones_like(next(iter(events_bg.values())), dtype=bool)

    mask = tight_mask if mwin is None else (tight_mask & mwin)
    mask = mask.astype(bool)

    # Finalize numeric 1D arrays
    out = {}
    for k, v in events_bg.items():
        if isinstance(v, np.ndarray) and v.ndim == 1 and len(v) == len(mask) and np.issubdtype(v.dtype, np.number):
            vv = v[mask]
            vv = vv[np.isfinite(vv)]
            out[k] = vv
    return out


# ---------------------- Feature Selection ----------------------
def pick_1d_numeric_features(sig_dict, bg_dict):
    """Return sorted list of feature names present with >=2 entries in both dicts."""
    common = []
    for k in sig_dict.keys():
        if k in bg_dict and sig_dict[k].size >= 2 and bg_dict[k].size >= 2:
            common.append(k)
    prioritise = [
        "vertex.pos_.fZ",
        "ele.track_.z0_",
        "pos.track_.z0_",
        "ele.track_.d0_",
        "pos.track_.d0_",
        "vertex.invM_",
        "psum",
        "vtx_proj_sig",
        "ele.track_.chi2_",
        "pos.track_.chi2_",
        "ele.track_.px_",
        "ele.track_.py_",
        "ele.track_.pz_",
        "pos.track_.px_",
        "pos.track_.py_",
        "pos.track_.pz_",
    ]
    seen = set()
    ordered = []
    for p in prioritise:
        if p in common and p not in seen:
            ordered.append(p)
            seen.add(p)
    for k in sorted(common):
        if k not in seen:
            ordered.append(k)
            seen.add(k)
    return ordered

# ---------------------- Plotting ----------------------
def sanitize(name: str) -> str:
    bad = [" ", "/", "\\", "(", ")", "[", "]", "{", "}", ":", ";", ",", "|", "<", ">", "?", "*", "'", '"', "."]
    out = name
    for b in bad:
        out = out.replace(b, "_")
    while "__" in out:
        out = out.replace("__", "_")
    return out.strip("_")

# ---------------------- Background Path Resolver ----------------------
def _resolve_bg_paths(bg_module):
    """Return a list of ROOT file paths for background.
    Accepts:
      - Exact file path (string)
      - Directory containing ROOT files
      - Glob pattern(s)
      - List/tuple of any of the above
    """
    import glob
    paths = []
    cand = getattr(bg_module, "BACKGROUND_PATH", None)
    if cand is None:
        return paths
    def add_candidate(c):
        if c is None:
            return
        if isinstance(c, (list, tuple)):
            for cc in c:
                add_candidate(cc)
            return
        c = str(c)
        p = Path(c)
        if any(ch in c for ch in ["*", "?", "["]):
            paths.extend(sorted(glob.glob(c)))
        elif p.is_dir():
            paths.extend(sorted(str(pp) for pp in p.rglob("*.root")))
        elif p.exists():
            paths.append(str(p))
        else:
            # allow silent miss; caller may print debug
            pass
    add_candidate(cand)
    return paths

def _debug_bg_info(bg_module, files, t=None):
    try:
        print("[debug] bk_eff_selection imported from:", getattr(bg_module, "__file__", "<unknown>"))
        print("[debug] BACKGROUND_PATH:", getattr(bg_module, "BACKGROUND_PATH", None))
        print("[debug] resolved background files:", len(files))
        for f in files[:5]:
            print("   -", f)
        if t is not None:
            try:
                print("[debug] tree keys count:", len(t.keys()))
                print(t.keys())
            except Exception:
                pass
    except Exception:
        pass

def plot_1d_overlaid(sig_arr, bg_arr, feat, outdir, mass_mev, Val, bins):
    fig, ax = plt.subplots(figsize=(7.8, 4.8))
    both = np.concatenate([sig_arr, bg_arr])
    q1, q99 = np.quantile(both, [0.005, 0.995])
    pad = 0.05 * (q99 - q1 + 1e-12)
    rrange = (q1 - pad, q99 + pad)
    ax.hist(sig_arr, bins=bins, range=rrange, density=True, histtype="step", linewidth=1.8, label="signal")
    ax.hist(bg_arr,  bins=bins, range=rrange, density=True, histtype="step", linewidth=1.8, label="background")
    ax.set_title(f"{feat}\nunit-normalized; mass={mass_mev:g} MeV, Val={Val}")
    ax.set_xlabel(feat)
    ax.set_ylabel("density")
    ax.grid(True, alpha=0.30)
    ax.legend()
    f = os.path.join(outdir, f"oned_{sanitize(feat)}_{int(round(mass_mev))}MeV_V{Val}.png")
    fig.tight_layout(); fig.savefig(f, dpi=160, bbox_inches="tight"); plt.close(fig)
    return f

def plot_2d_single(data_x, data_y, feat_x, feat_y, label, outdir, mass_mev, Val, bins2d):
    fig, ax = plt.subplots(figsize=(6.6, 5.8))
    def rrange(a):
        q1, q99 = np.quantile(a, [0.01, 0.99])
        pad = 0.05 * (q99 - q1 + 1e-12)
        return (q1 - pad, q99 + pad)
    rx = rrange(data_x)
    ry = rrange(data_y)
    h = ax.hist2d(data_x, data_y, bins=bins2d, range=[rx, ry])
    cb = fig.colorbar(h[3], ax=ax)
    cb.set_label("counts")
    ax.set_title(f"{label} 2D: {feat_x} vs {feat_y}\nmass={mass_mev:g} MeV, Val={Val}")
    ax.set_xlabel(feat_x); ax.set_ylabel(feat_y)
    ax.grid(True, alpha=0.15)
    f = os.path.join(outdir, f"twoD_{label}_{sanitize(feat_x)}__{sanitize(feat_y)}_{int(round(mass_mev))}MeV_V{Val}.png")
    fig.tight_layout(); fig.savefig(f, dpi=160, bbox_inches="tight"); plt.close(fig)
    return f

# ---------------------- Main ----------------------
def main():
    ap = argparse.ArgumentParser(description="Plot overlaid unit-normalized 1D and separate 2D histograms for signal/background.")
    ap.add_argument("--mass", type=float, required=True, help="Mass in MeV used to choose files in signal module.")
    ap.add_argument("--epsilon", type=float, default=None, help="(Currently unused) Optional epsilon for future per-event weighting.")
    ap.add_argument("--Val", type=int, default=25, help="Tight selection parameter used in selections. Default 25.")
    ap.add_argument("--outdir", type=str, default="plots_all_features", help="Output directory for plots.")
    ap.add_argument("--bins", type=int, default=80, help="Bins for 1D plots.")
    ap.add_argument("--bins2d", type=int, default=80, help="Bins per axis for 2D plots.")
    ap.add_argument("--features", type=str, nargs="*", default=None, help="Optional subset of feature names to include (must match keys).")
    ap.add_argument("--max-pairs", type=int, default=None, help="Optional cap on number of 2D feature pairs (per sample).")
    ap.add_argument("--base-module", type=str, default="decayLength5sel", help="Signal base module name.")
    ap.add_argument("--debug", action="store_true", help="Print background resolution and tree info.")
    args = ap.parse_args()

    _ensure_dir(args.outdir)

    base = import_signal_base(args.base_module)

    sig = load_signal_events(base, args.mass, args.Val)
    if not sig:
        sys.stderr.write("[error] No signal features loaded after selection.\n"); sys.exit(2)

    all_sig_feats = sorted([k for k, v in sig.items() if isinstance(v, np.ndarray) and v.ndim == 1 and v.size >= 2])
    if "vertex.pos_.fZ" not in all_sig_feats and "vertex.pos_.z" in all_sig_feats:
        all_sig_feats.insert(0, "vertex.pos_.fZ")

    bg_dict = load_background_events(args.mass, args.Val, desired_keys=set(all_sig_feats), debug=args.debug)

    if args.features:
        selected = [f for f in args.features if f in sig and (not bg_dict or f in bg_dict)]
        if not selected:
            sys.stderr.write("[warn] --features list produced no usable features; falling back to common set.\n")
            selected = None
    else:
        selected = None

    common_feats = pick_1d_numeric_features(sig, bg_dict) if bg_dict else sorted(list(sig.keys()))
    features_1d = selected if selected else common_feats

    # 1D overlaid plots
    out_1d = []
    if bg_dict:
        for feat in features_1d:
            try:
                fpath = plot_1d_overlaid(sig[feat], bg_dict[feat], feat, args.outdir, args.mass, args.Val, args.bins)
                out_1d.append(fpath)
            except Exception as e:
                sys.stderr.write(f"[warn] 1D overlay failed for {feat}: {e}\n")
    else:
        for feat in features_1d:
            try:
                fig, ax = plt.subplots(figsize=(7.8, 4.8))
                arr = sig[feat]
                q1, q99 = np.quantile(arr, [0.005, 0.995])
                pad = 0.05 * (q99 - q1 + 1e-12)
                rrange = (q1 - pad, q99 + pad)
                ax.hist(arr, bins=args.bins, range=rrange, density=True, histtype="step", linewidth=1.8, label="signal")
                ax.set_title(f"{feat}\nunit-normalized; mass={args.mass:g} MeV, Val={args.Val}")
                ax.set_xlabel(feat); ax.set_ylabel("density"); ax.grid(True, alpha=0.30); ax.legend()
                f = os.path.join(args.outdir, f"oned_signal_{sanitize(feat)}_{int(round(args.mass))}MeV_V{args.Val}.png")
                fig.tight_layout(); fig.savefig(f, dpi=160, bbox_inches="tight"); plt.close(fig)
                out_1d.append(f)
            except Exception as e:
                sys.stderr.write(f"[warn] 1D signal-only failed for {feat}: {e}\n")

    # 2D plots (separate)
    feats_for_2d = features_1d
    pairs = list(itertools.combinations(feats_for_2d, 2))
    if args.max_pairs is not None:
        pairs = pairs[: int(args.max_pairs)]

    out_2d_sig = []
    out_2d_bg = []

    # Signal 2D
    for fx, fy in pairs:
        try:
            x = sig[fx]; y = sig[fy]
            if x.size >= 10 and y.size >= 10:
                f = plot_2d_single(x, y, fx, fy, "signal", args.outdir, args.mass, args.Val, args.bins2d)
                out_2d_sig.append(f)
        except Exception as e:
            sys.stderr.write(f"[warn] 2D signal failed for {fx} vs {fy}: {e}\n")

    # Background 2D
    if bg_dict:
        for fx, fy in pairs:
            try:
                x = bg_dict[fx]; y = bg_dict[fy]
                if x.size >= 10 and y.size >= 10:
                    f = plot_2d_single(x, y, fx, fy, "background", args.outdir, args.mass, args.Val, args.bins2d)
                    out_2d_bg.append(f)
            except Exception as e:
                sys.stderr.write(f"[warn] 2D background failed for {fx} vs {fy}: {e}\n")

    print("[done] 1D plots written:", len(out_1d))
    print("[done] 2D signal plots written:", len(out_2d_sig))
    print("[done] 2D background plots written:", len(out_2d_bg))
    if out_1d:
        print("examples:")
        for p in out_1d[: min(5, len(out_1d))]:
            print("  ", p)

if __name__ == "__main__":
    main()

