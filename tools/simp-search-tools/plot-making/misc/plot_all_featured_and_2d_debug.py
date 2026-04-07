#!/usr/bin/env python3
import os
import sys
import argparse
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
import uproot, awkward as ak  # for data reading
import math

# Attempt to import background efficiency module
try:
    import bk_eff_selection as bg
except Exception as e:
    sys.stderr.write(f"[warn] Could not import bk_eff_selection as bg: {e}.\n")
    bg = None

# Constants and global settings
N_B_TOTAL_RECO = 3.2e9  # total number of reconstructed background events (baseline)

# Define Zbi significance calculation (from combine_zbi.py)
try:
    from scipy.special import betainc, erfinv
except ImportError:
    betainc = erfinv = None

def zbi_significance(S: float, B: float) -> float:
    """Compute the Zbi significance for given signal (S) and background (B) yields."""
    if B <= 0:
        # No background events: significance is infinite if S>0, or 0 if S=0
        return float('inf') if S > 0 else 0.0
    if betainc is None or erfinv is None:
        # Fallback to approximate significance if SciPy not available
        return S / math.sqrt(B) if B > 0 else 0.0
    # Compute the one-sided tail probability using the beta incomplete function
    p = betainc(S + B, 1.0 + B, 0.5)
    # Convert tail probability to significance
    z = math.sqrt(2.0) * erfinv(1.0 - 2.0 * p)
    # Cap extremely large significances at 9.0 for display purposes
    if p < 1e-16:
        z = 9.0
    return float(z)

def import_signal_base(module_name: str = "decayLength5sel"):
    """Dynamically import the signal base module (e.g., decayLength5sel)."""
    try:
        base = __import__(module_name)
        return base
    except SystemExit:
        sys.stderr.write(f"[fatal] Importing '{module_name}' triggered SystemExit (argparse at top-level?). "
                         f"Please guard CLI in that module with if __name__ == '__main__'.\n")
        raise
    except Exception:
        # Fallback: try loading from a file in the current directory
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
                sys.stderr.write(f"[fatal] Executing '{candidate}' still triggered argparse at import.\n")
                raise
        else:
            raise

def _ensure_dir(directory: str):
    """Create the directory if it does not exist."""
    if directory and not os.path.isdir(directory):
        os.makedirs(directory, exist_ok=True)

def load_signal_events(base_module, mass_mev: float, Val: int):
    """Load signal events after tight selection, returning a dict of feature->np.array."""
    mkey = base_module._mass_key(mass_mev)  # find closest available mass key
    events = base_module._events_cache(mkey)  # load cached events (numerator file)
    mask = base_module.tight_selection(events, Val)  # apply tight selection mask
    out = {}
    # Filter each numeric 1D branch with the selection mask
    for k, v in events.items():
        if isinstance(v, np.ndarray) and v.ndim == 1 and len(v) == len(mask):
            vv = v[mask]
            if np.issubdtype(vv.dtype, np.number):
                vv = vv[np.isfinite(vv)]  # remove any non-finite values
                out[k] = vv
    # Ensure 'vertex.pos_.fZ' is present (use alias if needed)
    if "vertex.pos_.fZ" not in out and "vertex.pos_.z" in out:
        out["vertex.pos_.fZ"] = out["vertex.pos_.z"]
    return out

def _map_desired_to_available(avail_names, desired_list):
    """
    Map canonical desired branch names (e.g. 'vertex.invM_') to actual branch names in file.
    Strategy:
      1) exact match
      2) suffix match on '/{desired}' or '.{desired}' or end match
      3) match by last token after '/' or '.'
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
        # Try suffix matches
        cand = [a for a in avail if a.endswith("/" + d) or a.endswith("." + d) or a.endswith(d)]
        if not cand:
            dtok = last_token(d)
            cand = [a for a in avail if last_token(a) == dtok]
        if cand:
            # choose the shortest path match (most likely direct branch name)
            mapping[d] = sorted(cand, key=len)[0]
    return mapping

def _get_array_by_canonical(arrays, canonical, name_map):
    """Fetch array corresponding to canonical branch name from Awkward arrays using the mapping."""
    real = name_map.get(canonical)
    if real is None and canonical in getattr(arrays, "fields", []):
        real = canonical
    if real is None:
        # Try suffix matches in available fields
        for f in getattr(arrays, "fields", []):
            if f.endswith("/" + canonical) or f.endswith("." + canonical) or f.endswith(canonical):
                real = f
                break
    if real is None:
        return None
    try:
        return np.asarray(ak.to_numpy(arrays[real]))
    except Exception:
        return None

def load_background_events(mass_mev: float, Val: int, desired_keys=None, debug=False):
    """
    Load background events after tight selection (within mass window) from the background ROOT file.
    Returns a dict of feature->np.array for selected background events.
    """
    if bg is None:
        return {}
    # Resolve background file path(s)
    paths = []
    cand = getattr(bg, "BACKGROUND_PATH", None)
    if cand is None:
        return {}
    # Helper to add candidate paths (file, dir, pattern, or list of such)
    def add_candidate(c):
        if c is None:
            return
        if isinstance(c, (list, tuple)):
            for cc in c:
                add_candidate(cc)
            return
        c_str = str(c)
        p = Path(c_str)
        if any(ch in c_str for ch in ["*", "?", "["]):
            paths.extend(sorted(Path(x) for x in Path().glob(c_str)))
        elif p.is_dir():
            paths.extend(sorted(p.rglob("*.root")))
        elif p.exists():
            paths.append(p)
        else:
            # if path not found, ignore silently (debug can print)
            pass
    add_candidate(cand)
    paths = [str(pp) for pp in paths]
    if not paths:
        if debug:
            sys.stderr.write(f"[debug] BACKGROUND_PATH: {cand}, resolved to 0 files.\n")
        return {}
    # Open the first background file (expected to contain the tree)
    file_path = paths[0]
    with uproot.open(file_path) as f:
        # If module provides a helper to get the tree, use it; otherwise pick the first TTree
        if hasattr(bg, "_open_first_tree"):
            tree = bg._open_first_tree(f)
        else:
            # Find first TTree key
            tree_keys = [k for k in f.keys() if ";" in k]
            tname = tree_keys[0] if tree_keys else None
            if tname is None:
                if debug:
                    sys.stderr.write(f"[debug] no TTree found in: {file_path}\n")
                return {}
            tree = f[tname]
        # Determine available branch keys and map requested ones
        try:
            avail_keys = list(tree.keys())
        except Exception:
            avail_keys = []
        base_needed = {
            "vertex.invM_", "psum", "vtx_proj_sig", "vertex.pos_",
            "vertex.pos_.fX", "vertex.pos_.fY", "vertex.pos_.fZ",
            "vertex.pos_.X", "vertex.pos_.Y", "vertex.pos_.Z",
            "vertex.pos_.x", "vertex.pos_.y", "vertex.pos_.z",
        }
        req_all = set(desired_keys or []) | base_needed | set(getattr(bg, "BRANCHES", []))
        name_map = _map_desired_to_available(avail_keys, sorted(req_all))
        branch_req = sorted(set(name_map.values()))
        # Read the requested branches into an Awkward arrays object
        arrays = tree.arrays(branch_req, library="ak")
    # Build events dictionary with canonical keys
    events_bg = {}
    # Extract Z positions (if custom extractor exists in bg module, use it)
    if hasattr(bg, "_extract_z_from_arrays"):
        try:
            zvals = bg._extract_z_from_arrays(arrays)
            events_bg["vertex.pos_.fZ"] = np.asarray(zvals, dtype=float)
        except Exception:
            pass
    # If Z not set, attempt robust extraction
    if "vertex.pos_.fZ" not in events_bg:
        z = _get_array_by_canonical(arrays, "vertex.pos_.fZ", name_map)
        if z is None:
            for alias in ["vertex.pos_.Z", "vertex.pos_.z"]:
                z = _get_array_by_canonical(arrays, alias, name_map)
                if z is not None:
                    break
        if z is not None:
            events_bg["vertex.pos_.fZ"] = z
    # Transfer desired keys (features) from arrays to events_bg using mapping
    for k in (desired_keys or []):
        arr = _get_array_by_canonical(arrays, k, name_map)
        if arr is not None and np.issubdtype(arr.dtype, np.number):
            events_bg[k] = arr
    # Ensure essential branches are present (psum and vtx_proj_sig)
    if "psum" not in events_bg:
        tmp = None
        # Try common aliases for psum
        for nm in ["psum", "vertex.psum", "p_sum", "pSum", "sumP"]:
            if nm in getattr(arrays, "fields", []):
                tmp_arr = ak.to_numpy(arrays[nm])
                if tmp_arr is not None:
                    tmp = np.asarray(tmp_arr)
                    break
        if tmp is None:
            tmp = _get_array_by_canonical(arrays, "psum", name_map)
        if tmp is not None:
            events_bg["psum"] = np.asarray(tmp)
    if "vtx_proj_sig" not in events_bg:
        tmp = None
        for nm in ["vtx_proj_sig", "vertex.projSig", "vtxProjSig", "vtx_sigma_proj"]:
            if nm in getattr(arrays, "fields", []):
                tmp_arr = ak.to_numpy(arrays[nm])
                if tmp_arr is not None:
                    tmp = np.asarray(tmp_arr)
                    break
        if tmp is None:
            tmp = _get_array_by_canonical(arrays, "vtx_proj_sig", name_map)
        if tmp is not None:
            events_bg["vtx_proj_sig"] = np.asarray(tmp)
    # Determine mass window mask (±10 MeV around mass_mev)
    invM = None
    for inv_alias in ["vertex.invM_", "invM_", "invMass", "m_inv", "mInv", "vtxInvM", "vtx.invM"]:
        invM = _get_array_by_canonical(arrays, inv_alias, name_map)
        if invM is not None:
            break
    if invM is not None and hasattr(bg, "_mass_window_mask"):
        mwin_mask = bg._mass_window_mask(invM, mass_mev)
    else:
        mwin_mask = np.ones(len(events_bg.get(next(iter(events_bg.keys())), [])), dtype=bool)
        if debug and invM is None:
            sys.stderr.write("[debug] invariant mass branch not found; skipping mass window filter.\n")
    # Apply tight selection mask
    if hasattr(bg, "_tight_selection_mask"):
        try:
            tight_mask = bg._tight_selection_mask(events_bg, Val)
        except Exception as e:
            if debug:
                sys.stderr.write(f"[debug] bg._tight_selection_mask failed: {e}\n")
            tight_mask = None
    else:
        tight_mask = None
    if tight_mask is None:
        # If no selection mask available, default to requiring finite z
        zvals = events_bg.get("vertex.pos_.fZ")
        tight_mask = np.isfinite(zvals) if zvals is not None else np.ones_like(mwin_mask, dtype=bool)
    # Combine mass window and tight selection
    mask = (tight_mask.astype(bool) & mwin_mask.astype(bool))
    # Filter each numeric 1D branch by the combined mask
    out = {}
    for k, v in events_bg.items():
        if isinstance(v, np.ndarray) and v.ndim == 1 and len(v) == len(mask) and np.issubdtype(v.dtype, np.number):
            vv = v[mask]
            vv = vv[np.isfinite(vv)]
            out[k] = vv
    return out

def pick_1d_numeric_features(sig_dict, bg_dict):
    """Return a sorted list of feature names present in both signal and background (with at least 2 entries each)."""
    common = []
    for k in sig_dict.keys():
        if k in bg_dict and sig_dict[k].size >= 2 and bg_dict[k].size >= 2:
            common.append(k)
    prioritise = [
        "vertex.pos_.fZ", "ele.track_.z0_", "pos.track_.z0_",
        "ele.track_.d0_", "pos.track_.d0_", "vertex.invM_", "psum", "vtx_proj_sig",
        "ele.track_.chi2_", "pos.track_.chi2_",
        "ele.track_.px_", "ele.track_.py_", "ele.track_.pz_",
        "pos.track_.px_", "pos.track_.py_", "pos.track_.pz_",
    ]
    ordered = []
    seen = set()
    # Add prioritized features first (if present)
    for p in prioritise:
        if p in common and p not in seen:
            ordered.append(p)
            seen.add(p)
    # Add remaining common features in sorted order
    for k in sorted(common):
        if k not in seen:
            ordered.append(k)
            seen.add(k)
    return ordered

def sanitize(name: str) -> str:
    """Sanitize a string to be used in file names (replace special characters with underscore)."""
    bad_chars = [" ", "/", "\\", "(", ")", "[", "]", "{", "}", ":", ";", ",", "|", "<", ">", "?", "*", "'", '"', "."]
    out = name
    for b in bad_chars:
        out = out.replace(b, "_")
    # Collapse multiple underscores to single
    while "__" in out:
        out = out.replace("__", "_")
    return out.strip("_")

def plot_1d_overlaid(sig_arr, bg_arr, feat_name, outdir, mass_mev, epsilon, Val, bins, S_yield, B_yield, Zbi):
    """Plot a 1D histogram of a given feature, overlaying signal and background, normalized to expected yields."""
    fig, ax = plt.subplots(figsize=(7.8, 4.8))
    # Determine a suitable range for the histogram (focus between 0.5% and 99.5% quantiles of combined data)
    if bg_arr.size > 0:
        both = np.concatenate([sig_arr, bg_arr])
    else:
        both = sig_arr
    q1, q99 = np.quantile(both, [0.005, 0.995]) if both.size > 0 else (0, 1)
    pad = 0.05 * (q99 - q1 + 1e-12)
    hist_range = (q1 - pad, q99 + pad)
    # Compute per-event weights so that total area equals expected yields
    sig_weights = None
    bg_weights = None
    if sig_arr.size > 0:
        sig_weights = np.full(sig_arr.shape, S_yield / sig_arr.size)
    if bg_arr.size > 0:
        bg_weights = np.full(bg_arr.shape, B_yield / bg_arr.size)
    # Plot signal and background histograms with weights (no density normalization)
    ax.hist(sig_arr, bins=bins, range=hist_range, weights=sig_weights, histtype="step",
            linewidth=1.8, label="signal")
    if bg_arr.size > 0:
        ax.hist(bg_arr, bins=bins, range=hist_range, weights=bg_weights, histtype="step",
                linewidth=1.8, label="background")
    # Annotate the plot with yields and significance
    title_line1 = f"{feat_name}"
    title_line2 = f"S={S_yield:.2g}, B={B_yield:.2g}, Zbi={Zbi:.2f}; mass={mass_mev:g} MeV, "
    title_line2 += f"epsilon={epsilon}" if epsilon is not None else "epsilon=None"
    title_line2 += f", Val={Val}"
    ax.set_title(f"{title_line1}\n{title_line2}")
    ax.set_xlabel(feat_name)
    ax.set_ylabel("Expected events")
    ax.grid(True, alpha=0.3)
    ax.legend()
    
    ax.set_yscale("log")
    ymin, ymax = ax.get_ylim()
    ax.set_ylim(bottom=max(ymin, 1e-6), top=ymax)
    
    # Save plot to file
    fname = f"oned_{sanitize(feat_name)}_{int(round(mass_mev))}MeV"
    if epsilon is not None:
        # Include epsilon in filename (sanitized)
        eps_str = str(epsilon).replace('.', 'p').replace('-', 'm')
        fname += f"_eps{eps_str}"
    fname += f"_V{Val}.png"
    fpath = os.path.join(outdir, fname)
    fig.tight_layout()
    fig.savefig(fpath, dpi=160, bbox_inches="tight")
    plt.close(fig)
    return fpath

def plot_2d_single(data_x, data_y, feat_x, feat_y, label, outdir, mass_mev, epsilon, Val, bins2d, weight_per_event):
    """Plot a single 2D histogram for either signal or background data for two features."""
    fig, ax = plt.subplots(figsize=(6.6, 5.8))
    # Determine ranges for x and y (1% to 99% quantiles)
    def compute_range(a):
        if a.size == 0:
            return (0, 1)
        q1, q99 = np.quantile(a, [0.01, 0.99])
        pad = 0.05 * (q99 - q1 + 1e-12)
        return (q1 - pad, q99 + pad)
    rx = compute_range(data_x)
    ry = compute_range(data_y)
    weights = None
    if weight_per_event is not None:
        # Create weights array matching data length
        weights = np.full(data_x.shape, weight_per_event)
    # Plot 2D histogram with weights (if provided)
    H, xedges, yedges, img = ax.hist2d(data_x, data_y, bins=bins2d, range=[rx, ry], weights=weights)
    cb = fig.colorbar(img, ax=ax)
    cb.set_label("counts" if weights is None else "Expected events")
    title = f"{label} 2D: {feat_x} vs {feat_y}\nmass={mass_mev:g} MeV"
    if epsilon is not None:
        title += f", epsilon={epsilon}"
    title += f", Val={Val}"
    ax.set_title(title)
    ax.set_xlabel(feat_x)
    ax.set_ylabel(feat_y)
    ax.grid(True, alpha=0.15)
    # Save to file
    fname = f"twoD_{label}_{sanitize(feat_x)}__{sanitize(feat_y)}_{int(round(mass_mev))}MeV"
    if epsilon is not None:
        eps_str = str(epsilon).replace('.', 'p').replace('-', 'm')
        fname += f"_eps{eps_str}"
    fname += f"_V{Val}.png"
    fpath = os.path.join(outdir, fname)
    fig.tight_layout()
    fig.savefig(fpath, dpi=160, bbox_inches="tight")
    plt.close(fig)
    return fpath

def main():
    ap = argparse.ArgumentParser(description="Plot overlaid 1D and 2D histograms for signal/background with yield normalization.")
    ap.add_argument("--mass", type=float, required=True, help="A' mass in MeV (used to select signal files and mass window).")
    ap.add_argument("--epsilon", type=float, default=None, help="Kinetic mixing parameter (epsilon) for signal yield weighting.")
    ap.add_argument("--Val", type=int, default=25, help="Tight selection parameter (e.g., z-threshold index). Default 25.")
    ap.add_argument("--outdir", type=str, default="plots_all_features", help="Output directory for plots.")
    ap.add_argument("--bins", type=int, default=80, help="Number of bins for 1D plots.")
    ap.add_argument("--bins2d", type=int, default=80, help="Number of bins per axis for 2D plots.")
    ap.add_argument("--features", type=str, nargs="*", default=None, help="Optional subset of feature names to include.")
    ap.add_argument("--max-pairs", type=int, default=None, help="Optional cap on number of 2D feature pairs (per sample).")
    ap.add_argument("--base-module", type=str, default="decayLength5sel", help="Signal base module name (default uses decayLength5sel.py).")
    ap.add_argument("--debug", action="store_true", help="Print debug information about background loading.")
    args = ap.parse_args()

    _ensure_dir(args.outdir)

    # Import the signal base module and load signal events
    base = import_signal_base(args.base_module)
    sig = load_signal_events(base, args.mass, args.Val)
    if not sig:
        sys.stderr.write("[error] No signal features loaded after selection.\n")
        sys.exit(2)

    # Determine all signal feature names that have sufficient entries
    all_sig_feats = sorted([k for k, v in sig.items() if isinstance(v, np.ndarray) and v.ndim == 1 and v.size >= 2])
    # Ensure 'vertex.pos_.fZ' is included if only alias present
    if "vertex.pos_.fZ" not in all_sig_feats and "vertex.pos_.z" in all_sig_feats:
        all_sig_feats.insert(0, "vertex.pos_.fZ")

    # Load background events (after tight selection and within ±10 MeV mass window)
    bg_dict = load_background_events(args.mass/1.8, args.Val, desired_keys=set(all_sig_feats), debug=args.debug)

    # Filter features if user provided a specific list
    if args.features:
        selected_feats = [f for f in args.features if f in sig and (not bg_dict or f in bg_dict)]
        if not selected_feats:
            sys.stderr.write("[warn] --features list produced no usable features; falling back to common set.\n")
            selected_feats = None
    else:
        selected_feats = None

    # Determine the final set of features to plot (common to both signal and background if available, else all signal features)
    common_feats = pick_1d_numeric_features(sig, bg_dict) if bg_dict else sorted(list(sig.keys()))
    features_1d = selected_feats if selected_feats else common_feats

    # If epsilon is not provided, we cannot compute actual yields; inform the user and exit
    if args.epsilon is None:
        sys.stderr.write("[error] --epsilon must be specified to compute signal/background yields.\n")
        sys.exit(1)

    # Compute expected signal and background yields, and Zbi significance
    epsilon = args.epsilon
    mass_mev =  float(args.mass)
    # Signal acceptance and yield:
    # Compute acceptance-weighted probability (sR, sP) for signal decays in acceptance using getSum
    try:
        sR, sP = base.getSum(epsilon, mass_mev, args.Val)
    except Exception as e:
        sys.stderr.write(f"[error] Failed to compute signal acceptance fraction: {e}\n")
        sys.exit(1)
    signal_acceptance_frac = float(sR + sP)  # total accepted fraction of decays for one A'
    # Compute theoretical yield factor using aprime_yield logic (scale_const * ratio * mA * eps^2)
    scale_const = 3.0 * math.pi / (2.0 * 1.0 * (1.0 / 137.0459991))  # from decayLength5sel (parralel_aprime)
    ratio_val = base.ratio(mass_mev)  # ratio(mA) includes polynomial m(x) fraction and cross-section factors
    core = scale_const * ratio_val * mass_mev * (epsilon ** 2)  # expected number of A' produced per baseline (per N_B events baseline)
    signal_fraction = signal_acceptance_frac * core  # fraction of baseline events that result in an accepted signal
    S_yield = N_B_TOTAL_RECO * signal_fraction  # expected signal yield (S)
    # Background yield:
    # Compute polynomial mass-bin fraction m(x) for this mass (from combine_zbi.py)
    x_gev = mass_mev / 1000.0
    poly_num = (-6860.03 + 299358.0 * x_gev - 4087220.0 * (x_gev ** 2) +
                25209900.0 * (x_gev ** 3) - 73485900.0 * (x_gev ** 4) +
                82579800.0 * (x_gev ** 5))
    m_fraction = poly_num / (82.9268041667 * 1000.0)  # polynomial fraction m(x)
    # Determine background selection efficiency (fraction of background in mass window that passes tight selection)
    bg_fraction = 0.0
    if bg_dict:
        # Numerator: number of background events after selection (within mass window)
        numer = len(next(iter(bg_dict.values()))) if bg_dict else 0
        # Denominator: total background events in mass window (no selection). We need to compute this by reading invM from file.
        denom = 0
        try:
            if bg and hasattr(bg, "BACKGROUND_PATH"):
                # Use background file path from module to count events in mass window
                bg_paths = []
                cand = getattr(bg, "BACKGROUND_PATH", None)
                if cand:
                    if isinstance(cand, (list, tuple)):
                        for cc in cand:
                            bg_paths.append(str(cc))
                    else:
                        bg_paths.append(str(cand))
                if bg_paths:
                    # Use first background file (same as used in bg_dict)
                    bg_file = bg_paths[0]
                    with uproot.open(bg_file) as f:
                        t = None
                        if hasattr(bg, "_open_first_tree"):
                            t = bg._open_first_tree(f)
                        else:
                            # fallback: pick first TTree
                            keys = [k for k in f.keys() if ";" in k]
                            t = f[keys[0]] if keys else None
                        if t is not None:
                            invM_array = ak.to_numpy(t["vertex.invM_"].array(library="ak"))
                            center = mass_mev / (1000.0*1.8)  # center of mass window in GeV
                            low, high = center - 0.01, center + 0.01
                            mask_mass = (invM_array > low) & (invM_array < high)
                            denom = int(np.sum(mask_mass))
        except Exception as e:
            if args.debug:
                sys.stderr.write(f"[debug] Unable to compute background mass-window count: {e}\n")
        if denom > 0:
            bg_fraction = float(numer) / float(denom)
    # Compute expected background yield in the ±10 MeV mass window after selection
    # N_b_massbin = N_B_TOTAL_RECO * m_fraction * (mass window width in units of 0.1 GeV)
    # The factor 10 corresponds to 0.01 GeV * 10 = 0.1 GeV (since poly_m_of_x returned fraction per 0.1 GeV)
    N_b_massbin = N_B_TOTAL_RECO * m_fraction * 10.0
    B_yield = N_b_massbin * bg_fraction  # expected background yield (B) after selection
    # Compute Zbi significance
    Zbi_value = zbi_significance(S_yield, B_yield)

    # 1D overlaid plots
    out_1d_files = []
    if bg_dict:
        # Overlay signal and background for each feature
        for feat in features_1d:
            try:
                fpath = plot_1d_overlaid(sig[feat], bg_dict[feat], feat, args.outdir,
                                         mass_mev, epsilon, args.Val, args.bins,
                                         S_yield, B_yield, Zbi_value)
                out_1d_files.append(fpath)
            except Exception as e:
                sys.stderr.write(f"[warn] 1D overlay failed for {feat}: {e}\n")
    else:
        # If no background available, plot signal only (weighted by S yield)
        for feat in features_1d:
            try:
                arr = sig[feat]
                # Use a simplified version of the 1D plotting for signal-only
                fig, ax = plt.subplots(figsize=(7.8, 4.8))
                q1, q99 = np.quantile(arr, [0.005, 0.995]) if arr.size > 0 else (0, 1)
                pad = 0.05 * (q99 - q1 + 1e-12)
                rrange = (q1 - pad, q99 + pad)
                weights = np.full(arr.shape, S_yield / arr.size) if arr.size > 0 else None
                ax.hist(arr, bins=args.bins, range=rrange, weights=weights, histtype="step",
                        linewidth=1.8, label="signal")
                ax.set_title(f"{feat}\nS={S_yield:.2g}, mass={args.mass:g} MeV, epsilon={epsilon}, Val={args.Val}")
                ax.set_xlabel(feat)
                ax.set_ylabel("Expected events")
                ax.grid(True, alpha=0.3)
                ax.legend()
                fname = f"oned_signal_{sanitize(feat)}_{int(round(args.mass))}MeV"
                if epsilon is not None:
                    eps_str = str(epsilon).replace('.', 'p').replace('-', 'm')
                    fname += f"_eps{eps_str}"
                fname += f"_V{args.Val}.png"
                f = os.path.join(args.outdir, fname)
                fig.tight_layout()
                fig.savefig(f, dpi=160, bbox_inches="tight")
                plt.close(fig)
                out_1d_files.append(f)
            except Exception as e:
                sys.stderr.write(f"[warn] 1D signal-only failed for {feat}: {e}\n")

    # 2D histograms for signal and background separately
    feats_for_2d = features_1d
    pairs = list(itertools.combinations(feats_for_2d, 2))
    if args.max_pairs is not None:
        pairs = pairs[:int(args.max_pairs)]
    out_2d_sig = []
    out_2d_bg = []
    # Signal 2D plots (each pair)
    for fx, fy in pairs:
        try:
            x = sig[fx]
            y = sig[fy]
            if x.size >= 10 and y.size >= 10:
                # Weight per signal event
                w_sig = S_yield / x.size if x.size > 0 else None
                fpath = plot_2d_single(x, y, fx, fy, "signal", args.outdir,
                                       mass_mev, epsilon, args.Val, args.bins2d, w_sig)
                out_2d_sig.append(fpath)
            # If fewer than 10 events, skip plotting to avoid sparse meaningless plots
        except Exception as e:
            sys.stderr.write(f"[warn] 2D signal failed for {fx} vs {fy}: {e}\n")
    # Background 2D plots (each pair)
    if bg_dict:
        for fx, fy in pairs:
            try:
                x = bg_dict[fx]
                y = bg_dict[fy]
                if x.size >= 10 and y.size >= 10:
                    w_bg = B_yield / x.size if x.size > 0 else None
                    fpath = plot_2d_single(x, y, fx, fy, "background", args.outdir,
                                           mass_mev, epsilon, args.Val, args.bins2d, w_bg)
                    out_2d_bg.append(fpath)
            except Exception as e:
                sys.stderr.write(f"[warn] 2D background failed for {fx} vs {fy}: {e}\n")

    # Print summary of outputs
    print(f"[done] 1D plots written: {len(out_1d_files)}")
    print(f"[done] 2D signal plots written: {len(out_2d_sig)}")
    print(f"[done] 2D background plots written: {len(out_2d_bg)}")
    if out_1d_files:
        print("Example output files:")
        for p in out_1d_files[:min(5, len(out_1d_files))]:
            print("  ", p)

if __name__ == "__main__":
    import itertools  # import here to use combinations for 2D pairs
    main()

