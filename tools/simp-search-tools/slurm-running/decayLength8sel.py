import numpy as np
import matplotlib.pyplot as plt
import argparse
import uproot
import awkward as ak
import sys
from functools import lru_cache
import joblib  # Added to load BDT model

HBAR_C = 1.973e-14  # in GeV*cm

L = 36
MASSES = [25 + 5 * i for i in range(L)]
ALICLOC = "/sdf/data/hps/users/rodwyer1/alicSIMPs/gen/"
#/fs/ddn/sdf/group/hps/users/alspellm/projects/THESIS/mc/2021/simps/gen/recon/
PRIORRECON = [ALICLOC+"slic/simps_3pt7/tuple_ana/files/hadd_mass_" + str(25 + 5 * i) + "_simp_slic_ana.root" for i in range(L)]
POSTRECON  = [ALICLOC+"recon/simps_3pt7/tuple_ana/files/hadd_mass_" + str(25 + 5 * i) + "_simp_recon_ana.root" for i in range(L)]

def beta(x, y):
    return (1 + (y ** 2) - (x ** 2) - 2 * y) * (1 + (y ** 2) - (x ** 2) + 2 * y)

def decay_length_dark_photon_to_vector_boson(alpha_D, f_pi_D, m_pi_D, m_V_D, m_Ap, multiplicity=1.0):
    """Sec 2.12-2.14: Partial width for Aprime to V_D + pi_D (returns width in GeV)."""
    x = m_V_D / m_Ap
    y = m_pi_D / m_Ap
    prefactor = (alpha_D * multiplicity) / (192.0 * (np.pi ** 4))
    ratio_terms = (m_Ap / m_pi_D) ** 2 * (m_V_D / m_pi_D) ** 2 * (m_pi_D / f_pi_D) ** 4
    width = prefactor * ratio_terms * m_Ap * beta(x, y) ** 1.5
    print(width)
    return width

def decay_length_dark_photon_to_pions(alpha_D, m_pi_D, m_V_D, m_Ap):
    """Sec 2.9: Width for A' -> pi_D pi_D with phase space and VMD factor (returns width in GeV)."""
    term1 = (1 - (4.0 * (m_pi_D ** 2)) / ((m_Ap ** 2)))
    term2 = ((m_V_D ** 2) / ((m_Ap ** 2) - (m_V_D ** 2))) ** 2
    width = ((2.0 * alpha_D) / 3.0) * m_Ap * (term1 ** 1.5) * term2
    print(width)
    return width

def total_length(alpha_D, f_pi_D, m_pi_D, m_V_D, m_Ap):
    """Sum of widths for hidden-sector channels (returns total width in GeV)."""
    rho  = decay_length_dark_photon_to_vector_boson(alpha_D, f_pi_D, m_pi_D, m_V_D, m_Ap, 0.75)
    phi  = decay_length_dark_photon_to_vector_boson(alpha_D, f_pi_D, m_pi_D, m_V_D, m_Ap, 1.5)
    invis = decay_length_dark_photon_to_pions(alpha_D, m_pi_D, m_V_D, m_Ap)
    return rho + phi + invis

def decay_length_vector_to_leptons(alpha_D, epsilon, f_pi_D, m_V, m_Ap, m_lep, is_rho=True):
    """Leptonic width for V_D -> l+l-, convert to proper length L0 (cm)."""
    F = ((m_V ** 2) / ((m_Ap ** 2) - (m_V ** 2))) ** 2
    PhaseSpace = np.sqrt(1.0 - ((4.0 * (m_lep ** 2)) / ((m_V ** 2)))) * (1.0 + (2.0 * (m_lep ** 2)) / (m_V ** 2))
    coeff = 1.0 if is_rho else 2.0
    width = coeff * ((16.0 * np.pi * alpha_D * (epsilon ** 2) * (f_pi_D ** 2)) / (3.0 * (m_V ** 2))) * F * PhaseSpace * (m_V/1000)
    L0 = HBAR_C / width
    return L0

# ==================== CACHED PSUM FOR ENERGY SAMPLING (unchanged behavior) ====================

@lru_cache(maxsize=None)
def _psum_hist_cache(index):
    with uproot.open(POSTRECON[index]) as f:
        hist = f["vtxana_kf_vtxSelection/vtxana_kf_vtxSelection_Psum_h"]
        counts, edges = hist.to_numpy()
    counts = np.asarray(counts, dtype=float)
    total = counts.sum()
    pdf = (np.ones_like(counts, dtype=float) / len(counts)) if total <= 0 else (counts / total)
    return np.asarray(edges, dtype=float), pdf

def getAEnergy(mV):
    INDEX = int(sum([i * (MASSES[i] <= mV) * (MASSES[i + 1] > mV) for i in range(len(MASSES) - 1)]))
    edges, pdf = _psum_hist_cache(INDEX)
    bin_index = np.random.choice(len(pdf), p=pdf)
    sample = float(np.random.uniform(edges[bin_index], edges[bin_index + 1]))
    return sample

##THE NEW WAY WE DO THINGS BELOW
PSUM_PATH_TMPL = "/sdf/data/hps/users/rodwyer1/SIMPS/allmassesD/psum{mass}.root"
PSUM_HIST_NAME = "h_psum_vector"
@lru_cache(maxsize=None)
def _gamma_cache(mass_key):
    psum_path = PSUM_PATH_TMPL.format(mass=int(mass_key))
    with uproot.open(psum_path) as f:
        h = f[PSUM_HIST_NAME]
        edges = np.asarray(h.axes[0].edges())
        vals = np.asarray(h.values(),dtype=float)
    return edges, vals




# ==================== NEW SIMP FILE STRUCTURE WITH CACHED I/O ====================

AVAILABLE_NEW_MASSES = [30, 45, 60, 75, 90, 105, 120, 135, 150, 165, 180, 195, 210] #[60, 90, 120, 150, 180, 210, 240]
#LOCATION = "/sdf/data/hps/users/rodwyer1/SIMPS/allmassesD/"
LOCATION = "/sdf/group/hps/users/rodwyer1/run/BigSIMPCollection2021/PRESELECTION/"
DEN_PATH_TMPL = LOCATION+"logger_{mass}.root"
DEN_HIST_NAME = "h_z_eepair"
#NUM_PATH_TMPL = LOCATION+"{mass}MeVpres.root"
NUM_PATH_TMPL = LOCATION+"simp{mass}v2.root"
#"simp{mass}pres.root"
NUM_TREE_CANDIDATES = ["preselection", "preselection;1"]


# Branches to load from numerator files (based on Rory's BDT backbone)
BRANCHES = [
    "psum","vertex.pos_","vertex.invM_","vertex.invMerr_",
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
]
# Vector-like branches to load (kept in awkward; we reduce to per-event scalars/booleans)
VECTOR_BRANCHES = [
    "ele.track_.hit_layers_",
    "pos.track_.hit_layers_",
    #"ele.track_.lambda_kinks_",
    #"pos.track_.lambda_kinks_",
]

def _closest_available_mass(m):
    return min(AVAILABLE_NEW_MASSES, key=lambda mm: abs(mm - float(m)))

@lru_cache(maxsize=None)
def _mass_key(m):
    return int(_closest_available_mass(float(m)))

def _open_first_tree(file):
    for name in NUM_TREE_CANDIDATES:
        if name in file:
            return file[name]
    for _, obj in file.items():
        try:
            if obj.classname.startswith("TTree"):
                return obj
        except Exception:
            pass
    raise KeyError("No TTree found (expected 'preselection').")

@lru_cache(maxsize=None)
def _den_cache(mass_key):
    """Cache denominator histogram once per mass."""
    den_path = DEN_PATH_TMPL.format(mass=int(mass_key))
    with uproot.open(den_path) as f:
        h = f[DEN_HIST_NAME]
        edges = np.asarray(h.axes[0].edges())
        vals  = np.asarray(h.values(), dtype=float)
    return edges, vals

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

@lru_cache(maxsize=None)
def _events_cache(mass_key):
    """Load once per mass: all requested branches into numpy arrays; keep in memory for fast selections."""
    num_path = NUM_PATH_TMPL.format(mass=int(mass_key))
    with uproot.open(num_path) as f:
        t = _open_first_tree(f)
        arrays = t.arrays(BRANCHES + VECTOR_BRANCHES, library="ak")
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

def preload_caches(mass_list=None):
    masses = mass_list or AVAILABLE_NEW_MASSES
    for m in masses:
        mk = _mass_key(m)
        _ = _den_cache(mk)
        _ = _events_cache(mk)
        _ = _gamma_cache(mk)

# Load the trained BDT model once
BDT_MODEL_PATH = "/sdf/group/hps/users/rodwyer1/run/reach_curves/optimization/bdt_trainer_2426/bdt_model.joblib"
try:
    bdt_model = joblib.load(BDT_MODEL_PATH)
except Exception as e:
    sys.stderr.write(f"[error] Could not load BDT model: {e}\n")
    bdt_model = None

def tight_selection(events, Val, Val2, hitcat="isL1L1"):
    """
    Return a boolean mask of events to keep, using BDT score cut instead of z0 cut.
    Available keys include those in BRANCHES, plus 'vertex.pos_.fZ'.
    """
    z    = events.get("vertex.pos_.fZ")
    invM = events.get("vertex.invM_")
    proj_sig = events.get("vtx_proj_sig")
    elez0 = events.get("ele.track_.z0_")
    posz0 = events.get("pos.track_.z0_")
    psum = events.get("psum")

    # Base mask: finite z
    mask = np.isfinite(z)
    mask = np.asarray(mask, dtype=bool)
    # [HIT CATEGORY] Use the hitcat parameter to select which TTree flag to apply.
    # Pass hitcat=None to skip the hit category cut entirely (no-category mode).
    # To switch category, pass e.g. hitcat="isL2L2". All categories must be loaded in BRANCHES/_events_cache.
    _hitcat = events.get(hitcat) if hitcat is not None else None
    if _hitcat is not None:
        mask &= np.asarray(_hitcat, dtype=bool)
    mask &= (psum>=1.5)&(psum<=3.0)

    print("How many survived L1L1 and psum: "+str(sum([m==True for m in mask])))

    # Compute BDT score and apply BDT cut (replaces zval cut)
    threshold_val = 1.0 - ((1.0-float(Val)/25.0)**3.0)
    #1.0 * (float(Val) / 25.0)
    if bdt_model is not None:
        # Prepare feature matrix for BDT
        features = []
        # vertex.invM_, psum
        #features.append(events["vertex.invM_"])
        features.append(events["psum"])
        # vertex.pos_ fields
        features.append(events["vertex.pos_.fX"])
        features.append(events["vertex.pos_.fY"])
        features.append(events["vertex.pos_.fZ"])
        # electron track features
        ele_keys = ["n_hits_", "d0_", "phi0_", "z0_", "tan_lambda_",
                    "px_", "py_", "pz_", "chi2_", "x_at_ecal_", "y_at_ecal_", "z_at_ecal_"]
        for key in ele_keys:
            features.append(events[f"ele.track_.{key}"])
        # positron track features
        pos_keys = ["n_hits_", "d0_", "phi0_", "z0_", "tan_lambda_",
                    "px_", "py_", "pz_", "chi2_", "x_at_ecal_", "y_at_ecal_", "z_at_ecal_"]
        for key in pos_keys:
            features.append(events[f"pos.track_.{key}"])
        # vertex chi2 and invMerr
        features.append(events["vertex.chi2_"])
        #features.append(events["vertex.invMerr_"])
        # vertex projection features
        features.append(events["vtx_proj_sig"])
        features.append(events["vtx_proj_x_sig"])
        features.append(events["vtx_proj_y_sig"])
        # Stack features into matrix
        X = np.column_stack([f.reshape(-1) for f in features])
        
        import gc
        del features
        gc.collect()

        X = np.nan_to_num(X, nan=0.0, posinf=0.0, neginf=0.0)
        bdt_scores = bdt_model.predict_proba(X)[:,1]
        print("The BDT scores are as follows: ")
        print(bdt_scores)
        print("The threshold: ")
        print(threshold_val)
        print("Val: ")
        print(Val)
        mask &= (bdt_scores > threshold_val)
        print("Things above threshold")
        print(mask.sum())
    else:
        # If model not loaded, skip BDT cut (conservative: no additional cut)
        pass

    projval=50.0*(float(Val2)/10)+3
    #1.0+np.max([0,float(Val2)/10.0-4.0])**3.0
    mask &= (proj_sig<projval)
    print("Things above threshold surviving proj: ")
    print(mask.sum())
    return mask


"""def tight_selection(events,Val,Val2):
    
    Return a boolean mask of events to keep. Edit to your desired 'tight' cuts.
    Available keys include those in BRANCHES, plus 'vertex.pos_.fZ'.
    Default: keep finite z and loose mass window.
    
    z    = events.get("vertex.pos_.fZ")
    invM = events.get("vertex.invM_")
    proj_sig = events.get("vtx_proj_sig")
    elez0 = events.get("ele.track_.z0_")
    posz0 = events.get("pos.track_.z0_")
    psum = events.get("psum")

    #hitlayers = events.get("ele.track_.hit_layers_")

    #mask = (z>.20)
    mask = np.isfinite(z)
    # ensure base mask is explicit bool (optional but robust)
    mask = np.asarray(mask, dtype=bool)
    _ehas = events.get("ele.hasL0L1")
    if _ehas is not None:
        _ehas = np.asarray(_ehas, dtype=bool)  # force bool, avoid object
        mask &= _ehas

    _phas = events.get("pos.hasL0L1")
    if _phas is not None:
        _phas = np.asarray(_ehas, dtype=bool)  # force bool, avoid object
        mask &= _phas
    mask &= (psum>=1.5)&(psum<=3.0)
    #print("I am printing positron y0")
    #print(posz0)
    #print((posz0<-2.0*(float(Val)*(1.0/25.0))-1.5))
    #print(((posz0>2.0*(float(Val)*(1.0/25.0))+1.5)))
    #print("Do I exceed threshold")
    #print(((posz0>2.0*(float(Val)*(1.0/25.0))+1.5)|(posz0<-2.0*(float(Val)*(1.0/25.0))-1.5)))
    #print("I am printing electron y0")
    #print(elez0)
    #print("Do I exceed threshold")
    #print(((elez0>2.0*(float(Val)*(1.0/25.0))+1.5)|(elez0<-2.0*(float(Val)*(1.0/25.0))-1.5)))
    #print("Am I below significance")
    #print((proj_sig<1.8))
    #print(mask)
    zval=.5*(float(Val)*(1.0/25.0))
    mask &=((posz0>zval)|(posz0<-zval))
    mask &=((elez0>zval)|(elez0<-zval))
    projval=2*(float(Val2)/10)+3
    #(float(Val)/25.0)
    mask &= (proj_sig<projval)
    #<4*(float(Val)*(1/25.0)))
    print(mask)
    print("\n")
    #if invM is not None:
    #mask &= (invM > -100.0) & (invM < 0.18)
    # Example additional cuts (uncomment to use):
    # chi2 = events.get("vertex.chi2_")
    # if chi2 is not None: mask &= (chi2 < 10.0)
    # nhit_e = events.get("ele.track_.n_hits_")
    # nhit_p = events.get("pos.track_.n_hits_")
    # if nhit_e is not None: mask &= (nhit_e >= 12)
    # if nhit_p is not None: mask &= (nhit_p >= 12)
    return mask"""

def read_den_hist(mass):
    mk = _mass_key(mass)
    return _den_cache(mk)

def read_psum_hist(mass):
    mk = _mass_key(mass)
    return _gamma_cache(mk)

def getFrac(mV, z,Val):
    """
    Acceptance fraction at a given z: N_acc(z) / N_gen(z),
    using cached numerator arrays and user-editable `tight_selection`.
    """
    den_edges, den_vals = read_den_hist(mV)
    if not (den_edges[0] <= z < den_edges[-1]):
        return 0.0
    mk = _mass_key(mV)
    events = _events_cache(mk)
    mask   = tight_selection(events,Val)
    zvals  = events["true_vd.vtx_z_"][mask]
    #zvals  = events["vertex.pos_.fZ"][mask]
    num_vals, _ = np.histogram(zvals, bins=den_edges)
    i = np.searchsorted(den_edges, z, side="right") - 1
    Ngen = float(den_vals[i])
    Nacc = float(num_vals[i])
    print("z value is ")
    print(zvals)
    print("The number of entries in bins")
    print(num_vals)
    print("Which Bin")
    print(i)
    print("Number generated")
    print(Ngen)
    print("Number accepted")
    print(Nacc)
    print("\n") 
    return (Nacc / Ngen) if Ngen > 0 else 0.0

def plot_ratio_for_mass(mass, out_png=None, title=None):
    """Quick validator: (numerator/denominator) vs z after tight_selection()."""
    den_edges, den_vals = read_den_hist(mass)
    mk = _mass_key(mass)
    events = _events_cache(mk)
    mask   = tight_selection(events,Val)
    zvals  = events["vertex.pos_.fZ"][mask]
    num_vals, _ = np.histogram(zvals, bins=den_edges)
    with np.errstate(divide="ignore", invalid="ignore"):
        ratio = np.where(den_vals > 0, num_vals / den_vals, np.nan)
    centers = 0.5 * (den_edges[:-1] + den_edges[1:])
    plt.figure(figsize=(7.2, 4.2))
    plt.step(centers, ratio, where="mid")
    plt.xlabel("z [mm]")
    plt.ylabel("acceptance  N_acc / N_gen")
    plt.title(title or f"Acceptance ratio (num/den), mass≈{_closest_available_mass(mass)} MeV")
    plt.grid(True, alpha=0.3)
    if out_png:
        plt.savefig(out_png, dpi=140, bbox_inches="tight")
        print(f"[plot] wrote {out_png}")
    return centers, ratio

# ==================== Physics driver code (unchanged) ====================

def getProb(z, epsilon, mA,Val):
    """Sec 5.2: exponential decay along z, weighted by F(z)."""
    mpi = mA / 3.0
    mV  = mA / 1.8
    gamma1 = (getAEnergy(mV)*1000)/mV
    gamma2 = (getAEnergy(mV)*1000)/mV
    L0rho = decay_length_vector_to_leptons(.01, epsilon, mpi / (3.0 * 3.1415926), mV, mA, .000511, True)
    L0phi = decay_length_vector_to_leptons(.01, epsilon, mpi / (3.0 * 3.1415926), mV, mA, .000511, False)
    Lrho = gamma1 * L0rho * 10.0
    Lphi = gamma2 * L0phi * 10.0
    frac = getFrac(mV, z,Val)
    print(frac)
    print(gamma1)
    print(gamma2)
    print(Lrho)
    print(Lphi)
    prho = frac * np.exp(-z / Lrho) / Lrho
    pphi = frac * np.exp(-z / Lphi) / Lphi
    return prho, pphi

def getSum(epsilon, mA,Val):
    """Integral over z of getProb to obtain acceptance-weighted probabilities per channel."""
    Zmax = 240
    sR = 0.0
    sP = 0.0
    for z in range(Zmax):
        pr, pp = getProb(z, epsilon, mA,Val)
        sR += pr
        sP += pp
    return sR, sP

def mass_func(m_V_D):
    x = m_V_D/1000
    m = (-6860.03 + 299358 * x - 4087220 * (x * x) + 25209900 * (x ** 3) - 73485900 * (x ** 4) + 82579800 * (x ** 5)) / 82.9268041667
    return m/1000.0

def ratio(x):
    x = x / 1000
    f = -.16647 + 8.0747 * x - 111.31 * x * x + 727.92 * (x ** 3) - 2241.3 * (x ** 4) + 2604.3 * (x ** 5)
    A = .091562 - 10.339 * x + 256.94 * (x * x) - 1940.0 * (x ** 3) + 5812.8 * (x ** 4) - 5999.9 * (x ** 5)
    if (x < .05) or (x > .25):
        return 0.0
    return ((f / A))

def aprime_yield(mass_mev, eps2, scale_const, eot_norm=1.0, cap=None):
    mA = float(mass_mev)
    core = scale_const * ratio(mA) * mA * eps2
    print("Scale conts: " + str(scale_const))
    print("ratio: " + str(ratio(mA)))
    print("mass: " + str(mA))
    print("eps2: " + str(eps2))
    print("core: " + str(np.log(core) / np.log(10.0)) + "\\n")
    val = eot_norm * core
    if cap is not None:
        val = min(val, cap)
    return val

def parralel_aprime(I, LL, eps2_min=1e-10, eps2_max=1e-4):
    Length = int(LL)
    mA = (240.0 / float(Length)) * float(I)
    eps2_vals = [10.0 ** (-4.0 - (6.0 * j) / float(Length)) for j in range(Length)]
    Value = 3 * np.pi / (2 * 1 * (1 / 137.0459991))
    row = [aprime_yield(mA, e2, Value) for e2 in eps2_vals]
    outpath = "/sdf/group/hps/users/rodwyer1/run/reach_curves/outputTxt/aprime_only_output" + str(I) + ".txt"
    from functools import lru_cache as _sys
    print(row)
    with open(outpath, "w") as f:
        _old = _sys.stdout
        _sys.stdout = f
        print(row)
        _sys.stdout = _old
    return row

def makeBRplot():
    mA = 100.0
    mpi = mA / 3.0
    mV = mA / 1.8
    N = 100
    Xaxis = [3.0 * (1 - float(i) / N) + 4.0 * 3.1415926 * float(i) / N for i in range(int(N))]
    rho = [decay_length_dark_photon_to_vector_boson(.01, ((mA / float(9)) * (1 / x)), mpi, mV, mA, 3.0 / 4.0) / total_length(.01, ((mA / float(9)) * (1 / x)), mpi, mV, mA) for x in Xaxis]
    phi = [decay_length_dark_photon_to_vector_boson(.01, ((mA / float(9)) * (1 / x)), mpi, mV, mA, 3.0 / 2.0) / total_length(.01, ((mA / float(9)) * (1 / x)), mpi, mV, mA) for x in Xaxis]
    plt.plot(Xaxis, rho, label='rho fraction')
    plt.plot(Xaxis, phi, label='phi fraction')
    plt.legend()
    plt.show()

def plotAbundancesB4Frac():
    N = 100
    Length = 100
    Xaxis = [3.0 * (1 - float(i) / N) + 4.0 * 3.1415926 * float(i) / N for i in range(int(N))]
    rho = [decay_length_dark_photon_to_vector_boson(.01, (10.0 / float(9)) * (1 / x), 10.0 / 3.0, 10.0 / 1.8, 10.0, 3.0 / 4.0) / total_length(.01, (10.0 / float(9)) * (1 / x), 10.0 / 3.0, 10.0 / 1.8, 10.0) for x in Xaxis]
    phi = [decay_length_dark_photon_to_vector_boson(.01, (10.0 / float(9)) * (1 / x), 10.0 / 3.0, 10.0 / 1.8, 10.0, 3.0 / 2.0) / total_length(.01, (10.0 / float(9)) * (1 / x), 10.0 / 3.0, 10.0 / 1.8, 10.0) for x in Xaxis]
    Highfrho = np.array([[np.log(min([329.2643 * (10.0 * i) * (10 ** (-4.0 - (6.0 * j) / Length)) * rho[len(rho) - 1], 1.0])) for j in range(Length)] for i in range(Length)])
    Highfphi = np.array([[np.log(min([329.2643 * (10.0 * i) * (10 ** (-4.0 - (6.0 * j) / Length)) * phi[len(phi) - 1], 1.0])) for j in range(Length)] for i in range(Length)])
    x_vals = [10.0 * i for i in range(Length)]
    y_vals = [10 ** (-4.0 - (6.0 * j) / float(Length)) for j in range(Length)]
    Highfrho = Highfrho.T
    X, Y = np.meshgrid(x_vals, y_vals)
    plt.pcolormesh(X, Y, Highfrho, shading='auto', cmap='plasma')
    plt.colorbar(label="Signal over Background")
    plt.xlabel("Mass in 10 MeV")
    plt.ylabel("Epsilon Squared")
    plt.title("Rate of Signal to Background prior to Acceptance Effects")
    plt.yscale("log")
    plt.xscale("linear")
    plt.show()

def plotAbundances():
    N = 100
    Length = 100
    Xaxis = [3.0 * (1 - float(i) / N) + 4.0 * 3.1415926 * float(i) / N for i in range(int(N))]
    rho = [decay_length_dark_photon_to_vector_boson(.01, (10.0 / float(9)) * (1 / x), 10.0 / 3.0, 10.0 / 1.8, 10.0, 3.0 / 4.0) / total_length(.01, (10.0 / float(9)) * (1 / x), 10.0 / 3.0, 10.0 / 1.8, 10.0) for x in Xaxis]
    phi = [decay_length_dark_photon_to_vector_boson(.01, (10.0 / float(9)) * (1 / x), 10.0 / 3.0, 10.0 / 1.8, 10.0, 3.0 / 2.0) / total_length(.01, (10.0 / float(9)) * (1 / x), 10.0 / 3.0, 10.0 / 1.8, 10.0) for x in Xaxis]
    CON = 329.2643
    window = 4.0
    Highfrho = np.array([[np.log(min([CON * ratio((10.0 * i)) * mass_func(10.0*i)* window * (10.0 * i) * (10 ** (-4.0 - (6.0 * j) / Length)) * rho[len(rho) - 1] * getSum(np.sqrt(10 ** (-4.0 - (6.0 * j) / Length)), (10.0) * float(i))[0], 1.0])) for j in range(Length)] for i in range(Length)])
    Highfphi = np.array([[np.log(min([CON * ratio((10.0 * i)) * mass_func(10.0*i)* window * (10.0 * i) * (10 ** (-4.0 - (6.0 * j) / Length)) * phi[len(phi) - 1] * getSum(np.sqrt(10 ** (-4.0 - (6.0 * j) / Length)), (10.0) * float(i))[1], 1.0])) for j in range(Length)] for i in range(Length)])
    Highfsum = np.array([[Highfrho[j][i] + Highfphi[j][i] for i in range(Length)] for j in range(Length)])
    Highfsum = Highfsum.T
    x_vals = [10.0 * i for i in range(Length)]
    y_vals = [10 ** (-4.0 - (6.0 * j) / float(Length)) for j in range(Length)]
    X, Y = np.meshgrid(x_vals, y_vals)
    plt.pcolormesh(X, Y, Highfsum, shading='auto', cmap='plasma')
    plt.colorbar(label="Signal over Background")
    plt.xlabel("Mass in 10 MeV")
    plt.ylabel("Epsilon Squared")
    plt.title("Rate of Signal to Background with Acceptance Effects")
    plt.yscale("log")
    plt.xscale("linear")
    plt.savefig("dingdop.png")
    plt.show()

def parralel(I, LL,Val):
    mA = 100.0
    mpi = mA / 3.0
    mV = mA / 1.8
    N = 100
    Length = LL
    Xaxis = [3.0 * (1 - float(i) / N) + 4.0 * 3.1415926 * float(i) / N for i in range(int(N))]
    rho = [decay_length_dark_photon_to_vector_boson(.01, ((240.0 / float(Length)) * (1 / x)), ((240.0 / float(Length)) * I) / 3.0, ((240.0 / float(Length)) * I) / 1.8, ((240.0 / float(Length)) * I), 3.0 / 4.0) / total_length(.01, ((240.0 / float(Length)) * (1 / x)), ((240.0 / float(Length)) * I) / 3.0, ((240.0 / float(Length)) * I) / 1.8, ((240.0 / float(Length)) * I)) for x in Xaxis]
    phi = [decay_length_dark_photon_to_vector_boson(.01, ((240.0 / float(Length)) * (1 / x)), ((240.0 / float(Length)) * I) / 3.0, ((240.0 / float(Length)) * I) / 1.8, ((240.0 / float(Length)) * I), 3.0 / 2.0) / total_length(.01, ((240.0 / float(Length)) * (1 / x)), ((240.0 / float(Length)) * I) / 3.0, ((240.0 / float(Length)) * I) / 1.8, ((240.0 / float(Length)) * I)) for x in Xaxis]
    TOT = 1.0
    CON = 3 * np.pi / (2 * 1 * (1 / 137.0459991))
    Highfrho = [TOT * min([CON * ratio((240.0 / float(Length)) * float(I)) * ((240.0 / float(Length)) * I) * (10 ** (-4.0 - (6.0 * j) / Length)) * rho[len(rho) - 1] * getSum(np.sqrt(10 ** (-4.0 - (6.0 * j) / Length)), (240.0 / float(Length)) * float(I),Val)[0], 1.0]) for j in range(Length)]
    Highfphi = [TOT * min([CON * ratio((240.0 / float(Length)) * float(I)) * ((240.0 / float(Length)) * I) * (10 ** (-4.0 - (6.0 * j) / Length)) * phi[len(phi) - 1] * getSum(np.sqrt(10 ** (-4.0 - (6.0 * j) / Length)), (240.0 / float(Length)) * float(I),Val)[1], 1.0]) for j in range(Length)]
    Highfsum = [Highfrho[j] + 1.0 * Highfphi[j] for j in range(Length)]
    with open("/sdf/group/hps/users/rodwyer1/run/reach_curves/optimization/outputText/output" + str(I) + "_"+str(Val)+".txt", "w") as f:
        sys.stdout = f
        print(Highfsum)
    sys.stdout = sys.__stdout__

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Sample script with command line arguments.")
    parser.add_argument('--II', type=int, required=False, help='Index Of Mass')
    parser.add_argument('--L', type=int, required=False, help='Length of Array')
    parser.add_argument('--Val', type=int, required=False, help='Value of Hyperparameter')
    args = parser.parse_args()
    #plt.plot([float(i) for i in range(100)],[getProb(float(i), .0005, 60) for i in range(100)],"r")
    #plt.savefig("Helper.png")

    #plot_ratio_for_mass(55,"hello_ding_10232025.png") 
    #makeBRplot()
    #plotAbundances()
    #parralel_aprime(args.II,args.L)
    parralel(args.II,args.L,args.Val)
