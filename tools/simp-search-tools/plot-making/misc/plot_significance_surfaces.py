#!/usr/bin/env python3
import argparse, os, re, ast, glob, numpy as np, matplotlib.pyplot as plt, math, warnings

# -------- helpers --------

def epsilon2_grid(N: int) -> np.ndarray:
    j = np.arange(N, dtype=float)
    return 10.0 ** (-4.0 - 6.0 * j / float(N))

_float_token_re = re.compile(
    r'([+-]?(?:nan|inf))|([+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?)'
)

def extract_last_numeric_list(text: str) -> np.ndarray:
    """Return the numeric list represented by the *last* [...] block in text.
       Tolerates 'nan'/'inf' tokens and scientific notation.
    """
    # find last [...] span by counting brackets
    spans = []
    stack = []
    for i, ch in enumerate(text):
        if ch == '[':
            stack.append(i)
        elif ch == ']':
            if stack:
                start = stack.pop()
                spans.append((start, i+1))
    if not spans:
        raise ValueError("No bracketed list found")
    start, end = spans[-1]
    payload = text[start:end]

    # Extract numeric tokens
    values = []
    for m in _float_token_re.finditer(payload):
        token = m.group(1) or m.group(2)
        if token is None:
            continue
        t = token.lower()
        if t in ('nan', '+nan', '-nan'):
            values.append(np.nan)
        elif t in ('inf', '+inf'):
            values.append(np.inf)
        elif t == '-inf':
            values.append(-np.inf)
        else:
            try:
                values.append(float(token))
            except Exception:
                # ignore anything odd
                pass
    return np.asarray(values, dtype=float)

_float_line_re = re.compile(r'^[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$')

def parse_combined_file(path: str):
    """Robustly parse combined_sig file that may contain labels like 'Signal:' etc.
       Returns: (sig_arr or None, bg_frac float, Z_arr).
    """
    with open(path, "r") as f:
        text = f.read()

    # background fraction: first line that's a bare float
    bg_frac = None
    for ln in text.splitlines():
        s = ln.strip()
        if not s:
            continue
        if _float_line_re.match(s):
            bg_frac = float(s)
            break
    if bg_frac is None:
        raise RuntimeError("Could not locate background fraction as a bare float line")

    # significance array: last bracketed numeric list
    Z_arr = extract_last_numeric_list(text)

    # signal array: first bracketed numeric list (optional)
    sig_arr = None
    try:
        # Find first block
        start = text.index('[')
        depth = 0
        end = None
        for i in range(start, len(text)):
            if text[i] == '[':
                depth += 1
            elif text[i] == ']':
                depth -= 1
                if depth == 0:
                    end = i+1
                    break
        if end is not None:
            # Parse like Z but from the first block
            sig_arr = extract_last_numeric_list(text[start:end])
    except Exception:
        pass

    return sig_arr, float(bg_frac), Z_arr

def collect_mass_indices(base: str, L: int, val: int):
    pat = os.path.join(base, f"combined_sig_I*_L{L}_V{val}.txt")
    files = glob.glob(pat)
    indices = []
    for fp in files:
        m = re.search(r"_I(\d+)_L"+str(L)+r"_V"+str(val)+r"\.txt$", fp)
        if m:
            indices.append(int(m.group(1)))
    return sorted(set(indices))

def mass_from_index(I: int, L: int) -> float:
    return 200.0/float(L) * float(I)

def edges_from_centers(centers: np.ndarray) -> np.ndarray:
    if centers.size == 1:
        return np.array([centers[0]-1.0, centers[0]+1.0])
    mids = 0.5*(centers[1:] + centers[:-1])
    first = centers[0] - (mids[0] - centers[0])
    last  = centers[-1] + (centers[-1] - mids[-1])
    return np.r_[first, mids, last]

# -------- main --------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--base", default="/sdf/group/hps/users/rodwyer1/run/reach_curves/optimization/outputText")
    ap.add_argument("--outdir", default="./sig_maps")
    ap.add_argument("--L", type=int, default=50)
    ap.add_argument("--val-min", type=int, default=0)
    ap.add_argument("--val-max", type=int, default=25)
    args = ap.parse_args()

    os.makedirs(args.outdir, exist_ok=True)

    for val in range(args.val_min, args.val_max + 1):
        mass_indices = collect_mass_indices(args.base, args.L, val)
        if not mass_indices:
            print(f"[warn] No files found for Val={val}. Skipping.")
            continue

        sample_path = os.path.join(args.base, f"combined_sig_I{mass_indices[0]}_L{args.L}_V{val}.txt")
        _, _, Z_sample = parse_combined_file(sample_path)
        N_eps = len(Z_sample)
        eps2 = epsilon2_grid(N_eps)

        masses = np.array([mass_from_index(I, args.L) for I in mass_indices], dtype=float)
        Zmat = np.full((N_eps, len(masses)), np.nan, dtype=float)

        for j, I in enumerate(mass_indices):
            path = os.path.join(args.base, f"combined_sig_I{I}_L{args.L}_V{val}.txt")
            try:
                _, _, Z = parse_combined_file(path)
            except Exception as e:
                print(f"[warn] Failed to parse {path}: {e}")
                continue
            if len(Z) != N_eps:
                print(f"[warn] Length mismatch in {path} (got {len(Z)} != {N_eps}); skipping.")
                continue
            Zmat[:, j] = [max([zz,-.25]) for zz in Z]

        Zmask = np.ma.masked_invalid(Zmat)
        # Matplotlib 3.1-compatible way to set 'bad' color:
        import copy
        cmap = copy.copy(plt.cm.get_cmap('viridis'))
        cmap.set_bad('white')
        

        plt.figure(figsize=(7.6, 5.2))
        x_edges = edges_from_centers(masses)

        logy = np.log10(eps2)
        if len(eps2) > 1:
            dy = np.diff(logy).mean()
            y_edges = 10**(np.r_[logy[0]-0.5*dy, 0.5*(logy[1:]+logy[:-1]), logy[-1]+0.5*dy])
        else:
            y_edges = np.array([eps2[0]/1.5, eps2[0]*1.5])

        mesh = plt.pcolormesh(x_edges, y_edges, Zmask, shading='auto', cmap=cmap)
        cbar = plt.colorbar(mesh)
        cbar.set_label("significance")

        finite_Z = Zmask.compressed()
        if finite_Z.size:
            zmax = np.nanmax(finite_Z)
            levels = np.arange(0, int(np.floor(zmax)) + 1, 1)
            if len(levels) >= 1:
                Xc, Yc = np.meshgrid(masses, eps2)
                with warnings.catch_warnings():
                    warnings.simplefilter("ignore", category=UserWarning)
                    cs = plt.contour(Xc, Yc, Zmask.filled(np.nan), levels=levels, linewidths=0.8, colors='k')
                plt.clabel(cs, inline=True, fontsize=8, fmt="%d")

        plt.yscale('log')
        plt.xlabel("reconstructed mass [MeV]")
        plt.ylabel(r"$\epsilon^2$")
        plt.title(f"Significance vs mass, epsilon^2  (Val={val})")

        out_png = os.path.join(args.outdir, f"sig_map_Val{val}.png")
        plt.savefig(out_png, dpi=160, bbox_inches="tight")
        plt.close()
        print(f"[write] {out_png}")

if __name__ == "__main__":
    main()

