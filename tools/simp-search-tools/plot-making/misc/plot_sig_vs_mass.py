#!/usr/bin/env python3
# ======= plot_sig_vs_mass.py =======
# Usage examples:
#   python plot_sig_vs_mass.py --val 21 --eps2-value 1e-7
#   python plot_sig_vs_mass.py --val 21 --eps2-index 3
#
# Notes:
#  - Choose the interaction strength via --eps2-value (preferred) or --eps2-index.
#  - Data files are expected as: combined_sig_I{I}_L{L}_V{val}.txt

import argparse, os, re, glob, warnings
import numpy as np
import matplotlib.pyplot as plt

def epsilon2_grid(N: int) -> np.ndarray:
    j = np.arange(N, dtype=float)
    return 10.0 ** (-4.0 - 6.0 * j / float(N))

_float_token_re = re.compile(
    r'([+-]?(?:nan|inf))|([+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?)'
)
_float_line_re = re.compile(r'^[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$')

def extract_last_numeric_list(text: str) -> np.ndarray:
    spans, stack = [], []
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

    values = []
    for m in _float_token_re.finditer(payload):
        token = (m.group(1) or m.group(2))
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
                pass
    return np.asarray(values, dtype=float)

def parse_combined_file(path: str):
    with open(path, "r") as f:
        text = f.read()
    bg_frac = None
    for ln in text.splitlines():
        s = ln.strip()
        if s and _float_line_re.match(s):
            bg_frac = float(s)
            break
    if bg_frac is None:
        raise RuntimeError("Could not locate background fraction as a bare float line")
    Z_arr = extract_last_numeric_list(text)
    return float(bg_frac), Z_arr

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

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--base", default="/sdf/group/hps/users/rodwyer1/run/reach_curves/optimization/outputText")
    ap.add_argument("--out", default="sig_vs_mass.png")
    ap.add_argument("--L", type=int, default=50)
    ap.add_argument("--val", type=int, required=True)
    # choose epsilon by value or index
    ap.add_argument("--eps2-value", type=float, default=None,
                    help="Pick the ε² point closest to this value (preferred).")
    ap.add_argument("--eps2-index", type=int, default=None,
                    help="Pick this ε² index (0-based) if --eps2-value not given.")
    # bad-value handling
    ap.add_argument("--insane-abs-threshold", type=float, default=1e6,
                    help="Mask any |Z| above this; set <=0 to disable. Default 1e6.")
    args = ap.parse_args()

    mass_indices = collect_mass_indices(args.base, args.L, args.val)
    if not mass_indices:
        raise SystemExit(f"No files found for Val={args.val} at {args.base}")

    # probe first file to learn length of epsilon grid
    probe = os.path.join(args.base, f"combined_sig_I{mass_indices[0]}_L{args.L}_V{args.val}.txt")
    try:
        _, Z0 = parse_combined_file(probe)
    except Exception as e:
        raise SystemExit(f"Failed to parse {probe}: {e}")
    N_eps = len(Z0)
    eps2 = epsilon2_grid(N_eps)

    # choose epsilon index
    if args.eps2_value is not None:
        k = int(np.argmin(np.abs(np.log10(eps2) - np.log10(args.eps2_value))))
        chosen_eps2 = eps2[k]
    else:
        if args.eps2_index is None:
            raise SystemExit("Provide either --eps2-value or --eps2-index")
        if not (0 <= args.eps2_index < N_eps):
            raise SystemExit(f"--eps2-index must be in [0,{N_eps-1}]")
        k = args.eps2_index
        chosen_eps2 = eps2[k]

    masses = []
    Zvals  = []

    for I in mass_indices:
        path = os.path.join(args.base, f"combined_sig_I{I}_L{args.L}_V{args.val}.txt")
        try:
            _, Z = parse_combined_file(path)
            if len(Z) != N_eps:
                print(f"[warn] length mismatch in {path}, skipping")
                continue
            z = float(Z[k])
        except Exception as e:
            print(f"[warn] {path}: {e}")
            continue

        # mask non-finite / absurd values
        if not np.isfinite(z):
            z = np.nan
        elif args.insane_abs_threshold > 0 and abs(z) > args.insane_abs_threshold:
            z = np.nan

        masses.append(mass_from_index(I, args.L))
        Zvals.append(z)

    masses = np.asarray(masses, dtype=float)
    Zvals  = np.asarray(Zvals, dtype=float)

    # sort by mass just in case
    order = np.argsort(masses)
    masses = masses[order]
    Zvals  = Zvals[order]

    # plot
    plt.figure(figsize=(7.0, 4.0))
    finite = np.isfinite(Zvals)
    if np.any(finite):
        plt.plot(masses[finite], [max([z,-.5]) for z in Zvals[finite]], '-', lw=2)
        plt.plot(masses[finite], [max([z,-.5]) for z in Zvals[finite]], 'o', ms=4)
    # mark missing/bad as gaps (no line), but show markers if desired:
    bad = ~finite
    if np.any(bad):
        plt.plot(masses[bad], np.zeros(np.count_nonzero(bad))*np.nan, 'o', alpha=0.0)  # keep x extent

    plt.xlabel("reconstructed mass [MeV]")
    plt.ylabel("significance")
    plt.title(f"Significance vs mass @ ε²≈{chosen_eps2:.2e}, Val={args.val}")
    plt.grid(True, alpha=0.25)
    plt.tight_layout()
    plt.savefig(args.out, dpi=160)
    print(f"[write] {args.out}")

if __name__ == "__main__":
    main()

