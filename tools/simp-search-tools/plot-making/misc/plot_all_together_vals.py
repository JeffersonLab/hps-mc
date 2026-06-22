#!/usr/bin/env python3
import argparse, os, re, glob, numpy as np, matplotlib.pyplot as plt, warnings, copy

# ---------- helpers (from your current flow) ----------
_float_line_re = re.compile(r'^[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?$')
_float_token_re = re.compile(
    r'([+-]?(?:nan|inf))|([+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?)'
)

def epsilon2_grid(N: int) -> np.ndarray:
    j = np.arange(N, dtype=float)
    return 10.0 ** (-4.0 - 6.0 * j / float(N))

def extract_last_numeric_list(text: str) -> np.ndarray:
    # find last [...] span by counting brackets
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
                pass
    return np.asarray(values, dtype=float)

def parse_combined_file(path: str):
    with open(path, "r") as f:
        text = f.read()
    bg_frac = None
    for ln in text.splitlines():
        s = ln.strip()
        if not s:
            continue
        if _float_line_re.match(s):
            bg_frac = float(s)
            break
    if bg_frac is None:
        raise RuntimeError("Could not locate background fraction")
    Z_arr = extract_last_numeric_list(text)
    return None, float(bg_frac), Z_arr  # keep signature parity

def collect_mass_indices(base: str, L: int, val: int):
    pat = os.path.join(base, f"combined_sig_I*_L{L}_V{val}.txt")
    files = glob.glob(pat)
    indices = []
    for fp in files:
        m = re.search(rf"_I(\d+)_L{L}_V{val}\.txt$", fp)
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

# ---------- main: overlay one iso-level across all V ----------
def main():
    ap = argparse.ArgumentParser(description="Overlay Z=level contour for all V on one plot")
    ap.add_argument("--base", default="/sdf/group/hps/users/rodwyer1/run/reach_curves/optimization/outputText")
    ap.add_argument("--out", default="./sig_maps/iso_overlay_Z8.png")
    ap.add_argument("--L", type=int, default=50)
    ap.add_argument("--val-min", type=int, default=0)
    ap.add_argument("--val-max", type=int, default=24)
    ap.add_argument("--level", type=float, default=6.0, help="Significance level for the iso-contour")
    ap.add_argument("--dpi", type=int, default=180)
    args = ap.parse_args()

    # Gather a reference epsilon2 grid length by peeking at any available file
    ref_eps = None
    ref_masses = None

    plt.figure(figsize=(8.6, 6.0))
    styles = ['-', '--', '-.', ':']
    cmap = plt.cm.get_cmap('tab20')  # enough distinct colors

    plotted_any = False
    legend_entries = []

    for val in range(args.val_min, args.val_max + 1):
        mass_indices = collect_mass_indices(args.base, args.L, val)
        if not mass_indices:
            print(f"[warn] No files for V={val}, skipping")
            continue

        # read one file to learn epsilon2 dimension
        sample_path = os.path.join(args.base, f"combined_sig_I{mass_indices[0]}_L{args.L}_V{val}.txt")
        try:
            _, _, Z_sample = parse_combined_file(sample_path)
        except Exception as e:
            print(f"[warn] parse fail for V={val} sample {sample_path}: {e}")
            continue

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
                print(f"[warn] Length mismatch in {path} (got {len(Z)} != {N_eps}); skipping this I")
                continue
            # cap crazy negatives a bit (like your existing)
            Zmat[:, j] = [max(zz, -0.25) for zz in Z]

        Zmask = np.ma.masked_invalid(Zmat)

        # Build center grids for contour
        Xc, Yc = np.meshgrid(masses, eps2)

        with warnings.catch_warnings():
            warnings.simplefilter("ignore", category=UserWarning)
            cs = plt.contour(
                Xc, Yc, Zmask.filled(np.nan),
                levels=[args.level],
                linewidths=1.8,
                colors=[cmap((val - args.val_min) % cmap.N)],
                linestyles=styles[(val - args.val_min) % len(styles)]
            )

        if len(cs.allsegs[0]) > 0:
            plotted_any = True
            # make a proxy line for legend
            line = plt.Line2D([0], [0],
                              color=cmap((val - args.val_min) % cmap.N),
                              linestyle=styles[(val - args.val_min) % len(styles)],
                              linewidth=2.0)
            legend_entries.append((line, f"V={val} (Z={args.level:g})"))

    if not plotted_any:
        print("[error] No contours were drawn at the requested level. Try lowering --level or checking inputs.")
        return

    # Axes/labels
    plt.yscale('log')
    plt.xlabel("reconstructed mass [MeV]")
    plt.ylabel(r"$\epsilon^2$")
    plt.title(f"Iso-significance overlay: Z = {args.level:g} for all V in [{args.val_min},{args.val_max}]")

    # tidy legend (cap to reasonable columns)
    if legend_entries:
        handles, labels = zip(*legend_entries)
        ncols = 2 if len(legend_entries) <= 16 else 3
        plt.legend(
            handles,
            labels,
            fontsize=9,
            ncol=ncols,
            frameon=True,
            loc='lower left'  # <-- moved here
        )

    os.makedirs(os.path.dirname(args.out), exist_ok=True)
    plt.savefig(args.out, dpi=args.dpi, bbox_inches="tight")
    plt.close()
    print(f"[write] {args.out}")

if __name__ == "__main__":
    main()

