#!/usr/bin/env python3
import argparse
import numpy as np
import uproot
import awkward as ak
import matplotlib.pyplot as plt

# ------------------------------------------------------------
# Compute |p_e-| + |p_e+|   (scalar-sum definition)
# ------------------------------------------------------------
def psum_scalar(tree):
    arr = tree.arrays([
        "ele.track_.px_", "ele.track_.py_", "ele.track_.pz_",
        "pos.track_.px_", "pos.track_.py_", "pos.track_.pz_",
    ], library="ak")

    ex, ey, ez = (ak.to_numpy(arr["ele.track_.px_"]),
                  ak.to_numpy(arr["ele.track_.py_"]),
                  ak.to_numpy(arr["ele.track_.pz_"]))
    px, py, pz = (ak.to_numpy(arr["pos.track_.px_"]),
                  ak.to_numpy(arr["pos.track_.py_"]),
                  ak.to_numpy(arr["pos.track_.pz_"]))

    pe = np.sqrt(ex*ex + ey*ey + ez*ez)
    pp = np.sqrt(px*px + py*py + pz*pz)
    return np.asarray(pe + pp, dtype=float)


# ------------------------------------------------------------
# Compute |p_e- + p_e+|   (vector-sum magnitude definition)
# ------------------------------------------------------------
def psum_vector(tree):
    arr = tree.arrays([
        "ele.track_.px_", "ele.track_.py_", "ele.track_.pz_",
        "pos.track_.px_", "pos.track_.py_", "pos.track_.pz_",
    ], library="ak")

    ex, ey, ez = (ak.to_numpy(arr["ele.track_.px_"]),
                  ak.to_numpy(arr["ele.track_.py_"]),
                  ak.to_numpy(arr["ele.track_.pz_"]))
    px, py, pz = (ak.to_numpy(arr["pos.track_.px_"]),
                  ak.to_numpy(arr["pos.track_.py_"]),
                  ak.to_numpy(arr["pos.track_.pz_"]))

    sx = ex + px
    sy = ey + py
    sz = ez + pz
    return np.asarray(np.sqrt(sx*sx + sy*sy + sz*sz), dtype=float)


# ------------------------------------------------------------
# Main plotting routine
# ------------------------------------------------------------
def main():
    ap = argparse.ArgumentParser(description="Compare psum definitions computed from momenta.")
    ap.add_argument("--in", dest="infile", required=True, help="Input ROOT file (preselection style)")
    ap.add_argument("--tree", default="preselection", help="Tree name (default: preselection)")
    ap.add_argument("--bins", type=int, default=100, help="Histogram bins")
    ap.add_argument("--xmin", type=float, default=0.0, help="x min")
    ap.add_argument("--xmax", type=float, default=6.0, help="x max")
    ap.add_argument("--out", default="psum_compare.png", help="Output PNG")
    args = ap.parse_args()

    with uproot.open(args.infile) as f:
        if args.tree in f:
            t = f[args.tree]
        else:
            t = next(obj for _, obj in f.items() if getattr(obj, "classname", "").startswith("TTree"))

        # explicitly compute both, never use psum branch
        psum1 = psum_scalar(t)
        psum2 = psum_vector(t)

    # Remove NaN/inf
    psum1 = psum1[np.isfinite(psum1)]
    psum2 = psum2[np.isfinite(psum2)]

    plt.figure(figsize=(7.2,4.2))
    plt.hist(psum1, bins=args.bins, range=(args.xmin, args.xmax),
             histtype="step", color="C0", label=r"$|\vec p_{e^-}|+|\vec p_{e^+}|$")
    plt.hist(psum2, bins=args.bins, range=(args.xmin, args.xmax),
             histtype="step", color="C3", label=r"$|\vec p_{e^-}+\vec p_{e^+}|$")
    plt.xlabel(r"$p_{\mathrm{sum}}$ [GeV]")
    plt.ylabel("Events")
    plt.title("Comparison of psum definitions (computed directly)")
    plt.legend()
    plt.grid(alpha=0.3)
    plt.tight_layout()
    plt.savefig(args.out, dpi=140)
    print(f"[write] {args.out}")

if __name__ == "__main__":
    main()

