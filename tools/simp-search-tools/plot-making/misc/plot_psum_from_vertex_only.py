#!/usr/bin/env python3
import argparse, re
import numpy as np
import uproot
import awkward as ak
import matplotlib.pyplot as plt

def find_xyz_leaf_keys(all_keys, prefix):
    """
    Find leaf names for x/y/z under a given prefix (e.g. 'vertex.p1_').
    Tries:
      vertex.p1_.fX / fY / fZ
      vertex.p1_.X  / Y  / Z
      vertex.p1_.x  / y  / z
      and one extra level like vertex.p1_.fCoordinates.fX (etc.)
    Returns (kx, ky, kz) or raises KeyError.
    """
    patterns = [
        (re.compile(rf"^{re.escape(prefix)}(?:fX|X|x)$"),
         re.compile(rf"^{re.escape(prefix)}(?:fY|Y|y)$"),
         re.compile(rf"^{re.escape(prefix)}(?:fZ|Z|z)$")),
        (re.compile(rf"^{re.escape(prefix)}.*\.(?:fX|X|x)$"),
         re.compile(rf"^{re.escape(prefix)}.*\.(?:fY|Y|y)$"),
         re.compile(rf"^{re.escape(prefix)}.*\.(?:fZ|Z|z)$")),
    ]
    for rx, ry, rz in patterns:
        kx = next((k for k in all_keys if rx.match(k)), None)
        ky = next((k for k in all_keys if ry.match(k)), None)
        kz = next((k for k in all_keys if rz.match(k)), None)
        if kx and ky and kz:
            return kx, ky, kz
    raise KeyError(f"Could not find x/y/z leaves under '{prefix}'. Run with --dump-keys to inspect.")

def load_vertex_ep_xyz_strict(tree):
    """Strictly load e− (P1) and e+ (P2) 3-vectors from vertex.* leaves only."""
    keys = list(tree.keys())
    p1x, p1y, p1z = find_xyz_leaf_keys(keys, "vertex.p1_")
    p2x, p2y, p2z = find_xyz_leaf_keys(keys, "vertex.p2_")
    need = [p1x, p1y, p1z, p2x, p2y, p2z]
    arr = tree.arrays(need, library="ak")
    ex, ey, ez = ak.to_numpy(arr[p1x]), ak.to_numpy(arr[p1y]), ak.to_numpy(arr[p1z])
    px, py, pz = ak.to_numpy(arr[p2x]), ak.to_numpy(arr[p2y]), ak.to_numpy(arr[p2z])
    return ex, ey, ez, px, py, pz, f"vertex leaves ({p1x},{p1y},{p1z}) & ({p2x},{p2y},{p2z})"

def main():
    ap = argparse.ArgumentParser(description="Overlay psum from vertex P1(e−) and P2(e+), no fallback.")
    ap.add_argument("--in", dest="infile", required=True, help="Input ROOT file")
    ap.add_argument("--tree", default="preselection", help="Tree name (default: preselection)")
    ap.add_argument("--bins", type=int, default=100)
    ap.add_argument("--xmin", type=float, default=0.0)
    ap.add_argument("--xmax", type=float, default=6.0)
    ap.add_argument("--out", default="psum_from_vertex_only.png")
    ap.add_argument("--dump-keys", action="store_true", help="List branch keys and exit")
    args = ap.parse_args()

    with uproot.open(args.infile) as f:
        if args.tree in f:
            t = f[args.tree]
        else:
            t = next(obj for _, obj in f.items() if getattr(obj, "classname", "").startswith("TTree"))

        if args.dump_keys:
            print("\n".join(sorted(t.keys())))
            return

        ex, ey, ez, px, py, pz, src = load_vertex_ep_xyz_strict(t)

    # Compute both definitions from vertex P1/P2 components
    pe = np.sqrt(ex*ex + ey*ey + ez*ez)
    pp = np.sqrt(px*px + py*py + pz*pz)
    psum_scalar = pe + pp

    sx, sy, sz = ex + px, ey + py, ez + pz
    psum_vector = np.sqrt(sx*sx + sy*sy + sz*sz)

    # Clean and overlay
    psum_scalar = psum_scalar[np.isfinite(psum_scalar)]
    psum_vector = psum_vector[np.isfinite(psum_vector)]

    plt.figure(figsize=(7.2, 4.2))
    plt.hist(psum_scalar, bins=args.bins, range=(args.xmin, args.xmax),
             histtype="step", label=r"$|\vec p_{e^-}|+|\vec p_{e^+}|$")
    plt.hist(psum_vector, bins=args.bins, range=(args.xmin, args.xmax),
             histtype="step", label=r"$|\vec p_{e^-}+\vec p_{e^+}|$")
    plt.xlabel(r"$p_{\mathrm{sum}}$ [GeV]")
    plt.ylabel("Events")
    plt.title(f"psum from {src}")
    plt.legend()
    plt.grid(alpha=0.3)
    plt.tight_layout()
    plt.savefig(args.out, dpi=140)
    print(f"[info] source: {src}")
    print(f"[write] {args.out}")

if __name__ == "__main__":
    main()

