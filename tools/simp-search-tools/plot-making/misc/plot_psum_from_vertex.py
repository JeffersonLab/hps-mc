#!/usr/bin/env python3
import argparse
import numpy as np
import uproot
import awkward as ak
import matplotlib.pyplot as plt

def extract_xyz_from_record(rec):
    """Return (x,y,z) from an Awkward record with common field layouts."""
    flds = set(ak.fields(rec))
    # direct triplets
    for trip in (("fX","fY","fZ"), ("X","Y","Z"), ("x","y","z")):
        if all(c in flds for c in trip):
            return (ak.to_numpy(rec[trip[0]]),
                    ak.to_numpy(rec[trip[1]]),
                    ak.to_numpy(rec[trip[2]]))
    # nested coordinates container
    for cname in ("fCoordinates","fCoord","coords","Coord","coord"):
        if cname in flds:
            c = rec[cname]
            cf = set(ak.fields(c))
            for trip in (("fX","fY","fZ"), ("X","Y","Z"), ("x","y","z")):
                if all(cc in cf for cc in trip):
                    return (ak.to_numpy(c[trip[0]]),
                            ak.to_numpy(c[trip[1]]),
                            ak.to_numpy(c[trip[2]]))
    return None, None, None

def load_p1p2(tree):
    keys = set(tree.keys())
    needed = [k for k in ("vertex.p1_", "vertex.p2_") if k in keys]
    if len(needed) < 2:
        raise KeyError("vertex.p1_ and/or vertex.p2_ not found in the tree.")
    arr = tree.arrays(needed, library="ak")
    v1, v2 = arr["vertex.p1_"], arr["vertex.p2_"]
    ex, ey, ez = extract_xyz_from_record(v1)  # e-
    px, py, pz = extract_xyz_from_record(v2)  # e+
    if ex is None or px is None:
        raise KeyError("Could not recognize x/y/z fields inside vertex.p1_/p2_.")
    return ex, ey, ez, px, py, pz

def main():
    ap = argparse.ArgumentParser(description="Overlay psum from vertex P1(e-) and P2(e+): |pe-|+|pe+| vs |pe-+pe+|.")
    ap.add_argument("--in", dest="infile", required=True, help="Input ROOT file")
    ap.add_argument("--tree", default="preselection", help="Tree name (default: preselection)")
    ap.add_argument("--bins", type=int, default=100)
    ap.add_argument("--xmin", type=float, default=0.0)
    ap.add_argument("--xmax", type=float, default=6.0)
    ap.add_argument("--out", default="psum_from_vertex.png")
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

        ex, ey, ez, px, py, pz = load_p1p2(t)

    # scalar-sum definition: |pe-| + |pe+|
    pe = np.sqrt(ex*ex + ey*ey + ez*ez)
    pp = np.sqrt(px*px + py*py + pz*pz)
    psum_scalar = pe + pp

    # vector-sum magnitude: |pe- + pe+|
    sx, sy, sz = ex + px, ey + py, ez + pz
    psum_vector = np.sqrt(sx*sx + sy*sy + sz*sz)

    # clean and plot
    psum_scalar = psum_scalar[np.isfinite(psum_scalar)]
    psum_vector = psum_vector[np.isfinite(psum_vector)]

    plt.figure(figsize=(7.2, 4.2))
    plt.hist(psum_scalar, bins=args.bins, range=(args.xmin, args.xmax),
             histtype="step", label=r"$|\vec p_{e^-}|+|\vec p_{e^+}|$")
    plt.hist(psum_vector, bins=args.bins, range=(args.xmin, args.xmax),
             histtype="step", label=r"$|\vec p_{e^-}+\vec p_{e^+}|$")
    plt.xlabel(r"$p_{\mathrm{sum}}$ [GeV]")
    plt.ylabel("Events")
    plt.title("psum from vertex P1(e−), P2(e+)")
    plt.legend()
    plt.grid(alpha=0.3)
    plt.tight_layout()
    plt.savefig(args.out, dpi=140)
    print(f"[write] {args.out}")

if __name__ == "__main__":
    main()

