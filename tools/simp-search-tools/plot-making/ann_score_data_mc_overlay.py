#!/usr/bin/env python3
import os
import sys
import gc
import argparse

import numpy as np
import awkward as ak
import uproot
import torch
from torch import nn
import ROOT as r

r.gROOT.SetBatch(True)
r.gStyle.SetOptStat(0)
r.gStyle.SetPadTopMargin(0.04)
r.gStyle.SetPadBottomMargin(0.12)
r.gStyle.SetPadRightMargin(0.05)
r.gStyle.SetPadLeftMargin(0.12)

# ============================================================
# User-editable configuration, copied from MC_data_comparison_standalone.py
# ============================================================
DO_RATIO = False
OUTDIR = os.getcwd()

DATA_FILE = "/sdf/data/hps/physics2021/preselection/v8/data_1pc_z0_calb_run_by_run/merged_hps_014713_job462.root" 
#"/sdf/data/hps/physics2021/preselection/v2/data/merged_hps_014272_job64.root"
TRITRIG_FILE = "/sdf/data/hps/users/sgaiser/analysis/pass_v9/tritrig_HPS_Run2021Pass1_v9_14272_hitSmearKill_1000files.root"
WAB_FILE = "/sdf/data/hps/users/sgaiser/analysis/pass_v9/wab_HPS_Run2021Pass1_v9_14272_hitSmearKill_2000files.root"
TREE_NAME = "preselection"
LUMI_14272 = 0.012653

ANN_HIST_CONFIG = {
    "nbins": 100,
    "xmin": 0.0,
    "xmax": 1.0,
    "xtitle": "ANN response score",
}


# ============================================================
# ANN utilities, copied from write_roc_overlay_all5.py
# ============================================================
class ANNClassifier(nn.Module):
    """Architecture matched to classifier_adv_2021_v9_pass5_run42QualCuts_*.pt."""
    def __init__(self, in_features: int):
        super().__init__()
        self.net = nn.Sequential(
            nn.Linear(in_features, 264, bias=False),
            nn.BatchNorm1d(264),
            nn.LeakyReLU(),
            nn.Linear(264, 264, bias=False),
            nn.BatchNorm1d(264),
            nn.LeakyReLU(),
            nn.Linear(264, 128, bias=False),
            nn.BatchNorm1d(128),
            nn.LeakyReLU(),
            nn.Linear(128, 128, bias=False),
            nn.BatchNorm1d(128),
            nn.LeakyReLU(),
            nn.Linear(128, 64, bias=False),
            nn.BatchNorm1d(64),
            nn.LeakyReLU(),
            nn.Linear(64, 1),
        )

    def forward(self, x):
        return self.net(x)


def _as_float32_col(arr):
    return np.asarray(arr, dtype=np.float32).reshape(-1, 1)


def ann_predict_score(model, scaler_mean, scaler_scale, X, batch_size=100000):
    n = X.shape[0]
    scores = np.empty(n, dtype=np.float32)
    for start in range(0, n, batch_size):
        end = min(start + batch_size, n)
        X_chunk = np.nan_to_num(X[start:end], nan=0.0, posinf=0.0, neginf=0.0)
        X_scaled = (X_chunk.astype(np.float32) - scaler_mean) / scaler_scale
        X_tensor = torch.from_numpy(X_scaled)
        with torch.no_grad():
            scores[start:end] = torch.sigmoid(model(X_tensor)).cpu().numpy().ravel()
    return scores


def load_ann_model(whichmass):
    ann_model_path = f"/sdf/group/hps/users/rodwyer1/run/reach_curves/annstuff/classifier_adv_2021_v9_pass5_run42QualCuts_{int(whichmass)}.pt"
    ann_scaler_path = f"/sdf/group/hps/users/rodwyer1/run/reach_curves/annstuff/scaler_arrays_{int(whichmass)}.npz"

    sys.modules['numpy._core'] = np.core
    ann_scaler = np.load(ann_scaler_path)
    ann_scaler_mean = ann_scaler["mean"].astype(np.float32)
    ann_scaler_scale = ann_scaler["scale"].astype(np.float32)

    ann_model = ANNClassifier(in_features=34)
    ann_state = torch.load(ann_model_path, map_location="cpu")
    ann_model.load_state_dict(ann_state)
    ann_model.eval()
    return ann_model, ann_scaler_mean, ann_scaler_scale


# ============================================================
# Branch loading and feature building, copied from write_roc_overlay_all5.py
# but adapted to run directly on the data-like ROOT files used in
# MC_data_comparison_standalone.py
# ============================================================
def common_branch_list():
    return [
        "psum",
        "vertex.pos_",
        "ele.track_.n_hits_", "ele.track_.d0_", "ele.track_.phi0_",
        "ele.track_.z0_", "ele.track_.tan_lambda_", "ele.track_.px_",
        "ele.track_.py_", "ele.track_.pz_", "ele.track_.chi2_",
        "ele.track_.x_at_ecal_", "ele.track_.y_at_ecal_", "ele.track_.z_at_ecal_",
        "pos.track_.n_hits_", "pos.track_.d0_", "pos.track_.phi0_",
        "pos.track_.z0_", "pos.track_.tan_lambda_", "pos.track_.px_",
        "pos.track_.py_", "pos.track_.pz_", "pos.track_.chi2_",
        "pos.track_.x_at_ecal_", "pos.track_.y_at_ecal_", "pos.track_.z_at_ecal_",
        "vertex.chi2_",
        "vtx_proj_sig", "vtx_proj_x_sig", "vtx_proj_y_sig",
        "ele.track_.hit_layers_", "pos.track_.hit_layers_",
        "ele_L1_iso_significance", "pos_L1_iso_significance",
    ]


def build_ann_matrix(arrays):
    vertex_pos = ak.to_numpy(arrays["vertex.pos_"])
    feats = [
        _as_float32_col(vertex_pos["fX"]),
        _as_float32_col(vertex_pos["fY"]),
        _as_float32_col(vertex_pos["fZ"]),
        _as_float32_col(ak.to_numpy(arrays["psum"])),
    ]
    track_keys = [
        "n_hits_", "d0_", "phi0_", "z0_", "tan_lambda_", "px_",
        "py_", "pz_", "chi2_", "x_at_ecal_", "y_at_ecal_", "z_at_ecal_"
    ]
    for key in track_keys:
        feats.append(_as_float32_col(ak.to_numpy(arrays[f"ele.track_.{key}"])))
    for key in track_keys:
        feats.append(_as_float32_col(ak.to_numpy(arrays[f"pos.track_.{key}"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["vertex.chi2_"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["vtx_proj_sig"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["vtx_proj_x_sig"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["vtx_proj_y_sig"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["ele_L1_iso_significance"])))
    feats.append(_as_float32_col(ak.to_numpy(arrays["pos_L1_iso_significance"])))
    return np.hstack(feats)


def derive_l1l1_from_ak(hit_layers):
    if hit_layers is None:
        return None
    hasL0 = ak.to_numpy(ak.any(hit_layers == 0, axis=-1))
    hasL1 = ak.to_numpy(ak.any(hit_layers == 1, axis=-1))
    return np.asarray(hasL0 & hasL1, dtype=bool)


def build_common_mask(psum, l1l1_mask, proj_sig, proj_fixed):
    psum_mask = (psum >= 1.5) & (psum <= 3.0)
    proj_mask = (proj_sig < proj_fixed)
    return psum_mask & l1l1_mask & proj_mask


def load_arrays(root_path, tree_name):
    with uproot.open(root_path) as f:
        tree = f[tree_name]
        arrays = tree.arrays(common_branch_list(), library="ak", how=dict)
    return arrays


def load_sample_for_ann(root_path, tree_name, proj_fixed):
    arrays = load_arrays(root_path, tree_name)
    psum = ak.to_numpy(arrays["psum"])
    proj_sig = ak.to_numpy(arrays["vtx_proj_sig"])

    ele_L1L1 = derive_l1l1_from_ak(arrays.get("ele.track_.hit_layers_"))
    pos_L1L1 = derive_l1l1_from_ak(arrays.get("pos.track_.hit_layers_"))
    if ele_L1L1 is None:
        ele_L1L1 = np.ones_like(psum, dtype=bool)
    if pos_L1L1 is None:
        pos_L1L1 = np.ones_like(psum, dtype=bool)

    l1l1_mask = ele_L1L1 & pos_L1L1
    common_mask = build_common_mask(psum, l1l1_mask, proj_sig, proj_fixed)
    X_ann = build_ann_matrix(arrays)
    return X_ann, common_mask


# ============================================================
# ROOT histogram / drawing helpers copied from MC_data_comparison_standalone.py
# ============================================================
def get_scale_factor(sample, lumi=LUMI_14272):
    if "wab" in sample or "WAB" in sample:
        xsec = 8.249 * 1e10
        N_gen = 2000. * 20 * 10000
    elif "tritrig" in sample:
        xsec = 4.025 * 1e9
        N_gen = 1000. * 8 * 10000
    elif "rad" in sample:
        xsec = 3.44 * 1e7
        N_gen = 387. * 5 * 10000
    else:
        return 1.0
    return (xsec * lumi) / N_gen


def sanitize(name):
    out = name
    for old, new in [
        (".", "_"),
        ("(", ""),
        (")", ""),
        ("[", "_"),
        ("]", ""),
        ("{", "_"),
        ("}", ""),
        ("+", "plus"),
        ("-", "minus"),
        ("*", "times"),
        ("/", "div"),
        (" ", ""),
        (":", "_"),
    ]:
        out = out.replace(old, new)
    return out


def make_hist_from_scores(scores, weights, tag, nbins, xmin, xmax):
    hname = f"h_ann_score_{sanitize(tag)}"
    hist = r.TH1F(hname, hname, nbins, xmin, xmax)
    hist.Sumw2()
    for val, wgt in zip(scores, weights):
        hist.Fill(float(val), float(wgt))
    hist.SetDirectory(0)
    return hist


def normalize(hist):
    integral = hist.Integral()
    if integral > 0:
        hist.Scale(1.0 / integral)


def style_hist(hist, color, width=2):
    hist.SetLineColor(color)
    hist.SetMarkerColor(color)
    hist.SetLineWidth(width)


def make_ratio_hist(num, den, name):
    ratio = num.Clone(name)
    ratio.SetDirectory(0)
    ratio.Divide(den)
    return ratio


def draw_comparison(h_data, h_mc, outbase, xtitle, do_ratio=False):
    pdf_name = os.path.join(OUTDIR, outbase + ".pdf")
    png_name = os.path.join(OUTDIR, outbase + ".png")

    style_hist(h_data, r.kBlack, 3)
    style_hist(h_mc, r.kRed + 1, 3)

    leg = r.TLegend(0.58, 0.75, 0.90, 0.90)
    leg.SetBorderSize(0)
    leg.SetFillStyle(0)
    leg.AddEntry(h_data, "run 14272 pass5_v9", "l")
    leg.AddEntry(h_mc, "tritrig + wab", "l")

    if do_ratio:
        can = r.TCanvas("can_ann_score", "can_ann_score", 800, 800)
        pad1 = r.TPad("pad1_ann_score", "", 0.0, 0.30, 1.0, 1.0)
        pad2 = r.TPad("pad2_ann_score", "", 0.0, 0.0, 1.0, 0.30)

        pad1.SetBottomMargin(0.02)
        pad2.SetTopMargin(0.03)
        pad2.SetBottomMargin(0.32)

        pad1.Draw()
        pad2.Draw()

        pad1.cd()
        ymax = 1.20 * max(h_data.GetMaximum(), h_mc.GetMaximum())
        h_data.SetMaximum(ymax)
        h_data.SetTitle(f";{xtitle};Events")
        h_data.Draw("hist")
        h_mc.Draw("hist same")
        leg.Draw()

        pad2.cd()
        ratio = make_ratio_hist(h_mc, h_data, "ratio_ann_score")
        style_hist(ratio, r.kBlue + 1, 2)
        ratio.SetTitle("")
        ratio.GetYaxis().SetTitle("MC/data")
        ratio.GetXaxis().SetTitle(xtitle)
        ratio.GetYaxis().SetNdivisions(505)
        ratio.GetYaxis().SetTitleSize(0.10)
        ratio.GetYaxis().SetTitleOffset(0.45)
        ratio.GetYaxis().SetLabelSize(0.09)
        ratio.GetXaxis().SetTitleSize(0.12)
        ratio.GetXaxis().SetLabelSize(0.10)
        ratio.SetMinimum(0.59)
        ratio.SetMaximum(1.43)
        ratio.Draw("hist")

        line = r.TLine(ANN_HIST_CONFIG["xmin"], 1.0, ANN_HIST_CONFIG["xmax"], 1.0)
        line.SetLineStyle(2)
        line.Draw("same")

        can.SaveAs(pdf_name)
        can.SaveAs(png_name)
    else:
        can = r.TCanvas("can_ann_score", "can_ann_score", 800, 600)
        normalize(h_data)
        normalize(h_mc)
        ymax = 1.20 * max(h_data.GetMaximum(), h_mc.GetMaximum())
        h_data.SetMaximum(ymax)
        h_data.SetTitle(f";{xtitle};normalized")
        h_data.Draw("hist")
        h_mc.Draw("hist same")
        leg.Draw()
        can.SaveAs(pdf_name)
        can.SaveAs(png_name)


# ============================================================
# Main
# ============================================================
def main():
    ap = argparse.ArgumentParser(description="Plot ANN response score for data and data-like MC.")
    ap.add_argument("--mass", type=float, default=150.0, help="Mass hypothesis in MeV used to choose the ANN model. Default: 150")
    ap.add_argument("--proj-fixed", type=float, default=10.0, help="Fixed proj_sig cut used in the common preselection. Default: 10")
    ap.add_argument("--outfile-base", type=str, default=None, help="Base name for output files, without extension")
    ap.add_argument("--data-file", type=str, default=DATA_FILE, help="Data ROOT file")
    ap.add_argument("--tritrig-file", type=str, default=TRITRIG_FILE, help="TriTrig ROOT file")
    ap.add_argument("--wab-file", type=str, default=WAB_FILE, help="WAB ROOT file")
    ap.add_argument("--tree-name", type=str, default=TREE_NAME, help="TTree name")
    ap.add_argument("--do-ratio", action="store_true", help="Draw MC/data ratio panel instead of normalized overlays")
    ap.add_argument("--debug", action="store_true", help="Print event counts and bookkeeping")
    args = ap.parse_args()

    os.makedirs(OUTDIR, exist_ok=True)

    whichmass = 1.8 * args.mass / 3.0
    ann_model, ann_scaler_mean, ann_scaler_scale = load_ann_model(whichmass)

    X_data, mask_data = load_sample_for_ann(args.data_file, args.tree_name, args.proj_fixed)
    X_tritrig, mask_tritrig = load_sample_for_ann(args.tritrig_file, args.tree_name, args.proj_fixed)
    X_wab, mask_wab = load_sample_for_ann(args.wab_file, args.tree_name, args.proj_fixed)

    ann_data = ann_predict_score(ann_model, ann_scaler_mean, ann_scaler_scale, X_data)
    ann_tritrig = ann_predict_score(ann_model, ann_scaler_mean, ann_scaler_scale, X_tritrig)
    ann_wab = ann_predict_score(ann_model, ann_scaler_mean, ann_scaler_scale, X_wab)

    del X_data, X_tritrig, X_wab
    gc.collect()

    tritrig_sf = get_scale_factor("tritrig", LUMI_14272)
    wab_sf = get_scale_factor("wab", LUMI_14272)

    h_data = make_hist_from_scores(
        ann_data[mask_data],
        np.ones(np.count_nonzero(mask_data), dtype=np.float64),
        "data",
        ANN_HIST_CONFIG["nbins"],
        ANN_HIST_CONFIG["xmin"],
        ANN_HIST_CONFIG["xmax"],
    )
    h_tritrig = make_hist_from_scores(
        ann_tritrig[mask_tritrig],
        np.full(np.count_nonzero(mask_tritrig), tritrig_sf, dtype=np.float64),
        "tritrig",
        ANN_HIST_CONFIG["nbins"],
        ANN_HIST_CONFIG["xmin"],
        ANN_HIST_CONFIG["xmax"],
    )
    h_wab = make_hist_from_scores(
        ann_wab[mask_wab],
        np.full(np.count_nonzero(mask_wab), wab_sf, dtype=np.float64),
        "wab",
        ANN_HIST_CONFIG["nbins"],
        ANN_HIST_CONFIG["xmin"],
        ANN_HIST_CONFIG["xmax"],
    )

    h_mc = h_tritrig.Clone("h_ann_score_mc_total")
    h_mc.SetDirectory(0)
    h_mc.Add(h_wab)

    outfile_base = args.outfile_base
    if outfile_base is None:
        outfile_base = f"ann_score_data_mc_m{int(round(args.mass))}"

    draw_comparison(h_data, h_mc, outfile_base, ANN_HIST_CONFIG["xtitle"], do_ratio=args.do_ratio or DO_RATIO)

    if args.debug:
        print("Using output directory:", OUTDIR)
        print("Using tritrig scale factor =", tritrig_sf)
        print("Using wab scale factor     =", wab_sf)
        print("Data events after common preselection    =", int(np.count_nonzero(mask_data)))
        print("TriTrig events after common preselection =", int(np.count_nonzero(mask_tritrig)))
        print("WAB events after common preselection     =", int(np.count_nonzero(mask_wab)))
        print("Saved plot base:", os.path.join(OUTDIR, outfile_base))


if __name__ == "__main__":
    main()

