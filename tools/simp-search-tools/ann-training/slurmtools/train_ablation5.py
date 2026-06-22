#!/usr/bin/env python3
"""
train_ablation.py
─────────────────
Standalone replacement for ablation_template.ipynb.
All notebook logic is reproduced faithfully; papermill is gone.

CLI (called by run_ablation_slurm.sh):
    python train_ablation.py \\
        --probe-idx      <full-list index of feature to probe, or -1 for baseline> \\
        --excluded-json  '<JSON list of full-list indices already culled>' \\
        --mass           <int> \\
        --output-dir     <path>

The feature at --probe-idx is dropped ON TOP OF --excluded-json, so the model
trains on:  all features  MINUS  excluded_baseline  MINUS  probe_feature.

Key outputs
───────────
  <output-dir>/result_<tag>.json               metrics JSON
  classifier_adv_..._<tag>.pt                  best adversarially trained classifier
  adversary_adv_..._<tag>.pt                   best adversary
  scaler_..._<MASS>.pkl                        fitted StandardScaler
  diag_score_hist_excl<idx>.png
  diag_ratio_panel_excl<idx>.png
  diag_roc_excl<idx>.png
  SUMMARY line on stdout (grep-able)
"""

import argparse
import copy
import json
import os
import random
import sys
import gc
from pathlib import Path

import joblib
import matplotlib
matplotlib.use("Agg")          # headless on compute nodes
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import sklearn
import sklearn.metrics
import torch
from torch import nn, optim
from torch.utils.data import TensorDataset, DataLoader
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler

import mytools

# ── Windowed training helpers (mirror ANN_NHP2.py exactly) ───────────────────

_BCE_loss_unreduced = nn.BCEWithLogitsLoss(reduction="none")
_CE_loss            = nn.CrossEntropyLoss(reduction="none")
_BCE_pretrain       = nn.BCEWithLogitsLoss(reduction="none")


def _full_loss(output_clas, output_adv, target_clas, target_adv, w, lambda_, clas_mask):
    """Combined classifier + adversary loss with mass-windowed classifier BCE."""
    clas_w = torch.where(w > 0, w, torch.ones_like(w)) * clas_mask
    loss1  = (_BCE_loss_unreduced(output_clas, target_clas) * clas_w).sum() \
             / clas_w.sum().clamp(min=1)
    nonzero = w > 0
    loss2  = (_CE_loss(output_adv, target_adv)[nonzero] * w[nonzero]).sum() \
             / w[nonzero].sum()
    return loss1 - lambda_ * loss2


def _train_clas_windowed(loader, model, optimizer, scheduler, device):
    model.train()
    total = 0.0
    for X_b, y_b, mask_b in loader:
        X_b, y_b, mask_b = X_b.to(device), y_b.to(device), mask_b.to(device)
        optimizer.zero_grad()
        out  = model(X_b)
        per  = _BCE_pretrain(out, y_b) * mask_b
        loss = per.sum() / mask_b.sum().clamp(min=1)
        loss.backward()
        optimizer.step()
        total += loss.item()
    scheduler.step()
    return total / len(loader)


def _validate_clas_windowed(loader, model, device):
    model.eval()
    total = 0.0
    with torch.no_grad():
        for X_b, y_b, mask_b in loader:
            X_b, y_b, mask_b = X_b.to(device), y_b.to(device), mask_b.to(device)
            out  = model(X_b)
            per  = _BCE_pretrain(out, y_b) * mask_b
            total += (per.sum() / mask_b.sum().clamp(min=1)).item()
    return total / len(loader)


def _train_full_windowed(loader, classifier, adv, loss_fn, lambda_,
                         crit_adv, opt_clas, opt_adv,
                         sched_clas, sched_adv, device, n_adv_steps=8):
    classifier.train(); adv.train()
    total_clas = total_adv = 0.0
    for X, y_c, y_a, w, cmask in loader:
        X, y_c, y_a, w, cmask = (
            X.to(device), y_c.to(device), y_a.to(device),
            w.to(device), cmask.to(device))
        # Adversary steps (on current batch, no dataloader re-iteration)
        for _ in range(n_adv_steps):
            opt_adv.zero_grad()
            with torch.no_grad():
                out_c = classifier(X)
            out_a = adv(out_c)
            nz = w.squeeze() > 0
            if nz.sum() > 0:
                al = crit_adv(out_a[nz], y_a[nz])
                (al * w.squeeze()[nz]).sum().div(w.squeeze()[nz].sum()).backward()
                opt_adv.step()
        # Classifier step
        opt_clas.zero_grad()
        out_c = classifier(X)
        out_a = adv(out_c)
        loss  = loss_fn(out_c, out_a, y_c, y_a, w.squeeze(), lambda_, cmask)
        loss.backward()
        opt_clas.step()
        total_clas += loss.item()
        with torch.no_grad():
            nz = w.squeeze() > 0
            if nz.sum() > 0:
                al = crit_adv(out_a[nz], y_a[nz])
                total_adv += (al * w.squeeze()[nz]).sum().div(
                    w.squeeze()[nz].sum()).item()
    sched_clas.step(); sched_adv.step()
    return total_clas / len(loader), total_adv / len(loader)


def _validate_full_windowed(loader, classifier, adv, loss_fn, lambda_,
                             crit_adv, device):
    classifier.eval(); adv.eval()
    total_clas = total_adv = 0.0
    with torch.no_grad():
        for X, y_c, y_a, w, cmask in loader:
            X, y_c, y_a, w, cmask = (
                X.to(device), y_c.to(device), y_a.to(device),
                w.to(device), cmask.to(device))
            out_c = classifier(X)
            out_a = adv(out_c)
            total_clas += loss_fn(
                out_c, out_a, y_c, y_a, w.squeeze(), lambda_, cmask).item()
            nz = w.squeeze() > 0
            if nz.sum() > 0:
                al = crit_adv(out_a[nz], y_a[nz])
                total_adv += (al * w.squeeze()[nz]).sum().div(
                    w.squeeze()[nz].sum()).item()
    return total_clas / len(loader), total_adv / len(loader)


# import psutil, os, time, threading
#
# def log_memory(interval=1):
#     process = psutil.Process(os.getpid())
#     while True:
#         mem = process.memory_info().rss / 1024**2  # MB
#         print(f"[MEM] {time.time():.0f} {mem:.2f} MB", flush=True)
#         time.sleep(interval)
#
# threading.Thread(target=log_memory, daemon=True).start()




# ═══════════════════════════════════════════════════════════════════════════════

def seed_everything(seed: int = 0):
    os.environ["PYTHONHASHSEED"] = str(seed)
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)


def parse_args():
    p = argparse.ArgumentParser(
        description="Single ablation probe: adversarial ANN training excluding one feature.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    p.add_argument("--probe-idx", type=int, required=True,
                   help="Full-list index of feature to probe (-1 = baseline).")
    p.add_argument("--excluded-json", type=str, default="[]",
                   help="JSON list of full-list indices already culled.")
    p.add_argument("--mass", type=int, default=180)
    p.add_argument("--output-dir", type=Path, default=Path("ablation_outputs"))
    p.add_argument("--cull-count", type=int, default=None,
                   help="Number of features culled so far (inferred from --excluded-json if omitted).")
    return p.parse_args()


# ═══════════════════════════════════════════════════════════════════════════════

def main():
    args = parse_args()

    # ── Device & hyper-parameters (unchanged from notebook) ───────────────────
    device      = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    print("Using device:", device)

    Num_classes = 10
    lambda_     = 10.0
    run         = 42
    bsize       = 2000
    MASS        = args.mass

    seed_everything(run)

    # ── Resolve exclusions ────────────────────────────────────────────────────
    excluded_baseline_indices: list[int] = json.loads(args.excluded_json)
    cull_count = (args.cull_count if args.cull_count is not None
                  else len(excluded_baseline_indices))

    # ── Load data ─────────────────────────────────────────────────────────────
    df_bigpreselectblind = pd.read_pickle("bigpreselectblind.pk")
    df_bigpreselectblind["PhiKK"] = 0.0

    df_simp = pd.read_pickle(f"simp{MASS}pres.pk")
    df_simp["PhiKK"] = 1.0

    df_wab_dm     = pd.read_pickle("wab_HPS_Run2021Pass1_v9_14272_hitSmearKill_2000files.pk")
    df_tritrig_dm = pd.read_pickle("tritrig_HPS_Run2021Pass1_v9_14272_hitSmearKill_1000files.pk")
    df_wab_dm["PhiKK"]     = 0.0
    df_tritrig_dm["PhiKK"] = 0.0
    df_mc_bkg = pd.concat([df_wab_dm, df_tritrig_dm], ignore_index=True, sort=False)
    del df_wab_dm, df_tritrig_dm
    gc.collect()

    print("data bkg:        ", len(df_bigpreselectblind))
    print("data-like MC bkg:", len(df_mc_bkg))
    print("simp:            ", len(df_simp))

    # ── Build feature list & apply exclusions ─────────────────────────────────
    _non_feature_cols = {"InvM", "PhiKK"}
    _all_cols = [c for c in df_bigpreselectblind.columns if c not in _non_feature_cols]

    _baseline_features = [_all_cols[i] for i in excluded_baseline_indices]
    if _baseline_features:
        print(f"Locked-out features from prior rounds ({len(_baseline_features)}): {_baseline_features}")

    REACH_LIST = [c for c in _all_cols if c not in _baseline_features]
    print(f"Surviving features after baseline exclusions: {len(REACH_LIST)} / {len(_all_cols)}")

    # Per-run probe exclusion (probe_idx is a *full-list* index)
    if args.probe_idx == -1:
        excluded_feature    = None
        active_features     = REACH_LIST
        EXCLUDE_FEATURE_IDX = -1
        tag   = f"cull{cull_count}_baseline_surviving_features"
        print(f"Probe: none — running with all {len(REACH_LIST)} surviving features (baseline).")
    else:
        if args.probe_idx >= len(_all_cols):
            print(f"ERROR: probe_idx={args.probe_idx} out of range "
                  f"(full list has {len(_all_cols)} features).")
            sys.exit(1)
        probe_name = _all_cols[args.probe_idx]
        if probe_name not in REACH_LIST:
            print(f"ERROR: probe feature '{probe_name}' is already in the baseline exclusion list.")
            sys.exit(1)
        EXCLUDE_FEATURE_IDX = REACH_LIST.index(probe_name)
        excluded_feature    = probe_name
        active_features     = [f for f in REACH_LIST if f != excluded_feature]
        tag = f"cull{cull_count}_excl{args.probe_idx}_{probe_name}"
        print(f"Probe: excluding surviving-list [{EXCLUDE_FEATURE_IDX}]: \"{excluded_feature}\"")
        print(f"Training with {len(active_features)} / {len(REACH_LIST)} surviving features.")

    print("Active features:", active_features)
    args.output_dir.mkdir(parents=True, exist_ok=True)

    # ── Train/val/test splits ─────────────────────────────────────────────────
    # CHANGE from memorytrim v2: align data-background split with ANN_NHP1.py to avoid
    # carrying an unusually large data test/diagnostic sample in memory.
    df_bigpreselectblind_train, df_bigpreselectblind_test = train_test_split(
        df_bigpreselectblind, test_size=0.20, random_state=42)
    df_simp_train, df_simp_test = train_test_split(
        df_simp, test_size=0.5, random_state=42)
    df_mc_bkg_train, df_mc_bkg_test = train_test_split(
        df_mc_bkg, test_size=0.5, random_state=42)
    del df_simp, df_mc_bkg
    gc.collect()

    df_train = pd.concat([df_bigpreselectblind_train, df_simp_train],
                          ignore_index=True, sort=False)
    df_train = df_train.sample(frac=1, random_state=42).reset_index(drop=True)

    one_hot_edges = mytools.get_one_hot_edges(df_bigpreselectblind.InvM, n_bins=Num_classes)

    df_train, df_val = train_test_split(df_train, test_size=0.33, random_state=42)
    del df_bigpreselectblind_train, df_simp_train
    gc.collect()

    # ── Drop non-feature cols, baseline-culled features, and probe feature ──────
    _drop = ["InvM", "PhiKK"] + _baseline_features + ([excluded_feature] if excluded_feature else [])

    X_train      = df_train.drop(columns=_drop)
    y_train      = df_train["PhiKK"]
    y_adv_train  = mytools.get_adv_labels(df_train.InvM, one_hot_edges)

    X_val        = df_val.drop(columns=_drop)
    y_val        = df_val["PhiKK"]
    y_adv_val    = mytools.get_adv_labels(df_val.InvM, one_hot_edges)

    df_test = pd.concat([df_bigpreselectblind_test, df_simp_test],
                         ignore_index=True, sort=False)
    df_test = df_test.sample(frac=1, random_state=42).reset_index(drop=True)

    X_test       = df_test.drop(columns=_drop)
    y_test       = df_test["PhiKK"]
    y_adv_test   = mytools.get_adv_labels(df_test.InvM, one_hot_edges)

    del df_bigpreselectblind, df_simp_test
    gc.collect()

    # ── Mass-window classifier mask (±8 MeV) — mirrors ANN_NHP2.py ───────────
    mass_window_GeV = 0.008
    mass_centre_GeV = MASS / 1000.0
    clas_mask_train = (
        (df_train.InvM >= mass_centre_GeV - mass_window_GeV) &
        (df_train.InvM <= mass_centre_GeV + mass_window_GeV)
    ).astype(np.float32).values
    clas_mask_val = (
        (df_val.InvM >= mass_centre_GeV - mass_window_GeV) &
        (df_val.InvM <= mass_centre_GeV + mass_window_GeV)
    ).astype(np.float32).values
    print(f"Mass window: [{1000*(mass_centre_GeV-mass_window_GeV):.1f}, "
          f"{1000*(mass_centre_GeV+mass_window_GeV):.1f}] MeV")
    print(f"Train events in window: {clas_mask_train.sum():.0f} / {len(clas_mask_train)}")
    print(f"Val   events in window: {clas_mask_val.sum():.0f} / {len(clas_mask_val)}")

    # ── Scale ─────────────────────────────────────────────────────────────────
    scaler  = StandardScaler()
    X_train = scaler.fit_transform(X_train).astype(np.float32, copy=False)
    X_val   = scaler.transform(X_val).astype(np.float32, copy=False)
    X_test  = scaler.transform(X_test).astype(np.float32, copy=False)
    joblib.dump(scaler, args.output_dir / f"scaler_2021_v9_pass5_run{run}_QualCuts_{MASS}_{tag}.pkl")
    print(f"Standardized {X_train.shape[1]} features using training-set statistics.")

    # ── Tensor helpers ────────────────────────────────────────────────────────
    def _t(arr, dtype=np.float32):
        return torch.from_numpy(np.asarray(arr).astype(dtype))

    y_train_np     = np.asarray(y_train, dtype=np.float32)
    y_val_np       = np.asarray(y_val,   dtype=np.float32)
    y_adv_train_np = np.asarray(y_adv_train, dtype=np.float32)
    y_adv_val_np   = np.asarray(y_adv_val,   dtype=np.float32)
    y_adv_test_np  = np.asarray(y_adv_test,  dtype=np.float32)

    # Adversary weights: background only (PhiKK==0), weight=1; signal=0.
    # train_ablation has no sample_weight column so we use uniform 1.0 for bkg,
    # matching ANN_NHP2.py's adv_w = (y==0)*sample_weight with sample_weight=1.
    adv_w_train = (y_train_np == 0).astype(np.float32)
    adv_w_val   = (y_val_np   == 0).astype(np.float32)

    del df_train, df_val
    gc.collect()

    X_train_t        = _t(X_train)
    X_val_t          = _t(X_val)
    X_test_t         = _t(X_test)
    y_train_t        = _t(y_train_np).unsqueeze(1)
    y_val_t          = _t(y_val_np).unsqueeze(1)
    y_adv_train_t    = _t(y_adv_train_np)
    y_adv_val_t      = _t(y_adv_val_np)
    y_adv_test_t     = _t(y_adv_test_np)
    adv_w_train_t    = _t(adv_w_train)
    adv_w_val_t      = _t(adv_w_val)
    clas_mask_train_t = _t(clas_mask_train).unsqueeze(1)
    clas_mask_val_t   = _t(clas_mask_val).unsqueeze(1)

    print(f"Training feature count: {X_train.shape[1]}")

    # ── DataLoaders ───────────────────────────────────────────────────────────
    # Phase 1: classifier pre-training (X, y, clas_mask)
    train_loader = DataLoader(
        TensorDataset(X_train_t, y_train_t, clas_mask_train_t),
        batch_size=bsize, shuffle=True)
    val_loader = DataLoader(
        TensorDataset(X_val_t, y_val_t, clas_mask_val_t),
        batch_size=bsize, shuffle=False)
    test_loader = DataLoader(
        TensorDataset(X_test_t),
        batch_size=bsize, shuffle=False)

    # Phase 2: adversary pre-training (X, y_adv, adv_w)
    train_adv_loader = DataLoader(
        TensorDataset(X_train_t, y_adv_train_t, adv_w_train_t),
        batch_size=bsize, shuffle=True)
    val_adv_loader = DataLoader(
        TensorDataset(X_val_t, y_adv_val_t, adv_w_val_t),
        batch_size=bsize, shuffle=True)
    test_adv_loader = DataLoader(
        TensorDataset(X_test_t, y_adv_test_t),
        batch_size=bsize, shuffle=True)

    # Phase 3: full adversarial training (X, y, y_adv, adv_w, clas_mask)
    train_full_loader = DataLoader(
        TensorDataset(X_train_t, y_train_t, y_adv_train_t,
                      adv_w_train_t, clas_mask_train_t),
        batch_size=bsize, shuffle=True)
    val_full_loader = DataLoader(
        TensorDataset(X_val_t, y_val_t, y_adv_val_t,
                      adv_w_val_t, clas_mask_val_t),
        batch_size=bsize, shuffle=True)

    # ═════════════════════════════════════════════════════════════════════════
    # Phase 1: Pre-train classifier (windowed BCE, mirrors ANN_NHP2.py)
    # ═════════════════════════════════════════════════════════════════════════
    print("\n── Phase 1: Pre-training classifier ─────────────────────────────")

    clas           = mytools.Classifier(in_features=X_train_t.shape[1]).to(device)
    optimizer_clas = optim.Adam(clas.parameters(), lr=1e-2)
    scheduler_clas = torch.optim.lr_scheduler.ExponentialLR(optimizer_clas, gamma=1.0)

    patience         = 100000
    TR_losses        = []
    VAL_losses       = []
    final_classifier = None

    for t in range(60):
        print(f"[Pretrain] Epoch {t+1}/60")
        TR_losses.append(
            _train_clas_windowed(train_loader, clas, optimizer_clas, scheduler_clas, device))
        VAL_losses.append(
            _validate_clas_windowed(val_loader, clas, device))

        if VAL_losses[-1] == min(VAL_losses):
            final_classifier = copy.deepcopy(clas)

        if len(VAL_losses) > patience:
            val_arr = np.array(VAL_losses)
            if np.sum((val_arr[-patience:] - val_arr[-patience-1:-1]) < 0) == 0:
                print("Early stopping!")
                break

    TR_losses  = np.array(TR_losses)
    VAL_losses = np.array(VAL_losses)
    print("Pre-training done.")

    val_pred = torch.sigmoid(torch.tensor(
        mytools.test_clas(val_loader, final_classifier, device))).numpy()
    fpr, tpr, _ = sklearn.metrics.roc_curve(
        y_val_np, val_pred, pos_label=1)
    print(f"[Pretrain] Val AUROC: {sklearn.metrics.auc(fpr, tpr):.4f}")

    best_epoch = np.argmin(VAL_losses) + 1
    plt.figure()
    plt.plot(np.arange(len(TR_losses))  + 1, TR_losses,  label="Training Loss")
    plt.plot(np.arange(len(VAL_losses)) + 1, VAL_losses, label="Validation Loss")
    plt.axvline(best_epoch, label="Final model stopped here", color="k")
    plt.xlabel("Epoch"); plt.ylabel("BCE Loss"); plt.legend(); plt.tight_layout()
    plt.savefig(str(args.output_dir / f"pretrain_loss_{tag}.png"), dpi=150)
    plt.close()

    # ═════════════════════════════════════════════════════════════════════════
    # Phase 2: Pre-train adversary (60 epochs, mirrors ANN_NHP2.py)
    # ═════════════════════════════════════════════════════════════════════════
    print("\n── Phase 2: Pre-training adversary ──────────────────────────────")

    adv           = mytools.Adversary_small(n_classes=Num_classes).to(device)
    criterion_adv = nn.CrossEntropyLoss(reduction="none")
    opt_adv       = optim.Adam(adv.parameters(), lr=1e-2)
    scheduler_adv = torch.optim.lr_scheduler.ExponentialLR(opt_adv, gamma=1.0)

    TR_losses  = []
    VAL_losses = []
    final_adv  = None

    for t in range(60):
        print(f"[Adv pretrain] Epoch {t+1}/60")
        TR_losses.append(
            mytools.train_adv(train_adv_loader, final_classifier, adv,
                              criterion_adv, opt_adv, scheduler_adv, device))
        VAL_losses.append(
            mytools.validate_adv(val_adv_loader, final_classifier, adv,
                                 criterion_adv, device))
        if VAL_losses[-1] == min(VAL_losses):
            final_adv = copy.deepcopy(adv)

    print("Adversary pre-training done.")

    # ═════════════════════════════════════════════════════════════════════════
    # Phase 3: Full adversarial training (windowed, mirrors ANN_NHP2.py)
    # ═════════════════════════════════════════════════════════════════════════
    print("\n── Phase 3: Full adversarial training ───────────────────────────")

    optimizer_clas = optim.Adam(final_classifier.parameters(), lr=1e-2, betas=(0.9, 0.999))
    optimizer_adv  = optim.Adam(final_adv.parameters(),        lr=1e-2, betas=(0.9, 0.999))
    scheduler_clas = torch.optim.lr_scheduler.ExponentialLR(optimizer_clas, gamma=0.999)
    scheduler_adv  = torch.optim.lr_scheduler.ExponentialLR(optimizer_adv,  gamma=0.999)

    TR_losses_clas  = []
    TR_losses_adv   = []
    VAL_losses_clas = []
    VAL_losses_adv  = []
    diff_scores     = []
    final_clas_adv  = None
    final_adv_adv   = None

    for t in range(19):
        print(f"[Full adv] Epoch {t+1}/19")

        e_clas_tr, e_adv_tr = _train_full_windowed(
            train_full_loader, final_classifier, final_adv,
            _full_loss, lambda_, criterion_adv,
            optimizer_clas, optimizer_adv,
            scheduler_clas, scheduler_adv, device)
        TR_losses_clas.append(e_clas_tr)
        TR_losses_adv.append(e_adv_tr)

        e_clas_val, e_adv_val = _validate_full_windowed(
            val_full_loader, final_classifier, final_adv,
            _full_loss, lambda_, criterion_adv, device)
        VAL_losses_clas.append(e_clas_val)
        VAL_losses_adv.append(e_adv_val)

        df_test["Class_adv"] = mytools.test_clas(test_loader, final_classifier, device)
        df_bkg  = df_test[df_test.PhiKK == 0]
        per     = np.percentile(df_bkg["Class_adv"], 90)
        df_cut  = df_bkg[df_bkg["Class_adv"] > per].reset_index(drop=True)
        diff    = mytools.get_diff_score(1000 * df_bkg.InvM.values,
                                         1000 * df_cut.InvM.values)
        diff_scores.append(diff)
        print(f"  Diff score: {diff:>7f}")

        if diff_scores[-1] == min(diff_scores):
            final_clas_adv = copy.deepcopy(final_classifier)
            final_adv_adv  = copy.deepcopy(final_adv)

    TR_losses_clas  = np.array(TR_losses_clas)
    TR_losses_adv   = np.array(TR_losses_adv)
    VAL_losses_clas = np.array(VAL_losses_clas)
    VAL_losses_adv  = np.array(VAL_losses_adv)
    diff_scores     = np.array(diff_scores)
    print("Full adversarial training done.")

    # ═════════════════════════════════════════════════════════════════════════
    # Evaluation: signal AUROC
    # ═════════════════════════════════════════════════════════════════════════
    test_pred = mytools.test_clas(test_loader, final_clas_adv, device)
    test_pred = torch.sigmoid(torch.tensor(test_pred)).numpy()
    y_test_np = np.asarray(y_test)
    fpr, tpr, _ = sklearn.metrics.roc_curve(y_test_np, test_pred, pos_label=1)
    auc_test = sklearn.metrics.auc(fpr, tpr)
    print(f"Test AUROC: {auc_test:.4f}")

    # Free phase-specific loaders/tensors before diagnostics.
    del train_loader, val_loader, test_loader
    del train_adv_loader, val_adv_loader, test_adv_loader
    del train_full_loader, val_full_loader
    del X_train_t, X_val_t, y_train_t, y_val_t
    del adv_w_train_t, adv_w_val_t, clas_mask_train_t, clas_mask_val_t
    del X_train, X_val, y_train_np, y_val_np, y_adv_train_np, y_adv_val_np
    gc.collect()
    if device.type == "cuda":
        torch.cuda.empty_cache()

    # ═════════════════════════════════════════════════════════════════════════
    # Diagnostic: data vs. data-like MC
    # ═════════════════════════════════════════════════════════════════════════
    # CHANGE from memorytrim v1 (2): build diagnostic features/labels directly
    # from the test splits, without copied diagnostic DataFrames.
    diag_feature_cols = active_features

    X_data_diag = scaler.transform(df_bigpreselectblind_test[diag_feature_cols].to_numpy())
    X_mc_diag   = scaler.transform(df_mc_bkg_test[diag_feature_cols].to_numpy())
    y_dm_diag   = np.concatenate([
        np.zeros(len(X_data_diag), dtype=np.float32),
        np.ones(len(X_mc_diag), dtype=np.float32),
    ])
    X_dm_diag   = np.concatenate([X_data_diag, X_mc_diag], axis=0).astype(np.float32, copy=False)

    del X_data_diag, X_mc_diag, df_mc_bkg_train, df_bigpreselectblind_test, df_mc_bkg_test
    gc.collect()

    dm_diag_loader = DataLoader(
        TensorDataset(torch.from_numpy(X_dm_diag)),
        batch_size=bsize, shuffle=False)

    scores_diag      = mytools.test_clas(dm_diag_loader, final_clas_adv, device)
    scores_diag      = torch.sigmoid(torch.tensor(scores_diag)).numpy()
    scores_data_diag = scores_diag[y_dm_diag == 0]
    scores_mc_diag   = scores_diag[y_dm_diag == 1]

    _title_suffix = (f'(excluded: "{excluded_feature}")' if excluded_feature
                     else "(all features — baseline)")

    # CHANGE from memorytrim v1 (5): drop large diagnostic arrays as soon as plots are done.
    # Score histogram
    bins = np.linspace(0, 1, 51)
    plt.figure(figsize=(7, 5))
    plt.hist(scores_data_diag, bins=bins, histtype="step", density=True,
             label="Real data", color="steelblue", linewidth=1.5)
    plt.hist(scores_mc_diag,   bins=bins, histtype="step", density=True,
             label="Data-like MC (wab+tritrig)", color="tomato", linewidth=1.5)
    plt.xlabel("Classifier score"); plt.ylabel("Normalised counts (a.u.)")
    plt.yscale("log")
    plt.title(f"ANN score: data vs. data-like MC\n{_title_suffix}")
    plt.legend(); plt.tight_layout()
    plt.savefig(str(args.output_dir / f"diag_score_hist_{tag}.png"), dpi=150)
    plt.close()

    # Score histogram + ratio panel
    bin_centers = 0.5 * (bins[:-1] + bins[1:])
    bin_widths  = np.diff(bins)
    data_counts, _ = np.histogram(scores_data_diag, bins=bins)
    mc_counts, _   = np.histogram(scores_mc_diag,   bins=bins)
    data_density   = data_counts / (np.sum(data_counts) * bin_widths)
    mc_density     = mc_counts   / (np.sum(mc_counts)   * bin_widths)
    ratio = np.divide(mc_density, data_density,
                      out=np.full_like(mc_density, np.nan, dtype=float),
                      where=data_density > 0)

    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(7, 7), sharex=True,
                                    gridspec_kw={"height_ratios": [3, 1], "hspace": 0.05})
    ax1.step(bin_centers, data_density, where="mid", label="Real data",
             color="steelblue", linewidth=1.5)
    ax1.step(bin_centers, mc_density,   where="mid",
             label="Data-like MC (wab+tritrig)", color="tomato", linewidth=1.5)
    ax1.set_ylabel("Normalised counts (a.u.)"); ax1.set_yscale("log")
    ax1.set_title(f"ANN score: data vs. data-like MC\n{_title_suffix}"); ax1.legend()
    ax2.step(bin_centers, ratio, where="mid", color="black", linewidth=1.2)
    ax2.axhline(1.0, linestyle="--", color="gray", linewidth=1.0)
    ax2.set_xlabel("Classifier score"); ax2.set_ylabel("MC / data"); ax2.set_ylim(0.0, 4.0)
    plt.tight_layout()
    plt.savefig(str(args.output_dir / f"diag_ratio_panel_{tag}.png"), dpi=150)
    plt.close()

    del data_counts, mc_counts, data_density, mc_density, ratio, bin_centers, bin_widths
    gc.collect()

    # Data-vs-MC ROC
    fpr_dm, tpr_dm, _ = sklearn.metrics.roc_curve(y_dm_diag, scores_diag, pos_label=1)
    auc_dm = sklearn.metrics.auc(fpr_dm, tpr_dm)
    print(f"Data-vs-MC AUROC {_title_suffix}: {auc_dm:.4f}")

    del dm_diag_loader, X_dm_diag, scores_data_diag, scores_mc_diag
    gc.collect()
    if device.type == "cuda":
        torch.cuda.empty_cache()

    plt.figure(figsize=(6, 5))
    plt.plot(fpr_dm, tpr_dm, label=f"Data vs MC AUROC: {auc_dm:.4f}")
    plt.plot([0, 1], [0, 1], "k--", label="Random")
    plt.xlabel("FPR"); plt.ylabel("TPR")
    plt.title(f"ROC – data vs. data-like MC\n{_title_suffix}")
    plt.legend(); plt.tight_layout()
    plt.savefig(str(args.output_dir / f"diag_roc_{tag}.png"), dpi=150)
    plt.close()

    del scores_diag, y_dm_diag, fpr_dm, tpr_dm
    gc.collect()
    if device.type == "cuda":
        torch.cuda.empty_cache()

    # ── Save models ───────────────────────────────────────────────────────────
    torch.save(final_clas_adv.state_dict(),
               args.output_dir / f"classifier_adv_2021_v9_pass5_run{run}_QualCuts_{MASS}_{tag}.pt")
    torch.save(final_adv_adv.state_dict(),
               args.output_dir / f"adversary_adv_2021_v9_pass5_run{run}_QualCuts_{MASS}_{tag}.pt")

    # ── Result JSON ───────────────────────────────────────────────────────────
    result = {
        "tag":                        tag,
        "probe_idx_full_list":        args.probe_idx,
        "probe_feature":              excluded_feature,
        "cull_count":                 cull_count,
        "mass":                       MASS,
        "excluded_baseline_indices":  excluded_baseline_indices,
        "excluded_baseline_features": _baseline_features,
        "active_features":            active_features,
        "n_active":                   len(active_features),
        "metrics": {
            "signal_auc":     round(float(auc_test), 4),
            "data_vs_mc_auc": round(float(auc_dm),   4),
        },
    }
    result_path = args.output_dir / f"result_{tag}.json"
    with open(result_path, "w") as f:
        json.dump(result, f, indent=2)
    print(f"Results written to {result_path}")

    # ── SUMMARY line (identical format to the notebook) ───────────────────────
    print(
        f"SUMMARY | cull_count={cull_count} | baseline_culled={_baseline_features} "
        f"| probe_excluded_idx={EXCLUDE_FEATURE_IDX} | probe_excluded_feature={excluded_feature} "
        f"| signal_auc={auc_test:.4f} | data_vs_mc_auc={auc_dm:.4f}"
    )


if __name__ == "__main__":
    main()
