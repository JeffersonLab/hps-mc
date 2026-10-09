#!/usr/bin/env python
"""
ANN_NHP1.py  –  adversarial neural network trainer for HPS SIMP search.

Usage
-----
    python ANN_NHP1.py <MASS_MEV> [--exclude VAR1 VAR2 ...]

Example
-------
    python ANN_NHP1.py 135
    python ANN_NHP1.py 150 --exclude ele.track_.z0_ pos.track_.z0_
"""

import argparse
import copy
import os
import random

import joblib
import matplotlib
matplotlib.use("Agg")          # non-interactive backend for batch jobs
import matplotlib.pylab as plt
import numpy as np
import pandas as pd
import sklearn
import sklearn.metrics
import torch
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from torch import nn, optim
from torch.utils.data import DataLoader, TensorDataset

import mytools


# ── CLI ────────────────────────────────────────────────────────────────────────
def parse_args():
    parser = argparse.ArgumentParser(description="Train adversarial ANN for HPS SIMP search.")
    parser.add_argument("mass", type=int,
                        help="Signal mass hypothesis in MeV (e.g. 135)")
    parser.add_argument("--exclude", nargs="*", default=[],
                        metavar="VAR",
                        help="Column names to drop from classifier input features.")
    return parser.parse_args()


# ── Reproducibility ────────────────────────────────────────────────────────────
def seed_everything(seed: int = 0):
    os.environ["PYTHONHASHSEED"] = str(seed)
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)


# ── Loss helpers ───────────────────────────────────────────────────────────────
BCE_loss_unreduced = nn.BCEWithLogitsLoss(reduction="none")
CE_loss            = nn.CrossEntropyLoss(reduction="none")


def full_loss(output_clas, output_adv, target_clas, target_adv, w, lambda_, clas_mask):
    """Combined classifier + adversary loss (windowed classifier BCE)."""
    # Classifier loss – mass-windowed
    clas_w = torch.where(w > 0, w, torch.ones_like(w)) * clas_mask
    loss1 = (BCE_loss_unreduced(output_clas, target_clas) * clas_w).sum() \
            / clas_w.sum().clamp(min=1)

    # Adversary loss – full mass range, background only
    nonzero = w > 0
    loss2 = (CE_loss(output_adv, target_adv)[nonzero] * w[nonzero]).sum() \
            / w[nonzero].sum()

    return loss1 - lambda_ * loss2


# ── Training / validation routines ────────────────────────────────────────────
BCE_pretrain = nn.BCEWithLogitsLoss(reduction="none")


def train_clas_windowed(loader, model, optimizer, scheduler, device):
    model.train()
    total = 0.0
    for X_b, y_b, mask_b in loader:
        X_b, y_b, mask_b = X_b.to(device), y_b.to(device), mask_b.to(device)
        optimizer.zero_grad()
        out = model(X_b)
        per = BCE_pretrain(out, y_b) * mask_b
        loss = per.sum() / mask_b.sum().clamp(min=1)
        loss.backward()
        optimizer.step()
        total += loss.item()
    scheduler.step()
    return total / len(loader)


def validate_clas_windowed(loader, model, device):
    model.eval()
    total = 0.0
    with torch.no_grad():
        for X_b, y_b, mask_b in loader:
            X_b, y_b, mask_b = X_b.to(device), y_b.to(device), mask_b.to(device)
            out = model(X_b)
            per = BCE_pretrain(out, y_b) * mask_b
            total += (per.sum() / mask_b.sum().clamp(min=1)).item()
    return total / len(loader)


def train_full_windowed(loader, classifier, adv, loss_fn, lambda_,
                        crit_adv, opt_clas, opt_adv,
                        sched_clas, sched_adv, device, n_adv_steps=8):
    classifier.train(); adv.train()
    total_clas = total_adv = 0.0
    for X, y_c, y_a, w, cmask in loader:
        X, y_c, y_a, w, cmask = (
            X.to(device), y_c.to(device), y_a.to(device),
            w.to(device), cmask.to(device))
        # Adversary steps
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
        loss = loss_fn(out_c, out_a, y_c, y_a, w.squeeze(), lambda_, cmask)
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


def validate_full_windowed(loader, classifier, adv, loss_fn, lambda_,
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


# ── Main ───────────────────────────────────────────────────────────────────────
def main():
    args = parse_args()
    MASS             = args.mass
    EXCLUDE_VARIABLES = args.exclude

    # Hyper-parameters
    Num_classes = 10
    lambda_     = 10.0
    run         = 42
    bsize       = 2000
    patience    = 100000

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    print("Using device:", device)
    print(f"MASS = {MASS} MeV")

    seed_everything(run)

    # ── Load data ──────────────────────────────────────────────────────────────
    df_bigpreselectblind = pd.read_pickle("bigpreselectblind.pk")
    df_bigpreselectblind["PhiKK"] = 0.0

    df_simp_pres = pd.read_pickle(f"simp{MASS}pres.pk")
    df_simp_pres["PhiKK"] = 1.0

    df_wab_dm     = pd.read_pickle("wab_HPS_Run2021Pass1_v9_14272_hitSmearKill_2000files.pk")
    df_tritrig_dm = pd.read_pickle("tritrig_HPS_Run2021Pass1_v9_14272_hitSmearKill_1000files.pk")
    df_wab_dm["PhiKK"]     = 0.0
    df_tritrig_dm["PhiKK"] = 0.0

    # Apply L1L1 mask
    for name, df in [
        ("df_bigpreselectblind", df_bigpreselectblind),
        (f"df_simp{MASS}pres",   df_simp_pres),
        ("df_wab_dm",            df_wab_dm),
        ("df_tritrig_dm",        df_tritrig_dm),
    ]:
        if "isL1L1" not in df.columns:
            raise KeyError(f"Column 'isL1L1' not found in {name}.")

    df_bigpreselectblind = df_bigpreselectblind[df_bigpreselectblind["isL1L1"].astype(bool)].reset_index(drop=True)
    df_simp_pres         = df_simp_pres        [df_simp_pres        ["isL1L1"].astype(bool)].reset_index(drop=True)
    df_wab_dm            = df_wab_dm           [df_wab_dm           ["isL1L1"].astype(bool)].reset_index(drop=True)
    df_tritrig_dm        = df_tritrig_dm       [df_tritrig_dm       ["isL1L1"].astype(bool)].reset_index(drop=True)

    df_mc_bkg = pd.concat([df_wab_dm, df_tritrig_dm], ignore_index=True, sort=False)

    print("data bkg (L1L1):        ", len(df_bigpreselectblind))
    print("data-like MC bkg (L1L1):", len(df_mc_bkg))
    print("simp (L1L1):            ", len(df_simp_pres))

    # ── Split ──────────────────────────────────────────────────────────────────
    df_bigpreselectblind_train, df_bigpreselectblind_test = train_test_split(
        df_bigpreselectblind, test_size=0.20, random_state=42)
    df_mc_bkg_train, df_mc_bkg_test = train_test_split(
        df_mc_bkg, test_size=0.5, random_state=42)
    df_simp_train, df_simp_test = train_test_split(
        df_simp_pres, test_size=0.5, random_state=42)

    N_mc_train   = len(df_mc_bkg_train)
    N_data_train = len(df_bigpreselectblind_train)
    mc_weight    = N_data_train / N_mc_train

    df_data_bkg_train_sub = df_bigpreselectblind_train.sample(n=N_mc_train, random_state=42).copy()
    df_mc_bkg_train       = df_mc_bkg_train.copy()
    df_simp_train         = df_simp_train.copy()

    df_data_bkg_train_sub["sample_weight"] = 1.0
    df_mc_bkg_train      ["sample_weight"] = 1.0
    df_simp_train        ["sample_weight"] = 1.0

    df_train = pd.concat(
        [df_data_bkg_train_sub, df_mc_bkg_train, df_simp_train],
        ignore_index=True, sort=False
    ).sample(frac=1, random_state=42).reset_index(drop=True)

    one_hot_edges = mytools.get_one_hot_edges(df_bigpreselectblind.InvM, n_bins=Num_classes)

    df_train, df_val = train_test_split(df_train, test_size=0.33, random_state=42)

    df_test = pd.concat(
        [df_bigpreselectblind_test, df_simp_test],
        ignore_index=True, sort=False
    ).sample(frac=1, random_state=42).reset_index(drop=True)

    print(f"Train: {len(df_train)}  Val: {len(df_val)}  Test: {len(df_test)}")
    print(f"MC upweight factor: {mc_weight:.3f}")

    # ── Feature selection ──────────────────────────────────────────────────────
    _always_drop_train = ["InvM", "PhiKK", "sample_weight", "isL1L1"]
    _always_drop_test  = ["InvM", "PhiKK", "isL1L1"]
    _extra_drop = [v for v in EXCLUDE_VARIABLES if v not in _always_drop_train]
    _drop_train = _always_drop_train + _extra_drop
    _drop_test  = _always_drop_test  + _extra_drop

    for v in EXCLUDE_VARIABLES:
        if v not in df_train.columns:
            print(f"[warn] EXCLUDE_VARIABLES: '{v}' not found – skipping")

    if EXCLUDE_VARIABLES:
        print(f"Excluding {len(EXCLUDE_VARIABLES)} variable(s): {EXCLUDE_VARIABLES}")
    else:
        print("EXCLUDE_VARIABLES is empty – training on all available features")

    X_train     = df_train.drop(columns=_drop_train, errors="ignore")
    y_train     = df_train["PhiKK"]
    w_train     = df_train["sample_weight"]
    y_adv_train = mytools.get_adv_labels(df_train.InvM, one_hot_edges)

    X_val       = df_val.drop(columns=_drop_train, errors="ignore")
    y_val       = df_val["PhiKK"]
    w_val       = df_val["sample_weight"]
    y_adv_val   = mytools.get_adv_labels(df_val.InvM, one_hot_edges)

    X_test      = df_test.drop(columns=_drop_test, errors="ignore")
    y_test      = df_test["PhiKK"]
    y_adv_test  = mytools.get_adv_labels(df_test.InvM, one_hot_edges)

    # Mass-window mask (±8 MeV)
    mass_window_GeV = 0.008
    mass_centre_GeV = MASS / 1000.0
    clas_mask_train = (
        (df_train.InvM >= mass_centre_GeV - mass_window_GeV) &
        (df_train.InvM <= mass_centre_GeV + mass_window_GeV)
    ).astype(np.float32).values
    clas_mask_val   = (
        (df_val.InvM   >= mass_centre_GeV - mass_window_GeV) &
        (df_val.InvM   <= mass_centre_GeV + mass_window_GeV)
    ).astype(np.float32).values

    print(f"Mass window: [{1000*(mass_centre_GeV-mass_window_GeV):.1f}, "
          f"{1000*(mass_centre_GeV+mass_window_GeV):.1f}] MeV")
    print(f"Train events in window: {clas_mask_train.sum():.0f} / {len(clas_mask_train)}")
    print(f"Val   events in window: {clas_mask_val.sum():.0f} / {len(clas_mask_val)}")
    print(f"Training feature count: {X_train.shape[1]}")

    # ── Scale ──────────────────────────────────────────────────────────────────
    scaler  = StandardScaler()
    X_train = pd.DataFrame(scaler.fit_transform(X_train),
                           columns=X_train.columns, index=X_train.index)
    X_val   = pd.DataFrame(scaler.transform(X_val),
                           columns=X_val.columns,   index=X_val.index)
    X_test  = pd.DataFrame(scaler.transform(X_test),
                           columns=X_test.columns,  index=X_test.index)

    scaler_path = f"scaler_2021_v9_pass5_run{run}_QualCuts_{MASS}_v3.pkl"
    joblib.dump(scaler, scaler_path)
    print(f"Scaler saved → {scaler_path}")

    # ── DataLoaders ────────────────────────────────────────────────────────────
    def _t(arr, dtype=np.float32):
        return torch.from_numpy(np.asarray(arr).astype(dtype))

    train_dataset = TensorDataset(
        _t(X_train.to_numpy()),
        _t(y_train.to_numpy()).unsqueeze(1),
        _t(clas_mask_train).unsqueeze(1))
    train_loader = DataLoader(train_dataset, batch_size=bsize, shuffle=True)

    val_dataset = TensorDataset(
        _t(X_val.to_numpy()),
        _t(y_val.to_numpy()).unsqueeze(1),
        _t(clas_mask_val).unsqueeze(1))
    val_loader = DataLoader(val_dataset, batch_size=bsize, shuffle=False)

    test_dataset = TensorDataset(_t(X_test.to_numpy()))
    test_loader  = DataLoader(test_dataset, batch_size=bsize, shuffle=False)

    # ── Pre-train classifier ───────────────────────────────────────────────────
    clas           = mytools.Classifier(in_features=X_train.shape[1]).to(device)
    optimizer_clas = optim.Adam(clas.parameters(), lr=1e-2)
    scheduler_clas = torch.optim.lr_scheduler.ExponentialLR(optimizer_clas, gamma=1.0)

    Training_losses   = np.array([])
    Validation_losses = np.array([])
    final_classifier  = None

    epochs = 60
    for t in range(epochs):
        print(f"[Pretrain] Epoch {t+1}/{epochs}")
        Training_losses   = np.append(Training_losses,
            train_clas_windowed(train_loader, clas, optimizer_clas, scheduler_clas, device))
        Validation_losses = np.append(Validation_losses,
            validate_clas_windowed(val_loader, clas, device))

        if Validation_losses[-1] == np.min(Validation_losses):
            final_classifier = copy.deepcopy(clas)

        if len(Validation_losses) > patience:
            if np.sum(
                (Validation_losses[-patience:] - Validation_losses[-patience-1:-1]) < 0
            ) == 0:
                print("Early stopping!")
                break

    print("Pre-training done.")

    # Pretrain ROC (validation set)
    val_pred = torch.sigmoid(torch.tensor(
        mytools.test_clas(val_loader, final_classifier, device))).numpy()
    fpr, tpr, _ = sklearn.metrics.roc_curve(y_val, val_pred, pos_label=1)
    print(f"[Pretrain] Val AUROC: {sklearn.metrics.auc(fpr, tpr):.4f}")

    # ── Pre-train adversary ────────────────────────────────────────────────────
    adv           = mytools.Adversary_small(n_classes=Num_classes).to(device)
    criterion_adv = nn.CrossEntropyLoss(reduction="none")
    opt_adv       = torch.optim.Adam(adv.parameters(), lr=1e-2)
    scheduler_adv = torch.optim.lr_scheduler.ExponentialLR(opt_adv, gamma=1.0)

    adv_w_train = ((y_train == 0).astype(float) * w_train).values.astype(np.float32)
    adv_w_val   = ((y_val   == 0).astype(float) * w_val  ).values.astype(np.float32)

    train_adv_dataset = TensorDataset(
        _t(X_train.to_numpy()), _t(y_adv_train, np.float32), _t(adv_w_train))
    train_adv_loader  = DataLoader(train_adv_dataset, batch_size=bsize, shuffle=True)

    val_adv_dataset   = TensorDataset(
        _t(X_val.to_numpy()),   _t(y_adv_val, np.float32),   _t(adv_w_val))
    val_adv_loader    = DataLoader(val_adv_dataset, batch_size=bsize, shuffle=True)

    test_adv_dataset  = TensorDataset(
        _t(X_test.to_numpy()), _t(y_adv_test, np.float32))
    test_adv_loader   = DataLoader(test_adv_dataset, batch_size=bsize, shuffle=True)

    Training_losses   = np.array([])
    Validation_losses = np.array([])
    final_adv         = None

    epochs = 60
    for t in range(epochs):
        print(f"[Adv pretrain] Epoch {t+1}/{epochs}")
        Training_losses   = np.append(Training_losses,
            mytools.train_adv(train_adv_loader, final_classifier, adv,
                              criterion_adv, opt_adv, scheduler_adv, device))
        Validation_losses = np.append(Validation_losses,
            mytools.validate_adv(val_adv_loader, final_classifier, adv,
                                 criterion_adv, device))
        if Validation_losses[-1] == np.min(Validation_losses):
            final_adv = copy.deepcopy(adv)

    print("Adversary pre-training done.")

    # ── Full adversarial training ──────────────────────────────────────────────
    train_full_dataset = TensorDataset(
        _t(X_train.to_numpy()),
        _t(y_train.to_numpy()).unsqueeze(1),
        _t(y_adv_train, np.float32),
        _t(adv_w_train),
        _t(clas_mask_train).unsqueeze(1))
    train_full_loader  = DataLoader(train_full_dataset, batch_size=bsize, shuffle=True)

    val_full_dataset   = TensorDataset(
        _t(X_val.to_numpy()),
        _t(y_val.to_numpy()).unsqueeze(1),
        _t(y_adv_val, np.float32),
        _t(adv_w_val),
        _t(clas_mask_val).unsqueeze(1))
    val_full_loader    = DataLoader(val_full_dataset, batch_size=bsize, shuffle=True)

    optimizer_clas = optim.Adam(final_classifier.parameters(), lr=1e-2, betas=(0.9, 0.999))
    optimizer_adv  = optim.Adam(final_adv.parameters(),        lr=1e-2, betas=(0.9, 0.999))
    scheduler_clas = torch.optim.lr_scheduler.ExponentialLR(optimizer_clas, gamma=0.999)
    scheduler_adv  = torch.optim.lr_scheduler.ExponentialLR(optimizer_adv,  gamma=0.999)

    Training_losses_clas   = np.array([])
    Training_losses_adv    = np.array([])
    Validation_losses_clas = np.array([])
    Validation_losses_adv  = np.array([])
    diff_scores            = np.array([])
    final_clas_adv         = None
    final_adv_adv          = None

    epochs = 19
    for t in range(epochs):
        print(f"[Full adv] Epoch {t+1}/{epochs}")
        e_clas_tr, e_adv_tr = train_full_windowed(
            train_full_loader, final_classifier, final_adv, full_loss, lambda_,
            criterion_adv, optimizer_clas, optimizer_adv,
            scheduler_clas, scheduler_adv, device)
        Training_losses_clas = np.append(Training_losses_clas, e_clas_tr)
        Training_losses_adv  = np.append(Training_losses_adv,  e_adv_tr)

        e_clas_val, e_adv_val = validate_full_windowed(
            val_full_loader, final_classifier, final_adv, full_loss, lambda_,
            criterion_adv, device)
        Validation_losses_clas = np.append(Validation_losses_clas, e_clas_val)
        Validation_losses_adv  = np.append(Validation_losses_adv,  e_adv_val)

        df_test["Class_adv"] = mytools.test_clas(test_loader, final_classifier, device)
        df_bkg = df_test[df_test.PhiKK == 0]
        per    = np.percentile(df_bkg["Class_adv"], 90)
        df_cut = df_bkg[df_bkg["Class_adv"] > per].reset_index(drop=True)
        diff   = mytools.get_diff_score(1000 * df_bkg.InvM.values,
                                        1000 * df_cut.InvM.values)
        diff_scores = np.append(diff_scores, diff)
        print(f"  Diff score: {diff:>7f}")

        if diff_scores[-1] == np.min(diff_scores):
            final_clas_adv = copy.deepcopy(final_classifier)
            final_adv_adv  = copy.deepcopy(final_adv)

    print("Full adversarial training done.")

    # ── Save models ────────────────────────────────────────────────────────────
    clas_path = f"classifier_adv_2021_v9_pass5_run{run}QualCuts_{MASS}_v3.pt"
    adv_path  = f"adversary_adv_2021_v9_pass5_run{run}QualCuts_{MASS}_v3.pt"
    torch.save(final_clas_adv.state_dict(), clas_path)
    torch.save(final_adv_adv.state_dict(),  adv_path)
    print(f"Classifier saved → {clas_path}")
    print(f"Adversary  saved → {adv_path}")

    # ── Final evaluation ───────────────────────────────────────────────────────
    test_pred = torch.sigmoid(torch.tensor(
        mytools.test_clas(test_loader, final_clas_adv, device))).numpy()
    fpr, tpr, _ = sklearn.metrics.roc_curve(y_test, test_pred, pos_label=1)
    auc_test = sklearn.metrics.auc(fpr, tpr)
    print(f"Final Test AUROC: {auc_test:.4f}")

    # Adversary per-class AUC
    adv_test_pred = torch.softmax(torch.tensor(
        mytools.test_adv(test_adv_loader, final_clas_adv, final_adv_adv,
                         Num_classes, device)), dim=1).numpy()
    score = 0.0
    for i in range(adv_test_pred.shape[1]):
        fpr_i, tpr_i, _ = sklearn.metrics.roc_curve(
            y_adv_test[:, i], adv_test_pred[:, i], pos_label=1)
        auc_i = sklearn.metrics.auc(fpr_i, tpr_i)
        print(f"  Adversary class {i} AUC: {auc_i:.4f}")
        score += auc_i
    print(f"  Adversary average AUC: {score / adv_test_pred.shape[1]:.4f}")

    # ── Data vs. MC diagnostic ─────────────────────────────────────────────────
    df_mc_diag   = df_mc_bkg_test.copy();            df_mc_diag  ["MC_label"] = 1.0
    df_data_diag = df_bigpreselectblind_test.copy(); df_data_diag["MC_label"] = 0.0
    df_dm = pd.concat([df_data_diag, df_mc_diag], ignore_index=True, sort=False)
    X_dm  = pd.DataFrame(
        scaler.transform(df_dm.drop(columns=["InvM","PhiKK","MC_label","isL1L1"] + _extra_drop,
                                    errors="ignore")),
        columns=df_dm.drop(columns=["InvM","PhiKK","MC_label","isL1L1"] + _extra_drop,
                           errors="ignore").columns)
    y_dm  = df_dm["MC_label"].to_numpy()

    dm_loader   = DataLoader(
        TensorDataset(_t(X_dm.to_numpy())),
        batch_size=bsize, shuffle=False)
    scores_diag = torch.sigmoid(torch.tensor(
        mytools.test_clas(dm_loader, final_clas_adv, device))).numpy()
    fpr_dm, tpr_dm, _ = sklearn.metrics.roc_curve(y_dm, scores_diag, pos_label=1)
    auc_dm = sklearn.metrics.auc(fpr_dm, tpr_dm)
    print(f"Data-vs-MC AUROC (ideally ~0.5): {auc_dm:.4f}")

    print(f"\nAll done for MASS = {MASS} MeV.")


if __name__ == "__main__":
    main()
