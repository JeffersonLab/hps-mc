import uproot
import awkward as ak
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.metrics import roc_auc_score, accuracy_score
import joblib
import argparse
import matplotlib.pyplot as plt

parser = argparse.ArgumentParser()
parser.add_argument("RUN", type=int)
args = parser.parse_args()

# --- 1. File paths and mass-point setup ---
background_file = "/sdf/group/hps/users/rodwyer1/run/reach_curves/datafiles/pres2/bigpreselectblind.root"

RUN = args.RUN
runs = [30 + i * 15 for i in range(13)]

if RUN < 0 or RUN >= len(runs):
    raise ValueError("RUN must be between 0 and " + str(len(runs) - 1))

signal_file = "/sdf/group/hps/users/rodwyer1/run/BigSIMPCollection2021/PRESELECTION/simp" + str(runs[RUN]) + "pres.root"
tree_name = "preselection"

# --- 2. Branch setup ---
# Keep the newer branch set, including the two newer isolation-significance features
branches = [
    "psum", "vertex.pos_",
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
    "ele_L1_iso_significance", "pos_L1_iso_significance"
]

all_data = []
all_labels = []
flattened_names = []

files_and_labels = [
    (background_file, 0),
    (signal_file, 1)
]

# --- 3. Data loading and flattening ---
for filepath, label in files_and_labels:
    with uproot.open(f"{filepath}:{tree_name}") as tree:
        arrays = tree.arrays(branches)
        arrays2 = tree.arrays(["vertex.invM_"])

        invM = ak.to_numpy(arrays2["vertex.invM_"])
        vertex_pos = ak.to_numpy(arrays["vertex.pos_"])
        z_coords = vertex_pos["fZ"]
        # Mass-dependent window copied from the older mass-dependent script
        center_geV = float(runs[RUN])/1000.0 #1.8 * float(runs[RUN]) / (1000.0 * 3.0)
        print(center_geV)
        print("Training mass point (MeV):", runs[RUN])
        print("Mass-window center (GeV):", center_geV)
        print("Mass window:", center_geV - 0.002, "to", center_geV + 0.002)

        cut_mask = (
            (invM > -100.0)
            & (invM < 100.0)
            #& (z_coords > 10.0)
            & (invM > (center_geV - 0.002))
            & (invM < (center_geV + 0.002))
        )

        if np.count_nonzero(cut_mask) == 0:
            print("No events passed cut_mask for file:", filepath)
            continue

        data_parts = []
        feature_names = []

        for br in branches:
            arr = ak.to_numpy(arrays[br])[cut_mask]

            if arr.dtype.fields is not None:
                for field in arr.dtype.names:
                    subarr = arr[field]
                    if not np.issubdtype(subarr.dtype, np.number):
                        continue
                    data_parts.append(subarr.reshape(-1, 1))
                    feature_names.append(f"{br}.{field}")

            elif np.issubdtype(arr.dtype, np.number):
                if arr.ndim == 1:
                    data_parts.append(arr.reshape(-1, 1))
                    feature_names.append(br)
                elif arr.ndim == 2:
                    for i in range(arr.shape[1]):
                        data_parts.append(arr[:, i].reshape(-1, 1))
                        feature_names.append(f"{br}[{i}]")
                else:
                    continue
            else:
                continue

        if data_parts:
            file_data = np.hstack(data_parts)
        else:
            file_data = np.empty((np.count_nonzero(cut_mask), 0))

        file_labels = np.full(file_data.shape[0], label, dtype=int)

        all_data.append(file_data)
        all_labels.append(file_labels)

        if not flattened_names:
            flattened_names = feature_names

print(len(file_data))
if len(all_data) == 0:
    raise RuntimeError("No data survived selection in either file.")

X = np.vstack(all_data)
y = np.concatenate(all_labels)

# --- Remove rows with NaN/inf before splitting ---
finite_mask = np.all(np.isfinite(X), axis=1)

print("Total events before finite cleaning:", X.shape[0])
print("Events with all-finite features:", np.count_nonzero(finite_mask))
print("Events removed for NaN/inf:", np.count_nonzero(~finite_mask))

X = X[finite_mask]
y = y[finite_mask]

if X.shape[0] == 0:
    raise RuntimeError("No fully finite events remain after NaN/inf cleaning.")

# --- 4. Train/test split ---
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.3, random_state=42, stratify=y
)

# --- 5. Train classifier ---
clf = GradientBoostingClassifier(
    max_depth=4,
    n_estimators=100,
    learning_rate=0.1,
    random_state=42
)

clf.fit(X_train, y_train)

# --- 6. Evaluate ---
y_pred_proba = clf.predict_proba(X_test)[:, 1]
y_pred_label = clf.predict(X_test)

roc_auc = roc_auc_score(y_test, y_pred_proba)
accuracy = accuracy_score(y_test, y_pred_label)

print(f"Test ROC AUC = {roc_auc:.3f}")
print(f"Test Accuracy = {accuracy:.3f}")

# --- 7. Save model ---
model_filename = "bdt_model_" + str(runs[RUN]) + ".joblib"
joblib.dump(clf, model_filename)
print(f"Saved trained model to '{model_filename}'")

# --- 8. Save feature importances ---
importances = clf.feature_importances_
sorted_idx = np.argsort(importances)[::-1]

feature_importance_filename = "feature_importances_" + str(runs[RUN]) + ".txt"
with open(feature_importance_filename, "w") as f:
    for rank, idx in enumerate(sorted_idx, start=1):
        feat_name = flattened_names[idx] if idx < len(flattened_names) else f"feature_{idx}"
        f.write(f"{rank}. {feat_name}: {importances[idx]:.6f}\n")

print(f"Wrote feature importances to '{feature_importance_filename}'")

# --- 9. Plot score distributions ---
signal_scores = y_pred_proba[y_test == 1]
background_scores = y_pred_proba[y_test == 0]

plt.figure(figsize=(8, 5))
plt.hist(signal_scores, bins=50, alpha=0.5, density=True, label="Signal", histtype="stepfilled")
plt.hist(background_scores, bins=50, alpha=0.5, density=True, label="Background", histtype="stepfilled")
plt.xlabel("BDT Score")
plt.ylabel("Density")
plt.title("BDT Response for Signal vs Background")
plt.legend(loc="upper center")
plt.grid(True)
plt.tight_layout()

plot_filename = "bdt_score_distribution_" + str(runs[RUN]) + ".png"
plt.savefig(plot_filename)
print(f"Saved BDT response plot to '{plot_filename}'")
