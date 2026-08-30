import uproot
import awkward as ak
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.metrics import roc_auc_score, accuracy_score
import joblib

# --- 1. File paths and branch setup ---
# Specify the input ROOT files for background and signal.
# (Fill these paths in with the actual file locations before running.)
#background_file = "/sdf/group/hps/users/rodwyer1/run/reach_curves/datafiles/pres/bigpreselect.root"
#signal_file = "/sdf/data/hps/users/rodwyer1/SIMPS/allmassesD/signalMeVpres.root"
background_file = "/sdf/group/hps/users/rodwyer1/run/reach_curves/datafiles/pres2/bigpreselectblind.root"
signal_file = "/sdf/group/hps/users/rodwyer1/run/BigSIMPCollection2021/PRESELECTION/simpSIGpres.root"

tree_name = "preselection"  # Tree name to load from the ROOT files

# Define the branches to read, matching those used in write_final_yields_v2.py.
# These include all features needed for the BDT.
branches = [
    "psum", "vertex.pos_",              # invariant mass, p(sum), and vertex position (with fX,fY,fZ)
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
    # Note: We omit "ele.track_.hit_layers_" and "pos.track_.hit_layers_" as features,
    # because they are array-like (lists of hit layers) rather than simple numeric variables.
    # These were included in the yield script for selection logic, not for BDT training.
]
#"vertex.invM_",
#"vertex.invMerr_",

# Prepare containers for data and labels
all_data = []    # will hold feature arrays for each file
all_labels = []  # will hold class labels for each file (0 for background, 1 for signal)
flattened_names = []  # will capture the final feature names (for importance ranking)

# Define file list with labels: 0 = background, 1 = signal
files_and_labels = [
    (background_file, 0),
    (signal_file, 1)
]

# --- 2. Data loading and flattening ---
for filepath, label in files_and_labels:
    # Open the ROOT file and get the specified TTree
    with uproot.open(f"{filepath}:{tree_name}") as tree:
        # Read all desired branches at once using uproot
        arrays = tree.arrays(branches)
        arrays2 = tree.arrays(["vertex.invM_"])

        # Convert to numpy and apply any preselection masks similar to the BDT script:
        invM = ak.to_numpy(arrays2["vertex.invM_"])             # Invariant mass of the vertex (GeV)
        # vertex.pos_ is a struct (e.g., with fields fX, fY, fZ). Convert to numpy structured array:
        vertex_pos = ak.to_numpy(arrays["vertex.pos_"])
        z_coords = vertex_pos["fZ"]                            # Extract the z-coordinate (displacement along beam)
        psum_values = ak.to_numpy(arrays["psum"])              # Sum of track momenta (GeV)

        # Apply the same loose preselection cuts as in the BDT training:
        # - Invariant mass window: invM < 0.18 GeV (and > -100 as a trivial lower bound)
        # - Vertex displacement: z > 10.0 mm (selecting displaced vertices far from the target)
        cut_mask = (invM > -100.0) & (invM < 0.18) & (z_coords > 10.0)
        # If no events pass this preselection, skip this file to avoid empty arrays
        if np.count_nonzero(cut_mask) == 0:
            continue

        # Flatten the data: build a 2D array of shape (num_events, num_features)
        # We'll iterate over each branch and handle structured or multi-dimensional data.
        data_parts = []    # list to accumulate column arrays
        feature_names = [] # names for each flattened feature
        for br in branches:
            # Convert branch to a NumPy array and apply the mask
            arr = ak.to_numpy(arrays[br])[cut_mask]
            if arr.dtype.fields is not None:
                # This branch is structured (e.g., vertex.pos_ has fields fX,fY,fZ)
                for field in arr.dtype.names:  # iterate over subfields
                    subarr = arr[field]
                    # Only include numeric subfields
                    if not np.issubdtype(subarr.dtype, np.number):
                        continue  # skip non-numeric types
                    data_parts.append(subarr.reshape(-1, 1))
                    feature_names.append(f"{br}.{field}")  # e.g., "vertex.pos_.fZ"
            elif np.issubdtype(arr.dtype, np.number):
                # This branch is a simple numeric array (or possibly a fixed-size vector)
                if arr.ndim == 1:
                    # 1D array: directly use as one feature
                    data_parts.append(arr.reshape(-1, 1))
                    feature_names.append(br)
                elif arr.ndim == 2:
                    # 2D array: flatten each column as a separate feature (e.g., if any branch had a fixed-length array per event)
                    for i in range(arr.shape[1]):
                        data_parts.append(arr[:, i].reshape(-1, 1))
                        feature_names.append(f"{br}[{i}]")
                else:
                    # Higher-dimensional data not expected; skip if encountered
                    continue
            else:
                # Non-numeric and non-structured data (e.g., object dtype) are skipped
                continue

        # Concatenate all feature columns horizontally to form the feature matrix for this file
        if data_parts:
            file_data = np.hstack(data_parts)  # shape: (N_events_file, N_features_total)
        else:
            file_data = np.empty((np.count_nonzero(cut_mask), 0))
        file_labels = np.full(file_data.shape[0], label, dtype=int)  # assign the class label (0 or 1) to all events from this file

        # Append to the global lists
        all_data.append(file_data)
        all_labels.append(file_labels)
        # Capture feature names from the first non-empty dataset (they should be the same for both signal and background)
        if not flattened_names:
            flattened_names = feature_names

# Combine data from both files into one dataset
X = np.vstack(all_data)   # Feature matrix (num_total_events x num_features)
y = np.concatenate(all_labels)  # Labels array (num_total_events,)

# --- 3. Split into training and testing sets ---
# We use stratified splitting to maintain the same signal/background ratio in train and test sets.
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.3, random_state=42, stratify=y
)
# Now X_train, y_train will be used to train the model, and X_test, y_test for evaluation.

# --- 4. Train the Gradient Boosting classifier ---
# Initialize the classifier. We choose a moderate tree depth (e.g., 4) to balance complexity and generalization.
clf = GradientBoostingClassifier(max_depth=4, n_estimators=100, learning_rate=0.1, random_state=42)
# - max_depth=4 allows each decision tree in the ensemble to have up to 4 levels (default is 3; 4 can capture more complex patterns).
# - n_estimators=100 and learning_rate=0.1 are default settings (100 trees). You can adjust these for tuning.
# - random_state=42 ensures reproducibility of the training process.

# Train the model on the training dataset
clf.fit(X_train, y_train)

# --- 5. Evaluate model performance on the test set ---
# Use the trained model to predict probabilities and classes for the test set.
y_pred_proba = clf.predict_proba(X_test)[:, 1]   # Probability of class 1 (signal) for each test event
y_pred_label = clf.predict(X_test)              # Predicted class labels (0 or 1)

# Compute evaluation metrics:
roc_auc = roc_auc_score(y_test, y_pred_proba)
accuracy = accuracy_score(y_test, y_pred_label)

# Print out the performance metrics
print(f"Test ROC AUC = {roc_auc:.3f}")
print(f"Test Accuracy = {accuracy:.3f}")

# These metrics give an idea of how well the classifier is separating signal and background.
# ROC AUC (Area Under the ROC Curve) is threshold-independent and 1.0 would be a perfect classifier (0.5 is random guessing).
# Accuracy is the fraction of correct classifications at the default 0.5 threshold.

# --- 6. Save the trained model to disk ---
model_filename = "bdt_model.joblib"
joblib.dump(clf, model_filename)
print(f"Saved trained model to '{model_filename}'")

# The model is saved using joblib, which preserves the sklearn model object (including its learned decision trees).
# This file can be loaded later in write_final_yields_v2.py using joblib.load, to apply the model for scoring events.

# --- 7. Write feature importance rankings to a file ---
importances = clf.feature_importances_
# feature_importances_ gives the relative importance of each feature in the trained model (based on reduction in impurity).
# We will rank these importances to see which features were most useful in the classification.

sorted_idx = np.argsort(importances)[::-1]  # indices of features sorted by importance (descending)
with open("feature_importances.txt", "w") as f:
    for rank, idx in enumerate(sorted_idx, start=1):
        feat_name = flattened_names[idx] if idx < len(flattened_names) else f"feature_{idx}"
        f.write(f"{rank}. {feat_name}: {importances[idx]:.6f}\n")
print("Wrote feature importances to 'feature_importances.txt'")

# --- Plot: BDT score distribution for signal and background ---
import matplotlib.pyplot as plt

# Extract scores for signal and background in the test set
signal_scores = y_pred_proba[y_test == 1]
background_scores = y_pred_proba[y_test == 0]

# Create the plot
plt.figure(figsize=(8, 5))
plt.hist(signal_scores, bins=50, alpha=0.5, density=True, label='Signal', histtype='stepfilled')
plt.hist(background_scores, bins=50, alpha=0.5, density=True, label='Background', histtype='stepfilled')
plt.xlabel('BDT Score')
plt.ylabel('Density')
plt.title('BDT Response for Signal vs Background')
plt.legend(loc='upper center')
plt.grid(True)
plt.tight_layout()
plt.savefig("bdt_score_distribution.png")
print("Saved BDT response plot to 'bdt_score_distribution.png'")

