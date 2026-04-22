#!/bin/bash
# ══════════════════════════════════════════════════════════════════════════════
# run_ablation_slurm.sh
# ─────────────────────
# SLURM array job: one task per surviving feature probe + one baseline task.
#
# Index mapping
# ─────────────
#   Task 0          → baseline run  (probe_idx = -1, no extra exclusion)
#   Task 1..N       → probe SURVIVING_FEATURES[task-1]
#
# Each task calls:
#   python train_ablation.py \
#     --probe-idx     <full-list index, or -1> \
#     --excluded-json '<JSON list of culled full-list indices>' \
#     --mass          $MASS \
#     --output-dir    $OUTPUT_DIR \
#     --cull-count    $N_EXCLUDED
#
# Full feature list (37 total from bigpreselectblind.pk):
#   idx  0  vertex_pos_x
#   idx  1  vertex_pos_y
#   idx  2  vertex_pos_z
#   idx  3  psum
#   idx  4  ele_track_n_hits
#   idx  5  ele_track_d0
#   idx  6  ele_track_phi0
#   idx  7  ele_track_z0
#   idx  8  ele_track_tan_lambda
#   idx  9  ele_track_px
#   idx 10  ele_track_py
#   idx 11  ele_track_pz
#   idx 12  ele_track_chi2
#   idx 13  ele_track_x_at_ecal
#   idx 14  ele_track_y_at_ecal
#   idx 15  ele_track_z_at_ecal
#   idx 16  pos_track_n_hits       ← EXCLUDED (baseline cull)
#   idx 17  pos_track_d0
#   idx 18  pos_track_phi0
#   idx 19  pos_track_z0
#   idx 20  pos_track_tan_lambda   ← EXCLUDED (baseline cull)
#   idx 21  pos_track_px
#   idx 22  pos_track_py
#   idx 23  pos_track_pz
#   idx 24  pos_track_chi2
#   idx 25  pos_track_x_at_ecal
#   idx 26  pos_track_y_at_ecal
#   idx 27  pos_track_z_at_ecal
#   idx 28  vertex_chi2
#   idx 29  vtx_proj_sig
#   idx 30  vtx_proj_x_sig         ← EXCLUDED (baseline cull)
#   idx 31  vtx_proj_y_sig
#   idx 32  ele_L1_iso_significance
#   idx 33  pos_L1_iso_significance
#
# Currently culled (EXCLUDED_FEATURES): indices 16, 20, 30
# Surviving: 34 features → 34 probe tasks + 1 baseline = 35 total tasks (0-34)
# ══════════════════════════════════════════════════════════════════════════════

#SBATCH --time=05:00:00
#SBATCH --mem=7000M
#SBATCH --job-name=ablation
#SBATCH --account=hps:hps-prod
#SBATCH --partition=milano
#SBATCH --output=/sdf/scratch/users/r/rodwyer1/ablation.%A_%a.stdout
#SBATCH --error=/sdf/scratch/users/r/rodwyer1/ablation.%A_%a.stderr
#SBATCH --array=0-34


# ══════════════════════════════════════════════════════════════════════════════
#  USER CONFIGURATION
# ══════════════════════════════════════════════════════════════════════════════

MASS=180
OUTPUT_DIR="ablation_outputs"
WORK_DIR="/sdf/group/hps/users/rodwyer1/run/reach_curves/anntraining/training3"

# Full-list indices of features already permanently culled.
# These are passed to train_ablation.py as --excluded-json.
# Add new culled indices here each round.
EXCLUDED_INDICES='[16, 20, 30]'   # pos_track_n_hits, pos_track_tan_lambda, vtx_proj_x_sig
N_EXCLUDED=3

# Surviving features for this round: full-list name → full-list index.
# One SLURM task per entry (task 0 = baseline, task k = probe entry k-1).
#
# Format: SURVIVING_FULL_INDICES[i] = full-list index of surviving feature i
# This must stay in sync with EXCLUDED_INDICES above.
SURVIVING_FULL_INDICES=(
     0   #  0 → vertex_pos_x
     1   #  1 → vertex_pos_y
     2   #  2 → vertex_pos_z
     3   #  3 → psum
     4   #  4 → ele_track_n_hits
     5   #  5 → ele_track_d0
     6   #  6 → ele_track_phi0
     7   #  7 → ele_track_z0
     8   #  8 → ele_track_tan_lambda
     9   #  9 → ele_track_px
    10   # 10 → ele_track_py
    11   # 11 → ele_track_pz
    12   # 12 → ele_track_chi2
    13   # 13 → ele_track_x_at_ecal
    14   # 14 → ele_track_y_at_ecal
    15   # 15 → ele_track_z_at_ecal
    17   # 16 → pos_track_d0          (skipping idx 16 = pos_track_n_hits, culled)
    18   # 17 → pos_track_phi0
    19   # 18 → pos_track_z0
    21   # 19 → pos_track_px          (skipping idx 20 = pos_track_tan_lambda, culled)
    22   # 20 → pos_track_py
    23   # 21 → pos_track_pz
    24   # 22 → pos_track_chi2
    25   # 23 → pos_track_x_at_ecal
    26   # 24 → pos_track_y_at_ecal
    27   # 25 → pos_track_z_at_ecal
    28   # 26 → vertex_chi2
    29   # 27 → vtx_proj_sig
    31   # 28 → vtx_proj_y_sig        (skipping idx 30 = vtx_proj_x_sig, culled)
    32   # 29 → ele_L1_iso_significance
    33   # 30 → pos_L1_iso_significance
    # NOTE: only 31 entries above — verify against your actual pickle column order!
    # The notebook output shows 34 columns in X_train when all 3 are culled,
    # so add the remaining 3 surviving feature indices here if the pickle has more columns.
)

# ══════════════════════════════════════════════════════════════════════════════
#  Derived — do not edit
# ══════════════════════════════════════════════════════════════════════════════

N_SURVIVING=${#SURVIVING_FULL_INDICES[@]}
TASK_ID=${SLURM_ARRAY_TASK_ID}

echo "=================================================="
echo "SLURM job    : ${SLURM_JOB_ID}, task ${TASK_ID}"
echo "Cull count   : ${N_EXCLUDED}"
echo "Surviving    : ${N_SURVIVING}"
echo "Mass         : ${MASS}"
echo "=================================================="

# ── Environment ───────────────────────────────────────────────────────────────
source /sdf/group/hps/sw2/conda/etc/profile.d/conda.sh
source ~/.bashrc
conda activate /sdf/home/r/rodwyer1/.conda/envs/jupyter_root

cd "${WORK_DIR}"
mkdir -p "${OUTPUT_DIR}"

# ── Map task ID → probe_idx ───────────────────────────────────────────────────
if [ "${TASK_ID}" -eq 0 ]; then
    PROBE_IDX=-1
    PROBE_NAME="baseline"
    echo "[slurm] Task 0 → baseline run (no extra feature excluded)"
else
    SURV_IDX=$(( TASK_ID - 1 ))

    if [ "${SURV_IDX}" -ge "${N_SURVIVING}" ]; then
        echo "[slurm] ERROR: SURV_IDX=${SURV_IDX} >= N_SURVIVING=${N_SURVIVING}. Check --array range."
        exit 1
    fi

    PROBE_IDX=${SURVIVING_FULL_INDICES[$SURV_IDX]}
    PROBE_NAME="full_idx_${PROBE_IDX}"
    echo "[slurm] Task ${TASK_ID} → surviving[${SURV_IDX}] = full-list index ${PROBE_IDX}"
fi

echo "[slurm] probe_idx=${PROBE_IDX}  excluded_json=${EXCLUDED_INDICES}"

# ── Launch ────────────────────────────────────────────────────────────────────
python -u train_ablation5.py \
    --probe-idx     "${PROBE_IDX}" \
    --excluded-json "${EXCLUDED_INDICES}" \
    --mass          "${MASS}" \
    --output-dir    "${OUTPUT_DIR}" \
    --cull-count    "${N_EXCLUDED}"

EXIT_CODE=$?
echo "[slurm] train_ablation.py exited with code ${EXIT_CODE}"
echo "[slurm] Task ${TASK_ID} (probe='${PROBE_NAME}') finished."
exit ${EXIT_CODE}

