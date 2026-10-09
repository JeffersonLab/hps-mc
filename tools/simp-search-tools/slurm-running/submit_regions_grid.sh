#!/bin/bash
#SBATCH --time=01:30:00
#SBATCH --mem=16000M
#SBATCH --job-name=hpstr
#SBATCH --account=hps:hps-prod
#SBATCH --partition=milano
#SBATCH --output=/sdf/scratch/users/r/rodwyer1/job.%A_%a.stdout
#SBATCH --array=4,10,6,12          # one task per hit category: isL1L1,isL1L2,isL2L2,isL2L3
#SBATCH --ntasks=1


# ---------------------------------------------------------------------------
# Usage:
#   mkdir -p logs
#   sbatch submit_regions_grid.sh
#
# Each array task maps to one task-id (suffix):
#   4  -> isL1L1_cut
#   10 -> isL1L2_cut
#   6  -> isL2L2_cut
#   12 -> isL2L3_cut
#
# The script loads all batch ROOT files (run <= RUN_CAP) exactly once per
# SLURM task, then runs the full (mass, epsilon) grid for that hit category.
# ---------------------------------------------------------------------------

set -euo pipefail

# --- Paths (edit as needed) -------------------------------------------------
SCRIPT_DIR="/sdf/group/hps/users/rodwyer1/run/reach_curves/optimization"
PYTHON_SCRIPT="${SCRIPT_DIR}/make_maxZbi_onthefly_6526.py"
#make_maxZbi_onthefly_6126_v6.py"
OUTDIR="${SCRIPT_DIR}/output6926"
#output6826"
BASE_MODULE="decayLength8sel"

# --- Run --------------------------------------------------------------------
mkdir -p "${OUTDIR}"

echo "=========================================="
echo "SLURM job  : ${SLURM_JOB_ID}"
echo "Array task : ${SLURM_ARRAY_TASK_ID}"
echo "Node       : $(hostname)"
echo "Start time : $(date)"
echo "Outdir     : ${OUTDIR}"
echo "=========================================="

/sdf/group/hps/users/rodwyer1/sw/acts/ParOpt_PyEnv36/bin/python3 "${PYTHON_SCRIPT}" \
    --task-id   "${SLURM_ARRAY_TASK_ID}" \
    --outdir    "${OUTDIR}" \
    --base-module "${BASE_MODULE}"

echo "Done: $(date)"
