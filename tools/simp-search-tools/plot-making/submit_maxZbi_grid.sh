#!/bin/bash
#SBATCH --time=00:10:00
#SBATCH --mem=3000M
#SBATCH --job-name=hpstr
#SBATCH --account=hps:hps-prod
#SBATCH --partition=roma
#SBATCH --output=/sdf/scratch/users/r/rodwyer1/job.%A_%a.stdout
#SBATCH --array=0-12
#SBATCH --ntasks=1

# Usage:
#   sbatch submit_maxZbi_grid.sh <indir> <outdir>
#
# Example:
#   sbatch submit_maxZbi_grid.sh textfileoutALLTogether33026 my_plots

INDIR="$1"
OUTDIR="$2"

if [ -z "$INDIR" ] || [ -z "$OUTDIR" ]; then
    echo "Usage: sbatch submit_maxZbi_grid.sh <indir> <outdir>"
    exit 1
fi

mkdir -p logs
mkdir -p "$OUTDIR"

/sdf/group/hps/users/rodwyer1/sw/acts/ParOpt_PyEnv36/bin/python3 make_maxZbi_grid_worker_v2.py --task-id "$SLURM_ARRAY_TASK_ID" \
                                   --indir   "$INDIR"               \
                                   --outdir  "$OUTDIR"
