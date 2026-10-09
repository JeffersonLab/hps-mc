#!/bin/bash
#SBATCH --time=03:00:00
#SBATCH --mem=5000M
#SBATCH --job-name=hpstr
#SBATCH --account=hps:hps-prod
#SBATCH --partition=roma
#SBATCH --output=/dev/null
#SBATCH --array=0-12

#/dev/null
#/sdf/scratch/users/r/rodwyer1/job.%A_%a.stdout
set -euo pipefail

# Usage: sbatch this_script.sh [VAL]
VAL="${1:-25}"
VAL2="${2:-3}"

OUTDIR="textfileoutALLTogether_NowMCInvariant"
mkdir -p "${OUTDIR}"

# Paths
PYTHON="/sdf/group/hps/users/rodwyer1/sw/acts/ParOpt_PyEnv36/bin/python3"
SCRIPT="./write_final_yields_v9_ALL3.py"   # adjust if needed
BASEMOD="decayLength8sel"

# Map array index -> mass
# indices 0..12 -> masses 50..350 step 25
IDX="${SLURM_ARRAY_TASK_ID}"
MASS=$((50 + 25 * IDX))

echo "SLURM_ARRAY_TASK_ID=${IDX} -> MASS=${MASS} MeV"

# 12 log-spaced epsilon values between 1e-2 and 1e-5 (inclusive)
N_EPS=12
N_VAL2=10
LOG10_MAX=-2.0
LOG10_MIN=-5.0

for ((i=0; i<${N_EPS}; i++)); do
  for ((j=0; j<${N_VAL2}; j++)); do
    # Compute epsilon (log-spaced)
    EPS=$(awk -v i="$i" -v n="$N_EPS" -v lmax="$LOG10_MAX" -v lmin="$LOG10_MIN" 'BEGIN{
      exp10 = lmax + (lmin - lmax) * i / (n - 1.0);
      val   = exp(log(10.0) * exp10);
      printf "%.8e", val;
    }')

    OUTTXT="${OUTDIR}/m${MASS}_Val${VAL}_epsIdx${i}_projIdx${j}.txt"

    echo "Running mass=${MASS} MeV, eps=${EPS}, Val=${VAL},Val2=${j} -> ${OUTTXT}"

    "${PYTHON}" "${SCRIPT}" \
      --mass "${MASS}" \
      --epsilon "${EPS}" \
      --Val "${VAL}" \
      --Val2 "${j}" \
      --base-module "${BASEMOD}" \
      --outtxt "${OUTTXT}"
  done
done

