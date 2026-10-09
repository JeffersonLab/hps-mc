#!/bin/bash
#SBATCH --time=00:30:00
#SBATCH --mem=5000M
#SBATCH --job-name=hpstr
#SBATCH --account=hps:hps-prod
#SBATCH --partition=milano
#SBATCH --output=/sdf/scratch/users/r/rodwyer1/job.%A_%a.stdout
#SBATCH --array=0-12

# Generate MASS values
MASSES=($(seq 30 15 210))

# Select the one for this array task
MASS=${MASSES[$SLURM_ARRAY_TASK_ID]}


# >>> conda initialize >>>
source /sdf/group/hps/sw2/conda/etc/profile.d/conda.sh
# <<< conda initialize <<<

source ~/.bashrc

conda activate /sdf/home/r/rodwyer1/.conda/envs/jupyter_root

python ANN_NHP2.py ${MASS}

python3 scale_shifter.py ${MASS}
wait $!
rm scaler_2021_v9_pass5_run42_QualCuts_${MASS}_v3.pkl

echo "Finished MASS=${MASS}"
