#!/usr/bin/scl enable devtoolset-8 -- /bin/bash
#SBATCH --ntasks=1
#SBATCH --time=36:00:00
#SBATCH --mem=2000M
#SBATCH --array=1-200
#SBATCH --partition=hps
#SBATCH --job-name=fee
#SBATCH --output=/dev/null

export FIRST_ID=0
export JOB_ID=$(($SLURM_ARRAY_TASK_ID+$FIRST_ID))
source /sdf/group/hps/users/bravo/src/hps-mc/install/bin/hps-mc-env.sh
mkdir -p /scratch/bravo/$JOB_ID
cd /scratch/bravo/$JOB_ID
/usr/bin/python3.6 /sdf/group/hps/users/bravo/src/hps-mc/install/lib/python/hpsmc/job.py run -o /scratch/bravo/logs/job.${JOB_ID}.out -e /scratch/bravo/logs/job.${JOB_ID}.err -l /scratch/bravo/logs/job.${JOB_ID}.log -d /scratch/bravo/$JOB_ID -c /sdf/group/hps/users/bravo/src/hps-mc/examples/fee_slic_to_recon/.hpsmc -i $JOB_ID /sdf/group/hps/users/bravo/src/hps-mc/install/lib/python/jobs/slic_to_recon_job.py /sdf/group/hps/users/bravo/src/hps-mc/examples/fee_slic_to_recon/jobs.json > /scratch/bravo/logs/job.${JOB_ID}.stdout
