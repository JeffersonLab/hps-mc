#!/usr/bin/scl enable devtoolset-8 -- /bin/bash
#SBATCH --ntasks=1
#SBATCH --time=02:00:00
#SBATCH --mem=1000M
#SBATCH --array=1-500
#SBATCH --partition=hps
#SBATCH --job-name=anaMC
#SBATCH --output=/dev/null

export FIRST_ID=0
export JOB_ID=$(($SLURM_ARRAY_TASK_ID+$FIRST_ID))
source /sdf/group/hps/users/bravo/src/hps-mc/install/bin/hps-mc-env.sh
mkdir -p /scratch/bravo/$JOB_ID
cd /scratch/bravo/$JOB_ID
/usr/bin/python3.6 /sdf/group/hps/users/bravo/src/hps-mc/install/lib/python/hpsmc/job.py run -o /scratch/bravo/logs/job.${JOB_ID}.out -e /scratch/bravo/logs/job.${JOB_ID}.err -l /scratch/bravo/logs/job.${JOB_ID}.log -d /scratch/bravo/$JOB_ID -c /sdf/group/hps/users/bravo/src/hps-mc/examples/slic_to_anaMC/.hpsmc -i $JOB_ID /sdf/group/hps/users/bravo/src/hps-mc/install/lib/python/jobs/slic_to_anaMC_job.py /sdf/group/hps/users/bravo/src/hps-mc/examples/slic_to_anaMC/jobs.json
