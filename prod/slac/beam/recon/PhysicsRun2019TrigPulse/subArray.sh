#!/usr/bin/scl enable devtoolset-8 -- /bin/bash
#SBATCH --ntasks=1
#SBATCH --time=04:00:00
#SBATCH --mem=1500M
#SBATCH --array=1-1000
#SBATCH --partition=hps
#SBATCH --output=/dev/null

source $HPSMC/install/bin/hps-mc-env.sh
export LD_LIBRARY_PATH=/sdf/group/hps/users/bravo/src/gsl-2.6/install/lib:$LD_LIBRARY_PATH

export FIRST_ID=0
export JOB_ID=$(($SLURM_ARRAY_TASK_ID+$FIRST_ID))
export JOBDIR=/sdf/group/hps/users/sgaiser/src/hps-mc/prod/slac/beam/recon/PhysicsRun2019TrigPulse
export RUNDIR=$SCRATCH/beam/recon/HPS_Nominal_3pt74GeV/PhysicsRun2019TrigPulse/$JOB_ID

mkdir -p $RUNDIR
cd $RUNDIR

/bin/python3 $HPSMC_DIR/lib/python/hpsmc/job.py run -o $RUNDIR/../logs/job.${JOB_ID}.out -e $RUNDIR/../logs/job.${JOB_ID}.err -l $RUNDIR/../logs/job.${JOB_ID}.log -d $RUNDIR -c $JOBDIR/.hpsmc -i ${JOB_ID} readout_recon $JOBDIR/jobs.json

# HPSMC points to hps-mc directory. You might need to set this variable before running this script.
