#!/usr/bin/scl enable devtoolset-8 -- /bin/bash
#SBATCH --ntasks=1
#SBATCH --time=4:00:00
#SBATCH --mem=6000M
#SBATCH --array=1
#SBATCH --partition=shared
#SBATCH --job-name=trkTruTuples
#SBATCH --output=/dev/null

export FIRST_ID=0
export JOB_ID=$(($SLURM_ARRAY_TASK_ID+$FIRST_ID))
source /sdf/home/a/alspellm/.bashrc
export JOBDIR=/sdf/home/a/alspellm/work/run/trackTruthRelations_05032021/2019/mc/tritrig_beam/ana/hpstr/
export RUNDIR=/scratch/alspellm/ntuples/$JOB_ID
mkdir -p $RUNDIR
mkdir $RUNDIR/../logs
cd $RUNDIR
/usr/bin/python3.6 /sdf/group/hps/users/alspellm/src/hps-mc/install/lib/python/hpsmc/job.py run -o $RUNDIR/../logs/job.${JOB_ID}.out -e $RUNDIR/../logs/job.${JOB_ID}.err -l $RUNDIR/../logs/job.${JOB_ID}.log -d $RUNDIR -c $JOBDIR/.hpsmc -i $JOB_ID hpstr $JOBDIR/job.json

