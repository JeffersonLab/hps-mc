#!/usr/bin/scl enable devtoolset-8 -- /bin/bash
#SBATCH --ntasks=1
#SBATCH --time=24:00:00
#SBATCH --partition=shared
#SBATCH --job-name=examples

#hps-mc-job run -d $PWD/scratch -l logs/job.log -o logs/job.out -e logs/job.err tritrig job.json
hps-mc-job run -d $PWD/scratch tritrig_slic_full_chain job.json
