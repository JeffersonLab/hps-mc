#!/bin/sh

# Use this ifarm partition (or other valid partition) for JLab
hps-mc-batch slurm -o -q ifarm -r 1:1 -E $HPSMC_DIR/bin/hps-mc-env.sh -W 5 -d $PWD/scratch -c $PWD/.hpsmc -l $PWD/scratch/logs -S $PWD/scratch slic $PWD/job_slurm.json
