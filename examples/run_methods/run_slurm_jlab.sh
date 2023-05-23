#!/bin/sh

# Use this ifarm partition (or other valid partition) for JLab
hps-mc-batch slurm -o -q ifarm -r 1:1 -W 5 -d /volatile/hallb/hps/$USER/hpsmc_test -c $PWD/.hpsmc -l $PWD/logs slic $PWD/job_slurm.json
