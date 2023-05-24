#!/bin/sh

# This uses the 'shared' partition which could be changed to 'hps' if the user has access.
hps-mc-batch slurm -q shared -r 1:1 -W 5 -c $PWD/.hpsmc -l $PWD/scratch/logs -S $PWD/scratch slic $PWD/job_slurm.json
