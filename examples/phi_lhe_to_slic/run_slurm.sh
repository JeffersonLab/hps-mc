#!/bin/sh

export JOBDIR=/home/groups/laurenat/majd/hps-mc-runs/phiKK/phi_lhe_to_slic
export RUNDIR=/home/groups/laurenat/majd/scratch/phiKK

hps-mc-batch slurm -o -r 1:176 -E /home/groups/laurenat/majd/HPS/hps-mc/install/bin/hps-mc-env.sh -W 9 -q normal -d $RUNDIR  -c $JOBDIR/.hpsmc phi_lhe_to_slic $JOBDIR/jobs.json 

