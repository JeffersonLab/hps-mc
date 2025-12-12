#!/bin/sh

export JOBDIR=/home/groups/laurenat/majd/HPS/hps-mc/prod/sherlock/mcprod_2021/phiKK
export RUNDIR=/scratch/users/mghrear/phiKK

hps-mc-batch slurm -o -r 1:120 -E /home/groups/laurenat/majd/HPS/hps-mc/install/bin/hps-mc-env.sh -W 9 -q normal -d $RUNDIR  -c $JOBDIR/.hpsmc phi_lhe_to_slic $JOBDIR/jobs.json 

