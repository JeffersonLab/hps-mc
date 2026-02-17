#!/bin/sh

export JOBDIR=/home/groups/laurenat/majd/HPS/hps-mc/prod/sherlock/mcprod_2019/wab/sig_to_recon
export RUNDIR=/scratch/users/mghrear/wab_spaced

hps-mc-batch slurm -o -r 1:100 -E /home/groups/laurenat/majd/HPS/hps-mc/install/bin/hps-mc-env.sh -W 16 -m 4000 -q normal -d $RUNDIR  -c $JOBDIR/.hpsmc signal_to_recon $JOBDIR/jobs.json 

