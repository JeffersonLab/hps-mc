#!/bin/sh

export JOBDIR=/home/groups/laurenat/majd/HPS/hps-mc/prod/sherlock/mc_prod_2021/TM
export RUNDIR=/scratch/users/mghrear/phiKK

hps-mc-batch slurm -o -r 1:100 -E /home/groups/laurenat/majd/HPS/hps-mc/install/bin/hps-mc-env.sh -W 9 -q normal -d $RUNDIR  -c $JOBDIR/.hpsmc TM_lhe_to_slic $JOBDIR/jobs.json 

