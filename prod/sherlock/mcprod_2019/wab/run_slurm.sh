#!/bin/sh

export JOBDIR=/home/groups/laurenat/majd/HPS/hps-mc/prod/sherlock/mcprod_2019/wab
export RUNDIR=/scratch/users/mghrear/wab

hps-mc-batch slurm -o -r 1:1000 -E /home/groups/laurenat/majd/HPS/hps-mc/install/bin/hps-mc-env.sh -W 16 -m 3000 -q normal -d $RUNDIR  -c $JOBDIR/.hpsmc  wab_gen_to_slic $JOBDIR/jobs.json 

