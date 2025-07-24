#!/bin/sh

export JOBDIR=/home/groups/laurenat/majd/HPS/hps-mc/prod/sherlock/mcprod_2019/tritrig
export RUNDIR=/home/groups/laurenat/majd/scratch/tritrig

hps-mc-batch slurm -o -r 1:100 -E /home/groups/laurenat/majd/HPS/hps-mc/install/bin/hps-mc-env.sh -W 9 -q normal -d $RUNDIR  -c $JOBDIR/.hpsmc tritrig_gen_to_slic $JOBDIR/jobs.json 

