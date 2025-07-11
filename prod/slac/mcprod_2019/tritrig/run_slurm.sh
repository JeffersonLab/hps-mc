#!/bin/sh

export JOBDIR=/sdf/home/s/sgaiser/src/hps-mc/prod/slac/mcprod_2021/tritrig
export RUNDIR=/fs/ddn/sdf/scratch/s/sgaiser/prod/tritrig/gen

hps-mc-batch slurm -o -r 1:100 -E /sdf/home/s/sgaiser/src/hps-mc/install/bin/hps-mc-env.sh -W 9 -q milano -A HPS:hps-prod -d $RUNDIR  -c $JOBDIR/.hpsmc -l /sdf/data/hps/physics2021/mc/gen/tritrig/pass01/logs tritrig_gen_to_slic $JOBDIR/jobs.json 

