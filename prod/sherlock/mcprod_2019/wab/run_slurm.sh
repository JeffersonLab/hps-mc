#!/bin/sh

export JOBDIR=/sdf/home/s/sgaiser/src/hps-mc/prod/slac/mcprod_2021/wab
export RUNDIR=/fs/ddn/sdf/scratch/s/sgaiser/wab/gen

hps-mc-batch slurm -o -r 1:500 -E /sdf/home/s/sgaiser/src/hps-mc/install/bin/hps-mc-env.sh -W 9 -q milano -A HPS:hps-prod -d $RUNDIR  -c $JOBDIR/.hpsmc -l /sdf/data/hps/physics2021/mc/gen/wab/pass01/logs wab_gen_to_slic $JOBDIR/jobs.json 

