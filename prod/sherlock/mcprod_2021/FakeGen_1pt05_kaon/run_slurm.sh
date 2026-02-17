#!/bin/sh

export JOBDIR=/home/groups/laurenat/majd/HPS/hps-mc/prod/sherlock/mcprod_2021/FakeGen_1pt05_kaon
export RUNDIR=/scratch/users/mghrear/phiKK/kaon

hps-mc-batch slurm -o -r 1:160 -E /home/groups/laurenat/majd/HPS/hps-mc/install/bin/hps-mc-env.sh -W 5 -m 3000 -q normal -d $RUNDIR  -c $JOBDIR/.hpsmc phi_lhe_to_slic $JOBDIR/jobs.json 

