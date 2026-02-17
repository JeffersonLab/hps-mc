#!/bin/sh

export JOBDIR=/home/groups/laurenat/majd/HPS/hps-mc/prod/sherlock/mcprod_2021/FakeGen_0pt8_pion/signal_pulser_overlay_to_recon
export RUNDIR=/scratch/users/mghrear/phiKK_pion

hps-mc-batch slurm -o -r 1:100 -E /home/groups/laurenat/majd/HPS/hps-mc/install/bin/hps-mc-env.sh -W 9 -q normal -d $RUNDIR  -c $JOBDIR/.hpsmc signal_pulser_slcio_overlay_to_recon $JOBDIR/jobs.json 

