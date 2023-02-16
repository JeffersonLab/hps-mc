#!/bin/sh
hps-mc-batch slurm -o -q hps -r 1:1 -E $HPSMC_DIR/bin/hps-mc-env.sh -W 5 -d $HPSMC/examples/run_methods/scratch -c $HPSMC/examples/run_methods/.hpsmc -l $HPSMC/examples/run_methods/scratch/logs slic /sdf/group/hps/users/sgaiser/src/hps-mc/examples/run_methods/job2.json 
