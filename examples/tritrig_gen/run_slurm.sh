#!/bin/sh
hps-mc-batch slurm -o -q hps -r 1:10000 -E /sdf/group/hps/users/bravo/src/hps-mc/install/bin/hps-mc-env.sh -W 5 -d /scratch/bravo -c /sdf/group/hps/users/bravo/src/hps-mc/examples/tritrig_gen/.hpsmc -l /scratch/bravo/logs tritrig_gen /sdf/group/hps/users/bravo/src/hps-mc/examples/tritrig_gen/jobs.json 
