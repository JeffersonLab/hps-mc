#!/bin/sh
hps-mc-batch slurm -o -q shared -r 3001:4000 -E /sdf/group/hps/users/bravo/src/hps-mc/install/bin/hps-mc-env.sh -W 5 -d /scratch/bravo -c /sdf/group/hps/users/bravo/src/hps-mc/examples/beam_gen/.hpsmc -l /scratch/bravo/logs beam_gen /sdf/group/hps/users/bravo/src/hps-mc/examples/beam_gen/jobs.json 
