#!/bin/sh
hps-mc-batch slurm -o -q hps -m 3000 -E /sdf/group/hps/users/bravo/src/hps-mc/install/bin/hps-mc-env.sh -W 5 -d /scratch/bravo/beam -c /sdf/group/hps/users/bravo/src/hps-mc/examples/beam_slic/.hpsmc -l /scratch/bravo/beam/logs beam_slic /sdf/group/hps/users/bravo/src/hps-mc/examples/beam_slic/jobs.json 
