#!/bin/sh
hps-mc-batch slurm -o -q shared -m 4000 -E /sdf/group/hps/users/bravo/src/hps-mc/install/bin/hps-mc-env.sh -W 48 -d /scratch/bravo/tt -c /sdf/group/hps/users/bravo/src/hps-mc/examples/slic_to_anaMC/.hpsmc -l /scratch/bravo/tt/logs slic_to_anaMC /sdf/group/hps/users/bravo/src/hps-mc/examples/slic_to_anaMC/jobs.json 
