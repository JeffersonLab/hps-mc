#!/bin/sh
hps-mc-batch slurm -o -q hps -m 4000 -E /sdf/group/hps/users/bravo/src/hps-mc/install/bin/hps-mc-env.sh -W 48 -d /scratch/bravo/tt -c /sdf/group/hps/users/bravo/src/hps-mc/examples/tritrig_beam_slic_to_recon/.hpsmc -l /scratch/bravo/tt/logs tritrig_beam_slic_to_recon /sdf/group/hps/users/bravo/src/hps-mc/examples/tritrig_beam_slic_to_recon/jobs.json 
