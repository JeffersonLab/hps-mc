#!/bin/bash
source /sdf/group/hps/users/bravo/src/hps-mc/install/bin/hps-mc-env.sh
hps-mc-job run -d $PWD/scratch beam_gen job.json
