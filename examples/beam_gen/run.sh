#!/bin/bash
source /sdf/group/hps/users/bravo/src/hps-mc/install/bin/hps-mc-env.sh
hps-mc-job run -d $PWD/scratch -l $PWD/logs/job.log beam_gen job.json
