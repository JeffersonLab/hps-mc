#!/bin/sh
hps-mc-batch pool -o -p 2 -r 1:2 -d $PWD/scratch -l $PWD/logs rawsvthit_histos jobs.json
hps-mc-batch pool -o -p 1 -r 1:1 -d $PWD/scratch2 -l $PWD/logs hadd jobs_hadd.json
hps-mc-batch pool -o -p 3 -r 1:3 -d $PWD/scratch2 -l $PWD/logs offline_baselines jobs_blfits.json
hadd <path to baseline fits>

