#!/bin/sh
hps-mc-batch pool -o -p 3 -r 1:3 -d $PWD/scratch2 -l $PWD/logs offline_baselines jobs_blfits.json
