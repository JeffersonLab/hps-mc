#!/bin/sh
hps-mc-batch pool -o -p 2 -r 4:5 -d $PWD/scratch -l $PWD/logs slic_to_anaMC jobs.json
