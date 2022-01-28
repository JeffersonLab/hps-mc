#!/bin/sh
#hps-mc-batch pool -o -r 1:4 -p 4 -d $PWD/scratch -l $PWD/logs dummy jobs.json
hps-mc-batch pool -l $PWD/logs -d $PWD/scratch dummy jobs.json
