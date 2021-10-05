#!/bin/sh
hps-mc-batch pool -o -p 1 -r 0:1 -d $PWD/scratch2 -l $PWD/logs hadd jobs_hadd.json
