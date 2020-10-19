#!/bin/sh
hps-mc-batch pool -o -r 1:8 -p 4 -d $PWD/scratch -l $PWD/logs -c .hpsmc tritrig jobs.json
