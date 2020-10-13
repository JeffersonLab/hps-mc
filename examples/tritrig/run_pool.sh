#!/bin/sh
hps-mc-batch pool -o -r 1:4 -p 4 -d $PWD/scratch -l $PWD/logs -c .hpsmc tritrig jobs.json
