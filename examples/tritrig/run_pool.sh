#!/bin/sh
hps-mc-batch pool -p 4 -d $PWD/scratch -l $PWD/logs -c .hpsmc tritrig jobs.json
