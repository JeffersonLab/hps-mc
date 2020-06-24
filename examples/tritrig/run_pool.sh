#!/bin/sh
hps-mc-batch pool -p 5 -d $PWD/scratch -l $PWD/logs -c .hpsmc tritrig jobs.json
