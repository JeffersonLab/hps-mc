#!/bin/sh
hps-mc-batch pool -d $PWD/scratch -l $PWD/logs -c .hpsmc -r 1:10 aprime_gen jobs.json
