#!/bin/sh
hps-mc-batch pool -d $PWD/scratch -c .hpsmc -r 1:10 aprime_gen jobs.json
