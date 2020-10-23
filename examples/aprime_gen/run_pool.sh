#!/bin/sh
hps-mc-batch pool -p 4 -d $PWD/scratch -l $PWD/logs -r 1:10 aprime_gen jobs.json
