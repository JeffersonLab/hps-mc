#!/bin/sh
hps-mc-batch local -d $PWD/scratch -l $PWD/scratch/log -c ~/.hpsmc -c .hpsmc beam_gen jobs.json 
