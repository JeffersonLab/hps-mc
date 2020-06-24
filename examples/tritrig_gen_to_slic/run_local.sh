#!/bin/sh
hps-mc-batch local -d $PWD/scratch -l $PWD/scratch/log -c .hpsmc tritrig_gen_to_slic jobs.json 
