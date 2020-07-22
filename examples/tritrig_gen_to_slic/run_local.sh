#!/bin/sh
hps-mc-batch local -d $PWD/scratch -l $PWD/scratch/log -c ../../config/jeremym_local.cfg -c .hpsmc tritrig_gen_to_slic jobs.json 
