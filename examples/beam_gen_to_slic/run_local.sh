#!/bin/sh
hps-mc-batch local -d $PWD/scratch -l $PWD/scratch/log -c .hpsmc beam_gen_to_slic jobs.json 
