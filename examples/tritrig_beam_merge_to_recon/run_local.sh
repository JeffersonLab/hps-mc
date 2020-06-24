#!/bin/sh
hps-mc-batch local -d $PWD/scratch -l $PWD/scratch/log -c .hpsmc tritrig_beam_merge_to_recon jobs.json 
