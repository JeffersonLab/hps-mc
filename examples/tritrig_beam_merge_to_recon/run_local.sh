#!/bin/sh
hps-mc-batch local -d $PWD/scratch -l $PWD/scratch/log -c ../../config/jlab_tongtong.cfg -c .hpsmc signal_beam_merge_to_recon jobs.json 
