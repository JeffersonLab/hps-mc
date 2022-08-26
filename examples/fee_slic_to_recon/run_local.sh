#!/bin/sh
hps-mc-batch local -r 1:3 -d $PWD/scratch -l $PWD/log slic_to_recon jobs.json
