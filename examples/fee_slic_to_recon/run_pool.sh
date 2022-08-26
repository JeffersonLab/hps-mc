#!/bin/sh
hps-mc-batch pool -o -p 8 -r 1:50 -d $PWD/scratch -l $PWD/logs slic_to_recon jobs.json
