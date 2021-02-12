#!/bin/sh
hps-mc-batch pool -p 5 -r 1:3 -d $PWD/scratchPool -l $PWD/logPool tritrig_beam_slic_to_recon jobs.json
