#!/bin/sh
hps-mc-batch auger -D -c ../../config/jlab_tongtong.cfg -c .hpsmc -W 4 -l $PWD/logs signal_beam_merge_to_recon jobs.json
