#!/bin/sh
hps-mc-batch auger -D -c ../../config/jlab_tongtong.cfg -c .hpsmc -W 4 -l $PWD/logs rad_gen_to_slic jobs.json
