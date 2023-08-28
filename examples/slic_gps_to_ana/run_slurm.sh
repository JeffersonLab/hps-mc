#!/bin/sh

_basedir=$PWD/slurm_scratch

# Run a slurm job using mostly default arguments.
hps-mc-batch slurm -l ${_basedir}/logs -d ${_basedir}/jobs -S ${_basedir}/sh -o -q shared -W 2 -m 2000 -c .hpsmc slic_gps_to_ana jobs.json


