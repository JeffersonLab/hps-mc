#!/bin/sh
hps-mc-pool -d $PWD/scratch -s 2 ${HPSMC_DIR}/lib/python/jobs/tritrig_job.py jobs.json
