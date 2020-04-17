#!/bin/sh
hps-mc-pool -d $PWD/scratch -c .hpsmc ${HPSMC_DIR}/lib/python/jobs/slic_job.py jobs.json
