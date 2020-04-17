#!/bin/sh
hps-mc-local -d $PWD/scratch -c .hpsmc ${HPSMC_DIR}/lib/python/jobs/slic_job.py jobs.json
