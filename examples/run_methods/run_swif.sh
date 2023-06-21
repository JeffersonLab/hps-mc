#!/bin/sh

# Workflow name
_wf=hps_swif_test

# Delete old workflow
swif2 cancel ${_wf} -delete &> /dev/null

# Use this ifarm partition (or other valid partition) for JLab
hps-mc-batch swif -w ${_wf} -r 1:1 -c $PWD/.hpsmc slic $PWD/job_swif.json

# Watch status of workflow
watch -n 10 "swif2 status ${_wf}"
