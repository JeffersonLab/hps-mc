#!/bin/bash

# Create 8 jobs starting from job ID 1 with padding of 4 chars, reading 1 event per job 
#hps-mc-job-template -r 8 -j 1 -p 4 -i events.txt 1 -a vars.json job.json.tmpl jobs.json
#hps-mc-job-template -r 10 -j 1 -d events /work/slac/data/stdhep .stdhep 1 -a vars.json job.json.tmpl jobs.json
#set -f && set -o noglob && set noglob && 
hps-mc-job-template -r 10 -j 1 -g events /work/slac/data/stdhep/tritrig\\*.stdhep 1 -a vars.json job.json.tmpl jobs.json
