# Create 8 jobs starting from job ID 1 with padding of 4 chars, reading 1 event per job 
hps-mc-job-template -j 1 -i events events.txt 1 -a vars.json job.json.tmpl jobs.json
