# Creates a job store starting with job ID 100 from the input file list
hps-mc-job-template -j 1 -i events events.txt 1 -a vars.json job.json.tmpl jobs.json
