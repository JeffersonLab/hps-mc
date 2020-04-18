# Creates a job store starting with job ID 100 from the input file list
hps-mc-jobstore -j 100 -i filelist.txt -a vars.json -o events.slcio job.json.tmpl jobs.json
#hps-mc-jobstore -j 100 -i filelist.txt -o events.slcio job.json.tmpl jobs.json
