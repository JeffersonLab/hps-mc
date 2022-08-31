hps-mc-job-template \
    -i beam beam_files.txt 1 \
    -i tritrig tritrig_files.txt 1 \
    -j 1 -a vars.json job.json.tmpl jobs.json
