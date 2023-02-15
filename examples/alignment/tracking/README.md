Tracking for Alignment {#trackingalign}
======================

1. Edit list of input files
2. Modify non-input-file variables
3. Generate JSON enumeration of all hps-mc jobs that need to be run
```
hps-mc-job-template -j 1 -a vars.json -i events events.txt 1 job.json.templ jobs.json
```
4. Submit generated jobs to batch computing
```
hps-mc-batch slurm track_align jobs.json
```
