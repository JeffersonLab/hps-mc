Tracking for Alignment {#trackingalign}
======================

1. Edit list of input files
2. Modify non-input-file variables
  - The current job input variables assume that the input files are 2019 MC
    and the detector was generated using the \ref misalign example.
3. Generate JSON enumeration of all hps-mc jobs that need to be run
```
hps-mc-job-template -j 1 -a vars.json -i events events.txt 1 job.json.templ jobs.json
```
4. Submit generated jobs to batch computing
```
hps-mc-batch slurm track_align jobs.json
```

### Note on Steering File
The parameter `outputFile` is provided to the steering file as the basename
of the first output file in the `output_files` dictionary. This is important
to remember when writing the steering file.
