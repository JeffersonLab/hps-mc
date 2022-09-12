Job Template {#jobtemplate}
============

This example illustrates how to use a job template `job.json.tmpl` and the `mkjobs.sh` script to generate configurations for multiple jobs. The template includes variables in place of the actual job parameters. These variables are defined in `vars.json`. In this example, `events.txt` is used to set the input files.
Running `mkjobs.sh` for the first time creates a new file `jobs.json` which will be overwritten after its initial creation every time `mkjobs.sh` is run. 10 jobs are written to `jobs.json`, using two input files from `events.txt` per job. As two run numbers are set in `vars.json`, five jobs are created for each run number.