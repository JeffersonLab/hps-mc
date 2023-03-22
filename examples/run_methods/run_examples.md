Run examples  {#runexamples}
============

This directory contains a selection of different methods of running a job. 
- `run.sh`: `hps-mc-job run -d $PWD/scratch slic job.json`
  - standard option for running a single job; class the jobs `run()` function
  - `-d $PWD/scratch`: run directory
  - `slic`: name of job script in python/jobs that is called
  - `job.json`: job params
- `run_local.sh`: `hps-mc-batch local -d $PWD/scratch slic job_pool.json`
  - `-d $PWD/scratch`: run directory
  - `slic`: name of job script in python/jobs that is called
  - `job_pool.json`: job params
- `run_pool.sh`: `hps-mc-batch pool -p 2 -d $PWD/scratch slic job_pool.json`
  - `-p 2`: job pool size
  - `-d $PWD/scratch`: run directory
  - `slic`: name of job script in python/jobs that is called
  - `job_pool.json`: job params
- `run_slurm.sh`: `hps-mc-batch slurm -o -q hps -r 1:1 -E $HPSMC_DIR/bin/hps-mc-env.sh -W 5 -d $HPSMC/examples/run_methods/scratch -c $HPSMC/examples/run_methods/.hpsmc -l $HPSMC/examples/run_methods/scratch/logs -S $HPSMC/examples/run_methods/scratch slic $HPSMC/examples/run_methods/job_slurm.json`
  - `-o`: check output
  - `-q hps`: job queue
  - `-r 1:1`: job range, submit jobs with id within range
  - `-E $HPSMC_DIR/bin/hps-mc-env.sh`: full path to env setup script
  - `-W 5`: max job length in hours
  - `-d $HPSMC/examples/run_methods/scratch`: run directory
  - `-c $HPSMC/examples/run_methods/.hpsmc`: config file
  - `-l $HPSMC/examples/run_methods/scratch/logs`: log file ouput dir
  - `-S $HPSMC/examples/run_methods/scratch`: dir to hold sh scripts to submit jobs via Slurm
  - `slic`: name of job script in python/jobs that is called
  - `job_slurm.json`: job params
