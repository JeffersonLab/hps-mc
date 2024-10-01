#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --time=6:00:00
#SBATCH --job-name=examples

hps-mc-job run -d $PWD/scratch $PWD/wab_gen_sample_job.py job.json
