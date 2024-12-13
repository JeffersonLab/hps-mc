#/bin/bash
#SBATCH --ntasks=1
#SBATCH --time=6:00:00
#SBATCH --job-name=examples

hps-mc-job run -d $PWD/scratch wab_gen_mg5 job.json
