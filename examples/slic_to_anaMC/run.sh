#!/usr/bin/scl enable devtoolset-8 -- /bin/bash
#SBATCH --ntasks=1
#SBATCH --time=06:00:00
#SBATCH --mem=2000M
#SBATCH --partition=hps
#SBATCH --job-name=ttb
#SBATCH --output=/sdf/home/b/bravo/sdf/src/hps-mc/examples/slic_to_anaMC/logs/slurm.out

source /sdf/home/b/bravo/sdf/src/hps-mc/install/bin/hps-mc-env.sh
hps-mc-job run -d /sdf/home/b/bravo/sdf/src/hps-mc/examples/slic_to_anaMC/scratch -l /sdf/home/b/bravo/sdf/src/hps-mc/examples/slic_to_anaMC/logs/log.txt -c .hpsmc slic_to_anaMC /sdf/home/b/bravo/sdf/src/hps-mc/examples/slic_to_anaMC/job.json
