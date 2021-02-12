#!/usr/bin/scl enable devtoolset-8 -- /bin/bash
#SBATCH --ntasks=1
#SBATCH --time=06:00:00
#SBATCH --mem=2000M
#SBATCH --partition=hps
#SBATCH --job-name=ttb
#SBATCH --output=/sdf/home/b/bravo/sdf/src/hps-mc/examples/tritrig_beam_slic_to_recon/logs/slurm.out

source /sdf/home/b/bravo/sdf/src/hps-mc/install/bin/hps-mc-env.sh
hps-mc-job run -d /sdf/home/b/bravo/sdf/src/hps-mc/examples/tritrig_beam_slic_to_recon/scratch -l /sdf/home/b/bravo/sdf/src/hps-mc/examples/tritrig_beam_slic_to_recon/logs/log.txt -c .hpsmc tritrig_beam_slic_to_recon /sdf/home/b/bravo/sdf/src/hps-mc/examples/tritrig_beam_slic_to_recon/job.json
