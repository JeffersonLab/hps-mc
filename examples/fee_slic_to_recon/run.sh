#!/usr/bin/scl enable devtoolset-8 -- /bin/bash
#SBATCH --ntasks=1
#SBATCH --time=24:00:00
#SBATCH --mem=2000M
#SBATCH --partition=hps
#SBATCH --job-name=fee
#SBATCH --output=/sdf/home/b/bravo/sdf/src/hps-mc/examples/fee_slic_to_recon/logs/slurm.out

source /sdf/home/b/bravo/sdf/src/hps-mc/install/bin/hps-mc-env.sh
hps-mc-job run -d /sdf/home/b/bravo/sdf/src/hps-mc/examples/fee_slic_to_recon/scratch -l /sdf/home/b/bravo/sdf/src/hps-mc/examples/fee_slic_to_recon/logs/log.txt -c .hpsmc fee_slic_to_recon /sdf/home/b/bravo/sdf/src/hps-mc/examples/fee_slic_to_recon/job.json
