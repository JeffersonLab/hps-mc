#!/bin/bash
#SBATCH --job-name=simp_sim  # Job name
#SBATCH --output=simp_sim_job.%j.out  # Output file name (includes job ID)
#SBATCH --error=simp_sim_job.%j.err   # Error file name (includes job ID)
#SBATCH --time=48:00:00             # Time limit (HH:MM:SS)
#SBATCH --partition=normal          # Partition to use (e.g., normal, gpu)
#SBATCH --ntasks=1                  # Number of tasks
#SBATCH --cpus-per-task=1           # Number of CPUs per task
#SBATCH --mem=4G                    # Memory per node
#SBATCH --mail-type=ALL             # Send email for all states
#SBATCH --mail-user=mghrear@stanford.edu # Replace with your email

# Setup env
source ~/.bashrc
source cleanup.sh


# Execute your commands
echo "Starting job with ID: $SLURM_JOB_ID"
hps-mc-job run -d $PWD/scratch -c .hpsmc phi_lhe_to_slic job.json
echo "Job finished"
