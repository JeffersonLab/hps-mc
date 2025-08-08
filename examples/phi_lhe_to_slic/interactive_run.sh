# Setup env
source ~/.bashrc
source cleanup.sh


# Execute your commands
hps-mc-job run -d $PWD/scratch -c .hpsmc phi_lhe_to_slic job.json

