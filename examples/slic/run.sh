# Print all output to the console
hps-mc-job run -d $PWD/scratch slic job.json

# Save output to three separate files (stdout and stderr of components and logging output)
#hps-mc-job run -d $PWD/scratch -c .hpsmc -l job.log -o job.out -e job.err slic job.json
