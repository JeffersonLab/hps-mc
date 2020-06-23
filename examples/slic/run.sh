# Save output to three separate files (stdout and stderr of components and logging output)
hps-mc-job run -d $PWD/scratch -c .hpsmc -l job.log -o job.out -e job.err slic job.json

# Print all output to the console
#hps-mc-job run -n -d $PWD/scratch -c .hpsmc slic job.json

# Don't do this! Same file name should NOT be used for stdout/stderr/logging.
#hps-mc-job run -n -d $PWD/scratch -c .hpsmc -l job.log -o job.log -e job.log slic job.json
