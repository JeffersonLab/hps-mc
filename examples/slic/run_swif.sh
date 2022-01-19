#!/bin/sh

# Replace with path to your log dir
_logdir=/volatile/hallb/hps/jeremym/hps-mc-test/output

# Use a custom workflow name
#hps-mc-batch swif -w jeremym_test -s 1 -l ${_logdir} slic jobs.json

# By default the name of the workflow will be the name of the job script
hps-mc-batch swif -s 1 -l ${_logdir} slic jobs.json
