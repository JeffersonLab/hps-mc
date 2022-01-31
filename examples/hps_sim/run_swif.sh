#!/bin/sh

# Replace with the path to your log directory.
_logdir=/volatile/hallb/hps/jeremym/hps-mc-test/output

# By default the name of the workflow will be the name of the job script.
# Use the -w arg to change this.
hps-mc-batch swif -s 1 -l ${_logdir} sim jobs.json
