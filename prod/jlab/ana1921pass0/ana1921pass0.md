Data conversion/reco -- evio to lcio  {#ana1921pass0}
===============================

This is the job configuration used for pass0 of the 2019 and 2021 data.
hps2021goldRuns.csv and hps2019goldRuns.csv are from the [run spreadsheets](https://wiki.jlab.org/hps-run/index.php/The_HPS_Run_Wiki).
build2021filenames.py and build2019filenames.py read these csv files and produce a txt file with full path to each file selected via glob.

The configuration for all the jobs to be submitted is generated via mkjobs.sh, which
utilizes the .tmpl files as a template for all the jobs.

The configuration for the jobs to be run are in jobs2019.json and jobs2021.json, where for
instance you can find the output location for the files, the hps-java steering file, etc.
These files are in a human readable format.

The jobs are submitted to the swif2 system at jlab via run_swif2.sh
The specfic location of the installations used are configured in .hpsmc
hpstr release that will be installed is v1.0.0
