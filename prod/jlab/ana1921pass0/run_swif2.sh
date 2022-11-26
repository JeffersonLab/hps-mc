#!/bin/sh
hps-mc-batch swif -r 1:4269 -w hps19 -m 5000 -E /work/hallb/hps/bravo/setup/swifEnv.sh -W 60 -c /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana1921pass0/.hpsmc -l $PWD/logs data_cnv /home/bravo/src/hps-mc/prod/jlab/ana1921pass0/jobs2019.json
mv temp.xml hps19.xml
hps-mc-batch swif -r 1:6013 -w hps21 -m 5000 -E /work/hallb/hps/bravo/setup/swifEnv.sh -W 60 -c /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana1921pass0/.hpsmc -l $PWD/logs data_cnv /home/bravo/src/hps-mc/prod/jlab/ana1921pass0/jobs2021.json
mv temp.xml hps21.xml
