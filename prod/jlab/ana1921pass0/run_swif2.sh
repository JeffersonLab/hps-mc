#!/bin/sh
hps-mc-batch swif -r 1:4269 -w hps19 -m 1200 -E /work/hallb/hps/bravo/setup/swifEnv.sh -W 96 -c /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana1921pass0/.hpsmc -l /farm_out/bravo/hps19 data_cnv /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana1921pass0/jobs2019.json
mv temp.xml hps19.xml
hps-mc-batch swif -r 1:6013 -w hps21 -m 1200 -E /work/hallb/hps/bravo/setup/swifEnv.sh -W 96 -c /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana1921pass0/.hpsmc -l /farm_out/bravo/hps21 data_cnv /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana1921pass0/jobs2021.json
mv temp.xml hps21.xml
