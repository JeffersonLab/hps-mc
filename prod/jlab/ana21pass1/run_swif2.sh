#!/bin/sh
hps-mc-batch swif -o -r 1:6013 -w hps21 -m 1400 -W 72 -c /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana21pass1/.hpsmc -l /farm_out/bravo/hps21 data_cnv /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana21pass1/jobs2021.json
mv temp.xml hps21.xml
