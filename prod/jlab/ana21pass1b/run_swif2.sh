#!/bin/sh
hps-mc-batch swif -o -r 1:6013 -w hps21 -m 1500 -W 72 -c /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana21pass1b/.hpsmc -l /farm_out/bravo/hps21v4 data_cnv /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana21pass1b/jobs2021_v4.json
mv temp.xml hps21pass1b_v4.xml

hps-mc-batch swif -o -r 1:6013 -w hps21 -m 1500 -W 72 -c /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana21pass1b/.hpsmc -l /farm_out/bravo/hps21v5 data_cnv /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana21pass1b/jobs2021_v5.json
mv temp.xml hps21pass1b_v5.xml
