#!/bin/sh
hps-mc-batch swif -o -r 1:4154 -w hps21fee -m 4000 -W 72 -c /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana21pass1b/fee/.hpsmc -l /farm_out/bravo/hps21v4fee data_cnv /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana21pass1b/fee/jobs2021_v4.json
#mv temp.xml hps21pass1b_v4.xml

hps-mc-batch swif -o -r 1:4154 -w hps21fee -m 4000 -W 72 -c /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana21pass1b/fee/.hpsmc -l /farm_out/bravo/hps21v5fee data_cnv /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana21pass1b/fee/jobs2021_v5.json
mv temp.xml hps21pass1b_v5.xml
#hps-mc-batch swif -o -r 4100:4110 -w hpsTest -m 2500 -W 72 -c /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana21pass1b/fee/.hpsmc -l /farm_out/bravo/hps21v5fee data_cnv /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana21pass1b/fee/jobs2021_v5.json
#mv temp.xml hps21pass1b_feeTest.xml
