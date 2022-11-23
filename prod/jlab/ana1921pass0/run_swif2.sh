#!/bin/sh
hps-mc-batch swif -r 41:50 -w hps19ll -m 3000 -E /work/hallb/hps/bravo/setup/swifEnv.sh -W 48 -c /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana1921pass0/.hpsmc -l /farm_out/bravo/hps19 data_cnv /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana1921pass0/jobs2019.json
mv temp.xml hps19ll.xml
hps-mc-batch swif -r 3731:3740 -w hps19hl -m 5000 -E /work/hallb/hps/bravo/setup/swifEnv.sh -W 60 -c /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana1921pass0/.hpsmc -l /farm_out/bravo/hps19 data_cnv /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana1921pass0/jobs2019.json
mv temp.xml hps19hl.xml
hps-mc-batch swif -r 3297:3306 -w hps21 -m 5000 -E /work/hallb/hps/bravo/setup/swifEnv.sh -W 60 -c /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana1921pass0/.hpsmc -l /farm_out/bravo/hps21 data_cnv /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana1921pass0/jobs2021.json
mv temp.xml hps21hl.xml
