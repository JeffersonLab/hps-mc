#!/bin/sh
hps-mc-batch swif -r 1:4131 -w hpstr19 -m 2000 -E /work/hallb/hps/bravo/setup/swifEnv.sh -W 12 -c /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana1921pass0/hpstr/.hpsmc -l /farm_out/bravo/hps19/hpstr hpstr /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana1921pass0/hpstr/jobs2019.json
mv temp.xml hpstr19.xml
hps-mc-batch swif -r 1:6005 -w hpstr21 -m 2000 -E /work/hallb/hps/bravo/setup/swifEnv.sh -W 12 -c /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana1921pass0/hpstr/.hpsmc -l /farm_out/bravo/hps21/hpstr hpstr /w/hallb-scshelf2102/hps/bravo/sw/hps-mc/prod/jlab/ana1921pass0/hpstr/jobs2021.json
mv temp.xml hpstr21.xml
