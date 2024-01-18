#hps-mc-batch swif -r 1:10000 -w rereco16 -m 1900 -o -W 18 -c /work/hallb/hps/bravo/sw/hps-mc/prod/jlab/ana16pass4kf/.hpsmc -l /farm_out/bravo/hps16/rereco rereco /work/hallb/hps/bravo/sw/hps-mc/prod/jlab/ana16pass4kf/jobs.json
#mv temp.xml rereco16pass4kf0.xml
hps-mc-batch swif -r 10001:16251 -w rereco16 -m 1900 -o -W 18 -c /work/hallb/hps/bravo/sw/hps-mc/prod/jlab/ana16pass4kf/.hpsmc -l /farm_out/bravo/hps16/rereco rereco /work/hallb/hps/bravo/sw/hps-mc/prod/jlab/ana16pass4kf/jobs.json
mv temp.xml rereco16pass4kf1.xml
