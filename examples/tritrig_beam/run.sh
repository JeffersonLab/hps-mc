#hps-mc-job tritrig -s 5 -d $PWD/scratch -c .hpsmc job.json
#hps-mc-job tritrig_beam -d $PWD/scratch -c .hpsmc -o job.out -e job.err job.json
hps-mc-job tritrig_beam -d $PWD/scratch -c .hpsmc job.json
