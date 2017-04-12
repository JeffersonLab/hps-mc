#!/bin/bash

paramdir="/u/group/hps/production/mc/run_params"
ebeam="1pt1"
file="/work/hallb/hps/mc_production/lhe/tri/${ebeam}/triv2_228.lhe.gz"
dz=`${paramdir}/dz.csh ${ebeam}`
ne=`${paramdir}/ne.csh ${ebeam}`
ebeam=`${paramdir}/ebeam.csh ${ebeam}`
mu=`/u/group/hps/production/mc/MadGraph/mu.csh $dz $ne $file`
echo "mu=$mu  dz=$dz ne=$ne ebeam=$ebeam" 
