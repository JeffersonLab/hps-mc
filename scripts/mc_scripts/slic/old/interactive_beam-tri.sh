#!/bin/tcsh


#`rm egs5job.inp data pgs5job.* egs5job.*  seed.dat iff.dat ics.dat brems.stdhep rot_tridents.stdhep sampled_tridents_1.stdhep rot_beam.stdhep`

set ebeam="1pt92"
set hpsjava="/u/group/hps/hps_soft/hps-java/hps-distribution-3.2-SNAPSHOT-20150306-bin.jar"
set egs5_dir="/u/group/hps/hps_soft/egs5"
set stdhep_dir="/u/group/hps/hps_soft/stdhep/bin"
set slic_dir="/u/group/hps/hps_soft/slic/v00-02"
set exe_dir="/u/group/hps/production/mc/egs5"
set det_dir="/u/group/hps/hps_soft/hps/detector-data/detectors"
set detector="HPS-ECalCommissioning-v3-fieldmap"
set param_dir="/u/group/hps/production/mc/run_params"
#set log_dir="/work/hallb/hps/mc_production/logs/slic/beam-tri"
set log_dir="."
#set dq_dir="/work/hallb/hps/mc_production/data_quality/slic/beam-tri"
set dq_dir="."
set out_dir="."
set out_file="egsv3-triv2-g4v1_s2d6"

set num=100
set beam="/cache/mss/hallb/hps/production/stdhep/beam/1pt92/egsv3_1.stdhep" 
set tri="/work/hallb/hps/mc_production/lhe/tri/1pt92/triv2_1.lhe.gz" 
#set had="mss:/mss/hallb/hps/production/stdhep/hadrons/1pt92/g4v1_${num100}.stdhep" 


set dz = `${param_dir}/dz.csh ${ebeam}`
set ne = `${param_dir}/ne.csh ${ebeam}`
set ebeam = `${param_dir}/ebeam.csh ${ebeam}`

echo "Getting brems from tridents"     
set mu=`/u/group/hps/production/mc/MadGraph/mu.csh $dz $ne $tri`
zcat $tri > egs5job.inp
ln -s ${egs5_dir}/data
ln -s ${exe_dir}/src/esa.inp pgs5job.pegs5inp
echo "11 $dz $ebeam 5000" > seed.dat 
${exe_dir}/lhe_v1.exe
cat egs5job.out

echo "Rotating  tridents"
echo "mu=$mu  dz=$dz ne=$ne ebeam=$ebeam" 
${stdhep_dir}/beam_coords brems.stdhep rot_tridents.stdhep -s 12
echo "${stdhep_dir}/merge_poisson -m"$mu" -N1 -n5000 rot_tridents.stdhep sampled_tridents -s 13${num}"
${stdhep_dir}/merge_poisson -m"$mu" -N1 -n5000 rot_tridents.stdhep sampled_tridents -s 13${num}

echo "Rotating  beam"
${stdhep_dir}/beam_coords $beam rot_beam.stdhep -s 14
${stdhep_dir}/random_sample rot_beam.stdhep sampled_beam -s 15

#                               @ file_num = ( ${num} - 1 ) % 100 + 1
#                               ${stdhep_dir}/beam_coords hadrons.stdhep rot_hadrons.stdhep -s 16${num}
#                               ${stdhep_dir}/merge_poisson -m"-1.1" -N100 -O$file_num -n500000 rot_hadrons.stdhep sampled_hadrons -s 17${num}

#                               ${stdhep_dir}/merge_files sampled_beam_1.stdhep sampled_tridents_1.stdhep sampled_hadrons*.stdhep beam-tri.stdhep
echo "Merging files"
${stdhep_dir}/merge_files sampled_beam_1.stdhep sampled_tridents_1.stdhep  beam-tri.stdhep
source ${slic_dir}/init_ilcsoft.csh
source ${slic_dir}/geant4/build-9.6.p01/geant4make.csh ${slic_dir}/geant4/build-9.6.p01

set seed = 18
set nevents = 0
rm out.slcio
#                                       echo "slic -i beam-tri.stdhep -g ${det_dir}/${detector}-${ebeam}/${detector}-${ebeam}.lcdd -o out.slcio -d$seed{${num}} -r5000000"
#                                       slic -i beam-tri.stdhep -g ${det_dir}/${detector}-${ebeam}/${detector}-${ebeam}.lcdd -o out.slcio -d$seed{${num}} -r5000000|grep -vE '^$|^>>>> .+Event <[0-9]+>$| has [0-9]+ hits$'

#                                       echo "slic -i beam-tri.stdhep -g ${det_dir}/${detector}/${detector}.lcdd -o out.slcio -d$seed{1} -r5000000"
#                                       slic -i beam-tri.stdhep -g ${det_dir}/${detector}/${detector}.lcdd -o out.slcio -d$seed{1} -r5000000|grep -vE '^$|^>>>> .+Event <[0-9]+>$| has [0-9]+ hits$'
#ls -l
#/apps/scicomp/java/jdk1.7/bin/java -Xmx100m -jar ${hps-java} -r /org/lcsim/hps/steering/DataQuality.lcsim -i out.slcio >! data_quality.txt
#set nevents=`grep '^Read.*events$' data_quality.txt | cut -d' ' -f2`
#echo "$nevents events in output"
#@ seed = $seed + 1




