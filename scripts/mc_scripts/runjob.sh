#!/bin/tcsh
if ( $#argv != 4 ) then
	echo "$0 <filename> <ebeam> <firstnum> <lastnum>"
	echo "starts 1 job per num, 1 input file per job"
	exit
endif
set nums=`seq $3 $4`
cp $1 temp.xml
set apmass=`/u/group/hps/production/mc/run_params/apmass.csh $2`
sed -i '/List .*\"num\"/{s/>.*</>'"$nums"'</}' temp.xml
sed -i '/List .*\"apmass\"/{s/>.*</>'"$apmass"'</}' temp.xml
sed -i '/Variable.*\"ebeam\"/{s/value=\".*\"/value=\"'"$2"'\"/}' temp.xml
#cat temp.xml
jsub -xml temp.xml
