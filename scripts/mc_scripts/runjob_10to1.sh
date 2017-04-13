#!/bin/tcsh
if ( $#argv != 4 ) then
	echo "$0 <filename> <ebeam> <firstnum> <lastnum>"
	echo "starts 1 job per num, 10 input files per job"
	exit
endif
set apmass=`/u/group/hps/production/mc/run_params/apmass.csh $2`
cp $1 temp.xml
sed -i '/List .*\"apmass\"/{s/>.*</>'"$apmass"'</}' temp.xml
sed -i '/Variable.*\"ebeam\"/{s/value=\".*\"/value=\"'"$2"'\"/}' temp.xml
foreach num ( `seq $3 $4` )
	echo $num
	@ num10 = ( 9 + $num ) / 10
	sed -i '/List .*\"num\"/{s/>.*</>'"$num"'</}' temp.xml
	sed -i '/Variable.*\"num100\"/{s/value=\".*\"/value=\"'"$num10"'\"/}' temp.xml
	#cat temp.xml
	awk -v num=$num -v ebeam=$2 -f files_10.awk temp.xml >! temp2.xml
	jsub -xml temp2.xml
	sed -i '/Email/{s/job=\".*\"/job=\"false\"/}' temp.xml
end
