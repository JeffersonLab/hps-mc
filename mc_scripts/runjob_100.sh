#!/bin/tcsh
if ( $#argv != 4 ) then
	echo "$0 <filename> <ebeam> <firstnum> <lastnum>"
	echo "starts a list of 100 jobs per num, 1 input file per job"
	exit
endif
cp $1 temp2.xml
set apmass=`/u/group/hps/production/mc/run_params/apmass.csh $2`
sed -i '/List .*\"apmass\"/{s/>.*</>'"$apmass"'</}' temp2.xml
sed -i '/Variable.*\"ebeam\"/{s/value=\".*\"/value=\"'"$2"'\"/}' temp2.xml
foreach num100 ( `seq $3 $4` )
	@ numlast = $num100 * 100
	@ numfirst = $numlast - 99
#	@ numlast = $num100 * 10
#	@ numfirst = $numlast - 9    
	echo ${numfirst} ${numlast}
	set nums=`seq $numfirst $numlast`
	sed -i '/List .*\"num\"/{s/>.*</>'"$nums"'</}' temp2.xml
	sed -i '/Variable.*\"num100\"/{s/value=\".*\"/value=\"'"$num100"'\"/}' temp2.xml
	#cat temp.xml
	jsub -xml temp2.xml
	sed -i '/Email/{s/job=\".*\"/job=\"false\"/}' temp2.xml
end
