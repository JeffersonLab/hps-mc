#!/bin/bash

dir='/sdf/group/hps/mc/beam/gen/3pt74/20pt0umW/*'
num=0
denom=0
constnum=15000000
re='^[0-9]+$'
for file in $dir
do
    # stdhep_count.sh $file
    declare numstring=($(stdhep_count.sh ${file}))
    # # ensure that numstring[1] is a number
    [[ ${numstring[1]} =~ $re ]] || continue
    num=$(($num+${numstring[1]}))
    denom=$(($denom+$constnum))
done

echo 'numerator:' 
echo $num
echo 'denominator:'
echo $denom
