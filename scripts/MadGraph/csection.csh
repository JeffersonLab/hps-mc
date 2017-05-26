#!/bin/csh -f
if  ($#argv != 1) then
	echo "Usage: $0 filename"
	exit 0
endif

zcat $1 | grep "Integrated weight" -m1|cut -d: -f2
