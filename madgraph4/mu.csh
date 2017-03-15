#!/bin/csh -f
if  ($#argv != 3) then
	echo "Usage: $0 thickness bunchsize filename"
	exit 0
endif

set dirname=`dirname $0`
set w=$1
set bunchsize=$2
set density="6.306e-2" # atoms per cm-barn
set lumi="$density*$w*$bunchsize" # integrated luminosity per bunch

set csection=`$dirname/csection.csh $3`
echo "$lumi*1e-12*$csection"|perl -nle 'print eval $_'
