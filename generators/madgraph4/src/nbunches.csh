#!/bin/csh -f
if  ($#argv != 3) then
	echo "Usage: $0 thickness bunchsize filename"
	exit 0
endif

set dirname=`dirname $0`

set n=`$dirname/nevents.csh $3`
set mu=`$dirname/mu.csh $argv`
echo "int($n/$mu)"|perl -nle 'print eval $_'
