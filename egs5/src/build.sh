#!/bin/bash
function build {
rm -f egs5job.*
cp src/$1.f egs5job.f
rm -f pgs5job.*
ln -s src/esa.inp pgs5job.pegs5inp    #Use this to build tungsten target
#ln -s src/esa-GS.inp pgs5job.pegs5inp    #Use this to build tungsten target using GS distribution
#ln -s src/esa-ch2.inp pgs5job.pegs5inp # Use this to build CH2 target
#ln -s src/esa-carbon.inp pgs5job.pegs5inp # Use this to build carbon target
./src/egs5run comp
mv egs5job.exe $1.exe
}

#build beam_v1
#build lhe_v1
#build lhe_v1_LOWCUT
#build beam_v2
#build moller_v1
build moller_v3
#build beam_v3_CH2
#build beam_v4_carbon
#build beam_v3-GS
#build beam_v5
