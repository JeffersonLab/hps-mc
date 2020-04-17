#!/bin/sh

# Get number of stdhep events in files by grepping information from stdhep lib print outs (ugly but works).

for f in "$@"
do
    n=$(stdhep_open $f | grep "[0-9]\+ events" | grep -v "expecting" | xargs echo -e | sed s/events//g)
    echo $f $n
done
