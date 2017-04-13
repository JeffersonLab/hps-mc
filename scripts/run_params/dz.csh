#!/bin/csh -f
switch ($1)
case "1pt1":
case "1pt92":
case "1pt05":
case "2pt2":
case "2pt3":
case "4pt4":
    #	echo 0.0004375
    echo 0.0004062 # based on 0.00782 g/cm2 (Source: https://misportal.jlab.org/mis/physics/hps_notes/viewFile.cfm/2015-008.pdf?documentId=11)
	breaksw
case "6pt6":
	echo 0.000875
	breaksw
endsw
