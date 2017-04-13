#!/bin/csh -f
switch ($1)
case "1pt05":
case "1pt1":
	echo 625
	breaksw
case "1pt92":
case "2pt2":
case "2pt3":
	echo 2500
        breaksw
case "4pt4":
        echo 5000
	breaksw
case "6pt6":
	echo 5625
	breaksw
endsw
