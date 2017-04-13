#!/bin/csh -f
switch ($1)
case "1pt05":
    echo "15 20 30 40 50 60 70 80 90"
    #echo "15 16 17 18 19 20 22 24 26 28 30 35 40 50 60 70 80 90"
	breaksw
case "1pt1":
	echo "15 20 30 40 50 60 70 80 90 100"
	breaksw
case "2pt2":
case "2pt3":
	echo "15 25 50 75 100 150 200 250"
        #echo 50 100 150 200 250 300
	breaksw
case "4pt4":
    #echo "50 75 100 150 200 250 300 350 400 450 500"
    echo "15 25 50 75 100 150 200 250 300 350 400 450 500"
    breaksw
case "6pt6":
	echo "50 100 200 300 400 500 600"
	breaksw
case "1pt92":
	echo "50 75 100"
	breaksw
endsw
