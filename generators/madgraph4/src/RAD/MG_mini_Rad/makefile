all: mg2 helas decay py-pgs ExRootAnalysis MadAnalysis
	@echo
	@echo "Please read the README file for instructions on"
	@echo "  how to generate and run processes"

helas: FORCE
	@echo "Running make for HELAS";cd HELAS;make;ranlib lib/libdhelas3.a;make clean

decay: FORCE
	@echo "Running make for DECAY";cd DECAY;make 

mg2: FORCE
	@echo "Running make for MadGraphII";cd MadGraphII;make

py-pgs: FORCE
	@if( test -d pythia-pgs ) then \
	echo "Running make for pythia-pgs";cd pythia-pgs;make -k; \
	fi

ExRootAnalysis: FORCE
	@if( test -d ExRootAnalysis ) then \
	echo "Running make for ExRootAnalysis";cd ExRootAnalysis;make; \
	fi

MadAnalysis: FORCE 
	@if( test -d MadAnalysis ) then \
	echo "Running make for MadAnalysis";cd MadAnalysis;make; \
	fi

FORCE:
