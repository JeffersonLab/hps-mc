#F77   = f77
F77   = gfortran
FFLAGS= -O -ffixed-line-length-132 -std=legacy 
#FFLAGS = -g -ffixed-line-length-132
LIBDIR = ../../lib/
BINDIR = ../../bin/
# $B$ obj_for_MW $B$ !this is a tag for MadWeight. Don't edit this line
PROG   = madevent
# $E$ obj_for_MW $E$ !this is a tag for MadWeight. Don't edit this line
LINKLIBS =  -L../../lib/ -ldhelas3 -ldsample -lmodel -lgeneric -lpdf
LIBS = $(LIBDIR)libdhelas3.a $(LIBDIR)libdsample.a $(LIBDIR)libgeneric.a $(LIBDIR)libpdf.a $(LIBDIR)libmodel.a


PROCESS= driver.o myamp.o auto_dsig.o genps.o matrix.o unwgt.o setcuts.o cuts.o cluster.o reweight.o initcluster.o addmothers.o setscales.o
SYMMETRY = myamp.o genps.o matrix.o  initcluster.o setscales.o reweight.o
# $B$ obj_for_MW $E$ !this is a tag for MadWeight. Don't edit this line


%.o : %.f
	$(F77) $(FFLAGS) -c $<

# $B$ main_make $B$ !this is a tag for MadWeight. Don't edit this line
$(PROG): $(PROCESS)  makefile $(LIBS)
	$(F77) $(FFLAGS) -o $(PROG) $(PROCESS) $(LINKLIBS)
# $E$ main_make $E$ !this is a tag for MadWeight. Don't edit this line

symmetry.dat: mg.sym gensym
	./gensym

gensym: symmetry.o genps.inc setcuts.o cuts.o cluster.o $(SYMMETRY) configs.inc  
	$(F77) $(FFLAGS) -o gensym symmetry.o setcuts.o cuts.o cluster.o $(SYMMETRY) $(LINKLIBS)

driver.f: genps.inc qmass.inc 
setcuts.f: qmass.inc

genps.inc: nexternal.inc
	touch genps.inc

qmass.inc: nexternal.inc
	touch qmass.inc

symmetry.o: genps.inc configs.inc
genps.o: genps.inc configs.inc 
cuts.o: genps.inc pmass.inc
invarients.o: genps.inc 
myamp.o: props.inc genps.inc
reweight.o: sudakov.inc cluster.inc sudakov.inc run.inc message.inc
cluster.o: cluster.inc genps.inc message.inc
addmothers.o: genps.inc symswap.inc message.inc
unwgt.o: genps.inc symswap.inc cluster.inc run.inc message.inc
initcluster.o: message.inc
