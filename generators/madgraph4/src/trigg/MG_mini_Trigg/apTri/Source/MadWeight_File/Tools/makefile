# ----------------------------------------------------------------------------
#
# Makefile for TOOLS library
# Apr. 25 2003
#
# ----------------------------------------------------------------------------


F77           = gfortran
G77	      = gfortran
FFLAGS        = -O -ffixed-line-length-132
LIBDIR        = ../../../lib/
PLOT= dbook.o topgraph.o
OBJS= nexper_2.o next_perm.o eq_solver.o vegas.o stat.o htuple.o $(PLOT)


.f.o: ; $(G77) $(FFLAGS) -c $*.f

all: $(LIBDIR)libtools.a

vegas.o : vegas.f
	$(G77) $(FFLAGS) -c vegas.f


$(LIBDIR)libtools.a: $(OBJS) 
	ar cru libtools.a $(OBJS)
	ranlib libtools.a
	mv libtools.a  $(LIBDIR)

next_perm.f: ../../nexternal.inc ../../genps.inc
