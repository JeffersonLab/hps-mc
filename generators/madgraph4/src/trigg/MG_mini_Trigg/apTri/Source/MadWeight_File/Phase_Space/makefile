# ----------------------------------------------------------------------------
#
# Makefile for PHASE SPACE library
# Apr. 25 2003
#
# ----------------------------------------------------------------------------
#this is a partial combinaison

F77           = gfortran
FFLAGS        = -O -ffixed-line-length-132
LIBDIR        = ../../../lib/
OBJS= get_point.o initialize.o


.f.o: ; $(F77) $(FFLAGS) -c $*.f

all: $(LIBDIR)libMWPS.a

$(LIBDIR)libMWPS.a: $(OBJS)
	ar cru libMWPS.a $(OBJS)
	ranlib libMWPS.a
	mv libMWPS.a  $(LIBDIR)
