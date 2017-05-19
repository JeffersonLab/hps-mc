# ----------------------------------------------------------------------------
#
# Makefile for PDF library
# Apr. 25 2003
#
# ----------------------------------------------------------------------------


F77           = gfortran
FFLAGS        = -O -ffixed-line-length-132
LIBRARY	      = ../libTF.a
LIBDIR        = ../../../lib/

OBJ           =  transfer_functions.o 

.f.o: ; $(F77) $(FFLAGS) -c $*.f

all: $(LIBDIR)libTF.a

$(LIBDIR)libTF.a: $(OBJ)
	ar cru libTF.a $(OBJ)
	ranlib libTF.a
	mv libTF.a  $(LIBDIR)
