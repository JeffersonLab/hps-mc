ifdef ROOTSYS
ROOTLIBS = `$(ROOTSYS)/bin/root-config --glibs`
ROOTCFLAGS = `$(ROOTSYS)/bin/root-config --cflags`
endif

BIN := ../bin
SRC_DIR := ../src
STDHEP_DIR := $(SRC_DIR)/stdhep-5-06-01
STDHEP_C := $(STDHEP_DIR)/lib/libstdhepC.a
MCFIO := $(STDHEP_DIR)/lib/libFmcfio.a
LOADLIBS=-L$(STDHEP_DIR)/lib
#LOADLIBS=-L/usr/local/lib/stdhep -L/afs/slac.stanford.edu/package/cernlib/@sys/cern/2002/lib
LDLIBS=-lstdhepC -lFmcfio $(ROOTLIBS) -lgsl -lgslcblas
INCFILES=-I$(STDHEP_DIR)/src/inc -I$(SRC_DIR)
CPP=g++
CPPFLAGS=$(LOADLIBS) $(LDLIBS) $(ROOTCFLAGS) -g
TARGETS := $(patsubst $(SRC_DIR)/%.cc,$(BIN)/%,$(wildcard $(SRC_DIR)/*.cc))
UTIL_SRC := $(wildcard $(SRC_DIR)/*.cpp)
UTIL_OBJ := $(UTIL_SRC:.cpp=.o)

all: $(TARGETS) $(UTIL_OBJ) $(STDHEP_C) $(MCFIO)
$(BIN)/%: $(SRC_DIR)/%.cc $(UTIL_OBJ) $(STDHEP_C) $(MCFIO)
#all: $(TARGETS) $(UTIL_OBJ)
#$(BIN)/%: $(SRC_DIR)/%.cc $(UTIL_OBJ)
	$(CPP) $< $(UTIL_OBJ) $(CPPFLAGS) $(INCFILES) -o $@
$(SRC_DIR)/%.o: $(SRC_DIR)/%.cpp $(SRC_DIR)/%.hh
	$(CPP) $< -c $(INCFILES) -g -o $@
$(STDHEP_C):
	cd $(STDHEP_DIR)/src/stdhep; make
$(MCFIO):
	cd $(STDHEP_DIR)/mcfio/src; make
clean:
	#@rm -f $(TARGETS) $(UTIL_OBJ)
	@rm -f $(TARGETS) $(UTIL_OBJ) $(STDHEP_DIR)/lib/*.a
	cd $(STDHEP_DIR)/src/stdhep; make clean
	cd $(STDHEP_DIR)/mcfio/src; make clean

