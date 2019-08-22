SYSTEMC		= /usr/local/systemc-2.3.0
TARGET_ARCH	= macosx64
CXX         = g++
LD	    = g++
AR = ar
CXXFLAGS    = -Wall  -I. -I$(SYSTEMC)/include -Wno-deprecated
LDFLAGS	    =  -L$(SYSTEMC)/lib-$(TARGET_ARCH) 

all: lib 

lib: rfsm.o

.cpp.o:
	$(CXX) $(CXXFLAGS) -c -o $@ $<

clean:
	-rm -f *.o *~ 
	-rm -f core

clobber: clean
	-rm -f *.a

rfsm.o: rfsm.h rfsm.cpp
