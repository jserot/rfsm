SYSTEMC		= /usr/local/systemc-2.3.0
RFSM=/Users/jserot/Dev/ml/rfsm/working
TARGET_ARCH	= macosx64
CXX         = g++
LD	    = g++
AR = ar
CXXFLAGS    = -Wall  -I. -I$(SYSTEMC)/include -I$(RFSM)/lib/systemc -Wno-deprecated
LDFLAGS	    =  -L$(SYSTEMC)/lib-$(TARGET_ARCH) 

all: test_tau

.cpp.o:
	$(CXX) $(CXXFLAGS) -c -o $@ $<

clean:
	-rm -f *.o *~ 
	-rm -f core

clobber: clean
	-rm -f *.a

test_tau: test_tau.cpp
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $< -lsystemc 2>&1 | c++filt
	./test_tau
