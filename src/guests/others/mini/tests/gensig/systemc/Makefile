LIBDIR=/Users/jserot/Dev/ml/rfsm/etc


include $(LIBDIR)/platform

SYSTEMC=/opt/systemc
TARGET_ARCH=macosx64
CXXFLAGS += -I$(SYSTEMC)/include -I$(LIBDIR)/lib/systemc

LDFLAGS	 += -L$(SYSTEMC)/lib

SOURCES     = $(wildcard *.cpp)
OBJS        = $(SOURCES:.cpp=.o)

all: main 

run: main
	./main

view: main.vcd
	$(VCDVIEWER) main.vcd main.gtkw > /tmp/gtkwave.log 2>&1

.cpp.o:
	$(CXX) $(CXXFLAGS) -c -o $@ $<

clean:
	-rm -f *.o *~ main
	-rm -f core
	-rm -f *.vcd *ghw

clobber: clean
	-rm -f *.cpp *.h
	-rm -f Makefile  # Generated

rfsm.cpp: $(LIBDIR)/lib/systemc/rfsm.cpp
	cp $< $@
g.o: g.h g.cpp 
inp_H.o: inp_H.h inp_H.cpp
inp_E.o: inp_E.h inp_E.cpp
main.o: g.h inp_H.h inp_E.h main.cpp
main: rfsm.o  g.o inp_H.o inp_E.o main.o
	$(LD) $(LDFLAGS) -o main rfsm.o  g.o inp_H.o inp_E.o main.o -lsystemc  2>&1 | c++filt

