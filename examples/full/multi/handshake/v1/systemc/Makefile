LIBDIR=/Users/jserot/.opam/4.10.0/share/rfsm

include $(LIBDIR)/platform

SYSTEMC=/usr/local/systemc-2.3.0
TARGET_ARCH=macosx64
CXXFLAGS += -I$(SYSTEMC)/include -I$(LIBDIR)/lib/systemc

LDFLAGS	 += -L$(SYSTEMC)/lib-$(TARGET_ARCH) 

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
	-rm -f *.vcd

clobber: clean
	-rm -f *.cpp *.h
	-rm -f *.gtkw *.vcd
	-rm -f Makefile  # Generated

rfsm.cpp: $(LIBDIR)/lib/systemc/rfsm.cpp
	cp $< $@

e.o: e.h e.cpp 
r.o: r.h r.cpp 
inp_Hr.o: inp_Hr.h inp_Hr.cpp
inp_He.o: inp_He.h inp_He.cpp
main.o: e.h r.h inp_Hr.h inp_He.h main.cpp
main: rfsm.o  e.o r.o inp_Hr.o inp_He.o main.o
	$(LD) $(LDFLAGS) -o main rfsm.o  e.o r.o inp_Hr.o inp_He.o main.o -lsystemc  2>&1 | c++filt
