include /Users/jserot/Dev/ml/rfsm/working/lib/etc/Makefile.systemc

d8.o: d8.h d8.cpp 
inp_N2.o: inp_N2.h inp_N2.cpp
inp_N1.o: inp_N1.h inp_N1.cpp
inp_Start.o: inp_Start.h inp_Start.cpp
inp_H.o: inp_H.h inp_H.cpp
tb.o: d8.h inp_N2.h inp_N1.h inp_Start.h inp_H.h tb.cpp
tb: /Users/jserot/Dev/ml/rfsm/working/lib/systemc/rfsm.o  d8.o inp_N2.o inp_N1.o inp_Start.o inp_H.o tb.o
	$(LD) $(LDFLAGS) -o tb /Users/jserot/Dev/ml/rfsm/working/lib/systemc/rfsm.o  d8.o inp_N2.o inp_N1.o inp_Start.o inp_H.o tb.o -lsystemc  2>&1 | c++filt
