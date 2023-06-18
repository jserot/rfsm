include /Users/jserot/Dev/ml/rfsm/working/lib/etc/Makefile.systemc

a1.o: a1.h a1.cpp 
a2.o: a2.h a2.cpp 
inp_H2.o: inp_H2.h inp_H2.cpp
inp_H1.o: inp_H1.h inp_H1.cpp
tb.o: a1.h a2.h inp_H2.h inp_H1.h tb.cpp
tb: /Users/jserot/Dev/ml/rfsm/working/lib/systemc/rfsm.o  a1.o a2.o inp_H2.o inp_H1.o tb.o
	$(LD) $(LDFLAGS) -o tb /Users/jserot/Dev/ml/rfsm/working/lib/systemc/rfsm.o  a1.o a2.o inp_H2.o inp_H1.o tb.o -lsystemc  2>&1 | c++filt
