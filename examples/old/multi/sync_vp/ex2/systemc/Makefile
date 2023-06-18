include /Users/jserot/Dev/ml/rfsm/working/lib/etc/Makefile.systemc

a1.o: a1.h a1.cpp 
a2.o: a2.h a2.cpp 
inp_H.o: inp_H.h inp_H.cpp
tb.o: a1.h a2.h inp_H.h tb.cpp
tb: /Users/jserot/Dev/ml/rfsm/working/lib/systemc/rfsm.o  a1.o a2.o inp_H.o tb.o
	$(LD) $(LDFLAGS) -o tb /Users/jserot/Dev/ml/rfsm/working/lib/systemc/rfsm.o  a1.o a2.o inp_H.o tb.o -lsystemc  2>&1 | c++filt
