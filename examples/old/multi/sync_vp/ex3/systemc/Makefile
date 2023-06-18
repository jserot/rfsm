include /Users/jserot/Dev/ml/rfsm/working/lib/etc/Makefile.systemc

A1_1.o: A1_1.h A1_1.cpp 
A2_1.o: A2_1.h A2_1.cpp 
inp_H2.o: inp_H2.h inp_H2.cpp
inp_H1.o: inp_H1.h inp_H1.cpp
tb.o: A1_1.h A2_1.h inp_H2.h inp_H1.h tb.cpp
tb: /Users/jserot/Dev/ml/rfsm/working/lib/systemc/rfsm.o  A1_1.o A2_1.o inp_H2.o inp_H1.o tb.o
	$(LD) $(LDFLAGS) -o tb /Users/jserot/Dev/ml/rfsm/working/lib/systemc/rfsm.o  A1_1.o A2_1.o inp_H2.o inp_H1.o tb.o -lsystemc  2>&1 | c++filt
