#include "systemc.h"
#include "rfsm.h"
#include "inp_H2.h"
#include "inp_H1.h"
#include "A1_1.h"
#include "A2_1.h"

int sc_main(int argc, char *argv[])
{
  sc_signal<bool> H2;
  sc_signal<bool> H1;
  sc_signal<int,SC_MANY_WRITERS> v;
  sc_trace_file *trace_file;
  trace_file = sc_create_vcd_trace_file ("tb");
  sc_trace(trace_file, H2, "H2");
  sc_trace(trace_file, H1, "H1");
  sc_trace(trace_file, v, "v");

  Inp_H2 Inp_H2("Inp_H2");
  Inp_H2(H2);
  Inp_H1 Inp_H1("Inp_H1");
  Inp_H1(H1);

  A1_1 A1_1("A1_1");
  A1_1(H1,v);
  A2_1 A2_1("A2_1");
  A2_1(H2,v);

  sc_start(100, SC_NS);

  sc_close_vcd_trace_file (trace_file);

  return EXIT_SUCCESS;
}
