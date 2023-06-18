#include "systemc.h"
#include "rfsm.h"
#include "inp_H.h"
#include "a1.h"
#include "a2.h"
#include "a3.h"

int sc_main(int argc, char *argv[])
{
  sc_signal<bool> H;
  sc_signal<bool,SC_MANY_WRITERS> v2;
  sc_signal<bool,SC_MANY_WRITERS> v1;
  sc_trace_file *trace_file;
  trace_file = sc_create_vcd_trace_file ("tb");
  sc_trace(trace_file, H, "H");
  sc_trace(trace_file, v2, "v2");
  sc_trace(trace_file, v1, "v1");

  Inp_H Inp_H("Inp_H");
  Inp_H(H);

  A1 a1("a1");
  a1(H,v1);
  A2 a2("a2");
  a2(H,v2,v1);
  A3 a3("a3");
  a3(H,v2);

  sc_start(100, SC_NS);

  sc_close_vcd_trace_file (trace_file);

  return EXIT_SUCCESS;
}
