#include "systemc.h"
#include "rfsm.h"
#include "inp_H.h"
#include "a1.h"
#include "a2.h"

int sc_main(int argc, char *argv[])
{
  sc_signal<bool> H;
  sc_signal<bool> e;
  sc_trace_file *trace_file;
  trace_file = sc_create_vcd_trace_file ("tb");
  sc_trace(trace_file, H, "H");
  sc_trace(trace_file, e, "e");

  Inp_H Inp_H("Inp_H");
  Inp_H(H);

  A1 a1("a1");
  a1(H,e);
  A2 a2("a2");
  a2(H,e);

  sc_start(100, SC_NS);

  sc_close_vcd_trace_file (trace_file);

  return EXIT_SUCCESS;
}
