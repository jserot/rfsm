#include "systemc.h"
#include "rfsm.h"
#include "inp_E.h"
#include "inp_H.h"
#include "a1.h"

int sc_main(int argc, char *argv[])
{
  sc_signal<bool> E;
  sc_signal<bool> H;
  sc_signal<bool> Err;
  sc_trace_file *trace_file;
  trace_file = sc_create_vcd_trace_file ("tb");
  sc_trace(trace_file, E, "E");
  sc_trace(trace_file, H, "H");
  sc_trace(trace_file, Err, "Err");

  Inp_E Inp_E("Inp_E");
  Inp_E(E);
  Inp_H Inp_H("Inp_H");
  Inp_H(H);

  A1 a1("a1");
  a1(H,E,Err);

  sc_start(120, SC_NS);

  sc_close_vcd_trace_file (trace_file);

  return EXIT_SUCCESS;
}
