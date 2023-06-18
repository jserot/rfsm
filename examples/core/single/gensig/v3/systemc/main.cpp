#include "systemc.h"
#include "rfsm.h"
#include "inp_E.h"
#include "inp_H.h"
#include "g.h"

int sc_main(int argc, char *argv[])
{
  sc_signal<bool> E;
  sc_signal<bool> H;
  sc_signal<bool> S;
  sc_trace_file *trace_file;
  trace_file = sc_create_vcd_trace_file ("main");
  sc_write_comment(trace_file, "Generated by RFSM v1.7.0 from model main.fsm");
  sc_trace(trace_file, E, "E");
  sc_trace(trace_file, H, "H");
  sc_trace(trace_file, S, "S");

  Inp_E Inp_E("Inp_E");
  Inp_E(E);
  Inp_H Inp_H("Inp_H");
  Inp_H(H);

  G g("g");
  g(H,E,S);

  sc_start(100, SC_NS);

  sc_close_vcd_trace_file (trace_file);

  return EXIT_SUCCESS;
}