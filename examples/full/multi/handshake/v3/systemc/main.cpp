#include "systemc.h"
#include "rfsm.h"
#include "inp_H.h"
#include "e.h"
#include "i.h"
#include "r.h"

int sc_main(int argc, char *argv[])
{
  sc_signal<bool> H;
  sc_signal<int> r_data;
  sc_signal<bool> e_rdy2;
  sc_signal<int> v2;
  sc_signal<bool> r_rdy2;
  sc_signal<bool> e_rdy1;
  sc_signal<int> v1;
  sc_signal<bool> r_rdy1;
  sc_signal<int> e_state;
  sc_signal<int> i_state;
  sc_signal<int> r_state;
  sc_trace_file *trace_file;
  trace_file = sc_create_vcd_trace_file ("main");
  sc_write_comment(trace_file, "Generated by RFSM v1.7.0 from model main.fsm");
  sc_trace(trace_file, H, "H");
  sc_trace(trace_file, r_data, "r_data");
  sc_trace(trace_file, e_rdy2, "e_rdy2");
  sc_trace(trace_file, v2, "v2");
  sc_trace(trace_file, r_rdy2, "r_rdy2");
  sc_trace(trace_file, e_rdy1, "e_rdy1");
  sc_trace(trace_file, v1, "v1");
  sc_trace(trace_file, r_rdy1, "r_rdy1");
  sc_trace(trace_file, e_state, "e_state");
  sc_trace(trace_file, i_state, "i_state");
  sc_trace(trace_file, r_state, "r_state");

  Inp_H Inp_H("Inp_H");
  Inp_H(H);

  E e("e");
  e(H,r_rdy1,v1,e_rdy1,e_state);
  I i("i");
  i(H,e_rdy1,v1,r_rdy2,r_rdy1,v2,e_rdy2,i_state);
  R r("r");
  r(H,e_rdy2,v2,r_rdy2,r_data,r_state);

  sc_start(800, SC_NS);

  sc_close_vcd_trace_file (trace_file);

  return EXIT_SUCCESS;
}