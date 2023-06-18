#include "systemc.h"
#include "rfsm.h"
#include "inp_Hr.h"
#include "inp_He.h"
#include "e.h"
#include "r.h"

int sc_main(int argc, char *argv[])
{
  sc_signal<bool> Hr;
  sc_signal<bool> He;
  sc_signal<int> r_data;
  sc_signal<bool> e_rdy;
  sc_signal<int> v;
  sc_signal<bool> r_rdy;
  sc_signal<int> e_state;
  sc_signal<int> r_state;
  sc_trace_file *trace_file;
  trace_file = sc_create_vcd_trace_file ("main");
  sc_write_comment(trace_file, "Generated by RFSM v1.7.0 from model main.fsm");
  sc_trace(trace_file, Hr, "Hr");
  sc_trace(trace_file, He, "He");
  sc_trace(trace_file, r_data, "r_data");
  sc_trace(trace_file, e_rdy, "e_rdy");
  sc_trace(trace_file, v, "v");
  sc_trace(trace_file, r_rdy, "r_rdy");
  sc_trace(trace_file, e_state, "e_state");
  sc_trace(trace_file, r_state, "r_state");

  Inp_Hr Inp_Hr("Inp_Hr");
  Inp_Hr(Hr);
  Inp_He Inp_He("Inp_He");
  Inp_He(He);

  E e("e");
  e(He,r_rdy,v,e_rdy,e_state);
  R r("r");
  r(Hr,e_rdy,v,r_rdy,r_data,r_state);

  sc_start(300, SC_NS);

  sc_close_vcd_trace_file (trace_file);

  return EXIT_SUCCESS;
}
