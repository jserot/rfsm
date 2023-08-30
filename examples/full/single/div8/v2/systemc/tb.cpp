#include "systemc.h"
#include "rfsm.h"
#include "inp_N2.h"
#include "inp_N1.h"
#include "inp_Start.h"
#include "inp_H.h"
#include "d8.h"

int sc_main(int argc, char *argv[])
{
  sc_signal<sc_uint<8> > N2;
  sc_signal<sc_uint<8> > N1;
  sc_signal<bool> Start;
  sc_signal<bool> H;
  sc_signal<sc_uint<8> > R;
  sc_signal<bool> Rdy;
  sc_trace_file *trace_file;
  trace_file = sc_create_vcd_trace_file ("tb");
  sc_trace(trace_file, N2, "N2");
  sc_trace(trace_file, N1, "N1");
  sc_trace(trace_file, Start, "Start");
  sc_trace(trace_file, H, "H");
  sc_trace(trace_file, R, "R");
  sc_trace(trace_file, Rdy, "Rdy");

  Inp_N2 Inp_N2("Inp_N2");
  Inp_N2(N2);
  Inp_N1 Inp_N1("Inp_N1");
  Inp_N1(N1);
  Inp_Start Inp_Start("Inp_Start");
  Inp_Start(Start);
  Inp_H Inp_H("Inp_H");
  Inp_H(H);

  D8 d8("d8");
  d8(H,Start,N1,N2,Rdy,R);

  sc_start(120, SC_NS);

  sc_close_vcd_trace_file (trace_file);

  return EXIT_SUCCESS;
}
