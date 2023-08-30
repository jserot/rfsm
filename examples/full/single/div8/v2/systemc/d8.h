#include "systemc.h"

SC_MODULE(D8)
{
  // Types
  typedef enum { Calcul, Repos } t_state;
  // IOs
  sc_in<bool> h;
  sc_in<bool> start;
  sc_in<sc_uint<8> > n1;
  sc_in<sc_uint<8> > n2;
  sc_out<bool> rdy;
  sc_out<sc_uint<8> > r;
  // Constants
  // Local variables
  t_state state;
  sc_uint<16>  a;
  sc_uint<16>  b;
  sc_uint<8>  c;
  int i;

  void react();

  SC_CTOR(D8) {
    SC_THREAD(react);
    }
};
