#include "systemc.h"

SC_MODULE(R)
{
  // Types
  typedef enum { R1, R2 } t_state;
  // IOs
  sc_in<bool> h;
  sc_in<bool> e_rdy;
  sc_in<int> data;
  sc_out<bool> r_rdy;
  sc_out<int> dout;
  sc_out<int> st;
  // Constants
  // Local variables
  t_state state;
  int t;

  void react();

  SC_CTOR(R) {
    SC_THREAD(react);
    }
};
