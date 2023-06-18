#include "systemc.h"

SC_MODULE(E)
{
  // Types
  typedef enum { E1, E2 } t_state;
  // IOs
  sc_in<bool> h;
  sc_in<bool> r_rdy;
  sc_out<int> data;
  sc_out<bool> e_rdy;
  sc_out<int> st;
  // Constants
  // Local variables
  t_state state;
  int cnt;

  void react();

  SC_CTOR(E) {
    SC_THREAD(react);
    }
};
