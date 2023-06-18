#include "systemc.h"

SC_MODULE(A1)
{
  // Types
  typedef enum { A, B } t_state;
  // IOs
  sc_in<bool> h;
  sc_out<bool> e;
  sc_out<int> st;
  // Constants
  // Local variables
  t_state state;

  void react();

  SC_CTOR(A1) {
    SC_THREAD(react);
    }
};
