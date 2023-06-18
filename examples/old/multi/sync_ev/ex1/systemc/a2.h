#include "systemc.h"

SC_MODULE(A2)
{
  // Types
  typedef enum { A, B } t_state;
  // IOs
  sc_in<bool> h;
  sc_in<bool> e;
  sc_out<int> st;
  // Constants
  // Local variables
  t_state state;

  void react();

  SC_CTOR(A2) {
    SC_THREAD(react);
    }
};
