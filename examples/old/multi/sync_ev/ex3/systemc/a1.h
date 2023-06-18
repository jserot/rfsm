#include "systemc.h"

SC_MODULE(A1)
{
  // Types
  typedef enum { A, B, C } t_state;
  // IOs
  sc_in<bool> h;
  sc_out<bool> e;
  // Constants
  // Local variables
  t_state state;

  void react();

  SC_CTOR(A1) {
    SC_THREAD(react);
    }
};
