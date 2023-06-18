#include "systemc.h"

SC_MODULE(A2)
{
  // Types
  typedef enum { S1, S2 } t_state;
  // IOs
  sc_in<bool> h;
  sc_out<bool> v2;
  sc_inout<bool> v1;
  // Constants
  // Local variables
  t_state state;

  void react();

  SC_CTOR(A2) {
    SC_THREAD(react);
    }
};
