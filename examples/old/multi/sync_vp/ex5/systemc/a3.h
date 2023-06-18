#include "systemc.h"

SC_MODULE(A3)
{
  // Types
  typedef enum { S1, S2 } t_state;
  // IOs
  sc_in<bool> h;
  sc_inout<bool> v;
  // Constants
  // Local variables
  t_state state;

  void react();

  SC_CTOR(A3) {
    SC_THREAD(react);
    }
};
