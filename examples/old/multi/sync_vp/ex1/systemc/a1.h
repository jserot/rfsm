#include "systemc.h"

SC_MODULE(A1)
{
  // Types
  typedef enum { S1, S2 } t_state;
  // IOs
  sc_in<bool> h;
  sc_out<bool> v;
  // Constants
  // Local variables
  t_state state;

  void react();

  SC_CTOR(A1) {
    SC_THREAD(react);
    }
};
