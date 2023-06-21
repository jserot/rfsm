#include "systemc.h"

SC_MODULE(A2)
{
  // Types
  typedef enum { S1, S2, S3 } t_state;
  // IOs
  sc_in<bool> h;
  sc_inout<bool> v;
  // Constants
  // Local variables
  t_state state;

  void react();

  SC_CTOR(A2) {
    SC_THREAD(react);
    }
};