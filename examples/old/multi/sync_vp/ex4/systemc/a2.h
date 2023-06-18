#include "systemc.h"

SC_MODULE(A2)
{
  // Types
  typedef enum { S3, S4 } t_state;
  // IOs
  sc_in<bool> h;
  sc_inout<int> v;
  // Constants
  // Local variables
  t_state state;

  void react();

  SC_CTOR(A2) {
    SC_THREAD(react);
    }
};
