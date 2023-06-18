#include "systemc.h"

SC_MODULE(A2)
{
  // Types
  typedef enum { C, D } t_state;
  // IOs
  sc_in<bool> h;
  sc_in<bool> e;
  // Constants
  // Local variables
  t_state state;

  void react();

  SC_CTOR(A2) {
    SC_THREAD(react);
    }
};
