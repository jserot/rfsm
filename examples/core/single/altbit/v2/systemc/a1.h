#include "systemc.h"

SC_MODULE(A1)
{
  // Types
  typedef enum { Init, R } t_state;
  // IOs
  sc_in<bool> h;
  sc_in<bool> e;
  sc_out<bool> err;
  sc_out<int> st;
  // Constants
  // Local variables
  t_state state;
  bool last;

  void react();

  SC_CTOR(A1) {
    SC_THREAD(react);
    }
};
