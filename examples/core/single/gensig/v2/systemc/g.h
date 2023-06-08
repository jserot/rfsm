#include "systemc.h"

SC_MODULE(G)
{
  // Types
  typedef enum { E0, E1 } t_state;
  // IOs
  sc_in<bool> h;
  sc_in<bool> e;
  sc_out<bool> s;
  // Constants
  static const int n;
  // Local variables
  t_state state;
  int k;

  void react();

  SC_CTOR(G) {
    SC_THREAD(react);
    }
};
