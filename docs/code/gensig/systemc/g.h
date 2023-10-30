#include "systemc.h"

SC_MODULE(G)
{
  // Types
  typedef enum { E0,E1 } t_state;
  // IOs
  sc_in<bool> H;
  sc_in<bool> E;
  sc_out<bool> S;
  // Local variables
  t_state state;
  int k;

  void react();

  SC_CTOR(G) {
    SC_THREAD(react);
    }
};

