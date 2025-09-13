#include "systemc.h"

SC_MODULE(G)
{
  // Types
  typedef enum { E0,E1,E2,E3 } t_state;
  // IOs
  sc_in<bool> H;
  sc_in<bool> e;
  sc_out<bool> s1;
  // Local variables
  t_state state;

  void react();

  SC_CTOR(G) {
    SC_THREAD(react);
    }
};

