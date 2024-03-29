#include "systemc.h"

SC_MODULE(Gensig)
{
  // Types
  typedef enum { E0,E1,E2,E3 } t_state;
  // IOs
  sc_in<bool> h;
  sc_in<bool> e;
  sc_out<bool> s;
  // Local variables
  t_state state;

  void react();

  SC_CTOR(Gensig) {
    SC_THREAD(react);
    }
};

