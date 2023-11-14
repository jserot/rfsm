#include "systemc.h"

template <int n>
SC_MODULE(Gensig)
{
  // Types
  typedef enum { E0,E1 } t_state;
  // IOs
  sc_in<bool> h;
  sc_in<bool> e;
  sc_out<bool> s;
  // Local variables
  t_state state;
  int k;

  void react();

  SC_CTOR(Gensig) {
    SC_THREAD(react);
    }
};

