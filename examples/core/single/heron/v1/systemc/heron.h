#include "systemc.h"

SC_MODULE(Heron)
{
  // Types
  typedef enum { Idle, Iter } t_state;
  // IOs
  sc_in<bool> h;
  sc_in<bool> start;
  sc_in<float> u;
  sc_out<bool> rdy;
  sc_out<float> r;
  // Constants
  static const int niter;
  // Local variables
  t_state state;
  float a;
  float x;
  int n;

  void react();

  SC_CTOR(Heron) {
    SC_THREAD(react);
    }
};
