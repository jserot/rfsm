#include "systemc.h"
#include "globals.h"

SC_MODULE(Heron)
{
  // Types
  typedef enum { Idle, Iter } t_state;
  // IOs
  sc_in<bool> h;
  sc_in<bool> start;
  sc_in<double> u;
  sc_out<bool> rdy;
  sc_out<int> niter;
  sc_out<double> r;
  // Constants
  static const double eps;
  // Local variables
  t_state state;
  double a;
  double x;
  int n;

  void react();

  SC_CTOR(Heron) {
    SC_THREAD(react);
    }
};
