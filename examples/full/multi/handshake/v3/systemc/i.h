#include "systemc.h"

SC_MODULE(I)
{
  // Types
  typedef enum { E1, E2, R1, R2, U } t_state;
  // IOs
  sc_in<bool> h;
  sc_in<bool> e_rdy1;
  sc_in<int> din;
  sc_in<bool> r_rdy2;
  sc_out<bool> r_rdy1;
  sc_out<int> dout;
  sc_out<bool> e_rdy2;
  sc_out<int> st;
  // Constants
  static const int delai;
  // Local variables
  t_state state;
  int data;
  int t;

  void react();

  SC_CTOR(I) {
    SC_THREAD(react);
    }
};
