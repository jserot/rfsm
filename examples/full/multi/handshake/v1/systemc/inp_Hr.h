#include "systemc.h"

SC_MODULE(Inp_Hr)
{
  // Output
  sc_out<bool> Hr;

  void step();
  int t;
  void gen(void);

  SC_CTOR(Inp_Hr) {
    SC_THREAD(gen);
    }
};
