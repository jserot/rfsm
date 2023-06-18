#include "systemc.h"

SC_MODULE(Inp_He)
{
  // Output
  sc_out<bool> He;

  void step();
  int t;
  void gen(void);

  SC_CTOR(Inp_He) {
    SC_THREAD(gen);
    }
};
