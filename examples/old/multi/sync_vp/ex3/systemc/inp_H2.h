#include "systemc.h"

SC_MODULE(Inp_H2)
{
  // Output
  sc_out<bool> H2;

  void step();
  int t;
  void gen(void);

  SC_CTOR(Inp_H2) {
    SC_THREAD(gen);
    }
};
