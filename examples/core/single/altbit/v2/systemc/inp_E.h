#include "systemc.h"

SC_MODULE(Inp_E)
{
  // Output
  sc_out<bool> E;

  void gen(void);

  SC_CTOR(Inp_E) {
    SC_THREAD(gen);
    }
};
