#include "systemc.h"

SC_MODULE(Inp_e)
{
  // Output
  sc_out<bool> e;

  void gen(void);

  SC_CTOR(Inp_e) {
    SC_THREAD(gen);
    }
};

