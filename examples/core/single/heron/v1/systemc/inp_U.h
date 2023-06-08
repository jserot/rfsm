#include "systemc.h"

SC_MODULE(Inp_U)
{
  // Output
  sc_out<float> U;

  void gen(void);

  SC_CTOR(Inp_U) {
    SC_THREAD(gen);
    }
};
