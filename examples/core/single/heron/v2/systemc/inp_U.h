#include "systemc.h"
#include "globals.h"

SC_MODULE(Inp_U)
{
  // Output
  sc_out<double> U;

  void gen(void);

  SC_CTOR(Inp_U) {
    SC_THREAD(gen);
    }
};
