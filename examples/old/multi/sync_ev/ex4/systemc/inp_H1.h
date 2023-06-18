#include "systemc.h"

SC_MODULE(Inp_H1)
{
  // Output
  sc_out<bool> H1;

  void gen(void);

  SC_CTOR(Inp_H1) {
    SC_THREAD(gen);
    }
};
