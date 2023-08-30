#include "systemc.h"

SC_MODULE(Inp_N2)
{
  // Output
  sc_out<sc_uint<8> > N2;

  void gen(void);

  SC_CTOR(Inp_N2) {
    SC_THREAD(gen);
    }
};
