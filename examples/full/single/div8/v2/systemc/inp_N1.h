#include "systemc.h"

SC_MODULE(Inp_N1)
{
  // Output
  sc_out<sc_uint<8> > N1;

  void gen(void);

  SC_CTOR(Inp_N1) {
    SC_THREAD(gen);
    }
};
