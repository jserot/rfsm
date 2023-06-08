#include "systemc.h"
#include "globals.h"

SC_MODULE(Inp_Start)
{
  // Output
  sc_out<bool> Start;

  void gen(void);

  SC_CTOR(Inp_Start) {
    SC_THREAD(gen);
    }
};
