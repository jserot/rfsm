#include "systemc.h"
#include "globals.h"

SC_MODULE(Inp_H)
{
  // Output
  sc_out<bool> H;

  void step();
  int t;
  void gen(void);

  SC_CTOR(Inp_H) {
    SC_THREAD(gen);
    }
};
