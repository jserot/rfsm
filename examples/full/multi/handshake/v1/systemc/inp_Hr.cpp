#include "inp_Hr.h"
#include "rfsm.h"

typedef struct { int period; int t1; int t2; } _periodic_t;

static _periodic_t _clk = { 4, 15, 300 };

void Inp_Hr::gen(void)
{
  t=0;
  Hr.write(0);
  wait(_clk.t1, SC_NS);
  t += _clk.t1;
  while ( t <= _clk.t2 ) {
    step();
    }
};

void Inp_Hr::step(void)
{
  Hr.write(1);
  wait(_clk.period/2.0, SC_NS);
  Hr.write(0);
  wait(_clk.period/2.0, SC_NS);
  t += _clk.period;
};
