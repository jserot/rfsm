#include "inp_H.h"
#include "rfsm.h"

typedef struct { int period; int t1; int t2; } _periodic_t;

static _periodic_t _clk = { 10, 10, 200 };

void Inp_H::gen(void)
{
  t=0;
  H.write(0);
  wait(_clk.t1, SC_NS);
  t += _clk.t1;
  while ( t <= _clk.t2 ) {
    step();
    }
};

void Inp_H::step(void)
{
  H.write(1);
  wait(_clk.period/2, SC_NS);
  H.write(0);
  wait(_clk.period/2, SC_NS);
  t += _clk.period;
};
