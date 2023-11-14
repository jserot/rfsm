#ifndef _globals_h
#define _globals_h

#include "systemc.h"

class Tau {
public:
  struct { int x; int y; } repr;
  ~Tau() { };
  Tau() { };
  Tau(int x, int y) { repr.x = x; repr.y = y; };
  inline Tau& operator = (const Tau& v) { repr = v.repr; return *this; }
  inline friend bool operator == ( const Tau& v1, const Tau& v2)
    { return v1.repr.x == v2.repr.x && v1.repr.y == v2.repr.y; }
  inline friend ::std::ostream& operator << ( ::std::ostream& os, const Tau& v) {
    os << "{" << v.repr.x << "," << v.repr.y << "} ";
    return os;
    }
  inline friend ::std::istream& operator >> ( ::std::istream& is, Tau& v) {
    is >> v.repr.x >> v.repr.y;
    return is;
  }
};

inline void sc_trace(sc_trace_file *tf, const Tau& v, const std::string& n) { sc_trace(tf, "<Tau>", n);}

#endif
