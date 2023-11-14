#include "globals.h"

int main(int argc, char *argv[])
{
  Tau t(1,2);
  Tau tt;
  cout << t << endl;
  tt = t;
  if ( tt.repr.x == tt.repr.y ) cout << "yes"; else cout << "no";
  cout << endl;
}
