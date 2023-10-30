task Gensig<int n>(
  in event h;
  in bool e;
 out bool s;
  )
{
  int<1:n> k;
  enum { E0,E1 } state = E0;
  while ( 1 ) {
    switch ( state ) {
    case E0:
      s = false;
      wait_ev(h);
      if ( e==true ) {
        k = 1;
        state = E1;
        }
      break;
    case E1:
      s = true;
      wait_ev(h);
      if ( k==n ) {
        state = E0;
        }
      else if ( k<n ) {
        k = k+1;
        }
      break;
    }
  }
};
