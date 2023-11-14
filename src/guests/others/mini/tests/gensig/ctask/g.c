task Gensig(
  in event H;
  in bool E;
 out bool S;
  )
{
  enum { E0,E1,E2,E3 } state = E0;
  S = false;
  while ( 1 ) {
    switch ( state ) {
    case E0:
      wait_ev(H);
      if ( E ) {
        S = true;
        state = E1;
        }
      break;
    case E1:
      wait_ev(H);
      state = E2;
      break;
    case E2:
      wait_ev(H);
      state = E3;
      break;
    case E3:
      wait_ev(H);
      S = false;
      state = E0;
      break;
    }
  }
};
