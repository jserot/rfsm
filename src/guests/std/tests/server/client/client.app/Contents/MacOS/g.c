task Gensig(
  in event H;
  in bool e;
 out bool s1;
  )
{
  enum { E0,E1,E2,E3 } state = E0;
  s1 = false;
  while ( 1 ) {
    switch ( state ) {
    case E0:
      wait_ev(H);
      if ( e==true ) {
        s1 = true;
        state = E1;
        }
      break;
    case E1:
      wait_ev(H);
      s1 = true;
      state = E2;
      break;
    case E2:
      wait_ev(H);
      s1 = true;
      state = E3;
      break;
    case E3:
      wait_ev(H);
      s1 = false;
      state = E0;
      break;
    }
  }
};
