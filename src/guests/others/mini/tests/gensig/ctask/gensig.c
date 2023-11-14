task Gensig(
  in event h;
  in bool e;
 out bool s;
  )
{
  enum { E0,E1,E2,E3 } state = E0;
  s = false;
  while ( 1 ) {
    switch ( state ) {
    case E0:
      wait_ev(h);
      if ( e ) {
        s = true;
        state = E1;
        }
      break;
    case E1:
      wait_ev(h);
      state = E2;
      break;
    case E2:
      wait_ev(h);
      state = E3;
      break;
    case E3:
      wait_ev(h);
      s = false;
      state = E0;
      break;
    }
  }
};
