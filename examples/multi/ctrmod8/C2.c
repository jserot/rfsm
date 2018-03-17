task C2(
  in event h;
 out int s;
 out event r;
  )
{
  enum {E0,E1} state = E0;
  s=0;
  while ( 1 ) {
    switch ( state ) {
    case E1:
      wait_ev(h);
      notify_ev(r);
      s=0;
      state = E0;
      break;
    case E0:
      wait_ev(h);
      s=1;
      state = E1;
      break;
    }
  }
};
