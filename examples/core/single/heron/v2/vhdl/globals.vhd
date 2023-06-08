library ieee;
use ieee.std_logic_1164.all;
use work.rfsm.all;

package globals is
  function f_abs(x:real) return real;
end globals;

package body globals is
function f_abs(x:real) return real is
  begin
    return cond(x<0.000000E+00,-(x),x);
  end;

end globals;
