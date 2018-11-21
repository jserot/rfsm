library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package core is

  function cond(e1: boolean; e2: signed; e3: signed) return signed;
  function cond(e1: boolean; e2: unsigned; e3: unsigned) return unsigned;
  function cond(e1: boolean; e2: std_logic; e3: std_logic) return std_logic;
  function cond(e1: boolean; e2: std_logic_vector; e3: std_logic_vector) return std_logic_vector;
  function cond(e1: boolean; e2: integer; e3: integer) return integer;
  function cond(e1: boolean; e2: real; e3: real) return real;
  function cond(e1: boolean; e2: character; e3: character) return character;
  function eq(e1: signed; e2: signed) return unsigned;
  function eq(e1: unsigned; e2: unsigned) return unsigned;
  function eq(e1: integer; e2: integer) return unsigned;
  function eq(e1: real; e2: real) return unsigned;
  function mul(e1: signed; e2: signed) return signed;
  function mul(e1: unsigned; e2: unsigned) return unsigned;
  function mul(e1: unsigned; e2: integer) return unsigned;
  function mul(e1: signed; e2: integer) return signed;
  function mul(e1: integer; e2: unsigned) return unsigned;
  function mul(e1: integer; e2: signed) return signed;
  function to_std_logic_vector(e: unsigned; s: natural) return std_logic_vector;
  function to_std_logic_vector(e: signed; s: natural) return std_logic_vector;
  function to_std_logic_vector(e: boolean; s: natural) return std_logic_vector;
  function from_std_logic_vector(e: std_logic_vector; s: natural) return unsigned;
  function from_std_logic_vector(e: std_logic_vector; s: natural) return signed;
  function from_std_logic_vector(e: std_logic_vector; s: natural) return boolean;
  function from_std_logic_vector(e: std_logic_vector; s: natural) return std_logic_vector;
  function to_bool(e: integer) return boolean;
  function to_bool(e: unsigned) return boolean;
  function to_bool(e: signed) return boolean;
  function to_bool(e: std_logic_vector) return boolean;
  function to_char(e: integer) return character;
  function to_char(e: unsigned) return character;
  function conv_unsigned(e: unsigned; s: natural) return unsigned;
  function conv_unsigned(e: signed; s: natural) return unsigned;
  function conv_unsigned(e: boolean; s: natural) return unsigned;
  function conv_unsigned(e: character; s: natural) return unsigned;
  function conv_unsigned(e: integer; s: natural) return unsigned;
  function conv_signed(e: signed; s: natural) return signed;
  function conv_signed(e: unsigned; s: natural) return signed;
  function conv_signed(e: boolean; s: natural) return signed;
  function conv_signed(e: integer; s: natural) return signed;
  function to_integer(e: integer) return integer;
  function to_integer(e: boolean) return integer;
  function to_integer(e: character) return integer;

  procedure notify_ev(signal s: out std_logic; duration: time);
  
  function to_string(v : std_logic_vector) return string;  -- for debug only
  procedure dump_slv(name: string; v: std_logic_vector); -- for debug only

end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- use STD.textio.all;   -- FOR DEBUG ONLY

package body core is

  function cond(e1: boolean; e2: signed; e3: signed) return signed is
  begin
    if e1 then return e2; else return e3; end if;
  end;

  function cond(e1: boolean; e2: unsigned; e3: unsigned) return unsigned is
  begin
    if e1 then return e2; else return e3; end if;
  end;

  function cond(e1: boolean; e2: std_logic; e3: std_logic) return std_logic is
  begin
    if e1 then return e2; else return e3; end if;
  end;

  function cond(e1: boolean; e2: std_logic_vector; e3: std_logic_vector) return std_logic_vector is
  begin
    if e1 then return e2; else return e3; end if;
  end;

  function cond(e1: boolean; e2: integer; e3: integer) return integer is
  begin
    if e1 then return e2; else return e3; end if;
  end;

  function cond(e1: boolean; e2: real; e3: real) return real is
  begin
    if e1 then return e2; else return e3; end if;
  end;

  function cond(e1: boolean; e2: character; e3: character) return character is
  begin
    if e1 then return e2; else return e3; end if;
  end;

  function eq(e1: signed; e2: signed) return unsigned is
  begin
     if ( e1 = e2 ) then
       return "1";
     else
       return "0";
     end if;
  end;

  function eq(e1: unsigned; e2: unsigned) return unsigned is
  begin
     if ( e1 = e2 ) then
       return "1";
     else
       return "0";
     end if;
  end;

  function eq(e1: integer; e2: integer) return unsigned is
  begin
     if ( e1 = e2 ) then
       return "1";
     else
       return "0";
     end if;
  end;

  function eq(e1: real; e2: real) return unsigned is
  begin
     if ( e1 = e2 ) then
       return "1";
     else
       return "0";
     end if;
  end;

  function maximum(L, R: INTEGER) return INTEGER is
    begin
        if L > R then
            return L;
        else
            return R;
        end if;
  end;

  function mul (e1: signed; e2: signed) return signed is
        constant length: INTEGER := maximum(e1'length, e2'length);
    begin
        return resize(e1*e2, length);
    end;

  function mul (e1: unsigned; e2: unsigned) return unsigned is
        constant length: INTEGER := maximum(e1'length, e2'length);
    begin
        return resize(e1*e2, length);
    end;

  function mul (e1: unsigned; e2: integer) return unsigned is
        constant length: INTEGER := e1'length;
    begin
        return resize(e1*to_unsigned(e2,length), length);
    end;

  function mul (e1: signed; e2: integer) return signed is
        constant length: INTEGER := e1'length;
    begin
        return resize(e1*to_signed(e2,length), length);
    end;

  function mul (e1: integer; e2: unsigned) return unsigned is
        constant length: INTEGER := e2'length;
    begin
        return resize(to_unsigned(e1,length)*e2, length);
    end;

  function mul (e1: integer; e2: signed) return signed is
        constant length: INTEGER := e2'length;
    begin
        return resize(to_signed(e1,length)*e2, length);
    end;

  function to_std_logic_vector(e: unsigned; s:natural) return std_logic_vector is
  begin
    return STD_LOGIC_VECTOR(resize(e,s));
  end;

  function to_std_logic_vector(e: signed; s:natural) return std_logic_vector is
  begin
    return STD_LOGIC_VECTOR(resize(e,s));
  end;

  function to_std_logic_vector(e: boolean; s:natural) return std_logic_vector is
  begin
    if e then
      return STD_LOGIC_VECTOR(TO_UNSIGNED(0,s-1)) & "1";
    else
      return STD_LOGIC_VECTOR(TO_UNSIGNED(0,s-1)) & "0";
    end if;
  end;

  function from_std_logic_vector(e: std_logic_vector; s:natural) return unsigned is
  begin
    -- return UNSIGNED(e(s-1 downto 0));
    -- return UNSIGNED(e(e'high downto e'high-s+1));
    return UNSIGNED(e(s-1 downto 0));  -- Changed in v2.6.2: in variants, fields are RIGHT justified
  end;

  function from_std_logic_vector(e: std_logic_vector; s:natural) return signed is
  begin
    -- return SIGNED(e(s-1 downto 0));
    -- return SIGNED(e(e'high downto e'high-s+1));
    return SIGNED(e(s-1 downto 0));  -- Changed in v2.6.2: in variants, fields are RIGHT justified
  end;

  function from_std_logic_vector(e: std_logic_vector; s:natural) return boolean is
  begin
    -- if e(e'high downto e'high) = "1" then
    if e(0 downto 0) = "1" then  -- Changed in v2.6.2: in variants, fields are RIGHT justified
      return true;
    else
      return false;
    end if;
  end;

  function from_std_logic_vector(e: std_logic_vector; s:natural) return std_logic_vector is
  begin
    return (e(s-1 downto 0));  -- Changed in v2.6.2: in variants, fields are RIGHT justified
    -- return e(e'high downto e'high-s+1);
  end;

  function to_bool(e: integer) return boolean is
  begin
    if e = 0 then return false; else return true; end if;
  end;
                                      
  function to_bool(e: unsigned) return boolean is
  begin
    if e = (e'range=>'0') then return false; else return true; end if;
  end;
                                      
  function to_bool(e: signed) return boolean is
  begin
    if e = (e'range=>'0') then return false; else return true; end if;
  end;

  function to_bool(e: std_logic_vector) return boolean is
  begin
    if e = (e'range=>'0') then return false; else return true; end if;
  end;

  function to_char(e: integer) return character is
  begin
      return character'val(e);
  end;

  function to_char(e: unsigned) return character is
  begin
      return to_char(to_integer(e));
  end;
                                      
  function conv_unsigned(e: unsigned; s: natural) return unsigned is
  begin
    return resize(e, s);
  end;
  
  function conv_unsigned(e: signed; s: natural) return unsigned is
  begin
    return resize(unsigned(e), s);
  end;
  
  function conv_unsigned(e: boolean; s: natural) return unsigned is
  begin
    if ( e ) then return to_unsigned(1,s); else return to_unsigned(0,s); end if;
  end;
  
  function conv_unsigned(e: integer; s: natural) return unsigned is
  begin
    return to_unsigned(e,s);
  end;

  function conv_unsigned(e: character; s: natural) return unsigned is
  begin
    return to_unsigned(to_integer(e),s);
  end;
  
  function conv_signed(e: signed; s: natural) return signed is
  begin
    return resize(e, s);
  end;
  
  function conv_signed(e: unsigned; s: natural) return signed is
  begin
    return resize(signed('0' & e), s);
  end;
  
  function conv_signed(e: boolean; s: natural) return signed is
  begin
    if ( e ) then return to_signed(1,s); else return to_signed(0,s); end if;
  end;
  
  function conv_signed(e: integer; s: natural) return signed is
  begin
    return to_signed(e,s);
  end;

  function to_integer(e: integer) return integer is
  begin
    return e;
  end;

  function to_integer(e: boolean) return integer is
  begin
    if ( e ) then return 1; else return 0; end if;
  end;

  function to_integer(e: character) return integer is
  begin
      return character'pos(e);
  end;

  procedure notify_ev(signal s: out std_logic; duration: time) is
  begin
     s <= '1'; 
     wait for duration;
     s <= '0'; 
  end;

  -- Debug aux fns 

  function to_string(v : std_logic_vector) return string is
    variable s : string(1 to v'length) := (others => 'x');
    variable c : string(1 to 3);
    variable j : integer := 1;
  begin
      for i in v'high downto v'low loop
        c := std_logic'image(v(i));
        s(j to j) := c(2 to 2);  -- c is a 3 character string (with quotes) ! 
        j := j+1;
      end loop;
    return s;
  end to_string;

  procedure dump_slv(name: string; v: std_logic_vector) is
  begin
    report name & "[" & integer'image(v'high) & ":" & integer'image(v'low) & "]=" & to_string(v);
  end;

end package body;
