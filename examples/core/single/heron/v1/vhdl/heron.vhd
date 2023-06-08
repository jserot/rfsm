library ieee;
use ieee.std_logic_1164.all;
use work.rfsm.all;

entity heron is
  port(
        h: in std_logic;
        start: in std_logic;
        u: in real;
        rdy: out std_logic;
        r: out real;
        rst: in std_logic
        );
end entity;

architecture RTL of heron is
  type t_state is ( Idle, Iter );
  signal state: t_state;
begin
  process(rst, h)
    variable a: real;
    variable x: real;
    variable n: integer;
  begin
    if ( rst='1' ) then
      state <= Idle;
    elsif rising_edge(h) then 
      case state is
      when Iter =>
        if ( n<8 ) then
          x := (x+(a/x))/2.000000E+00;
          n := n+1;
        elsif  ( n = 8 ) then
          r <= x;
          state <= Idle;
        end if;
      when Idle =>
        if ( start = '1' ) then
          a := u;
          x := u;
          n := 0;
          state <= Iter;
        end if;
    end case;
    end if;
  end process;
  process(state)
  begin
    case state is
    when Idle =>
      rdy <= '1';
    when Iter =>
      rdy <= '0';
    end case;
  end process;
end architecture;
