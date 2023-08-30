library ieee;
use ieee.std_logic_1164.all;
library rfsm;
use rfsm.core.all;

entity d8 is
  port(
        h: in std_logic;
        start: in std_logic;
        n1: in integer range 0 to 255;
        n2: in integer range 0 to 255;
        rdy: out std_logic;
        r: out integer range 0 to 255;
        rst: in std_logic
        );
end d8;

architecture RTL of d8 is
  type t_state is ( Calcul, Repos );
  signal state: t_state;
begin
  process(rst, h)
    variable a: integer range 0 to 65535;
    variable b: integer range 0 to 65535;
    variable c: integer range 0 to 255;
    variable i: integer range 0 to 8;
  begin
    if ( rst='1' ) then
      state <= Repos;
      rdy <= '1';
    elsif rising_edge(h) then 
      case state is
      when Repos =>
        if ( start = '1' ) then
          a := n1;
          b := n2*128;
          c := 0;
          rdy <= '0';
          i := 0;
          state <= Calcul;
        end if;
      when Calcul =>
        if ( i<8 and a<b ) then
          c := c*2;
          i := i+1;
          b := b/2;
        elsif  ( i<8 and a>=b ) then
          a := a-b;
          c := (c*2)+1;
          i := i+1;
          b := b/2;
        elsif  ( i = 8 ) then
          r <= c;
          rdy <= '1';
          state <= Repos;
        end if;
    end case;
    end if;
  end process;
end RTL;
