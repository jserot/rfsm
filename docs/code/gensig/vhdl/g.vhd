library ieee;
use ieee.std_logic_1164.all;
use work.rfsm.all;

entity G is
  port(
        H: in std_logic;
        E: in std_logic;
        S: out std_logic;
        rst: in std_logic
        );
end entity;

architecture RTL of G is
  type t_state is ( E0, E1 );
  signal state: t_state;
begin
  process(rst, H)
  variable k: integer;
  begin
    if ( rst='1' ) then
      state <= E0;
      S <= '0';
    elsif rising_edge(H) then 
      case state is
      when E0 =>
        if ( E = '1' ) then
          S <= '1';
          k := 1;
          state <= E1;
        end if;
      when E1 =>
        if ( k = 3 ) then
          S <= '0';
          state <= E0;
        elsif  ( k<3 ) then
          S <= '1';
          k := k+1;
        end if;
    end case;
    end if;
  end process;
end architecture;

