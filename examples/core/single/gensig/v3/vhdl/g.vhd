library ieee;
use ieee.std_logic_1164.all;
use work.rfsm.all;

entity g is
  port(
        h: in std_logic;
        e: in std_logic;
        s: out std_logic;
        rst: in std_logic
        );
end entity;

architecture RTL of g is
  type t_state is ( E0, E1 );
  signal state: t_state;
begin
  process(rst, h)
    variable k: integer range 0 to 4;
  begin
    if ( rst='1' ) then
      state <= E0;
      s <= '0';
    elsif rising_edge(h) then 
      case state is
      when E1 =>
        if ( k<4 ) then
          k := k+1;
        elsif  ( k = 4 ) then
          s <= '0';
          state <= E0;
        end if;
      when E0 =>
        if ( e = '1' ) then
          k := 1;
          s <= '1';
          state <= E1;
        end if;
    end case;
    end if;
  end process;
  process(state)
  begin
    case state is
    when E0 =>
    when E1 =>
    end case;
  end process;
end architecture;
