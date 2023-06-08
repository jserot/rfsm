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
  type t_state is ( E0, E1, E2, E3 );
  signal state: t_state;
begin
  process(rst, h)
  begin
    if ( rst='1' ) then
      state <= E0;
    elsif rising_edge(h) then 
      case state is
      when E3 =>
        state <= E0;
      when E2 =>
        state <= E3;
      when E1 =>
        state <= E2;
      when E0 =>
        if ( e = '1' ) then
          state <= E1;
        end if;
    end case;
    end if;
  end process;
  process(state)
  begin
    case state is
    when E0 =>
      s <= '0';
    when E1 =>
      s <= '1';
    when E2 =>
      s <= '1';
    when E3 =>
      s <= '1';
    end case;
  end process;
end architecture;
