library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity e is
  port(
        h: in std_logic;
        r_rdy: in std_logic;
        data: out integer;
        e_rdy: out std_logic;
        rst: in std_logic;
        st: out integer
        );
end entity;

architecture RTL of e is
  type t_state is ( E1, E2 );
  signal state: t_state;
begin
  process(rst, h)
    variable cnt: integer;
  begin
    if ( rst='1' ) then
      state <= E1;
      cnt := 1;
    elsif rising_edge(h) then 
      case state is
      when E2 =>
        if ( r_rdy = '0' ) then
          state <= E1;
        end if;
      when E1 =>
        if ( r_rdy = '1' ) then
          data <= cnt;
          cnt := cnt+1;
          state <= E2;
        end if;
    end case;
    end if;
  end process;
  process(state)
  begin
    case state is
    when E1 =>
      e_rdy <= '0';
    when E2 =>
      e_rdy <= '1';
    end case;
  end process;
  st <= 0 when state=E1 else 1 when state=E2;
end architecture;
