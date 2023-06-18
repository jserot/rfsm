library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity r is
  port(
        h: in std_logic;
        e_rdy: in std_logic;
        data: in integer;
        r_rdy: out std_logic;
        dout: out integer;
        rst: in std_logic;
        st: out integer
        );
end entity;

architecture RTL of r is
  type t_state is ( R1, R2 );
  signal state: t_state;
begin
  process(rst, h)
    variable t: integer;
  begin
    if ( rst='1' ) then
      state <= R1;
    elsif rising_edge(h) then 
      case state is
      when R2 =>
        if ( e_rdy = '0' ) then
          state <= R1;
        end if;
      when R1 =>
        if ( e_rdy = '1' ) then
          dout <= data;
          state <= R2;
        end if;
    end case;
    end if;
  end process;
  process(state)
  begin
    case state is
    when R1 =>
      r_rdy <= '1';
    when R2 =>
      r_rdy <= '0';
    end case;
  end process;
  st <= 0 when state=R1 else 1 when state=R2;
end architecture;
