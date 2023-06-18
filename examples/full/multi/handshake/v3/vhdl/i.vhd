library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity i is
  port(
        h: in std_logic;
        e_rdy1: in std_logic;
        din: in integer;
        r_rdy2: in std_logic;
        r_rdy1: out std_logic;
        dout: out integer;
        e_rdy2: out std_logic;
        rst: in std_logic;
        st: out integer
        );
end entity;

architecture RTL of i is
  type t_state is ( E1, E2, R1, R2, U );
  signal state: t_state;
begin
  process(rst, h)
    variable data: integer;
    variable t: integer;
  begin
    if ( rst='1' ) then
      state <= R1;
      r_rdy1 <= '1';
      e_rdy2 <= '0';
    elsif rising_edge(h) then 
      case state is
      when U =>
        if ( t<5 ) then
          t := t+1;
        elsif  ( t = 5 ) then
          state <= E1;
        end if;
      when R2 =>
        if ( e_rdy1 = '0' ) then
          r_rdy1 <= '1';
          t := 0;
          state <= U;
        end if;
      when R1 =>
        if ( e_rdy1 = '1' ) then
          data := din;
          r_rdy1 <= '0';
          state <= R2;
        end if;
      when E2 =>
        if ( r_rdy2 = '0' ) then
          e_rdy2 <= '0';
          state <= R1;
        end if;
      when E1 =>
        if ( r_rdy2 = '1' ) then
          dout <= data;
          e_rdy2 <= '1';
          state <= E2;
        end if;
    end case;
    end if;
  end process;
  process(state)
  begin
    case state is
    when E1 =>
    when E2 =>
    when R1 =>
    when R2 =>
    when U =>
    end case;
  end process;
  st <= 0 when state=E1 else 1 when state=E2 else 2 when state=R1 else 3 when state=R2 else 4 when state=U;
end architecture;
