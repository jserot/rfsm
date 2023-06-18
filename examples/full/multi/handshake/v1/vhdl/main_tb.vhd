library ieee;
use ieee.std_logic_1164.all;	   
use ieee.numeric_std.all;

entity main_tb is
end entity;

architecture struct of main_tb is

component main_top is
  port(
        Hr: in std_logic;
        He: in std_logic;
        r_data: out integer;
        e_state: out integer;
        r_state: out integer;
        rst: in std_logic
        );
end component;

signal Hr: std_logic;
signal He: std_logic;
signal r_data: integer;
signal rst: std_logic;
signal e_state: integer;
signal r_state: integer;

begin

  inp_Hr: process
    type t_periodic is record period: time; t1: time; t2: time; end record;
    constant periodic : t_periodic := ( 4 ns, 15 ns, 300 ns );
    variable t : time := 0 ns;
    begin
      Hr <= '0';
      wait for periodic.t1;
      t := t + periodic.t1;
      while ( t < periodic.t2 ) loop
        Hr <= '1';
        wait for periodic.period/2;
        Hr <= '0';
        wait for periodic.period/2;
        t := t + periodic.period;
      end loop;
      wait;
  end process;
  inp_He: process
    type t_periodic is record period: time; t1: time; t2: time; end record;
    constant periodic : t_periodic := ( 10 ns, 10 ns, 300 ns );
    variable t : time := 0 ns;
    begin
      He <= '0';
      wait for periodic.t1;
      t := t + periodic.t1;
      while ( t < periodic.t2 ) loop
        He <= '1';
        wait for periodic.period/2;
        He <= '0';
        wait for periodic.period/2;
        t := t + periodic.period;
      end loop;
      wait;
  end process;
  reset: process
  begin
    rst <= '1';
    wait for 1 ns;
    rst <= '0';
    wait for 300 ns;
    wait;
  end process;

  Top: main_top port map(Hr,He,r_data,e_state,r_state,rst);

end architecture;
