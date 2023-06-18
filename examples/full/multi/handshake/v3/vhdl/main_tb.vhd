library ieee;
use ieee.std_logic_1164.all;	   
use ieee.numeric_std.all;

entity main_tb is
end entity;

architecture struct of main_tb is

component main_top is
  port(
        H: in std_logic;
        r_data: out integer;
        e_state: out integer;
        i_state: out integer;
        r_state: out integer;
        rst: in std_logic
        );
end component;

signal H: std_logic;
signal r_data: integer;
signal rst: std_logic;
signal e_state: integer;
signal i_state: integer;
signal r_state: integer;

begin

  inp_H: process
    type t_periodic is record period: time; t1: time; t2: time; end record;
    constant periodic : t_periodic := ( 10 ns, 10 ns, 800 ns );
    variable t : time := 0 ns;
    begin
      H <= '0';
      wait for periodic.t1;
      t := t + periodic.t1;
      while ( t < periodic.t2 ) loop
        H <= '1';
        wait for periodic.period/2;
        H <= '0';
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
    wait for 800 ns;
    wait;
  end process;

  Top: main_top port map(H,r_data,e_state,i_state,r_state,rst);

end architecture;
