library ieee;
use ieee.std_logic_1164.all;	   

entity main_tb is
end entity;

architecture struct of main_tb is

component main_top is
  port(
        U: in real;
        Start: in std_logic;
        H: in std_logic;
        R: out real;
        Rdy: out std_logic;
        rst: in std_logic
        );
end component;

signal U: real;
signal Start: std_logic;
signal H: std_logic;
signal R: real;
signal Rdy: std_logic;
signal rst: std_logic;

begin

  inp_U: process
    type t_vc is record date: time; val: real; end record;
    type t_vcs is array ( 0 to 0 ) of t_vc;
    constant vcs : t_vcs := ( others => (5 ns,2.000000E+00) );
    variable i : natural := 0;
    variable t : time := 0 ns;
    begin
      for i in 0 to 0 loop
        wait for vcs(i).date-t;
        U <= vcs(i).val;
        t := vcs(i).date;
      end loop;
      wait;
  end process;
  inp_Start: process
    type t_vc is record date: time; val: std_logic; end record;
    type t_vcs is array ( 0 to 2 ) of t_vc;
    constant vcs : t_vcs := ( (0 ns,'0'), (25 ns,'1'), (35 ns,'0') );
    variable i : natural := 0;
    variable t : time := 0 ns;
    begin
      for i in 0 to 2 loop
        wait for vcs(i).date-t;
        Start <= vcs(i).val;
        t := vcs(i).date;
      end loop;
      wait;
  end process;
  inp_H: process
    type t_periodic is record period: time; t1: time; t2: time; end record;
    constant periodic : t_periodic := ( 10 ns, 10 ns, 200 ns );
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
    wait for 200 ns;
    wait;
  end process;

  Top: main_top port map(U,Start,H,R,Rdy,rst);

end architecture;
