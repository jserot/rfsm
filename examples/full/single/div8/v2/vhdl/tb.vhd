library ieee;
use ieee.std_logic_1164.all;	   
library rfsm;
use rfsm.core.all;

entity tb is
end tb;

architecture struct of tb is

component top is
  port(
        N2: in integer range 0 to 255;
        N1: in integer range 0 to 255;
        Start: in std_logic;
        H: in std_logic;
        R: out integer range 0 to 255;
        Rdy: out std_logic;
        rst: in std_logic
        );
end component;

signal N2: integer range 0 to 255;
signal N1: integer range 0 to 255;
signal Start: std_logic;
signal H: std_logic;
signal R: integer range 0 to 255;
signal Rdy: std_logic;
signal rst: std_logic;

begin

  inp_N2: process
    type t_vc is record date: time; val: integer range 0 to 255; end record;
    type t_vcs is array ( 0 to 1 ) of t_vc;
    constant vcs : t_vcs := ( (5 ns,25), (45 ns,0) );
    variable i : natural := 0;
    variable t : time := 0 ns;
    begin
      for i in 0 to 1 loop
        wait for vcs(i).date-t;
        N2 <= vcs(i).val;
        t := vcs(i).date;
      end loop;
      wait;
  end process;
  inp_N1: process
    type t_vc is record date: time; val: integer range 0 to 255; end record;
    type t_vcs is array ( 0 to 1 ) of t_vc;
    constant vcs : t_vcs := ( (5 ns,150), (45 ns,0) );
    variable i : natural := 0;
    variable t : time := 0 ns;
    begin
      for i in 0 to 1 loop
        wait for vcs(i).date-t;
        N1 <= vcs(i).val;
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
    wait for 120 ns;
    wait;
  end process;

  U_Top: top port map(N2,N1,Start,H,R,Rdy,rst);

end architecture;
