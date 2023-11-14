library ieee;
use ieee.std_logic_1164.all;	   
use ieee.numeric_std.all;

entity tb is
end tb;

architecture Bench of tb is

component syracuse 
  port(
        in H: std_logic;
        in Start: boolean;
        in u0: unsigned(7 downto 0);
       out Rdy: boolean;
       out N: unsigned(7 downto 0);
        in rst: std_logic
        );
end component;

  signal t_N: unsigned(7 downto 0);
  signal t_Rdy: boolean;
  signal t_u0: unsigned(7 downto 0);
  signal t_Start: boolean;
  signal t_H: std_logic;
  signal t_rst: std_logic;

begin

  U0: syracuse
    port map(t_H,t_Start,t_u0,t_Rdy,t_N,t_rst);

  process

  begin
  wait for 201 ns;
  H <= 0;
  wait for -1 ns;
  H <= 1;
    
    wait;

  end process;
end Bench;
