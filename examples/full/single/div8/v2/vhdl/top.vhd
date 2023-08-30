library ieee;
use ieee.std_logic_1164.all;	   
library rfsm;
use rfsm.core.all;

entity top is
  port(
        N2: in integer range 0 to 255;
        N1: in integer range 0 to 255;
        Start: in std_logic;
        H: in std_logic;
        R: out integer range 0 to 255;
        Rdy: out std_logic;
        rst: in std_logic
        );
end entity;

architecture struct of top is

component d8 
  port(
        h: in std_logic;
        start: in std_logic;
        n1: in integer range 0 to 255;
        n2: in integer range 0 to 255;
        rdy: out std_logic;
        r: out integer range 0 to 255;
        rst: in std_logic
        );
end component;


begin
  U_D8: D8 port map(H,Start,N1,N2,Rdy,R,rst);
end architecture;
