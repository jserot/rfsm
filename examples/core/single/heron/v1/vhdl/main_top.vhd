library ieee;
use ieee.std_logic_1164.all;	   

entity main_top is
  port(
        U: in real;
        Start: in std_logic;
        H: in std_logic;
        R: out real;
        Rdy: out std_logic;
        rst: in std_logic
        );
end entity;

architecture struct of main_top is

component heron 
  port(
        h: in std_logic;
        start: in std_logic;
        u: in real;
        rdy: out std_logic;
        r: out real;
        rst: in std_logic
        );
end component;


begin
  heron1: heron port map(H,Start,U,Rdy,R,rst);
end architecture;
