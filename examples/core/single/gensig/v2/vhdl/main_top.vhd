library ieee;
use ieee.std_logic_1164.all;	   

entity main_top is
  port(
        E: in std_logic;
        H: in std_logic;
        S: out std_logic;
        rst: in std_logic
        );
end entity;

architecture struct of main_top is

component g 
  port(
        h: in std_logic;
        e: in std_logic;
        s: out std_logic;
        rst: in std_logic
        );
end component;


begin
  g1: g port map(H,E,S,rst);
end architecture;
