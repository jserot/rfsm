library ieee;
use ieee.std_logic_1164.all;	   

entity main_top is
  port(
        H: in std_logic;
        E: in std_logic;
        S: out std_logic;
        rst: in std_logic        );
end entity;

architecture struct of main_top is

component G 
  port(
        H: in std_logic;
        E: in std_logic;
        S: out std_logic;
        rst: in std_logic
        );
end component;


begin
  G0: G port map(H,E,S,rst);
end architecture;

