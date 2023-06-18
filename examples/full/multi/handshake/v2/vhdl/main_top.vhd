library ieee;
use ieee.std_logic_1164.all;	   
use ieee.numeric_std.all;

entity main_top is
  port(
        H: in std_logic;
        r_data: out integer;
        e_state: out integer;
        r_state: out integer;
        rst: in std_logic
        );
end entity;

architecture struct of main_top is

component e 
  port(
        h: in std_logic;
        r_rdy: in std_logic;
        data: out integer;
        e_rdy: out std_logic;
        rst: in std_logic;
        st: out integer
        );
end component;
component r 
  port(
        h: in std_logic;
        e_rdy: in std_logic;
        data: in integer;
        r_rdy: out std_logic;
        dout: out integer;
        rst: in std_logic;
        st: out integer
        );
end component;

signal e_rdy: std_logic;
signal v: integer;
signal r_rdy: std_logic;

begin
  e1: e port map(H,r_rdy,v,e_rdy,rst,e_state);
  r2: r port map(H,e_rdy,v,r_rdy,r_data,rst,r_state);
end architecture;
