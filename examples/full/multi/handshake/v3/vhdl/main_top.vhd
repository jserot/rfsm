library ieee;
use ieee.std_logic_1164.all;	   
use ieee.numeric_std.all;

entity main_top is
  port(
        H: in std_logic;
        r_data: out integer;
        e_state: out integer;
        i_state: out integer;
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
component i 
  port(
        h: in std_logic;
        e_rdy1: in std_logic;
        din: in integer;
        r_rdy2: in std_logic;
        r_rdy1: out std_logic;
        dout: out integer;
        e_rdy2: out std_logic;
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

signal e_rdy2: std_logic;
signal v2: integer;
signal r_rdy2: std_logic;
signal e_rdy1: std_logic;
signal v1: integer;
signal r_rdy1: std_logic;

begin
  e1: e port map(H,r_rdy1,v1,e_rdy1,rst,e_state);
  i2: i port map(H,e_rdy1,v1,r_rdy2,r_rdy1,v2,e_rdy2,rst,i_state);
  r3: r port map(H,e_rdy2,v2,r_rdy2,r_data,rst,r_state);
end architecture;
