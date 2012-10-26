-- Name: Clock generator which generates 3 clocks with 3-alternate-cycle sequences 
-- This code was written by Chia-Tien Dan Lo. 
-- April 28th, 1998 
-- Purpose : generate 3 clock sequences 
-- After reset signal, these clocks function like:  b -> c -> a -> b -> c ... 
library ieee; 
use ieee.std_logic_1164.all; 
USE ieee.std_logic_arith.all;
entity clock_gen is 
 port( 
  clock: in std_logic; 
  reset: in std_logic; 
  clk_a: out std_logic; 
  clk_b: out std_logic; 
  clk_c: out std_logic 
  ); 
end clock_gen; 
architecture flow of clock_gen is 
 signal u_temp : std_logic_vector(2 downto 0); 
begin 
  
 p1: process(clock, reset) 
 begin 
  if reset = '1' then 
   u_temp <= "100"; 
  elsif clock'event and clock = '1' then 
   u_temp <= u_temp(0) & u_temp(2) & u_temp(1); 
  end if; 
 end process; 
 clk_a <= u_temp(2); 
 clk_b <= u_temp(1); 
 clk_c <= u_temp(0);

end flow; 
