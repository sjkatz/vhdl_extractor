-- Name: Clock generator which generates exact number of clocks 
-- This code was written by Chia-Tien Dan Lo. 
-- Copyright (c) 1999 by Chia-Tien Dan Lo. 
-- Feb. 18th, 1999 
-- Purpose : generate exact number of clocks with perfect waveform 
-- After reset signal, it will output exact number of clock through clk_out 
-- The clk_out counts are specified by num_clk. 
-- Check waveform here 
library ieee; 
use ieee.std_logic_1164.all; 
USE ieee.std_logic_arith.all;
entity clock_cnt is 
 port( 
  clock: in std_logic; 
  reset: in std_logic; 
  num_clk: in integer range 0 to 15; 
  clk_out: out std_logic 
  ); 
end clock_cnt; 
architecture flow of clock_cnt is 
 -- This num_reg will be used to infer a 4-bit register. 
 signal num_reg : integer range 0 to 15; 
begin 
 -- process p1 infers a 4-bit register as a counter 
 p1: process(clock, reset) 
 begin 
  if reset = '1' then 
   -- reset is used to set the register content 
   num_reg <= num_clk; 
  elsif clock'event and clock = '0' then 
   -- decrement counter by 1 at the falling edge of system clock 
   if num_reg /= 0 then 
    num_reg <= num_reg - 1; 
   end if; 
  end if; 
 end process; 
  
 -- generate clk_out if counter is greater than 0 
 clk_out <= clock and not reset when num_reg /= 0 else 
       '0'; 
end flow; 
