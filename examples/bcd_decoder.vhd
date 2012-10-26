library IEEE;
use IEEE.std_logic_1164.all;
entity BCD_Decoder is
port (
   BCD : in Bit_Vector (2 downto 0);
   Enable : in Bit;
   LED : out Std_Ulogic_Vector (3 downto 0));
constant ZERO : Std_Ulogic_Vector(3 downto 0) := "0000";
begin
assert (BCD /= "111") report "BCD = 7 " severity note;
end entity BCD_Decoder;
