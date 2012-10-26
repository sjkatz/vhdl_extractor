-- Name: A Package Declaration 
-- The program is written by Chia-Tien Dan Lo. Date: 3/1/98 
-- This is a feature of 1993 VHDL version. 
-- The user defined constants, data types, functions and components are illustrated. 
-- The package file must exist in the working directory. However, the real entity of components can be 
-- put on a different directory for maintenance purposes.
library ieee; 
use ieee.std_logic_1164.all; 
package dan_pack is

 -- constant definitions 
 constant n : integer := 16;

 -- user defined data types 
 type s_array is array (integer range <>) of std_logic_vector(15 downto 0); 
 type s32_array is array (integer range <>) of std_logic_vector(31 downto 0); 
 type std_bus4 is array (integer range 3 downto 0) of std_logic; 
 type std_bus5 is array (integer range 4 downto 0) of std_logic; 
 type std_bus8 is array (integer range 7 downto 0) of std_logic; 
 type std_bus16 is array (integer range 15 downto 0) of std_logic; 
 type std_bus32 is array (integer range 31 downto 0) of std_logic; 
 type std_bus64 is array (integer range 63 downto 0) of std_logic;

 -- mux1 
 component mux1 
  port(a,b,sel: in bit; y: out bit); 
 end component;

 function f_cout(a,b,cin: in std_logic) 
  return std_logic; 
 component fadd_fcn 
  port(a,b,cin: in std_logic; sum, cout: out std_logic); 
 end component;

 component nibble1 
  port(a,b,gl,eq,ll: in bit; 
  aglb, aeqb, allb: out bit); 
 end component;

 -- n-bit adder using generic 
 component adder_n 
 generic (N: integer := 8); 
  port(a,b : in std_logic_vector(n-1 downto 0); 
   cin : in std_logic; 
   sum : out std_logic_vector(n-1 downto 0); 
   cout : out std_logic); 
 end component;

 -- 32-bit register 
 component i_reg 
  port( 
   clock: in std_logic; 
   d_in : in std_logic_vector(31 downto 0); 
   d_out: out std_logic_vector(31 downto 0) 
   ); 
 end component;

 -- clock generator 
 component clock_gen 
 port( 
  clock: in std_logic; 
  reset: in std_logic; 
  clk_a: out std_logic; 
  clk_b: out std_logic; 
  clk_c: out std_logic 
  ); 
 end component; 
  
 -- program counter 
 component pc 
 port( 
  clock: in std_logic; 
  wea: in std_logic; 
  wvalue: in std_logic_vector(4 downto 0); 
  rout : out std_logic_vector(4 downto 0) 
  ); 
 end component;

 -- ram 20 x 32 
 component ram20x32 
 port( 
  clock : in std_logic; 
  rw_sel: in std_logic; 
  wea: in std_logic; 
  wm_add: in std_logic_vector(4 downto 0); 
  wr_add: in std_logic_vector(4 downto 0); 
  wvalue: in std_logic_vector(31 downto 0); 
  rout : out std_logic_vector(31 downto 0) 
  ); 
 end component;

 -- decoder 16-bit version 
 component mips_dec16 
 port( 
  IR : in std_logic_vector(31 downto 0); 
  reg_w : out std_logic; -- write enable for register files 
  wr_add : out std_logic_vector(4 downto 0); -- provide register file writing address 
  rs : out std_logic_vector(4 downto 0); -- provide register file rs address 
  rt : out std_logic_vector(4 downto 0); -- provide register file rt address 
  ri : out std_logic_vector(15 downto 0); -- provide immediate addressing value 
  funct : out std_logic_vector(2 downto 0); -- provide ALU the op code 
  aluSrc : out std_logic; -- '1' I-type, '0' R-type 
  slrl : out std_logic; -- '1' shift right 
  shift: out std_logic; -- shift command 
  shamt : out std_logic_vector(4 downto 0); 
  lw : out std_logic; 
  sw : out std_logic; 
  beq : out std_logic; 
  bne : out std_logic; 
  j : out std_logic; 
  jr : out std_logic; 
  jal : out std_logic 
  );

 end component; 
 -- register file 
 component reg8x16 
  port( 
   clock: in std_logic; 
   wea: in std_logic; 
   wr_add: in std_logic_vector(4 downto 0); 
   wvalue: in std_logic_vector(n-1 downto 0); 
   rout : out s_array(0 to 7) 
   ); 
 end component;

 -- multiplex 
 component mux8x16 
  port( 
   a: in s_array(0 to 7); 
   sel: in std_logic_vector(4 downto 0); 
   b: out std_logic_vector(n-1 downto 0) 
   ); 
 end component; 
  
 -- ALU 16-bit version 
 component alun 
 port(a, b : in std_logic_vector(15 downto 0); 
   result : out std_logic_vector(15 downto 0); 
   zero, overflow, carryout : out std_logic; 
   control: in std_logic_vector(2 downto 0) 
  );

 end component;

 -- shifter 
 component shrla 
 port(a: in std_logic_vector(n-1 downto 0); 
  ctrl: in std_logic_vector(4 downto 0); 
  sr  : in std_logic; -- sr ='1' shift right 
  result: out std_logic_vector(n-1 downto 0) 
  ); 
 end component; 
end dan_pack;

-- functions can be defined here 
package body dan_pack is 
 function f_cout(a,b,cin: in std_logic) 
  return std_logic is 
 begin 
  return (a and b) or (a and cin) or (b and cin); 
 end f_cout; 
end dan_pack;
