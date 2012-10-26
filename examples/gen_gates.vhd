entity Gen_Gates is
generic (Delay : in Time := 10 ns);
port (In1, In2 : in Std_Logic;
      Output : out Std_Logic);
end Gen_Gates;

architecture Gates of Gen_Gates is
begin
  . . .
  Output <= In1 or In2 after Delay;
  . . .
end Gates;
