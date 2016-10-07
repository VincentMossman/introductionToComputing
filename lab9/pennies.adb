with Ada.Text_IO;
procedure Pennies is

   type Decimal is delta 0.01 digits 8;
   Three_Pennies : constant Decimal := 0.03;
   Total_Dollars : Decimal;

   package Decimal_IO is new Ada.Text_IO.Decimal_IO (Decimal);


begin
   Total_Dollars := 0.0;                 -- Initialize the sum
   for Count in 1 .. 100_000 loop
      Total_Dollars := Total_Dollars + Three_Pennies;
   end loop;

   Decimal_IO.Put (Item => Total_Dollars,
                          Fore => 10,
                          Aft  => 2,
                          Exp  => 0);
   Ada.Text_IO.New_Line;

end Pennies;
      