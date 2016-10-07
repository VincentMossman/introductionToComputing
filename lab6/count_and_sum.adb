with Ada.Integer_Text_IO;
with Ada.Text_IO;
procedure Count_And_Sum is

   N        : Integer range 0 .. Integer'Last;   -- The number of integers to process
   Count    : Integer range 0 .. Integer'Last;   -- Loop Control Variable
   Temp     : Integer range 0 .. Integer'Last;   -- Temporary variable used before designation
   Odd_Num  : Integer range 0 .. Integer'Last;   -- Number of odd integers
   Even_Sum : Integer range 0 .. Integer'Last;   -- Sum of even integers

begin
   Ada.Text_IO.Put (Item => "Enter N and then N integers");
   Ada.Text_IO.New_Line;
   Ada.Integer_Text_IO.Get (Item => N);

   Count    := 1;   -- Initialize LCV
   Even_Sum := 0;   -- Initialize even sum
   Odd_Num  := 0;   -- Initialize odd num

   -- Determines number of odd numbers and sum of even numbers
   -- Each iteration, determines if odd or even and increments necessary variables
   GetInt_Loop :
   loop
      exit GetInt_Loop when Count > N;   -- Test the LCV

      Ada.Text_IO.Put (Item => "Enter integer: ");
      Ada.Integer_Text_IO.Get (Item => Temp);

      if Temp rem 2 = 0 then
         Even_Sum := Even_Sum + Temp;   -- Update the even sum
      else
         Odd_Num := Odd_Num + 1;        -- Update the odd num
      end if;

      Count := Count + 1;   -- Update the LCV

   end loop GetInt_Loop;


   -- Display the results
   Ada.Text_IO.New_Line (Spacing => 2);
   Ada.Text_IO.Put (Item => "The number of odd numbers is ");
   Ada.Integer_Text_IO.Put (Item => Odd_Num,
                            Width => 1);
   Ada.Text_IO.New_Line (Spacing => 2);
   Ada.Text_IO.Put (Item => "The sum of the even numbers is ");
   Ada.Integer_Text_IO.Put (Item => Even_Sum,
                            Width => 1);
   Ada.Text_IO.New_Line (Spacing => 2);
   Ada.Text_IO.Put (Item => "All done");

end Count_And_Sum;

   