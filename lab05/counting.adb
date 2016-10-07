with Ada.Integer_Text_IO;
with Ada.Text_IO;
procedure Counting is

   Start  : Integer;
   Finish : Integer;
   Count  : Integer;  -- Loop control variable
   Count_By : Integer;

begin

   -- Get the data

   Ada.Text_IO.Put (Item => "Start? ");
   Ada.Integer_Text_IO.Get (Item => Start);

   Ada.Text_IO.Put (Item => "Finish? ");
   Ada.Integer_Text_IO.Get (Item => Finish);

   Ada.Text_IO.Put (Item => "What should the program count by? ");
   Ada.Integer_Text_IO.Get (Item => Count_By);
   Ada.Text_IO.New_Line (Spacing => 2);

   -- This loop displays the numbers from Start to Finish
   -- Each iteration, displays next number based on Count_By variable

   Count := Start;   -- Initialize the loop control variable
   Count_Loop :
   loop
      exit Count_Loop when Count > Finish;   -- Test the loop control variable
      Ada.Integer_Text_IO.Put (Item => Count);
      Ada.Text_IO.New_Line;
      Count := Count + Count_By;   -- Increment the loop control variable
   end loop Count_Loop;



   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "The numbers above should count by ");
   Ada.Integer_Text_IO.Put (Item  => Count_By,
                            Width => 1);
   Ada.Text_IO.Put (Item => "'s from ");
   Ada.Integer_Text_IO.Put (Item => Start,
                            Width => 1);
   Ada.Text_IO.Put (Item => " to ");
   Ada.Integer_Text_IO.Put (Item => Finish,
                            Width => 1);
   Ada.Text_IO.New_Line (Spacing => 2);
   Ada.Text_IO.Put (Item => "All done");
end Counting;

   