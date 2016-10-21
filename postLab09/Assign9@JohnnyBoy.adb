with Ada.Numerics;                                -- For the value of Pi
with Ada.Numerics.Generic_Elementary_Functions;   -- For the Sin function
with Ada.Text_IO;
with Ada.Integer_Text_IO;
procedure Assign9 is











   -- The width of the degree values displayed on the left (includes blank afterward)
   -- Assumes degree values are displayed with Fore => 5, Aft => 1, and Exp => 0
   Number_Width : constant Ada.Text_IO.Count := 8;


   package Functions is new Ada.Numerics.Generic_Elementary_Functions ( -- Fill this in with your own type -- );
   use Functions;  -- Use resources from this package without prefixing




   ----------------------------------------------------------------------------
   function To_Radians (Value : in Degrees) return Radians is
   -- Convert a value in degrees to radians
   -- Preconditions  : None
   -- Postconditions : The equivalent number of radians for Value degrees is returned

      Radians_Per_Degree : constant Radians := Ada.Numerics.Pi / 180.0;

   begin
      -- First convert the type from Degrees to Radians and
      -- then multiply to make the unit conversion
      return Radians (Value) * Radians_Per_Degree;
   end To_Radians;




   ----------------------------------------------------------------------------
   procedure Put_Headings (Width : in                 ) is
      use Ada.Text_IO;  -- So we don't have to prefix Count with Ada.Text_IO

   -- Display the headings
   -- Preconditions  : Width is even (so 0 will be centered correctly)
   -- Postconditions : Headings are displayed with a centered title and
   --                  horizontal axis with labels of -1, 0, and +1
   --

   begin

      -- Display a centered title

      -- Notice the type conversion from integer to type Ada.Text_IO.Count
      Ada.Text_IO.Set_Col (To => Number_Width + 2 + Count (Width - 15) / 2);
      Ada.Text_IO.Put_Line ("Sine Curve Plot");  -- This string has 15 characters
      Ada.Text_IO.New_Line;

      -- Display the labels

      Ada.Text_IO.Set_Col (To => Number_Width);
      Ada.Text_IO.Put ("-1");
      Ada.Text_IO.Set_Col (To => Number_Width + Count (Width) / 2 + 1);
      Ada.Text_IO.Put ('0');
      Ada.Text_IO.Set_Col (To => Number_Width + Count (Width));
      Ada.Text_IO.Put_Line ("+1");

      -- Display a line of dashes (for columns 0 to Width)
      -- Each iteration, display one dash
      Ada.Text_IO.Set_Col (To => Number_Width + 1);
      for Count in 0 .. Width loop
         Ada.Text_IO.Put ('-');
      end loop;
      Ada.Text_IO.New_Line (2);

   end Put_Headings;






Ada.Text_IO.Put_Line ("Enter the desired height of your plot");



Ada.Text_IO.Put ("Invalid height  Please enter a value between ");


Ada.Text_IO.Put (" and ");






Ada.Text_IO.Put_Line ("Enter the desired width of your plot");



Ada.Text_IO.Put ("Invalid width  Please enter an even value between ");


Ada.Text_IO.Put (" and ");




Ada.Text_IO.Put_Line ("Enter the starting and ending angles for your plot");
Ada.Text_IO.Put_Line ("To end the program, enter a starting value " &
                      "greater than or equal to the ending value");

Ada.Text_IO.New_Line (3);  -- Put some blanks lines after the graph




end Assign9;