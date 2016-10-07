with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
procedure Assign9 is

   -- Author: Vincent T. Mossman
   -- Date:   November 2013
   --
   -- This program displays plots of sine curves
   --
   -- Input from keyboard
   --   Starting angle (in degrees) for the sine plot
   --   Ending angle (in degrees) for the sine plot
   --   Plot width (even whole number between 40 and 120 inclusive)
   --   Plot height (whole number between 20 and 720 inclusive)
   --
   -- Output to screen
   --   Headings with horizontal axis of dashes and labels (-1, 0, and +1)
   --   Vertical axis labels (degrees)
   --   Plot symbols (the * character)
   --
   -- Assumptions
   --   All numbers entered by user are valid

   -- Constants
   Number_Width : constant Ada.Text_IO.Count := 8;

   -- Type and subtype declarations
   type Degree_Type     is digits 12;
   type Radian_Type     is digits 12;
   subtype Width_Range  is Integer range 40 .. 120;
   subtype Height_Range is Integer range 20 .. 720;

   -- Package instantiations
   package Degree_IO        is new Ada.Text_IO.Float_IO (Num => Degree_Type);
   package Radian_Functions is new Ada.Numerics.Generic_Elementary_Functions (Radian_Type);

   ---------------------------------------------------------------------------------------

   procedure Radian_To_Column (Value  : in Radian_Type;             -- Radian value to be converted
                               Width  : in Integer;                 -- Width of graph
                               Column : out Ada.Text_IO.Count) is   -- Column corresponding to radian value

   -- Converts a value in radians to a corresponding column value
   --
   -- Preconditions  : Width must be within Width_Range
   --
   -- Postconditions : The corresponding column value is returned

      -- Use clauses
      use Ada.Text_IO;   -- To eliminate prefixing Count

      -- Variables
      Graph_Zero         : Width_Range;   -- Location of middle of graph
      Radians_Per_Column : Radian_Type;   -- Radians per column

   begin   -- Radian_To_Column

      -- Determine location of middle of graph
      Graph_Zero := Integer (Number_Width) + ((Width / 2) + 1);

      -- Determine radians per column
      Radians_Per_Column := Radian_Type (Width) / 2.0;

      -- Evaluate corresponding column
      Column := Count (Graph_Zero + Integer (Value * Radians_Per_Column));

   end Radian_To_Column;

   ---------------------------------------------------------------------------------------

   function To_Radians (Value : in Degree_Type) return Radian_Type is

   -- Convert a value in degrees to radians
   --
   -- Preconditions  : None
   --
   -- Postconditions : The equivalent number of radians for Value degrees is returned

      -- Constants
      Radians_Per_Degree : constant Radian_Type := Ada.Numerics.Pi / 180.0;   -- Radians per degree

   begin   -- To_Radians

      return Radian_Type (Value) * Radians_Per_Degree;

   end To_Radians;

   ---------------------------------------------------------------------------------------

   procedure Display_Sine_Plot (Start_Angle : in Degree_Type;    -- Start angle of plot
                                End_Angle   : in Degree_Type;    -- End angle of plot
                                Height      : in Integer;        -- Height of plot
                                Width       : in Integer) is     -- Width of plot

   -- Procedure displays sine plot
   --
   -- Preconditions  : Height is a whole number between 20 and 270
   --
   -- Postconditions : Sine plot is displayed on screen

      -- Use clauses
      use Ada.Text_IO;        -- To eliminate prefixing Count
      use Radian_Functions;   -- To make Sin function visible

      -- Variables
      Incrementation : Degree_Type;   -- Increment of angles on plot
      Current_Angle  : Degree_Type;   -- Angle of plot row and LCV
      Radian         : Radian_Type;   -- Radian value of current_angle
      Column         : Count;         -- Corresponding column to Radians value
      Sine_Value     : Radian_Type;   -- Sine value of Radian

   begin

      -- Determine incrementation
      Incrementation := End_Angle / Degree_Type (Height);

      -- Loop displays sine plot
      -- Each iteration, displays one line
      Current_Angle := Start_Angle;   -- Initialize LCV
      Plot_Loop :
      loop
         exit Plot_Loop when Current_Angle > End_Angle;

         Degree_IO.Put (Item => Current_Angle,
                         Fore => 5,
                         Aft  => 1,
                         Exp  => 0);

         -- Convert degrees to radians
         Radian := To_Radians (Current_Angle);

         -- Determine Sine value of Radian
         Sine_Value := Sin (Radian);

         -- Determine corresponding column
         Radian_To_Column (Value =>  Sine_Value,
                           Width =>  Width,
                           Column => Column);

         -- Put *
         Ada.Text_IO.Set_Col (To => Column);
         Ada.Text_IO.Put ('*');

         Ada.Text_IO.New_Line;

         Current_Angle := Current_Angle + Incrementation;
      end loop Plot_Loop;

   end Display_Sine_Plot;

   ---------------------------------------------------------------------------------------

   procedure Put_Headings (Width : in Integer) is

   -- Procedure displays the headings
   --
   -- Preconditions  : Width is even
   --
   -- Postconditions : Headings are displayed with a centered title and
   --                  horizontal axis with labels of -1, 0, and +1

      -- Use clauses
      use Ada.Text_IO;   -- To avoid need to prefix Count

   begin

      -- Display a centered title
      Ada.Text_IO.Set_Col (To => Number_Width + 2 + Count (Width - 15) / 2);
      Ada.Text_IO.Put_Line ("Sine Curve Plot");
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
      Ada.Text_IO.New_Line (Spacing => 2);

   end Put_Headings;

   ---------------------------------------------------------------------------------------

   procedure Get_Height (Height : out Integer) is   -- Height of graph

   -- Procedure gets a valid height value
   --
   -- Preconditions  : None
   --
   -- Postconditions : Returns valid height value

   begin   -- Get_Height

      -- Loop gets valid value for height
      -- Each iteration, checks for valid value
      Width_Validation_Loop :
      loop
         Ada.Text_IO.Put_Line ("Enter the desired height of your plot");
         Ada.Integer_Text_IO.Get (Height);
         exit Width_Validation_Loop when Height in Height_Range;
         Ada.Text_IO.Put_Line ("Invalid height. Please enter a whole number between 20 and 720");
      end loop Width_Validation_Loop;

   end Get_Height;

   ---------------------------------------------------------------------------------------

   procedure Get_Width (Width : out Integer) is   -- Width of graph

   -- Procedure gets a valid width value
   --
   -- Preconditions  : None
   --
   -- Postconditions : Returns valid width value

   begin   -- Get_Width

      -- Loop gets valid value for width
      -- Each iteration, checks for valid value
      Width_Validation_Loop :
      loop
         Ada.Text_IO.Put_Line ("Enter the desired width of your plot");
         Ada.Integer_Text_IO.Get (Width);
         exit Width_Validation_Loop when Width in Width_Range and Width rem 2 = 0;
         Ada.Text_IO.Put_Line ("Invalid width. Please enter an even number between 40 and 120");
      end loop Width_Validation_Loop;

   end Get_Width;

   ---------------------------------------------------------------------------------------

   -- Variables
   Starting_Angle : Degree_Type;    -- Starting angle of graph
   Ending_Angle   : Degree_Type;    -- Ending angle of graph
   Width          : Integer;        -- Width of graph
   Height         : Integer;        -- Height of graph

begin   -- Assign9

   -- Loop creates sine plots
   -- Each iteration, displays one sine plot
   Sine_Loop :
   loop

      -- Prompt for starting and ending angles
      Ada.Text_IO.Put_Line ("Enter the starting and ending angles for your plot");
      Ada.Text_IO.Put_Line ("To end the program, enter a starting value " &
                            "greater than or equal to the ending value");

      -- Get starting angle
      Degree_IO.Get (Starting_Angle);
      -- Get ending angle
      Degree_IO.Get (Ending_Angle);

      Ada.Text_IO.New_Line;

      exit Sine_Loop when Starting_Angle >= Ending_Angle;   -- Test for sentinal

      -- Get plot width
      Get_Width (Width => Width);
      Ada.Text_IO.New_Line;

      -- Get plot height
      Get_Height (Height => Height);
      Ada.Text_IO.New_Line (Spacing => 3);

      -- Display Headings
      Put_Headings (Width => Width);

      -- Display Sine Plot
      Display_Sine_Plot (Start_Angle => Starting_Angle,
                         End_Angle   => Ending_Angle,
                         Height      => Height,
                         Width       => Width);

      Ada.Text_IO.New_Line (Spacing => 4);

   end loop Sine_Loop;

end Assign9;