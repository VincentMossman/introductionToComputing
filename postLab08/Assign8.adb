with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
procedure Assign8 is

   -- Author: Vincent T. Mossman
   -- Date:   October 2013
   --
   -- This program classifies grapefruits received by a grocery wholesaler
   --
   -- Input from keyboard
   --   Grapefruit diameter (float)     (e.g. 4.5)
   --   Grapefruit condition (string)   (e.g. Excellent)
   --
   -- Output to screen
   --   Prompts
   --   Grapefruit Classification Totals table
   --   Grapefruit Classification Results graph
   --
   -- Assumptions
   --   All data entered by user is valid

   -- Types for grapefruit classification
   type Grade_Type     is (A, B, C, D, E, R);
   type Condition_Type is (Excellent, Good, Fair, Poor);

   -- I/O Packages for enumeration types
   package Grade_IO     is new Ada.Text_IO.Enumeration_IO (Enum => Grade_Type);
   package Condition_IO is new Ada.Text_IO.Enumeration_IO (Enum => Condition_Type);

   -------------------------------------------------------------------------------------------

   procedure Display_Row (Grade : in Grade_Type;   -- Grade type
                          Total : in Natural;      -- Total count of grapefruit grade
                          X_Val : in Natural) is   -- Value of each X

      -- Procedure displays classification results as a graph
      --
      -- Preconditions  : None
      --
      -- Postconditions : Displays proper amount of Xs

   begin -- Display_Row

      Grade_IO.Put (Item => Grade);
      Ada.Text_IO.Set_Col (To => 5);
      -- Loop displays proper amount of Xs for each grade
      -- Each iteration, puts one X
      X_Loop :
      for Count in 1 .. Natural (Float (Total) / Float (X_Val)) loop
         Ada.Text_IO.Put (Item => 'X');
      end loop X_Loop;

   end Display_Row;

   -------------------------------------------------------------------------------------------

   procedure Display_Classification_Results (Grade_A_Total  : in Natural;    -- Number of Grade A grapefruit
                                             Grade_B_Total  : in Natural;    -- Number of Grade B grapefruit
                                             Grade_C_Total  : in Natural;    -- Number of Grade C grapefruit
                                             Grade_D_Total  : in Natural;    -- Number of Grade D grapefruit
                                             Grade_E_Total  : in Natural;    -- Number of Grade E grapefruit
                                             Rejected_Total : in Natural) is -- Number of Rejected grapefruit

   -- Procedure displays classification results as a graph
   --
   -- Preconditions  : None
   --
   -- Postconditions : Graph of totals is displayed neatly

      -- Variables
      X_Val     : Integer;   -- Value of each X
      Max_Count : Integer;   -- Maximum count of each grapefruit

   begin -- Display_Classification_Results

      -- Determine Max_Count
      Max_Count := Integer'Max (Grade_A_Total, Grade_B_Total);
      Max_Count := Integer'Max (Max_Count, Grade_C_Total);
      Max_Count := Integer'Max (Max_Count, Grade_D_Total);
      Max_Count := Integer'Max (Max_Count, Grade_E_Total);
      Max_Count := Integer'Max (Max_Count, Rejected_Total);
      Max_Count := Integer'Max (Max_Count, 1);

      -- Determine Value of each X
      X_Val := Integer (Float'Ceiling (Float (Max_Count) / 20.0));

      -- Display graph headers
      Ada.Text_IO.Put (Item => "Grapefruit Classification Results (Each X represents ");
      Ada.Integer_Text_IO.Put (Item  => X_Val,
                               Width => 1);
      Ada.Text_IO.Put (Item => " grapefruit)");
      Ada.Text_IO.New_Line (Spacing => 2);

      -- Display graph

      -- Display A
      Display_Row (Grade => A,
                   Total => Grade_A_Total,
                   X_Val => X_Val);
      Ada.Text_IO.New_Line;

      -- Display B
      Display_Row (Grade => B,
                   Total => Grade_B_Total,
                   X_Val => X_Val);
      Ada.Text_IO.New_Line;

      -- Display C
      Display_Row (Grade => C,
                   Total => Grade_C_Total,
                   X_Val => X_Val);
      Ada.Text_IO.New_Line;

      -- Display D
      Display_Row (Grade => D,
                   Total => Grade_D_Total,
                   X_Val => X_Val);
      Ada.Text_IO.New_Line;

      -- Display E
      Display_Row (Grade => E,
                   Total => Grade_E_Total,
                   X_Val => X_Val);
      Ada.Text_IO.New_Line;

      -- Display R
      Display_Row (Grade => R,
                   Total => Rejected_Total,
                   X_Val => X_Val);
      Ada.Text_IO.New_Line;

   end Display_Classification_Results;

   -------------------------------------------------------------------------------------------

   procedure Display_Classification_Totals (Grade_A_Total  : in Natural;    -- Number of Grade A grapefruit
                                            Grade_B_Total  : in Natural;    -- Number of Grade B grapefruit
                                            Grade_C_Total  : in Natural;    -- Number of Grade C grapefruit
                                            Grade_D_Total  : in Natural;    -- Number of Grade D grapefruit
                                            Grade_E_Total  : in Natural;    -- Number of Grade E grapefruit
                                            Rejected_Total : in Natural) is -- Number of rejected grapefruit

   -- Procedure displays classification totals
   --
   -- Preconditions  : None
   --
   -- Postconditions : Totals are displayed neatly

   begin -- Display_Classification_Totals

      -- Display Classification Table
      Ada.Text_IO.Put (Item => "Grapefruit Classification Totals");
      Ada.Text_IO.New_Line (Spacing => 2);
      Ada.Text_IO.Put (Item => "Grade");
      Ada.Text_IO.Set_Col (To => 10);
      Ada.Text_IO.Put (Item => "Total");
      Ada.Text_IO.New_Line (Spacing => 2);
      Grade_IO.Put (Item => A);
      Ada.Text_IO.Set_Col (To => 10);
      Ada.Integer_Text_IO.Put (Item  => Grade_A_Total,
                               Width => 5);
      Ada.Text_IO.New_Line;
      Grade_IO.Put (Item => B);
      Ada.Text_IO.Set_Col (To => 10);
      Ada.Integer_Text_IO.Put (Item  => Grade_B_Total,
                               Width => 5);
      Ada.Text_IO.New_Line;
      Grade_IO.Put (Item => C);
      Ada.Text_IO.Set_Col (To => 10);
      Ada.Integer_Text_IO.Put (Item  => Grade_C_Total,
                               Width => 5);
      Ada.Text_IO.New_Line;
      Grade_IO.Put (Item => D);
      Ada.Text_IO.Set_Col (To => 10);
      Ada.Integer_Text_IO.Put (Item  => Grade_D_Total,
                               Width => 5);
      Ada.Text_IO.New_Line;
      Grade_IO.Put (Item => E);
      Ada.Text_IO.Set_Col (To => 10);
      Ada.Integer_Text_IO.Put (Item  => Grade_E_Total,
                               Width => 5);
      Ada.Text_IO.New_Line;
      Grade_IO.Put (Item => R);
      Ada.Text_IO.Set_Col (To => 10);
      Ada.Integer_Text_IO.Put (Item  => Rejected_Total,
                               Width => 5);
      Ada.Text_IO.New_Line;

   end Display_Classification_Totals;

   -------------------------------------------------------------------------------------------

   function Classify_Grapefruit (Diameter  : in Float;
                                 Condition : in Condition_Type) return Grade_Type is

   -- Given diameter and condition, determines proper grapefruit grade
   --
   -- Preconditions  : Diameter is valid, Condition is valid
   --
   -- Postconditions : Proper grade is returned

      -- Variables
      Grade : Grade_Type;   -- Grade of grapefruit

   begin   -- Classify_Grapefruit

      if Diameter >= 4.25 then
         Grade := A;
      elsif Diameter >= 4.0 then
         Grade := B;
      elsif Diameter >= 3.75 then
         Grade := C;
      elsif Diameter >= 3.5 then
         Grade := D;
      elsif Diameter < 3.5 then
         Grade := E;
      end if;

      -- Loop downgrades grapefruit properly
      -- Each iteration, downgrades one grade
      Downgrade :
      for Count in 1 .. Condition_Type'Pos (Condition) loop
         Grade := Grade_Type'Succ (Grade);
      end loop Downgrade;

      return Grade;

   exception
      when CONSTRAINT_ERROR =>
         return R;

   end Classify_Grapefruit;

   -------------------------------------------------------------------------------------------

   -- Variables
   Diameter       : Float;            -- Diameter of each grapefruit
   Condition      : Condition_Type;   -- Condition of each grapefruit
   Grade          : Grade_Type;       -- Class of each grapefruit
   Grade_A_Total  : Natural := 0;     -- Total number of Grade A grapefruit
   Grade_B_Total  : Natural := 0;     -- Total number of Grade B grapefruit
   Grade_C_Total  : Natural := 0;     -- Total number of Grade C grapefruit
   Grade_D_Total  : Natural := 0;     -- Total number of Grade D grapefruit
   Grade_E_Total  : Natural := 0;     -- Total number of Grade E grapefruit
   Rejected_Total : Natural := 0;     -- Total number of Rejected grapefruit

begin   -- Assign8

   -- Prompt for grapefruit information
   Ada.Text_IO.Put (Item => "Enter grapefruit information, one grapefruit per line. <Diameter> <Condition>");
   Ada.Text_IO.New_Line;

   -- Loop gets and classifies grapefruit
   -- Each iteration, classifies one grapefruit
   Process_Grapefruit :
   loop

      -- Get grapefruit information
      Ada.Float_Text_IO.Get (Item => Diameter);
      exit Process_Grapefruit when Diameter <= 0.0;
      Condition_IO.Get (Item => Condition);

      -- Classify grapefruit
      Grade := Classify_Grapefruit (Diameter  => Diameter,
                                    Condition => Condition);

      -- Update proper count
      case Grade is
         when A =>
            Grade_A_Total := Grade_A_Total + 1;
         when B =>
            Grade_B_Total := Grade_B_Total + 1;
         when C =>
            Grade_C_Total := Grade_C_Total + 1;
         when D =>
            Grade_D_Total := Grade_D_Total + 1;
         when E =>
            Grade_E_Total := Grade_E_Total + 1;
         when R =>
            Rejected_Total := Rejected_Total + 1;
      end case;

   end loop Process_Grapefruit;

   Ada.Text_IO.New_Line (Spacing => 2);

   -- Display Grapefruit Classification Totals table
   Display_Classification_Totals (Grade_A_Total  => Grade_A_Total,
                                  Grade_B_Total  => Grade_B_Total,
                                  Grade_C_Total  => Grade_C_Total,
                                  Grade_D_Total  => Grade_D_Total,
                                  Grade_E_Total  => Grade_E_Total,
                                  Rejected_Total => Rejected_Total);

   Ada.Text_IO.New_Line (Spacing => 4);

   -- Display Grapefruit Classification Results graph
   Display_Classification_Results (Grade_A_Total  => Grade_A_Total,
                                   Grade_B_Total  => Grade_B_Total,
                                   Grade_C_Total  => Grade_C_Total,
                                   Grade_D_Total  => Grade_D_Total,
                                   Grade_E_Total  => Grade_E_Total,
                                   Rejected_Total => Rejected_Total);

end Assign8;
   