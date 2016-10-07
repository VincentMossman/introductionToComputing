with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Inlab12 is

-- The purpose of this program is to give students practice
-- with 2-dimensional array processing
--
-- Written by John McCormick, November 2007

   subtype Element_Type is Integer range 10 .. 99;

   type Row_Index is range 1 .. Integer'Last;
   type Col_Index is range 1 .. Integer'Last;

   type Element_Array is array (Row_Index range <>, Col_Index range <>) of Element_Type;

   -- Create a package with operations for producing random numbers in Element_Type'Range
   package Element_Random is new
                          Ada.Numerics.Discrete_Random (Result_Subtype => Element_Type);


   ----------------------------------------------------------------------------
   function Value_Count (Table : in Element_Array;
                         Value : in Element_Type) return Natural is
   -- Returns the number of times that Value occurs in Table
      Result : Natural;
   begin

      Result := 0;   -- Initialize value

      -- Search My_Table for smallest value
      -- Each iteration, search one row
      for Row in Table'Range (1) loop

         -- Search one row for smallest value
         -- Each iteration, search one value
         for Col in Table'Range (2) loop
            if Table (Row, Col) = Value then
               Result := Result + 1;
            end if;
         end loop;
      end loop;

      return Result;
   end Value_Count;


   ----------------------------------------------------------------------------
   function Most_Often (Table : in Element_Array) return Element_Type is
   -- Returns the element that occurs most often in Table
   -- Precondition : Table contains at least one value
      Times      : Natural;       -- The number of times some value occurs in Table
      Result     : Element_Type;  -- The element that occurs most often in Table
      Most_Times : Natural;       -- The number of times that Result occurs in Table
   begin

      Result     := Element_Type'First;   -- Initialize values
      Most_Times := Value_Count (Table => Table,
                                 Value => Element_Type'First);

      -- Loop determines most frequent number and how many times it occurs
      -- Each iteration, evaluates one value
      for Element in Element_Type loop
         Times := Value_Count (Table => Table,
                               Value => Element);
         if Times > Most_Times then
            Most_Times := Times;
            Result     := Element;
         end if;
      end loop;
      return Result;
   end Most_Often;

   ----------------------------------------------------------------------------
   function Max_Value (Table : in Element_Array) return Element_Type is
   -- Returns the largest value in Table
   -- Precondition : Table contains at least one value
      Result : Element_Type;
   begin

      Result := Table (Table'First (1), Table'First (2));   -- Initialize variable

      -- Search My_Table for smallest value
      -- Each iteration, search one row
      for Row in Table'Range (1) loop

         -- Search one row for smallest value
         -- Each iteration, search one value
         for Col in Table'Range (2) loop
            if Table (Row, Col) > Result then
               Result := Table (Row, Col);
            end if;
         end loop;
      end loop;

      return Result;
   end Max_Value;

   ----------------------------------------------------------------------------
   function Min_Value (Table : in Element_Array) return Element_Type is
   -- Returns the smallest value in Table
   -- Precondition : Table contains at least one value
      Result : Element_Type;
   begin

      Result := Table (Table'First (1), Table'First (2));   -- Initialize variable

      -- Search My_Table for smallest value
      -- Each iteration, search one row
      for Row in Table'Range (1) loop

         -- Search one row for smallest value
         -- Each iteration, search one value
         for Col in Table'Range (2) loop
            if Table (Row, Col) < Result then
               Result := Table (Row, Col);
            end if;
         end loop;
      end loop;

      return Result;

   end Min_Value;


   ----------------------------------------------------------------------------
   procedure Display_Table (Table : in Element_Array) is
   -- Displays Table
   begin

      -- Display My_Table of random element values
      -- Each iteration, Display one row
      for Row in Table'Range (1) loop

         -- Display one row of random element values
         -- Each iteration, display one value
         for Col in Table'Range (2) loop
            Ada.Integer_Text_IO.Put (Item  => Table (Row, Col),
                                     Width => 3);
         end loop;
         Ada.Text_IO.New_Line;
      end loop;

   end Display_Table;

   ----------------------------------------------------------------------------
   procedure Array_Practice (Rows    : in Row_Index;
                             Columns : in Col_Index) is

   -- A procedure to give students practice with a two-dimensional array

      subtype Table_Type is Element_Array (5 .. Rows + 4, 7 .. Columns + 6);

      My_Table : Table_Type;
      Value    : Natural;

      -- The following object (variable) generates random elements
      My_Generator : Element_Random.Generator;

   begin
      -- Give a seed to the random number generator so that it produces the same sequence
      -- of random numbers each run.  This is useful for debugging.  If you want a
      -- different set of random numbers each run, omit the Initiator parameter.  Without
      -- that parameter, the generator will take its seed from the system clock.
      Element_Random.Reset (Gen       => My_Generator,
                            Initiator => 50614);

      -- Fill My_Table with random element values
      -- Each iteration, fill one row with random element values
      for Row in My_Table'Range (1) loop

         -- Fill in one row with random element values
         -- Each iteration, fill in one value
         for Col in My_Table'Range (2) loop
            My_Table (Row, Col) := Element_Random.Random (My_Generator);
         end loop;

      end loop;

      Ada.Text_IO.New_Line (2);
      Ada.Text_IO.Put_Line ("The table is");
      Ada.Text_IO.New_Line;
      Display_Table (My_Table);

      Ada.Text_IO.New_Line (3);
      Ada.Text_IO.Put ("The largest value in the table is ");
      Ada.Integer_Text_IO.Put (Item  => Max_Value (My_Table),
                               Width => 1);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("The smallest value in the table is ");
      Ada.Integer_Text_IO.Put (Item  => Min_Value (My_Table),
                               Width => 1);
      Ada.Text_IO.New_Line (3);

      -- Uncomment the remaining lines in this procedure for the questions on value counting

      -- Carry out all the value counts desired
      -- Each iteration, display a count of one value
      loop
         Ada.Text_IO.Put_Line ("Enter a value to count (enter less than 10 to exit)");
         Ada.Integer_Text_IO.Get (Value);
         exit when Value < 10;
         Ada.Text_IO.Put ("Your value occurs ");
         Ada.Integer_Text_IO.Put (Item  => Value_Count (Table => My_Table,
                                                        Value => Value),
                                  Width => 1);
         Ada.Text_IO.Put_Line (" times in the table");
         Ada.Text_IO.New_Line;
      end loop;


      Ada.Text_IO.New_Line (3);
      Value := Most_Often (My_Table);
      Ada.Text_IO.Put ("The value ");
      Ada.Integer_Text_IO.Put (Item => Value,
                               Width => 1);
      Ada.Text_IO.Put (" occurs most often (");
      Ada.Integer_Text_IO.Put (Item => Value_Count (Table => My_Table,
                                                    Value => Value),
                               Width => 1);
      Ada.Text_IO.Put_Line (" times)");



   end Array_Practice;

-------------------------------------------------------------------------------

   Rows    : Positive;
   Columns : Positive;

begin
   Ada.Text_IO.Put_Line ("How many rows do you want in your table?");
   Ada.Integer_Text_IO.Get (Rows);
   Ada.Text_IO.Put_Line ("How many columns do you want in your table?");
   Ada.Integer_Text_IO.Get (Columns);

   Array_Practice (Rows    => Row_Index (Rows),
                   Columns => Col_Index (Columns));

end Inlab12;
