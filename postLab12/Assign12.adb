with Ada.Float_Text_IO;
with Ada.Text_IO;
procedure Assign12 is

   -- Author: Vincent T. Mossman
   -- Date:   December 2013
   --
   -- This program performs the basic matrix operations of addition, subtraction, and multiplication
   -- All operations are performed on square matrices (number of rows equals number of columns)
   --
   -- Input from keyboard
   --    Matrix size
   --    Two Matrices
   --
   -- Output to screen
   --    The two input matrices
   --    Sum of the two input matrices
   --    Difference of the two input matrices
   --    Product of the two input matrices
   --
   -- Assumptions
   --    All input values are valid
   --    The correct number of values are entered

   type Row_Index is range 1 .. Integer'Last;
   type Col_Index is range 1 .. Integer'Last;

   -- I/O Packages for index types
   package Row_Index_IO is new Ada.Text_IO.Integer_IO (Num => Row_Index);
   use Row_Index_IO;

   package Column_Index_IO is new Ada.Text_IO.Integer_IO (Num => Col_Index);
   use Column_Index_IO;

   type Unconstrained_Matrix_Type is array (Row_Index range <>, Col_Index range <>) of Float;

   ----------------------------------------------------------------------------

   function "+" (Left  : in Unconstrained_Matrix_Type;
                 Right : in Unconstrained_Matrix_Type) return Unconstrained_Matrix_Type is

   -- This function adds two matrices
   --
   -- Preconditions  : Left'Range(1) is the same as Right'Range(1)
   --                  Left'Range(2) is the same as Right'Range(2)
   --
   -- Postconditions : The sum of Left and Right is returned

      subtype Matrix_Type is Unconstrained_Matrix_Type (Left'Range (1), Left'Range (2));
      Result : Matrix_Type;

   begin   -- "+"

      -- Loop performs matrix addition
      -- Each iteration, calculates one row
      Row_Addition_Loop :
      for Row in Result'Range (1) loop
         -- Loop performs patrix addition
         -- Each iteration, calculates one value
         Col_Addition_Loop :
         for Col in Result'Range (2) loop
            Result (Row, Col) := Left (Row, Col) + Right (Row, Col);
         end loop Col_Addition_Loop;
      end loop Row_Addition_Loop;

      return Result;

   end "+";

   ----------------------------------------------------------------------------

   function "-" (Left  : in Unconstrained_Matrix_Type;
                 Right : in Unconstrained_Matrix_Type) return Unconstrained_Matrix_Type is

   -- This function subtracts two matrices
   --
   -- Preconditions  : Left'Range(1) is the same as Right'Range(1)
   --                  Left'Range(2) is the same as Right'Range(2)
   --
   -- Postconditions : the difference of Left and Right is returned

      subtype Matrix_Type is Unconstrained_Matrix_Type (Left'Range (1), Left'Range (2));
      Result : Matrix_Type;

   begin   -- "-"

      -- Loop performs matrix subtraction
      -- Each iteration, calculates one row
      Row_Subtraction_Loop :
      for Row in Result'Range (1) loop
         -- Loop performs patrix subtraction
         -- Each iteration, calculates one value
         Col_Subtraction_Loop :
         for Col in Result'Range (2) loop
            Result (Row, Col) := Left (Row, Col) - Right (Row, Col);
         end loop Col_Subtraction_Loop;
      end loop Row_Subtraction_Loop;

      return Result;

   end "-";

   ----------------------------------------------------------------------------

   function "*" (Left  : in Unconstrained_Matrix_Type;
                 Right : in Unconstrained_Matrix_Type) return Unconstrained_Matrix_Type is

   -- This function multiplies two matrices
   --
   -- Preconditions  : Left'Range(2) is the same as Right'Range(1)
   --
   -- Postconditions : The product of Left and Right is returned

      subtype Matrix_Type is Unconstrained_Matrix_Type (Left'Range (1), Right'Range (2));
      Result      : Matrix_Type;
      Dot_Product : Float;

   begin   -- "*"

      -- Loop performs matrix multiplication
      -- Each iteration, calculates one row
      Row_Multiplication_Loop :
      for Row in Left'Range (1) loop
         -- Loop performs matrix multiplication
         -- Each iteration, calculates one value
         Col_Multiplication_Loop :
         for Col in Right'Range (2) loop
            Dot_Product := 0.0;
            -- Loop calculates dot product for each value
            -- Each iteration, calculates corresponding dot product
            Dot_Product_Loop :
            for I in Left'Range (2) loop
               Dot_Product := Dot_Product + Left (Row, Col_Index (I)) * Right (Row_Index (I), Col);
            end loop Dot_Product_Loop;
            Result (Row, Col) := Dot_Product;
         end loop Col_Multiplication_Loop;
      end loop Row_Multiplication_Loop;

      return Result;

   end "*";


   ----------------------------------------------------------------------------

   procedure Put (Item : in Unconstrained_Matrix_Type;
                  Fore : in Positive := 4;
                  Aft  : in Positive := 1;
                  Exp  : in Natural  := 0) is

   -- Procedure displays contents of a matrix
   --
   -- Preconditions  : None
   --
   -- Postconditions : Matrix is displayed on screen

   begin   -- Put
      -- Loop displays matrix
      -- Each iteration, displays one row
      Row_Display_Loop :
      for Row in Item'Range (1) loop
         -- Loop displays matrix
         -- Each iteration, displays one item
         Col_Display_Loop :
         for Col in Item'Range (2) loop
            Ada.Float_Text_IO.Put (Item => Item (Row, Col),
                                   Fore => Fore,
                                   Aft  => Aft,
                                   Exp  => Exp);
         end loop Col_Display_Loop;
         Ada.Text_IO.New_Line;
      end loop Row_Display_Loop;

   end Put;

   ----------------------------------------------------------------------------

   procedure Get (Item : out Unconstrained_Matrix_Type) is

   -- Procedure gets values of a matrix
   --
   -- Preconditions  : None
   --
   -- Postconditions : Passes out matrix

   begin   -- Get

      -- Loop gets values of a matrix
      -- Each iteration, gets one row of values
      Row_Get_Loop :
      for Row in Item'Range (1) loop
         -- Loop gets values of a matrix
         -- Each iteration, gets one value
         Col_Get_Loop :
         for Col in Item'Range (2) loop
            Ada.Float_Text_IO.Get (Item => Item (Row, Col));
         end loop Col_Get_Loop;
      end loop Row_Get_Loop;

   end Get;

   ----------------------------------------------------------------------------

   procedure Do_Matrix_Arithmetic (Rows_In_A : in Row_Index;
                                   Cols_In_A : in Col_Index;
                                   Rows_In_B : in Row_Index;
                                   Cols_In_B : in Col_Index) is

   -- Procedure does matrix arithmetic
   --
   -- Preconditions  : None
   --
   -- Postconditions : Validates size of matrices and performs matrix arithmetic

      subtype A_Matrix_Type is Unconstrained_Matrix_Type (1 .. Rows_In_A, 1 .. Cols_In_A);
      subtype B_Matrix_Type is Unconstrained_Matrix_Type (1 .. Rows_In_B, 1 .. Cols_In_B);

      A : A_Matrix_Type;
      B : B_Matrix_Type;

   begin   -- Do_Matrix_Arithmetic

      Ada.Text_IO.Put_Line ("Enter the values for matrix A");
      Ada.Text_IO.New_Line;
      Get (A);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Enter the values for matrix B");
      Ada.Text_IO.New_Line;
      Get (B);
      Ada.Text_IO.New_Line (2);

      Ada.Text_IO.Put_Line ("Matrix A is");
      Ada.Text_IO.New_Line;
      Put (A);
      Ada.Text_IO.New_Line (2);

      Ada.Text_IO.Put_Line ("Matrix B is");
      Ada.Text_IO.New_Line;
      Put (B);
      Ada.Text_IO.New_Line (2);

      if A'Last (1) = B'Last (1) and A'Last (2) = B'Last (2) then
         Ada.Text_IO.Put_Line ("A + B is");
         Ada.Text_IO.New_Line;
         Put (A + B);
         Ada.Text_IO.New_Line (2);

         Ada.Text_IO.Put_Line ("A - B is");
         Ada.Text_IO.New_Line;
         Put (A - B);
      else
         Ada.Text_IO.Put_Line ("Matrices are of wrong size to add or subtract");
      end if;
      Ada.Text_IO.New_Line (2);

      if Row_Index (A'Last (2)) = B'Last (1) then -- need type conversion to compare
         Ada.Text_IO.Put_Line ("A * B is");
         Ada.Text_IO.New_Line;
         Put (A * B);
      else
         Ada.Text_IO.Put_Line ("Matrices are of wrong size to multiply");
      end if;
      Ada.Text_IO.New_Line (2);

   end Do_Matrix_Arithmetic;

   ----------------------------------------------------------------------------

   -- The sizes of the two matrices
   Rows_In_First     : Row_Index;
   Columns_In_First  : Col_Index;
   Rows_In_Second    : Row_Index;
   Columns_In_Second : Col_Index;

begin   -- Assign12
   Ada.Text_IO.Put_Line ("Enter the number of rows and columns in your first matrix");
   Get (Rows_In_First);
   Get (Columns_In_First);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Enter the number of rows and columns in your second matrix");
   Get (Rows_In_Second);
   Get (Columns_In_Second);
   Ada.Text_IO.New_Line;
   Do_Matrix_Arithmetic (Rows_In_A => Rows_In_First,
                         Cols_In_A => Columns_In_First,
                         Rows_In_B => Rows_In_Second,
                         Cols_In_B => Columns_In_Second);
end Assign12;