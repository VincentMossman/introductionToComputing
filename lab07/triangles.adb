with Ada.Text_IO;
with Ada.Integer_Text_IO;
procedure Triangles is

   -- Variables
   Rows       : Integer; -- Number of rows in triangle
   Drawing_Ch : Character;   -- Character in triangle

   -------------------------------------------------------------------------------------

   procedure Display_One_Line (Char_Count : in Integer;      -- Number of chars in a line
                               Draw       : in Character) is -- Character in triangle

      -- Variables
      Count : Integer;   -- Loop control variable

   begin

      -- Display one line
      -- Each iteration, display on character
      Count := 1;
      Display_One_Line :      loop

         exit Display_One_Line when Count > Char_Count;

         Ada.Text_IO.Put (Item => Draw);

         Count := Count + 1;

      end loop Display_One_Line;

   end Display_One_Line;

   -------------------------------------------------------------------------------------

   procedure Get_Valid_Char (Draw : out Character) is   -- Character in triangle

   begin

      -- Gets valid triangle character
      -- Each iteration, checks for valid character
      Get_Valid_Char :
      loop

         Ada.Text_IO.Put (Item => "Please enter '*' or '#' for use in triangle: ");
         Ada.Text_IO.Get (Item => Draw);
         exit Get_Valid_Char when Draw = '*' or Draw = '#';

         Ada.Text_IO.Put (Item => "That choice is not valid.");
         Ada.Text_IO.New_Line;

      end loop Get_Valid_Char;

   end Get_Valid_Char;

   -------------------------------------------------------------------------------------

   procedure Draw_Bottom_Triangle (Rows : in Integer;        -- Rows in triangle
                                   Draw : in Character) is   -- Character in triangle

      -- Variables
      Count : Integer;   -- Loop Control Variable

   begin

      -- Draw bottom triangle
      -- Each iteration, draws bottom triagle
      Count := Rows / 2;
      Draw_Top_Triangle :
      loop

         exit Draw_Top_Triangle when Count < 1;

         Display_One_Line (Char_Count => Count,
                           Draw => Draw);
         Ada.Text_IO.New_Line;

         Count := Count - 1;

      end loop Draw_Top_Triangle;

   end Draw_Bottom_Triangle;

   -------------------------------------------------------------------------------------

   procedure Draw_Top_Triangle (Rows : in Integer;        -- Rows in triangle
                                Draw : in Character) is   -- Character in triangle

      -- Variables
      Count : Integer;   -- Loop Control Variable

   begin

      -- Draw top triangle
      -- Each iteration, draws top triagle
      Count := 1;
      Draw_Top_Triangle :
      loop

         exit Draw_Top_Triangle when Count > Rows / 2 + 1;

         Display_One_Line (Char_Count => Count,
                           Draw => Draw);
         Ada.Text_IO.New_Line;

         Count := Count + 1;

      end loop Draw_Top_Triangle;

   end Draw_Top_Triangle;

   -------------------------------------------------------------------------------------

begin

   -- Draw a number of triangles
   -- Each iteration, draw one triangle
   Triangle_Loop :
   loop

      Ada.Text_IO.Put (Item => "Enter a number of rows for the triangle: ");
      Ada.Integer_Text_IO.Get (Item => Rows);


      exit Triangle_Loop when Rows rem 2 = 0 or Rows < 0;

      Get_Valid_Char (Draw => Drawing_Ch);
      Ada.Text_IO.New_Line;

      -- the number of rows in the top is Rows / 2 + 1
      Draw_Top_Triangle (Rows => Rows, Draw => Drawing_Ch);

      -- the number of rows in the bottom is Rows / 2
      Draw_Bottom_Triangle (Rows => Rows, Draw => Drawing_Ch);

      Ada.Text_IO.New_Line (Spacing => 2);

   end loop Triangle_Loop;

end Triangles;

