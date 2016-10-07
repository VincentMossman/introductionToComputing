with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Sequential_IO;
procedure Display is

   -- Program displays contents of Pasta.Dat

   -- Types and Subtypes
   subtype Warehouse_Pounds is Float range 0.0 .. 20_000.0;  -- Ten ton maximum

   -- Package Instantiations
   package Seq_Pounds_IO is new Ada.Sequential_IO (Element_Type => Warehouse_Pounds);

   -- Constants
   File_Name : constant String := "Pasta.Dat";

   -- Variables
   Value       : Warehouse_Pounds;          -- Value read from file
   Pasta_File  : Seq_Pounds_IO.File_Type;   -- File to be read

begin   -- Display

   -- Prepare file
   Seq_Pounds_IO.Open (File => Pasta_File,
                       Mode => Seq_Pounds_IO.In_File,
                       Name => File_Name);

   -- Loop reads Pasta data from a file
   -- Each iteration, displays one pasta
   Pasta_Loop :
   loop
      exit Pasta_Loop when Seq_Pounds_IO.End_Of_File (Pasta_File);
      Seq_Pounds_IO.Read (File => Pasta_File,
                           Item => Value);
      Ada.Float_Text_IO.Put (Item => Value,
                     Fore => 6,
                     Aft  => 1,
                     Exp  => 0);
      Ada.Text_IO.New_Line;
   end loop Pasta_Loop;
   Seq_Pounds_IO.Close (Pasta_File);

end Display;