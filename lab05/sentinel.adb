with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Text_IO;
procedure Sentinel is

   -- Counts of the three grades of oranges
   Grade_A_Count : Integer;
   Grade_B_Count : Integer;
   Grade_C_Count : Integer;

   Diameter : Float;  -- The diameter of one orange
   Sum      : Float;  -- Holds the sum of all grapefruit diameters

   -- The data file
   My_File     : Ada.Text_IO.File_Type;
   File_Name   : String (1 .. 100);
   Name_Length : Integer range 0 .. 100;


begin

   Ada.Text_IO.Put ("Enter the name of your data file.");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Get_Line (Item => File_Name,
                         Last => Name_Length);
   Ada.Text_IO.New_Line (Spacing => 3);

   -- Open the data file
   Ada.Text_IO.Open (File => My_File,
                     Mode => Ada.Text_IO.In_File,
                     Name => File_Name (1 .. Name_Length));



   -- Initialize the counters
   Grade_A_Count := 0;
   Grade_B_Count := 0;
   Grade_C_Count := 0;
   Sum := 0.0;


   -- Classify all of the oranges
   -- Each iteration, classify one orange
   Orange_Loop :
   loop
      Ada.Float_Text_IO.Get (File => My_File,
                             Item => Diameter);
      exit Orange_Loop when Diameter <= 0.0;
      if Diameter >= 3.0 then
         Grade_A_Count := Grade_A_Count + 1;
      elsif Diameter >= 2.0 then
         Grade_B_Count := Grade_B_Count + 1;
      else
         Grade_C_Count := Grade_C_Count + 1;
      end if;
      Sum := Sum + Diameter;
   end loop Orange_Loop;


   Ada.Text_IO.Close (File => My_File);

   -- Display the results
   Ada.Text_IO.Put (Item => "Counts of the different grade oranges");
   Ada.Text_IO.New_Line (Spacing => 2);

   Ada.Integer_Text_IO.Put  (Item  => Grade_A_Count,
                             Width => 6);
   Ada.Text_IO.Put (Item => " grade A");
   Ada.Text_IO.New_Line;

   Ada.Integer_Text_IO.Put  (Item  => Grade_B_Count,
                             Width => 6);
   Ada.Text_IO.Put (Item => " grade B");
   Ada.Text_IO.New_Line;

   Ada.Integer_Text_IO.Put  (Item  => Grade_C_Count,
                             Width => 6);
   Ada.Text_IO.Put (Item => " grade C");
   Ada.Text_IO.New_Line (Spacing => 2);


   Ada.Text_IO.Put (Item => "The average diameter is ");
   Ada.Float_Text_IO.Put (Item => Sum / Float (Grade_A_Count + Grade_B_Count + Grade_C_Count),
                          Fore => 1,
                          Aft  => 2,
                          Exp  => 0);
   Ada.Text_IO.New_Line;

end Sentinel;