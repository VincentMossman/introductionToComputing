with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;

procedure Mileage is

-- This program computes miles per gallon given four amounts
-- for gallons used, and starting and ending mileages

   -- Variables

   Amt1 : Float;      -- Number of gallons for fillup 1
   Amt2 : Float;      -- Number of gallons for fillup 2
   Amt3 : Float;      -- Number of gallons for fillup 3
   Amt4 : Float;      -- Number of gallons for fillup 4

   Start_Miles : Float;  -- Starting mileage
   End_Miles   : Float;  -- Ending mileage

   MPG : Float range 4.0 .. 100.0;              -- Computed miles per gallon

   Mod_Year : Integer range 1910 .. 2013;   -- Model year of car
   Make     : String (1 .. 20);             -- Make of car
   Last_Ch  : Integer range 0 .. 20;            -- Last good character in Make string

begin     -- Mileage

   -- Prompt user for model year of the car
   Ada.Text_IO.Put (Item => "Please enter the model year of your car: ");

   -- Get model year of the car
   Ada.Integer_Text_IO.Get (Item => Mod_Year);

   Ada.Text_IO.Skip_Line;

   -- Prompt user for make of the car
   Ada.Text_IO.Put (Item => "Please enter the make of your car: ");

   -- Get make of the car
   Ada.Text_IO.Get_Line (Item => Make,
                         Last => Last_Ch);

   -- Prompt user for starting mileage.
   Ada.Text_IO.Put (Item => "Please enter the starting mileage: ");

   -- Get starting mileage
   Ada.Float_Text_IO.Get (Item => Start_Miles);

   -- Prompt user for four gallon amounts
   Ada.Text_IO.Put (Item => "Please enter the four gallon amounts (Separate with spaces): ");

   -- Get four gallon amounts
   Ada.Float_Text_IO.Get (Item => Amt1);
   Ada.Float_Text_IO.Get (Item => Amt2);
   Ada.Float_Text_IO.Get (Item => Amt3);
   Ada.Float_Text_IO.Get (Item => Amt4);

   -- Prompt user for ending mileage
   Ada.Text_IO.Put (Item => "Please enter the ending mileage: ");

   -- Get the ending mileage
   Ada.Float_Text_IO.Get (Item => End_Miles);

   Ada.Text_IO.New_Line (Spacing => 3);

   MPG := (End_Miles - Start_Miles) / (Amt1 + Amt2 + Amt3 + Amt4);
   Ada.Text_IO.Put (Item => "For the gallon amounts: ");
   Ada.Text_IO.New_Line (Spacing => 1);
   Ada.Float_Text_IO.Put (Item => Amt1,
                          Fore => 4,
                          Aft  => 1,
                          Exp  => 0);
   Ada.Text_IO.Put (Item => ',');
   Ada.Float_Text_IO.Put (Item => Amt2,
                          Fore => 4,
                          Aft  => 1,
                          Exp  => 0);
   Ada.Text_IO.Put (Item => ',');
   Ada.Float_Text_IO.Put (Item => Amt3,
                          Fore => 4,
                          Aft  => 1,
                          Exp  => 0);
   Ada.Text_IO.Put (Item => ',');
   Ada.Float_Text_IO.Put (Item => Amt4,
                          Fore => 4,
                          Aft  => 1,
                          Exp  => 0);
   Ada.Text_IO.New_Line (Spacing => 2);
   Ada.Text_IO.Put  (Item => "a starting mileage of ");
   Ada.Float_Text_IO.Put (Item => Start_Miles,
                          Fore => 6,
                          Aft  => 1,
                          Exp  => 0);
   Ada.Text_IO.New_Line (Spacing => 1);
   Ada.Text_IO.Put (Item => "and an ending mileage of ");
   Ada.Float_Text_IO.Put (Item => End_Miles,
                          Fore => 6,
                          Aft  => 1,
                          Exp  => 0);
   Ada.Text_IO.New_Line (Spacing => 2);
   Ada.Text_IO.Put (Item => "Your ");
   Ada.Integer_Text_IO.Put (Item => Mod_Year,
                            Width => 4);
   Ada.Text_IO.Put (Item => " ");
   Ada.Text_IO.Put (Item => Make (1 .. Last_Ch));
   Ada.Text_IO.Put (Item => " got ");
   Ada.Float_Text_IO.Put (Item  => MPG,
                          Fore  => 3,
                          Aft   => 1,
                          Exp   => 0);
   Ada.Text_IO.Put (Item => " miles per gallon");
   Ada.Text_IO.New_Line (Spacing => 1);

end Mileage;
