with Ada.Text_IO;
with Ada.Integer_Text_IO;

-- This program displays the age of a person in a certain year


procedure Age is

   -- Variables
   BirthYear   :   Integer range 1900 .. 2013;   -- Birth Year of person in question
   FutureYear  :   Integer range 2013 .. 2200;   -- Year in the future
   FirstName   :   String (1 .. 20);             -- A first name
   First_Last  :   Integer range 0 .. 20;        -- Last good character in first name
   LastName    :   String (1 .. 20);             -- A last name
   Last_Last   :   Integer range 0 .. 20;                   -- Last good character in last name
   FutureAge   :   Integer range 1 .. 200;       -- Future age of person

begin

   Ada.Text_IO.Put (Item => "Please enter a birth year: ");   -- Prompt for birth year
   Ada.Integer_Text_IO.Get (Item => BirthYear);               -- Get birth year

   Ada.Text_IO.Put (Item => "Please enter a year in the future: ");   -- Prompt for year in the future
   Ada.Integer_Text_IO.Get (Item => FutureYear);                      -- Get year in the future
   Ada.Text_IO.Skip_Line;

   Ada.Text_IO.Put (Item => "Please enter first name: ");   -- Prompt for first name
   Ada.Text_IO.Get_Line (Item => FirstName,
                         Last => First_Last);               -- Get first name

   Ada.Text_IO.Put (Item => "Please enter last name: ");    -- Prompt for last name
   Ada.Text_IO.Get_Line (Item => LastName,
                         Last => Last_Last);                -- Get last name

   Ada.Text_IO.New_Line (Spacing => 3);

   -- Calculate information
   FutureAge := FutureYear - BirthYear;

   -- Display calculated information
   Ada.Text_IO.Put (Item => LastName (1 .. Last_Last));
   Ada.Text_IO.Put (Item => ", ");
   Ada.Text_IO.Put (Item => FirstName (1 .. First_Last));
   Ada.Text_IO.Put (Item => " will be ");
   Ada.Integer_Text_IO.Put (Item => FutureAge,
                            Width => 3);
   Ada.Text_IO.Put (Item => " years old in ");
   Ada.Integer_Text_IO.Put (Item => FutureYear,
                            Width => 4);

end Age;
   