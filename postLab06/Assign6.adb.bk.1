with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Text_IO;
with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
procedure Assign6 is

   -- A program to determine the distances flown by an eagle fitted with a GPS transmitter
   --
   -- Started by John McCormick, February 2013
   --
   -- Completed by Vincent T. Mossman, October 2013
   --
   -- Input from the keyboard
   --   The latitude and longiture of the eagle's birthplace
   --       in degrees (e.g. 43.271 -91.78)
   --   Name of a data file containing location data
   --
   -- Input from the data file
   --    The number of lines in the file (a positive integer)
   --    Each line of data contains four pieces of information from one GPS transmission.
   --       The date in the form Year Month Day       (e.g. 2012 05 03)
   --       The 24 hour time in the form Hour Minute  (e.g. 19 05)
   --       The latitude in degrees                   (e.g. 46.514)
   --       The longitude in degrees                  (e.g. -90.321)
   --
   --
   -- Output to screen
   --   For each line of the data file
   --      The date and time (e.g. May 3, 2012  7:05PM)
   --      The distance flown from the previous GPS transmission
   --      The distance from the eagle's nest
   --
   -- Assumptions
   --    The data file name contains no more than 80 characters
   --    The data file name entered by the user exists
   --    All the data in the data file is formatted as described above
   --    The user types in numbers for the latitude and longitude of the nest

   Earth_Mean_Radius   : constant Float := 3_958.761;   -- miles
   Earth_Circumference : constant Float := Pi * 2.0 * Earth_Mean_Radius;
   Radians_Per_Degree  : constant Float := Pi / 180.0;


   -----------------------------------------------------------------------------
   function Distance (From_Latitude  : in Float;  -- all values in radians
                      From_Longitude : in Float;
                      To_Latitude    : in Float;
                      To_Longitude   : in Float) return Float is
   -- Calculate the distance between two locations using the haversine formula

      Delta_Latitude  : Float;
      Delta_Longitude : Float;
      A  : Float;
      C  : Float;

   begin

      Delta_Latitude  := abs (To_Latitude - From_Latitude);
      Delta_Longitude := abs (To_Longitude - From_Longitude);
      A := (sin (Delta_Latitude / 2.0)) ** 2.0 + cos (From_Latitude) * cos (To_Latitude) *
         (sin (Delta_Longitude / 2.0)) ** 2.0;
      C := 2.0 * arctan (sqrt (A), sqrt (1.0 - A));

      return Earth_Mean_Radius * C;
   end Distance;


   ----------------------------------------------------------------------------
   procedure Get_Nest_Location (Latitude  : out Float;
                                Longitude : out Float) is
   -- Obtain the location of the Eagle's nest in degrees, return the results in radians
   begin

         Ada.Text_IO.Put ("Enter the latitude of the Eagle's nest in degrees:   ");
         -- Gets latitude
         -- Each iteration, check for valid entry
         Get_Latitude :
         loop
            Ada.Float_Text_IO.Get (Item => Latitude);
            exit Get_Latitude when Latitude > -90.0 and Latitude < 90.0;
            Ada.Text_IO.Put (Item => "Please enter a valid latitude (between -90 and 90): ");
         end loop Get_Latitude;

         Ada.Text_IO.Put ("Enter the longitude of the Eagle's nest in degrees:  ");
         -- Gets longitude
         -- Each iteration, check for valid entry
         Get_Longitude :
         loop
            Ada.Float_Text_IO.Get (Item => Longitude);
            exit Get_Longitude when Longitude > -180.0 and Longitude < 180.0;
            Ada.Text_IO.Put (Item => "Please enter a valid longitude (between -180 and 180): ");
         end loop Get_Longitude;

      -- Convert the angles from degrees to radians
      Latitude  := Latitude  * Radians_Per_Degree;
      Longitude := Longitude * Radians_Per_Degree;
   end Get_Nest_Location;


   ----------------------------------------------------------------------------
   procedure Get_Location (Data      : in out Ada.Text_IO.File_Type;
                           Latitude  :    out Float;
                           Longitude :    out Float) is
   -- Obtain a location from file Data in degrees, return the results in radians
   begin
      Ada.Float_Text_IO.Get (File => Data,
                             Item => Latitude);
      Ada.Float_Text_IO.Get (File => Data,
                             Item => Longitude);
      -- Convert the angles from degrees to radians
      Latitude  := Latitude  * Radians_Per_Degree;
      Longitude := Longitude * Radians_Per_Degree;
   end Get_Location;



   ----------------------------------------------------------------------------
   procedure Get_Date (Data  : in out Ada.Text_IO.File_Type;
                       Year  :    out  Integer;
                       Month :    out  Integer;
                       Day   :    out  Integer) is
   -- This procedure gets a date from file Data.
   -- Assumption - the date in the file is formatted as 2012 05 03

   begin
      -- Get year
      Ada.Integer_Text_IO.Get (File => Data,
                               Item => Year);

      -- Get month
      Ada.Integer_Text_IO.Get (File => Data,
                               Item => Month);

      -- Get day
      Ada.Integer_Text_IO.Get (File => Data,
                               Item => Day);

   end Get_Date;


   ----------------------------------------------------------------------------
   procedure Put_Date (Year  : in Integer;
                       Month : in Integer;
                       Day   : in Integer) is
   -- This procedure displays a date in the form May 3, 2012
   begin

      -- Display the name of the month
      if Month = 1 then
         Ada.Text_IO.Put (Item => "January");
      elsif Month = 2 then
         Ada.Text_IO.Put (Item => "February");
      elsif Month = 3 then
         Ada.Text_IO.Put (Item => "March");
      elsif Month = 4 then
         Ada.Text_IO.Put (Item => "April");
      elsif Month = 5 then
         Ada.Text_IO.Put (Item => "May");
      elsif Month = 6 then
         Ada.Text_IO.Put (Item => "June");
      elsif Month = 7 then
         Ada.Text_IO.Put (Item => "July");
      elsif Month = 8 then
         Ada.Text_IO.Put (Item => "August");
      elsif Month = 9 then
         Ada.Text_IO.Put (Item => "September");
      elsif Month = 10 then
         Ada.Text_IO.Put (Item => "October");
      elsif Month = 11 then
         Ada.Text_IO.Put (Item => "November");
      else
         Ada.Text_IO.Put (Item => "December");
      end if;

      -- Display the day of the month
      Ada.Integer_Text_IO.Put (Item  => Day,
                               Width => 3);
      Ada.Text_IO.Put (", ");

      -- Display the year
      Ada.Integer_Text_IO.Put (Item  => Year,
                               Width => 1);
   end Put_Date;


   ----------------------------------------------------------------------------
   procedure Get_Time (Data   : in out Ada.Text_IO.File_Type;
                       Hour   :    out Integer;
                       Minute :    out Integer) is
   -- This procedure gets a time from file Data.
   -- Assumption - the time in the file is formatted as 19 03

   begin
      Ada.Integer_Text_IO.Get (File => Data,
                               Item => Hour);
      Ada.Integer_Text_IO.Get (File => Data,
                               Item => Minute);
   end Get_Time;

   ----------------------------------------------------------------------------
   procedure Put_Time (Hour   : in Integer;
                       Minute : in Integer) is
   -- This procedure displays a time in the form 7:32PM
   begin

      if Hour <= 12 then
         Ada.Integer_Text_IO.Put (Item  => Hour,
                                  Width => 2);
      else
         Ada.Integer_Text_IO.Put (Item => Hour - 12,
                                  Width => 2);
      end if;
      Ada.Text_IO.Put (Item => ':');

      if Minute < 10 then
         Ada.Text_IO.Put (Item => '0');
      end if;

      Ada.Integer_Text_IO.Put (Item  => Minute,
                               Width => 1);

      if Hour < 12 then
         Ada.Text_IO.Put (Item  => "AM");
      else
         Ada.Text_IO.Put (Item => "PM");
      end if;

   end Put_Time;

   ----------------------------------------------------------------------------
   procedure Process_File (Data_File      : in out Ada.Text_IO.File_Type;
                           Nest_Latitude  : in Float;
                           Nest_Longitude : in Float;
                           Total          : out Float) is
   -- Process the eagle flight data in file Data_File given the nest location
   -- Display the results and return the total number of miles flown

      --Current position reading  (angles in radians)
      Current_Latitude  : Float range -Pi / 2.0 .. +Pi / 2.0;
      Current_Longitude : Float range -Pi .. +Pi;

      -- Previous position reading (angles in radians)
      Previous_Latitude  : Float range -Pi / 2.0 .. +Pi / 2.0;
      Previous_Longitude : Float range -Pi .. +Pi;

      -- Variables for date and time
      Year   : Integer range 1753 .. Integer'Last;
      Month  : Integer range 1 .. 12;
      Day    : Integer range 1 .. 31;
      Hour   : Integer range 0 .. 23;
      Minute : Integer range 0 .. 59;

      -- Number of GPS readings in the data file
      Number_Of_Readings : Integer range 1 .. Integer'Last;

      -- Results of distance calculations
      Miles_Flown     : Float range 0.0 .. Earth_Circumference / 2.0;
      Miles_From_Nest : Float range 0.0 .. Earth_Circumference / 2.0;

      Count : Integer range 1 .. Integer'Last;  -- Loop control variable

   begin
      -- Set the previous location to the nest
      Previous_Latitude  := Nest_Latitude;
      Previous_Longitude := Nest_Longitude;

      -- Get the number of GPS readings in the data file
      Ada.Integer_Text_IO.Get (File => Data_File,
                               Item => Number_Of_Readings);

      -- Initialize loop variables
      Count           := 1;
      Miles_Flown     := 0.0;
      Miles_From_Nest := 0.0;
      Total           := 0.0;

      -- Process all of the GPS readings in the data file
      -- Each iteration, process one GPS reading
      GPS_Loop :
      loop
         exit GPS_Loop when Count > Number_Of_Readings;   -- Test loop control variable

         -- Get the date and time from the file
         Get_Date (Data  => Data_File,
                   Year  => Year,
                   Month => Month,
                   Day   => Day);
         Get_Time (Data   => Data_File,
                   Hour   => Hour,
                   Minute => Minute);

         -- Display the date and time
         Put_Date (Month => Month,
                   Day   => Day,
                   Year  => Year);
         Ada.Text_IO.Put (' ');
         Put_Time (Hour   => Hour,
                   Minute => Minute);

         -- Get the latitude and longitude from the file
         Get_Location (Data      => Data_File,
                       Latitude  => Current_Latitude,
                       Longitude => Current_Longitude);

         -- Calculate the miles flown from the previous location to the current location
         Miles_Flown := Distance (From_Latitude  => Previous_Latitude,
                                  From_Longitude => Previous_Longitude,
                                  To_Latitude    => Current_Latitude,
                                  To_Longitude   => Current_Longitude);

         Total := Total + Miles_Flown;   -- Update total

         -- Calculate the distance from the nest
         Miles_From_Nest := Distance (From_Latitude  => Nest_Latitude,
                                      From_Longitude => Nest_Longitude,
                                      To_Latitude    => Current_Latitude,
                                      To_Longitude   => Current_Longitude);


         -- Display the number of miles flown
         Ada.Float_Text_IO.Put (Item => Miles_Flown,
                                Fore => 5,
                                Aft  => 2,
                                Exp  => 0);
         Ada.Text_IO.Put (" miles flown from previous location");

         -- Display the number of miles from nest
         Ada.Float_Text_IO.Put (Item => Miles_From_Nest,
                                Fore => 6,
                                Aft  => 2,
                                Exp  => 0);
         Ada.Text_IO.Put (" miles from nest");
         Ada.Text_IO.New_Line;

         -- Save the current location as the previous
         Previous_Latitude  := Current_Latitude;
         Previous_Longitude := Current_Longitude;

         Count := Count + 1;   -- Increment loop control variable

      end loop GPS_Loop;

   end Process_File;

--------------------------------------------------------------------------------
   -- Variables for the data file
   File_Name : String (1 .. 80);
   Length    : Integer range 0 .. 80;  -- number of charcters in File_Name
   Data_File : Ada.Text_IO.File_Type;

   -- Position of nest (angles in radians)
   Nest_Latitude  : Float range -Pi / 2.0 .. +Pi / 2.0;
   Nest_Longitude : Float range -Pi .. +Pi;

   Total_Miles : Float range 0.0 .. Float'Last; -- Total miles the eagle flew

begin
   -- Get the location of the eagle's nest
   Get_Nest_Location (Latitude  => Nest_Latitude,
                      Longitude => Nest_Longitude);
   Ada.Text_IO.New_Line;

   -- Get the data file name
   Ada.Text_IO.Skip_Line;
   Ada.Text_IO.Put ("Enter the name of the data file");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Get_Line (Item => File_Name,
                         Last => Length);
   Ada.Text_IO.New_Line (2);

   -- Prepare the data file
   Ada.Text_IO.Open (File => Data_File,
                     Mode => Ada.Text_IO.In_File,
                     Name => File_Name (1 .. Length));

   -- Process the file to display the report
   Process_File (Data_File      => Data_File,
                 Nest_Latitude  => Nest_Latitude,
                 Nest_Longitude => Nest_Longitude,
                 Total          => Total_Miles);

   -- Display the total
   Ada.Text_IO.New_Line (2);
   Ada.Text_IO.Put ("The eagle flew a total of ");
   Ada.Float_Text_IO.Put (Item => Total_Miles,
                          Fore => 1,
                          Aft  => 2,
                          Exp  => 0);
   Ada.Text_IO.Put (" miles.");
   Ada.Text_IO.New_Line;

   -- Sever the connection to the data file
   Ada.Text_IO.Close (Data_File);
end Assign6;