with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.IO_Exceptions;
procedure Assign11 is

   -- Author: Vincent T. Mossman
   -- Date:   November 2013
   --
   -- This program determines number of user defined nucleotide patterns in a
   --  text file containing nucleotides
   --
   -- Input from keyboard
   --   Name of data file containing the lines of a DNA sequence
   --   A DNA sequence (pattern) to search for in the file
   --
   -- Input from file
   --   Lines of nucleotides
   --
   -- Output to screen
   --   Prompts
   --   Error message for missing file
   --   Error message for no nucleotides in the search pattern
   --   Number of lines in the data file
   --   Pattern entered by user
   --   Number of times pattern is found in the data file
   --
   -- Assumptions
   --   The pattern contains at least one nucldotide code and no more than
   --    10,000 nucleotide codes
   --   The name of the data file containes no more than 80 characters
   --   Each line in data file contains no more than 10,000 nucleotide codes

   -- Types and subtypes
   subtype File_Name        is String (1 .. 80);           -- Name of data file
   subtype Name_Length      is Integer  range 0 .. 80;     -- Last good character
   subtype Nucleotide_Range is Positive range 1 .. 10_000; -- Maximum range of nucleotide pattern

   -- Enumeration types
   type Nucleotide is (A, C, G, T);

   -- Unconstrained array types
   type Nucleotide_Array is array (Integer range <>) of Nucleotide;

   -- Constrained array types
   subtype Pattern_Array is Nucleotide_Array (Nucleotide_Range);

   -- Instantiations of I/O packages
   package Nucleotide_IO is new Ada.Text_IO.Enumeration_IO (Enum => Nucleotide);

   -------------------------------------------------------------------------------

   function Count_Matches (Pattern   : in Nucleotide_Array;
                           To_Search : in Nucleotide_Array) return Natural is

   -- This function counts the number of matches found between two arrays
   --
   -- Preconditions  : None
   --
   -- Postconditions : Number of matches is returned

      -- Variables
      Matches_Found : Natural;

   begin   -- Count_Matches

      if Pattern'Length > To_Search'Length then
         return 0;
      else

         Matches_Found := 0;   -- Initialize count

         -- Loop finds number of matches between two arrays
         -- Each iteration, checks one section
         Find_Matches_Loop :
         for Index in To_Search'First .. To_Search'Last - Pattern'Last + 1 loop
            if To_Search (Index .. Index + Pattern'Length - 1) = Pattern then
               Matches_Found := Matches_Found + 1;
            end if;
         end loop Find_Matches_Loop;

         return Matches_Found;

      end if;

   end Count_Matches;

   -------------------------------------------------------------------------------

   procedure Get_Pattern (File : in     Ada.Text_IO.File_Type;
                          Item :    out Nucleotide_Array;
                          Last :    out Natural) is

   -- This procedure gets a valid nucleotide pattern from the user or file
   --
   -- Preconditions  : None
   --
   -- Postconditions : Valid pattern and location of last good value is passed back

      -- Variables
      Line_Input : String (Nucleotide_Range);   -- Raw nucleotide input
      Length     : Natural;                     -- Last good character of raw nucleotide input

   begin   -- Get_Pattern

      Last := 0;   -- Initialize last valid nucleotide in array

      Ada.Text_IO.Get_Line (File => File,
                            Item => Line_Input,
                            Last => Length);

      -- Loop converts line of characters into valid nucleotide codes
      -- Each iteration, converts one character
      for Index in Line_Input'First .. Length loop
         if Line_Input (Index) = 'a' or Line_Input (Index) = 'A' then
            Last := Last + 1;
            Item (Last) := A;
         elsif Line_Input (Index) = 'c' or Line_Input (Index) = 'C' then
            Last := Last + 1;
            Item (Last) := C;
         elsif Line_Input (Index) = 'g' or Line_Input (Index) = 'G' then
            Last := Last + 1;
            Item (Last) := G;
         elsif Line_Input (Index) = 't' or Line_Input (Index) = 'T' then
            Last := Last + 1;
            Item (Last) := T;
         end if;
      end loop;

   end Get_Pattern;

   -------------------------------------------------------------------------------

   procedure Display_Results (Pattern : in Nucleotide_Array;
                              Lines   : in Natural;
                              Found   : in Natural) is

   -- This procedure displays results of search
   --
   -- Preconditions  : None
   --
   -- Postconditions : Results are printed onscreen

   begin   -- Display_Results

      Ada.Text_IO.New_Line (Spacing => 3);
      Ada.Integer_Text_IO.Put (Item  => Integer (Lines),
                               Width => 1);
      Ada.Text_IO.Put (" lines processed.");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("The pattern ");

      -- This loop displays patterh
      -- Each iteration, displays one element
      for Index in Pattern'Range loop
         Nucleotide_IO.Put (Pattern (Index));
      end loop;

      Ada.Text_IO.Put (" was found ");
      Ada.Integer_Text_IO.Put (Item  => Integer (Found),
                               Width => 1);
      Ada.Text_IO.Put (" times.");

   end Display_Results;

   -------------------------------------------------------------------------------

   procedure Search_File (File    : in     Ada.Text_IO.File_Type;
                          Pattern : in     Nucleotide_Array;
                          Lines   :    out Natural;
                          Found   :    out Natural) is

   -- This procedure searches the file for a given pattern of nucleotides
   --
   -- Preconditions  : None
   --
   -- Postconditions : Number of lines searched and times the pattern is found is passed out

      -- Variables
      Nucleotide_Line        : Pattern_Array;
      Nucleotide_Line_Length : Natural;

   begin   -- Search_File

      Lines := 0;   -- Initialize line count
      Found := 0;   -- Initialize found count

      Count_Matches_Loop :
      loop
         exit Count_Matches_Loop when Ada.Text_IO.End_Of_File (File => File);
         Get_Pattern (File => File,
                      Item => Nucleotide_Line,
                      Last => Nucleotide_Line_Length);
         if Nucleotide_Line_Length > 0 then
            Lines := Lines + 1;
         end if;
         Found := Found + Count_Matches (Pattern   => Pattern,
                                         To_Search => Nucleotide_Line (1 .. Nucleotide_Line_Length));
      end loop Count_Matches_Loop;

   end Search_File;

   -------------------------------------------------------------------------------

   procedure Prepare_Valid_File (Nucleotide_File : in out Ada.Text_IO.File_Type) is

   -- This procedure prepares an existing file defined by user
   --
   -- Preconditions  : None
   --
   -- Postconditions : Prepared data file is passed back

      -- Variables
      Nucleotide_File_Name : File_Name;   -- Name of nucleotide file
      Length               : Name_Length; -- Last good character

   begin   -- Prepare_Valid_File

      -- Loop gets valid file
      -- Each iteration, checks for valid file
      Get_Valid_File :
      loop

         Valid_File_Block :
         begin

            -- Get File Name
            Ada.Text_IO.Put_Line ("Please enter the name of the file of nucleotides");
            Ada.Text_IO.Get_Line (Item => Nucleotide_File_Name,
                                  Last => Length);

            -- Attempt to open file
            Ada.Text_IO.Open (File => Nucleotide_File,
                              Mode => Ada.Text_IO.In_File,
                              Name => Nucleotide_File_Name (1 .. Length));

            exit Get_Valid_File;   -- Exit if no errors

         exception
            when Ada.IO_Exceptions.Name_Error =>
               Ada.Text_IO.Put ("Cannot find a file called """);
               Ada.Text_IO.Put (Nucleotide_File_Name (1 .. Length));
               Ada.Text_IO.Put (""", please try again");
               Ada.Text_IO.New_Line (Spacing => 2);

         end Valid_File_Block;

      end loop Get_Valid_File;

   end Prepare_Valid_File;

-------------------------------------------------------------------------------

   -- Variables
   Nucleotide_File : Ada.Text_IO.File_Type;   -- File containing nucleotides
   Pattern         : Pattern_Array;           -- User defined nucleotide pattern
   Pattern_Length  : Natural;                 -- Last good value of user defined nucleotide pattern
   Total_Lines     : Natural;                 -- Total lines processed in file
   Found           : Natural;                 -- Total times pattern is found in file

begin   -- Assign11

   -- Prepare Valid File
   Prepare_Valid_File (Nucleotide_File);

   -- Get Valid Nucleotide Pattern
   Ada.Text_IO.New_Line;

   -- Loop gets a valid nucleotide pattern
   -- Each iteration, checks for valid pattern
   Valid_Pattern_Loop :
   loop
      Ada.Text_IO.Put_Line ("Please enter a pattern of nucleotides on a single line");
      Get_Pattern (File => Ada.Text_IO.Standard_Input,
                   Item => Pattern,
                   Last => Pattern_Length);
      exit Valid_Pattern_Loop when Pattern_Length > 0;
      Ada.Text_IO.Put_Line ("You must enter at least one valid nucleotide");
      Ada.Text_IO.New_Line;
   end loop Valid_Pattern_Loop;

   -- Search File for Pattern
   Search_File (File    => Nucleotide_File,
                Pattern => Pattern (1 .. Pattern_Length),
                Lines   => Total_Lines,
                Found   => Found);

   -- Display results
   Display_Results (Pattern => Pattern (1 .. Pattern_Length),
                    Lines   => Total_Lines,
                    Found   => Found);

end Assign11;
   