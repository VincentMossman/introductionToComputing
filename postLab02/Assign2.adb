--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--
--!!   Author:  Vincent T. Mossman                                !!--
--!!   Date:    September 9, 2013                                 !!--
--!!   Description:                                               !!--
--!!      Simple program that prints my initials (VTM) in large   !!--
--!!       block letters, with each letter made up of the same    !!--
--!!       character it represents.                               !!--
--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!--

with Ada.Text_IO;

procedure Assign2 is

   -- Constants
   Line_Length   :   constant Integer   :=   27;   -- Length required for each line of characters
   Line1         :   constant String (1 .. Line_Length)   :=   "V     V  TTTTTTT    M   M  ";   -- Top line of output
   Line2         :   constant String (1 .. Line_Length)   :=   " V   V      T       M   M  ";   -- Second line of output
   Line3         :   constant String (1 .. Line_Length)   :=   " V   V      T      MMM MMM ";   -- Third line of output
   Line4         :   constant String (1 .. Line_Length)   :=   "  V V       T      M M M M ";   -- Fourth line of output
   Line5         :   constant String (1 .. Line_Length)   :=   "  V V       T     MM MMM MM";   -- Fifth line of output
   Line6         :   constant String (1 .. Line_Length)   :=   "   V        T     M   M   M";   -- Sixth line of output
   Line7         :   constant String (1 .. Line_Length)   :=   "   V        T     M   M   M";   -- Seventh line of output

----------------------------------------------------------------------------------------------------------------------------------------------------

begin   -- Program Assign2

   -- Output the string constants defined above
   Ada.Text_IO.Put (Item => Line1);   -- Outputs Line1
   Ada.Text_IO.New_Line;              -- Places cursor on next line
   Ada.Text_IO.Put (Item => Line2);   -- Outputs Line2
   Ada.Text_IO.New_Line;              -- Places cursor on next line
   Ada.Text_IO.Put (Item => Line3);   -- Outputs Line3
   Ada.Text_IO.New_Line;              -- Places cursor on next line
   Ada.Text_IO.Put (Item => Line4);   -- Outputs Line4
   Ada.Text_IO.New_Line;              -- Places cursor on next line
   Ada.Text_IO.Put (Item => Line5);   -- Outputs Line5
   Ada.Text_IO.New_Line;              -- Places cursor on next line
   Ada.Text_IO.Put (Item => Line6);   -- Outputs Line6
   Ada.Text_IO.New_Line;              -- Places cursor on next line
   Ada.Text_IO.Put (Item => Line7);   -- Outputs Line7
   Ada.Text_IO.New_Line;              -- Places cursor on next line

end Assign2;

