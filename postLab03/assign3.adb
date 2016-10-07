--   Author:  Vincent T. Mossman
--   Date:    September 17, 2013
--   Purpose:
--      Program calculates amount of money in an investment
--       account after a given number of years.
--   Input:
--      User must input the amount of money invested (principle),
--       the interest rate, number of years to invest the money,
--       the number of times per year interest is compounded, and
--       the name of the account
--   Output:
--      Program will echo print user entered data as well as the
--       final calculation of amount of money in account after the
--       given time period.
--   Assumptions:
--      Amount of money is within architectural limits of computer
--      Name of account is no more than 20 characters in length
--      Number of times a year to compound interest is more than 0
--      User must enter valid data types for prompted information

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;

procedure assign3 is

   -- Constants
   StringLength : Integer := 20;   -- Defines maximum string length

   -- Variables
   Principle     :   Float range 0.0 .. Float'Last;     -- Amount of money invested (principle)
   FinalAmount   :   Float range 0.0 .. Float'Last;     -- Final amount of money
   Interest_Perc :   Float range 0.0 .. 100.0;          -- Interest rate as a percentage
   Interest_Frac :   Float range 0.0 .. 1.0;            -- Interest rate as a fraction
   Years         :   Integer range 0 .. Integer'Last;   -- Years to invest
   Freq          :   Integer range 0 .. 366;            -- Times a year to compound (frequency)
   Exponent      :   Integer range 0 .. Integer'Last;   -- Exponent part of equation
   Parenthetical :   Float range 0.0 .. Float'Last;     -- Parenthetical part of equation
   Name          :   String (1 .. StringLength);        -- Name of account
   Last_Ch       :   Integer range 0 .. StringLength;   -- Position of last good character

begin   -- assign3

   -- Get data
   Ada.Text_IO.Put (Item => "Enter principle (exclude dollar sign): ");
   Ada.Float_Text_IO.Get (Item => Principle);
   Ada.Text_IO.Put (Item => "Enter interest rate: ");
   Ada.Float_Text_IO.Get (Item => Interest_Perc);
   Ada.Text_IO.Put (Item => "Enter length of investment (years): ");
   Ada.Integer_Text_IO.Get (Item => Years);
   Ada.Text_IO.Put (Item => "Enter number of times per year interest is compounded: ");
   Ada.Integer_Text_IO.Get (Item => Freq);
   Ada.Text_IO.Skip_Line;
   Ada.Text_IO.Put (Item => "Enter name of the account: ");
   Ada.Text_IO.Get_Line (Item => Name,
                         Last => Last_Ch);

   -- Echo print data
   Ada.Text_IO.New_Line (Spacing => 5);       -- Spacing as defined by program specifications
   Ada.Text_IO.Put (Item => "Principle: $");
   Ada.Float_Text_IO.Put (Item => Principle,
                          Fore => 1,
                          Aft  => 2,
                          Exp  => 0);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "Interest rate: ");
   Ada.Float_Text_IO.Put (Item => Interest_Perc,
                          Fore => 1,
                          Aft  => 2,
                          Exp  => 0);
   Ada.Text_IO.Put (Item => "%");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "Number of years to invest: ");
   Ada.Integer_Text_IO.Put (Item  => Years,
                            Width => 1);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "Number of times per year interest is compounded: ");
   Ada.Integer_Text_IO.Put (Item  => Freq,
                            Width => 1);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "Account name: ");
   Ada.Text_IO.Put (Item => Name (1 .. Last_Ch));

   -- Calculate data
   Exponent      := Years * Freq;                            -- Exponent part of formula
   Interest_Frac := Interest_Perc / 100.0;                   -- Convert percentage to fraction
   Parenthetical := (1.0 + Interest_Frac / Float(Freq));     -- Calculate parenthetical
   FinalAmount   := Principle * Parenthetical ** Exponent;   -- Evaluate entire expression  |
-- FinalAmount := Principle * (1.0 + Interest_Frac / Float(Freq)) ** (Years * Freq);        |both of these give same answer

   -- Print results
   Ada.Text_IO.New_Line (Spacing => 2);
   Ada.Text_IO.Put (Item => "For the given information, the account '");
   Ada.Text_IO.Put (Item => Name (1 .. Last_Ch));
   Ada.Text_IO.Put (Item => "' will have $");
   Ada.Float_Text_IO.Put (Item => FinalAmount,
                          Fore => 1,
                          Aft  => 2,
                          Exp  => 0);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "  after ");
   Ada.Integer_Text_IO.Put (Item  => Years,
                            Width => 1);
   Ada.Text_IO.Put (Item => " years.");

end assign3;


   