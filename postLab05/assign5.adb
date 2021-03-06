with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
procedure assign5 is

-- Author:   Vincent T. Mossman
-- Date:     October 2, 2013
--
-- Purpose:
--   Program deterimes the winner of a game of rock, paper, scissors.
--
-- Input:
--   User must enter both players choice of either rock, paper, or
--    scissors.
--   User must enter maximum number of wins to process
--
-- Output:
--   Displays on-screen which player won the game.
--   Displays on-screen gameplay summary
--
-- Assumptions:
--   All data entered by user is valid.

   -- Types
   type RockPaperScissors_Choice is (Rock, Paper, Scissors);   -- Enumeration of player's choices

   -- Instantiate package for product input and output
   package RockPaperScissors_IO is new Ada.Text_IO.Enumeration_IO (Enum => RockPaperScissors_Choice);

   -- Variables
   Player1_Choice : RockPaperScissors_Choice;          -- Choice for player one
   Player2_Choice : RockPaperScissors_Choice;          -- Choice for player two
   Player1_Win    : Integer range 0 .. Integer'Last;   -- Number of games player 1 has won
   Player2_Win    : Integer range 0 .. Integer'Last;   -- Number of games player 2 has won
   Tie            : Integer range 0 .. Integer'Last;   -- Number of games resulting in a tie
   Rock_Win       : Integer range 0 .. Integer'Last;   -- Number of games rock was winning choice
   Paper_Win      : Integer range 0 .. Integer'Last;   -- Number of games paper was winning choice
   Scissors_Win   : Integer range 0 .. Integer'Last;   -- Number of games scissors was a winning choice
   Win_Max        : Integer range 0 .. Integer'Last;   -- User defined count control variable

begin -- assign5

   --Prompt user for number of winning games
   Ada.Text_IO.Put (Item => "Please enter the number of wins by one player for the program to process: ");
   Ada.Integer_Text_IO.Get (Item => Win_Max);

   Player1_Win  := 0;  -- Initialize variables used in loop
   Player2_Win  := 0;
   Tie          := 0;
   Rock_Win     := 0;
   Paper_Win    := 0;
   Scissors_Win := 0;
   -- Allows user to play multiple games without restarting program
   -- Each iteration, determine result of game of rock, paper, scissors and increment necessary variables
   RockPaperScissors_Loop :
   loop
      exit RockPaperScissors_Loop when Player1_Win > Win_Max - 1 or Player2_Win > Win_Max - 1;

      -- Get data
      Ada.Text_IO.New_Line (Spacing => 2);
      Ada.Text_IO.Put (Item => "Enter the choices made by the two players:");
      Ada.Text_IO.New_Line;
      RockPaperScissors_IO.Get (Item => Player1_Choice);
      RockPaperScissors_IO.Get (Item => Player2_Choice);
      Ada.Text_IO.New_Line;

      -- Determine winner or tie
      if (Player1_Choice = Paper and Player2_Choice = Rock) or
         (Player1_Choice = Rock and Player2_Choice = Scissors) or
         (Player1_Choice = Scissors and Player2_Choice = Paper) then
         Ada.Text_IO.Put (Item => "The first player's choice of ");
         RockPaperScissors_IO.Put (Item  => Player1_Choice,
                                   Width => 0,
                                   Set   => Ada.Text_IO.Lower_Case);
         Ada.Text_IO.Put (Item => " beats ");
         RockPaperScissors_IO.Put (Item  => Player2_Choice,
                                   Width => 0,
                                   Set   => Ada.Text_IO.Lower_Case);
         Ada.Text_IO.Put (Item => ".");

         Player1_Win := Player1_Win + 1;        -- Increment win count

         if Player1_Choice = Paper then
            Paper_Win := Paper_Win + 1;         -- Increment paper win count
         elsif Player1_Choice = Rock then
            Rock_Win := Rock_Win + 1;           -- Increment rock win count
         else
            Scissors_Win := Scissors_Win + 1;   -- Increment scissors win count
         end if;

      elsif (Player1_Choice = Rock and Player2_Choice = Paper) or
            (Player1_Choice = Paper and Player2_Choice = Scissors) or
            (Player1_Choice = Scissors and Player2_Choice = Rock) then
         Ada.Text_IO.Put (Item => "The second player's choice of ");
         RockPaperScissors_IO.Put (Item  => Player2_Choice,
                                   Width => 0,
                                   Set   => Ada.Text_IO.Lower_Case);
         Ada.Text_IO.Put (Item => " beats ");
         RockPaperScissors_IO.Put (Item  => Player1_Choice,
                                   Width => 0,
                                   Set   => Ada.Text_IO.Lower_Case);
         Ada.Text_IO.Put (Item => ".");

         Player2_Win := Player2_Win + 1;        -- Increment win count

         if Player2_Choice = Paper then
            Paper_Win := Paper_Win + 1;         -- Increment paper win count
         elsif Player2_Choice = Rock then
            Rock_Win := Rock_Win + 1;           -- Increment rock win count
         else
            Scissors_Win := Scissors_Win + 1;   -- Increment scissors win count
         end if;

      else
         Ada.Text_IO.Put (Item => "Both players entered ");
         RockPaperScissors_IO.Put (Item  => Player1_Choice,
                                   Width => 0,
                                   Set   => Ada.Text_IO.Lower_Case);
         Ada.Text_IO.Put (Item => ". The game is a tie.");

         Tie := Tie + 1;                        -- Increment tie count

      end if;

   end loop RockPaperScissors_Loop;

   -- Display summary of games
   Ada.Text_IO.New_Line (Spacing => 5);
   Ada.Text_IO.Put (Item => "Number of games won by first player: ");
   Ada.Integer_Text_IO.Put (Item  => Player1_Win,
                            Width => 1);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "Number of games won by second player: ");
   Ada.Integer_Text_IO.Put (Item  => Player2_Win,
                            Width => 1);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "Number of games resulting in a tie: ");
   Ada.Integer_Text_IO.Put (Item  => Tie,
                            Width => 1);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "Percentage of winning choices that were rock: ");
   Ada.Float_Text_IO.Put (Item => 100.0 * (Float (Rock_Win) / Float (Player1_Win + Player2_Win)),
                          Fore => 1,
                          Aft  => 1,
                          Exp  => 0);
   Ada.Text_IO.Put (Item => '%');
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "Percentage of winning choices that were paper: ");
   Ada.Float_Text_IO.Put (Item => 100.0 * (Float (Paper_Win) / Float (Player1_Win + Player2_Win)),
                          Fore => 1,
                          Aft  => 1,
                          Exp  => 0);
   Ada.Text_IO.Put (Item => '%');
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put (Item => "Percentage of winning choices that were scissors: ");
   Ada.Float_Text_IO.Put (Item => 100.0 * (Float (Scissors_Win) / Float (Player1_Win + Player2_Win)),
                          Fore => 1,
                          Aft  => 1,
                          Exp  => 0);
   Ada.Text_IO.Put (Item => '%');

end assign5;