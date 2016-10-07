-- Author:   Vincent T. Mossman
-- Date:     September 24, 2013
--
-- Purpose:
--   Program deterimes the winner of a game of rock, paper, scissors.
--
-- Input:
--   User must enter both players choice of either rock, paper, or
--    scissors.
--
-- Output:
--   Displays on-screen which player won the game.
--
-- Assumptions:
--   All data entered by user is valid.

with Ada.Text_IO;

procedure assign4 is

   -- Types
   type RPS_Choice is (Rock, Paper, Scissors);   -- Enumeration of player's choices

   -- Instantiate package for product input and output
   package RPS_IO is new Ada.Text_IO.Enumeration_IO (Enum => RPS_Choice);

   -- Variables
   Player1_Choice : RPS_Choice;   -- Choice for player one
   Player2_Choice : RPS_Choice;   -- Choice for player two
   Player1_Win1   : Boolean;      -- Conditional test boolean
   Player1_Win2   : Boolean;      -- Conditional test boolean
   Player1_Win3   : Boolean;      -- Conditional test boolean
   Player2_Win1   : Boolean;      -- Conditional test boolean
   Player2_Win2   : Boolean;      -- Conditional test boolean
   Player2_Win3   : Boolean;      -- Conditional test boolean

begin -- assign4

   -- Get data
   Ada.Text_IO.Put (Item => "Enter the choices made by the two players:");
   Ada.Text_IO.New_Line;
   RPS_IO.Get (Item => Player1_Choice);
   RPS_IO.Get (Item => Player2_Choice);
   Ada.Text_IO.New_Line;

   -- Determine winner or tie
   Player1_Win1 := Player1_Choice = Paper and Player2_Choice = Rock;
   Player1_Win2 := Player1_Choice = Rock and Player2_Choice = Scissors;
   Player1_Win3 := Player1_Choice = Scissors and Player2_Choice = Paper;
   Player2_Win1 := Player1_Choice = Rock and Player2_Choice = Paper;
   Player2_Win2 := Player1_Choice = Paper and Player2_Choice = Scissors;
   Player2_Win3 := Player1_Choice = Scissors and Player2_Choice = Rock;

   if Player1_Win1 or Player1_Win2 or Player1_Win3 then
      Ada.Text_IO.Put (Item => "The first player's choice of ");
      RPS_IO.Put (Item  => Player1_Choice,
                  Width => 0,
                  Set   => Ada.Text_IO.Lower_Case);
      Ada.Text_IO.Put (Item => " beats ");
      RPS_IO.Put (Item  => Player2_Choice,
                  Width => 0,
                  Set   => Ada.Text_IO.Lower_Case);
      Ada.Text_IO.Put (Item => ".");
   elsif Player2_Win1 or Player2_Win2 or Player2_Win3 then
      Ada.Text_IO.Put (Item => "The second player's choice of ");
      RPS_IO.Put (Item  => Player2_Choice,
                  Width => 0,
                  Set   => Ada.Text_IO.Lower_Case);
      Ada.Text_IO.Put (Item => " beats ");
      RPS_IO.Put (Item  => Player1_Choice,
                  Width => 0,
                  Set   => Ada.Text_IO.Lower_Case);
      Ada.Text_IO.Put (Item => ".");
   else
      Ada.Text_IO.Put (Item => "Both players entered ");
      RPS_IO.Put (Item  => Player1_Choice,
                  Width => 0,
                  Set   => Ada.Text_IO.Lower_Case);
      Ada.Text_IO.Put (Item => ". The game is a tie.");
   end if;

end assign4;