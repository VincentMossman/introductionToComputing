with Cards;
use type Cards.Playing_Card;
with Cards.Card_IO;
with Cards.Stacks;
with Ada.Text_IO;
procedure War is

   Deck  : Cards.Stacks.Stack;  -- The deck of cards

   Hand1 : Cards.Stacks.Stack;  -- The two players
   Hand2 : Cards.Stacks.Stack;  --   hands of cards

   Card1 : Cards.Playing_Card;  -- One of player #1's cards
   Card2 : Cards.Playing_Card;  -- One of player #2's cards

begin
   -- Create a new deck of cards and shuffle it seven times
   Cards.Stacks.Create (Deck);
   Cards.Stacks.Shuffle (Deck, 7);

   -- Ensure that each player's pile starts off empty
   Cards.Stacks.Destroy (Hand1);
   Cards.Stacks.Destroy (Hand2);

   -- Deal out ten cards
   -- Each iteration, deal a card to each of two players
   for Count in 1 .. 10 loop

      -- Deal a card to player #1
      Cards.Stacks.Take_Top_Card (Deck, Card1);
      Cards.Stacks.Put_Card_On_Top (Card1, Hand1);




      -- Deal a card to player #2
      Cards.Stacks.Take_Top_Card (Deck, Card2);
      Cards.Stacks.Put_Card_On_Top (Card2, Hand2);





   end loop;

   Ada.Text_IO.Put_Line ("Press enter to play one round");
   Ada.Text_IO.New_Line;

   -- Play ten rounds
   for Count in 1 .. 10 loop
      Ada.Text_IO.Skip_Line;
      -- Get a card from each player
      Cards.Stacks.Take_Top_Card (Hand1, Card1);
      Cards.Stacks.Take_Top_Card (Hand2, Card2);





      -- Display the two cards
      Ada.Text_IO.Put ("First Player's Card: ");
      Cards.Card_IO.Put (Card1);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Second Player's Card: ");
      Cards.Card_IO.Put (Card2);
      Ada.Text_IO.New_Line;

      -- Display the results of this round
      if Card1 > Card2 then
         Ada.Text_IO.Put ("Player One Wins");
      elsif Card1 < Card2 then
         Ada.Text_IO.Put ("Player Two Wins");
      else
         Ada.Text_IO.Put ("It's a Tie!");
      end if;

      Ada.Text_IO.New_Line;
   end loop;


end War;