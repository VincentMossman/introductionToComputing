with Cards;
use type Cards.Playing_Card;
with Cards.Card_IO;
with Cards.Stacks;
with Ada.Text_IO;
procedure Inlab14 is

   My_Deck : Cards.Stacks.Stack;
   My_Card : Cards.Playing_Card;
   Picked_Card : Cards.Playing_Card;


begin
   Cards.Stacks.Create (My_Deck);
--   Ada.Text_IO.Put_Line ("The unshuffled deck");
--   Ada.Text_IO.New_Line;
--   Cards.Stacks.Display_Stack (My_Deck);


   Ada.Text_IO.New_Line (4);
   Cards.Stacks.Shuffle (A_Stack => My_Deck,
                         Times   => 5);
   Ada.Text_IO.Put_Line ("The shuffled deck");
   Ada.Text_IO.New_Line;
   Cards.Stacks.Display_Stack (My_Deck);
   Ada.Text_IO.New_Line (3);

   Ada.Text_IO.Put ("Please enter a card (value followed by suit): ");
   Cards.Card_IO.Get (Card => My_Card);

   loop
      Ada.Text_IO.Skip_Line;
      Cards.Stacks.Take_Top_Card (A_Stack => My_Deck,
                                  A_Card  => Picked_Card);
      Ada.Text_IO.Put ("Card Drawn: ");
      Cards.Card_IO.Put (Picked_Card);
      exit when Cards.Same_Value (My_Card, Picked_Card) and not
                 Cards.Same_Suit (My_Card, Picked_Card);
   end loop;


end Inlab14;
