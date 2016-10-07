with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
with Cards.Card_IO;
package body Cards.Stacks is

   ----------------------------------------------------------------------------
   procedure Create (A_Deck : out Stack) is
      Index : Natural;
   begin
      Index := 0;

      -- Fill in the deck of cards with values
      -- Each iteration fill in one card suit
      for Suit in Suit_Type loop
         -- Fill in one card suit
         -- Each iteration, fill in one value for that suit
         for Value in Value_Type loop
            Index := Index + 1;
            A_Deck.Cards (Index) := (Suit, Value);
         end loop;
      end loop;
      A_Deck.Top := Deck_Array'Last;

   end Create;

   ----------------------------------------------------------------------------
   procedure Destroy (A_Stack : in out Stack) is
   begin
      A_Stack.Top := 0;
   end Destroy;

   ----------------------------------------------------------------------------
   function Empty (A_Stack : in Stack) return Boolean is
   begin
      return A_Stack.Top = 0;
   end Empty;

   ----------------------------------------------------------------------------
   procedure Shuffle (A_Stack : in out Stack;
                      Times   : in     Positive) is

      -- Define a generator that gives random values between 1 and Stack.Top
      subtype Shuffle_Range is Positive_Count range 1 .. A_Stack.Top;
      package Random_Index  is new Ada.Numerics.Discrete_Random (Shuffle_Range);
      My_Generator : Random_Index.Generator;

      -------------------------------------------------------------------------
      procedure Swap (Left  : in out Playing_Card;
                      Right : in out Playing_Card) is
         Temp : Playing_Card;
      begin
         Temp  := Left;
         Left  := Right;
         Right := Temp;
      end Swap;

      Position : Positive_Count;

   begin
      -- Use the system clock to seed the random number generator
      Random_Index.Reset (Gen => My_Generator);

      -- Shuffle the Stack of cards
      -- Each iteration, perform one shuffle
      for Count in 1 .. Times loop
         -- Do one shuffle of all the cards in the Stack
         -- Each iteration, swap two cards in the Stack
         for Index in 1 .. A_Stack.Top loop

            -- Pick a random position in the Stack of cards
            Position := Random_Index.Random (My_Generator);

            -- Swap the card at position Index with the
            -- card at the randomly selected Position
            Swap (Left  => A_Stack.Cards (Index),
                  Right => A_Stack.Cards (Position));

         end loop;
      end loop;
   end Shuffle;

   ----------------------------------------------------------------------------
   procedure Take_Top_Card (A_Stack : in out Stack;
                            A_Card :    out Playing_Card) is
   begin
      if A_Stack.Top = 0 then
         raise Empty_Stack;
      else
         -- Remove a card from the top of the Stack
         A_Card := A_Stack.Cards (A_Stack.Top);
         A_Stack.Top := A_Stack.Top - 1;
      end if;
   end Take_Top_Card;

   ----------------------------------------------------------------------------
   procedure Put_Card_On_Top (A_Card : in     Playing_Card;
                              A_Stack : in out Stack) is
   begin
      if A_Stack.Top = Deck_Array'Last then
         raise Full_Stack;
      else
         -- Add a card to the top of the Stack
         A_Stack.Top := A_Stack.Top + 1;
         A_Stack.Cards (A_Stack.Top) := A_Card;
      end if;
   end Put_Card_On_Top;

   ----------------------------------------------------------------------------
   procedure Display_Stack (A_Stack : in Stack) is
      -- Display all of the values in a stack of cards
   begin
      for Index in reverse 1 .. A_Stack.Top loop
         Cards.Card_IO.Put (A_Stack.Cards (Index));
         Ada.Text_IO.New_Line;
      end loop;
   end Display_Stack;


end Cards.Stacks;
