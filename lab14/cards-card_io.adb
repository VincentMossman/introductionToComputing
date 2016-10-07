with Ada.Text_IO;
package body Cards.Card_IO is

   package Suit_IO  is new Ada.Text_IO.Enumeration_IO (Suit_Type);
   package Value_IO is new Ada.Text_IO.Enumeration_IO (Value_Type);

   ---------------------------------------------------------------
   procedure Put (Card : in Playing_Card) is
   begin
      Value_IO.Put (Item  => Card.Value,
                    Set   => Ada.Text_IO.Lower_Case,
                    Width => 6);
      Ada.Text_IO.Put ("of ");
      Suit_IO.Put (Item => Card.Suit,
                   Set  => Ada.Text_IO.Lower_Case);
   end Put;

   ---------------------------------------------------------------
   procedure Get (Card : out Playing_Card) is
   begin
      Value_IO.Get (Item => Card.Value);
      Suit_IO.Get (Item => Card.Suit);

   end Get;

end Cards.Card_IO;
