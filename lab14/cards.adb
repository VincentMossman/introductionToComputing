package body Cards is

   function ">" (Left  : in Playing_Card;
                 Right : in Playing_Card) return Boolean is
   begin
      return Left.Value > Right.Value;
   end ">";

   ----------------------------------------------------------------------------
   function "<" (Left  : in Playing_Card;
                 Right : in Playing_Card) return Boolean is
   begin
      return Left.Value < Right.Value;
   end "<";

   ----------------------------------------------------------------------------
   function Same_Value (Left  : in Playing_Card;
                        Right : in Playing_Card) return Boolean is
   begin
      return Left.Value = Right.Value;
   end Same_Value;

   ----------------------------------------------------------------------------
   function Same_Suit (Left  : in Playing_Card;
                       Right : in Playing_Card) return Boolean is
   begin
      return Left.Suit = Right.Suit;
   end Same_Suit;

end Cards;
