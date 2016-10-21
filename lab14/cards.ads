package Cards is


   -- Definitions for playing cards

   type Suit_Type  is (hearts, diamonds, clubs, spades);
   type Value_Type is (two, three, four, five, six, seven, eight, nine, ten,
                       jack, queen, king, ace);

   type Playing_Card is
      record
         Suit  : Suit_Type;
         Value : Value_Type;
      end record;


   -- Operations that compare the VALUES of two cards

   function ">" (Left  : in Playing_Card;
                 Right : in Playing_Card) return Boolean;


   function "<" (Left  : in Playing_Card;
                 Right : in Playing_Card) return Boolean;

   function Same_Value (Left  : in Playing_Card;
                        Right : in Playing_Card) return Boolean;


   -- Operation that compares the SUITES of two cards

   function Same_Suit (Left  : in Playing_Card;
                       Right : in Playing_Card) return Boolean;


end Cards;