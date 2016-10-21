package Cards.Stacks is

   -- A type for a stack of cards
   type Stack is private;
   -- This is a new kind of type
   -- The details of this type are defined below in the "private" section
   -- Users may declare variables of this type and use the operations
   -- given below.  Users may not make use of the details given in the
   -- "private" section.



   -- Operations on stacks of cards

   procedure Create (A_Deck : out Stack);
   -- Create a new deck of 52 cards

   procedure Destroy (A_Stack : in out Stack);
   -- Destroy a stack of cards

   function Empty (A_Stack : in Stack) return Boolean;
   -- Returns True if A_Stack has no cards in it

   procedure Shuffle (A_Stack : in out Stack;
                      Times   : in     Positive);
   -- Shuffle a stack of cards Times times
   -- Precondition : The stack must have at least one card in it

   procedure Take_Top_Card (A_Stack : in out Stack;
                            A_Card  :    out Playing_Card);
   -- Take the top card from a stack of cards
   -- The exception Empty_Stack is raised if A_Stack has no cards left

   procedure Put_Card_On_Top (A_Card  : in     Playing_Card;
                              A_Stack : in out Stack);
   -- Puts A-Card on top of the stack A_Stake
   -- The exception Full_Stack is raised if A_Stack already had 52 cards

   procedure Display_Stack (A_Stack : in Stack);
   -- Displays all of the cards in A_Stack (from top to bottom)


   ----------------------------------------------------------------------------
   -- Exceptions that might be raised
   Empty_Stack : exception;
   Full_Stack  : exception;

private

   -- Declarations in this section may only be used in the body of this package
   -- They may not be used by users of this package

   type Card_Array is array (Positive range <>) of Playing_Card;

   subtype Count          is Integer range 0 .. 52;
   subtype Positive_Count is Count   range 1 .. 52;
   subtype Deck_Array is Card_Array (Positive_Count);
   type Stack is
      record
         Top   : Count := 0;
         Cards : Deck_Array;
      end record;

end Cards.Stacks;