with Ada.Text_IO;
procedure Animals is

   -- Some different names of animals
   type Animal_Type is (Mouse, Rat, Hamster, Cow, Pig, Chicken, Panther, Lion, Tiger, Monkey);

   -- Create a package of I/O operations for the animals
   package Animal_IO is new Ada.Text_IO.Enumeration_IO (Enum => Animal_Type);

   -- Variable
   My_Choice : Animal_Type;  -- Animal entered by user

begin
   -- Prompt for an animal
   Ada.Text_IO.Put_Line ("Enter one of the following animal names");

   -- Display the list of legal animals
   -- Each iteration, display one animal name
   for Animal in Animal_Type loop
      Animal_IO.Put (Animal);
      Ada.Text_IO.New_Line;
   end loop;

-- Delete this comment and write a for loop to display the animal names


   Ada.Text_IO.New_Line;

   -- Get an animal from the user
   Animal_IO.Get (Item => My_Choice);

   -- Display which group of animals the choice is in
   case My_Choice is
      when Mouse | Rat | Hamster =>
         Ada.Text_IO.Put_Line ("Your animal is a rodent.");
      when Cow | Pig | Chicken =>
         Ada.Text_IO.Put_Line ("Your animal is a farm animal.");
      when Panther | Lion | Tiger =>
         Ada.Text_IO.Put_Line ("Your animal is a ferocious cat");
   end case;

end Animals; 