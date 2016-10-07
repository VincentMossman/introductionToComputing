with Ada.Integer_Text_IO;
with Ada.Text_IO;
procedure Prime is

   -----------------------------------------------------------------

   function IsDivisible (Number  : in Integer;
                         Divisor : in Integer) return Boolean is

   -- Prereq : Divisor must not be 0

   begin   -- IsDivisible

      return Number rem Divisor = 0;

   end IsDivisible;

   -----------------------------------------------------------------

   function IsPrime (Int : in Integer) return Boolean is

      CheckCase : Integer := 2;

   begin   -- IsPrime

      if Int < 2 or Int = 4 then
         return False;
      elsif Int = 3 then
         return True;
      else
         loop
            exit when IsDivisible (Number  => Int,
                                   Divisor => CheckCase) or CheckCase = Int / 2;
            CheckCase := CheckCase + 1;
         end loop;

         return CheckCase = Int / 2;
      end if;

   end IsPrime;

   -----------------------------------------------------------------

   Int : Integer;
   Instance : Integer := 0;

begin   -- Prime

--   loop
--      Ada.Text_IO.Put ("Please enter an integer: ");
--      Ada.Integer_Text_IO.Get (Int);
--      exit when int not in Positive;
--      if IsPrime (Int) then
--         Ada.Text_IO.Put ("That number is prime");
--      else
--         Ada.Text_IO.Put ("That number is not prime");
--      end if;
--      Ada.Text_IO.New_Line (Spacing => 2);
--   end loop;

   for Num in 1 .. 100 loop
      if IsPrime (Num) then
         Instance := Instance + 1;
         Ada.Text_IO.Put ("Instance #");
         Ada.Integer_Text_IO.Put (Instance, 0);
         Ada.Text_IO.Set_Col (20);
         Ada.Integer_Text_IO.Put (Num, 0);
         Ada.Text_IO.New_Line;
      end if;
   end loop;

--   for Num in 1 .. 10001000 loop
--      if IsPrime (Num) and IsPrime (Num + 4) then
--         Instance := Instance + 1;
--         Ada.Text_IO.Put ("Instance #");
--         Ada.Integer_Text_IO.Put (Instance, 0);
--         Ada.Text_IO.Set_Col (20);
--         Ada.Integer_Text_IO.Put (Num, 0);
--         Ada.Text_IO.Put (" relates to ");
--         Ada.Integer_Text_IO.Put (Num + 4, 0);
--         Ada.Text_IO.New_Line;
--      end if;
--   end loop;

end Prime;