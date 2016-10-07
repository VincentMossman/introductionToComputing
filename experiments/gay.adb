with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure gay is

   function Puzzle (Base  : in Natural;
                    Limit : in Natural) return Integer is
      Result : Integer;
   begin
      if Base > Limit then
         Result := -1;
      elsif Base = Limit then
         Result := 1;
      else
         Result := Base * Puzzle (Base + 1, Limit);
      end if;
      return Result;
   end Puzzle;


begin
   Ada.Integer_Text_IO.Put (Item => Puzzle (0, 0));
end;
   