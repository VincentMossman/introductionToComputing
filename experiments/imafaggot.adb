with Ada.Text_IO;
procedure ImaFaggot is

Value : Character;
Count : Integer;

begin

Count := 5;
Value := '?';
Fun_Loop :
loop
   exit Fun_Loop when Count > 9;
   if Character'Pos(Value) rem 2 = 1 then
      Value := Character'Pred(Value);
   else
      Value := Character'Succ (Character'Succ (Value));
   end if;
   Ada.Text_IO.Put(Value & Integer'Image(Count));
   Ada.Text_IO.New_Line;
   Count := Count + 1;
end loop Fun_Loop;
end ImaFaggot;