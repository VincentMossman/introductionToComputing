with Ada.Text_IO;

procedure FuckErrbody is

Pear : String (1 .. 4);
Count : Integer;

begin

Count := 1;
Pear := "Help";
Fun_Loop :
loop
   exit Fun_Loop when Count > 4;
   if Count rem 2 = 0 then
      Pear(Count) := Character'Pred(Pear(Count - 1));
   else
      Pear(Count) := Character'Pred(Pear(Count + 1));
   end if;
   Count := Count + 1;
end loop Fun_Loop;
Ada.Text_IO.Put (Pear);
end FuckErrbody;
