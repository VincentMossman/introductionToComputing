with Ada.Text_IO;
with Ada.IO_Exceptions;
procedure Question_8 is

   type    Score_Type      is            range 0 .. 100;
   subtype Quiz_Score_Type is Score_Type range 0 .. 10;
   package Score_IO is new Ada.Text_IO.Integer_IO (Num => Score_Type);

   Score : Quiz_Score_Type;   -- See question parts a through d

begin
   loop -- an infinite loop to give you practice in stopping one.
      Validation_Block :
      begin
         Ada.Text_IO.Put_Line ("Enter a score");
         Score_IO.Get (Item => Score);
         Ada.Text_IO.Put_Line ("No exception detected");
      exception
         when Ada.IO_Exceptions.Data_Error =>
            Ada.Text_IO.Put_Line ("DATA_ERROR detected");
         when CONSTRAINT_ERROR =>
            Ada.Text_IO.Put_Line ("CONSTRAINT_ERROR detected");
      end Validation_Block;
   end loop;
end Question_8;