with Ada.Text_IO;
procedure fuckyourmom is

type Day_Type is (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
package Day_IO is new Ada.Text_IO.Enumeration_IO (Enum => Day_Type);

begin

for Day in Day_Type loop
   Day_IO.Put (Day);
   Ada.Text_IO.New_Line;
end loop;

end fuckyourmom;