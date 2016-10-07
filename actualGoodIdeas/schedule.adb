with Ada.Text_IO;

procedure Schedule is

         Assignment_ID   : Natural;
         Class_Name      : String (1 .. 30);
         Class_Length    : Natural (0 .. 30);
         Assignments     : Assignments_Array (1 .. 10);
         Assign_Length   : Natural (0 .. 10);
         Date_Assigned   : Day_Rec;
         Date_Due        : Day_Rec;

   type Day_Schedule is
      record
         Class_Assignment : Class_Array;
         Class_Length     : Natural (1 .. 6);
         Work_Assignment  : Work_Array;
         Work_Length      ; Natural (1 .. 6);
         Misc_Assignment  : Misc_Array;
         Misc_Length      : Natural (1 .. 6);
   end record;

begin



end Schedule;