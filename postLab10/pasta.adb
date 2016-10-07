with Ada.Text_IO;
with Ada.Direct_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.IO_Exceptions;
procedure Pasta is

   --   Author: Vincent T. Mossman (Section 1)
   --   Date:   November 2013
   --
   --   This program maintains the warehouse inventory of different pasta varieties
   --
   --   Input from keyboard
   --      Transaction selection
   --      Pasta description
   --      Pasta ID Number
   --      Sale and Shipment amount
   --
   --   Input from file
   --      Pasta description
   --      Amount of pasta in warehouse
   --      Total number of sales for each pasta
   --      Total amount of sales (in pounds) for each pasta
   --
   --   Output to screen
   --      Transaction options
   --      Pasta information
   --
   --   Output to file
   --      Pasta description
   --      Amount of pasta in warehouse
   --      Total number of sales for each pasta
   --      Total amount of sales (in pounds) for each pasta
   --
   --   Assumptions
   --      Description of pasta is not more than 20 characters
   --      Total amount of pasta (in pounds) in warehouse is not greater than Float'Last

   -- Name of inventory file
   Inventory_File_Name : constant String := "Records.Dat";   -- Name of file containing pasta information

   -- Type for the different transactions
   type Transaction_Type is (Sale, Shipment, New_Pasta, Amount, Total, Done);

   -- Types for pasta amounts
   subtype Warehouse_Pounds is Float range 0.0 .. 20_000.0;   -- Ten ton maximum
   subtype Shipping_Pounds  is Float range 1.0 ..  2_000.0;   -- One pound minimum, one ton maximum
   subtype Pounds           is Float range 0.0 .. Float'Last; -- Generic non-zero pound amount

   -- Types for pasta information
   type Pasta_Rec   is record
      Description : String (1 .. 20);    -- Type of pasta
      Amount      : Warehouse_Pounds;    -- Amount of pasta in warehouse
      Num_Sales   : Natural;             -- Number of sales of this pasta
      Total_Sales : Pounds;              -- Total sales amount (in pounds) of this pasta
   end record;

   -- Instantiations of I/O Packages
   package  Transaction_IO is new Ada.Text_IO.Enumeration_IO
                                  (Enum => Transaction_Type);

   package  Pasta_IO is new Ada.Direct_IO (Element_Type => Pasta_Rec);
   use type Pasta_IO.Count;  -- Make operators directly visible
   subtype  Pasta_ID_Type is Pasta_IO.Count;  -- A synonym for clarity

   package Pasta_ID_IO is new Ada.Text_IO.Integer_IO (Num => Pasta_IO.Count);

   ----------------------------------------------------------------------------

   procedure Prepare_File (Pasta_File : out Pasta_IO.File_Type) is

   -- This procedure prepares the direct access file containing
   -- the company's pasta inventory information
   --
   -- Preconditions  : None
   --
   -- Postconditions : Prepared data file is passed back

   begin   -- Prepare_File
      Pasta_IO.Open (File => Pasta_File,
                     Mode => Pasta_IO.Inout_File,
                     Name => Inventory_File_Name);
   exception
      when Ada.IO_Exceptions.Name_Error =>
         Ada.Text_IO.Put_Line ("Inventory file does not exist");
         Ada.Text_IO.Put_Line ("Creating the inventory file Records.Dat");
         Pasta_IO.Create (File => Pasta_File,
                          Mode => Pasta_IO.Inout_File,
                          Name => Inventory_File_Name);
   end Prepare_File;

   ----------------------------------------------------------------------------

   procedure Get_Transaction (File        : in out Pasta_IO.File_Type;
                              Transaction :    out Transaction_Type) is

   -- This procedure asks the user what type of transaction they want to
   -- do and gets it from them.  Handles invalid input.
   --
   -- Preconditions  : None
   --
   -- Postconditions : Passes out valid transaction type

   begin   -- Get_Transaction

      Pasta_IO.Reset (File);
         if Pasta_IO.End_Of_File (File) then
            Ada.Text_IO.New_Line (2);
            Ada.Text_IO.Put_Line ("There is currently no pasta in the data file.");
            Ada.Text_IO.New_Line (2);
            Ada.Text_IO.Put_Line ("The following operations are available:");
            Ada.Text_IO.Put_Line ("  New_Pasta - Obtain the number for a new pasta");
            Ada.Text_IO.Put_Line ("  Total     - Total pasta in the inventory");
            Ada.Text_IO.Put_Line ("  Done      - Exit this program");
            Ada.Text_IO.New_Line;
            -- Loop gets valid transaction choice
            -- Each iteration, one input value is checked
            loop
               Ada.Text_IO.Put ("Enter your choice:   ");
               begin
                  Transaction_IO.Get (Transaction);
                  exit when Transaction = New_Pasta or Transaction = Total or Transaction = Done;
                  Ada.Text_IO.Put_Line ("Invalid choice, please try again.");
                  Ada.Text_IO.Put_Line ("There is no pasta currently in file.");
                  Ada.Text_IO.Put_Line ("Please enter command <New_Pasta> to create new pasta.");
               exception
                  when Ada.IO_Exceptions.Data_Error =>
                     Ada.Text_IO.Skip_Line;  -- Skip over bad data
                     Ada.Text_IO.Put_Line ("Invalid choice, please try again.");
               end;
            end loop;
         else
            Ada.Text_IO.New_Line (2);
            Ada.Text_IO.Put_Line ("The following operations are available:");
            Ada.Text_IO.Put_Line ("  Sale      - Update inventory for a shipment out");
            Ada.Text_IO.Put_Line ("  Shipment  - Update inventory for a shipment in");
            Ada.Text_IO.Put_Line ("  New_Pasta - Obtain the number for a new pasta");
            Ada.Text_IO.Put_Line ("  Amount    - Amount of a pasta in the inventory");
            Ada.Text_IO.Put_Line ("  Total     - Total pasta in the inventory");
            Ada.Text_IO.Put_Line ("  Done      - Exit this program");
            Ada.Text_IO.New_Line;
            -- Loop gets valid transaction choice
            -- Each iteration, one input value is checked
            loop
               Ada.Text_IO.Put ("Enter your choice:   ");
               begin
                  Transaction_IO.Get (Transaction);
                  exit;
               exception
                  when Ada.IO_Exceptions.Data_Error =>
                     Ada.Text_IO.Skip_Line;  -- Skip over bad data
                     Ada.Text_IO.Put_Line ("Invalid choice, please try again.");
               end;
            end loop;
         end if;
   end Get_Transaction;

   ----------------------------------------------------------------------------

   procedure Get_Pasta_ID
             (Max_Value : in  Pasta_ID_Type;     -- The largest possible ID
              Pasta_ID  : out Pasta_ID_Type) is  -- The ID of the desired pasta

   -- This procedure prompts for and gets a valid pasta identification number.
   -- Handles invalid input.
   --
   -- Preconditions  : None
   --
   -- Postconditions : Pasta_ID is passed back

   begin   -- Get_Pasta_ID
      -- Loop gets a valid Pasta ID number
      -- Each iteration, one input value is checked
      Validation_Loop :
      loop
         Ada.Text_IO.Put ("Enter the Pasta Identification Number:  ");
         Validation_Block :
         begin
            Pasta_ID_IO.Get (Pasta_ID);
            exit Validation_Loop when Pasta_ID > 0 and Pasta_ID <= Max_Value;
            Ada.Text_IO.Put_Line ("Invalid number.");
            Ada.Text_IO.Put ("Pasta number must be between 1 and ");
            Pasta_ID_IO.Put (Item => Max_Value, Width => 1);
            Ada.Text_IO.New_Line;
         exception
            when Ada.IO_Exceptions.Data_Error =>
               Ada.Text_IO.Skip_Line;  -- Skip over bad data
               Ada.Text_IO.Put_Line ("Invalid number.  ");
            when CONSTRAINT_ERROR =>
               Ada.Text_IO.Put_Line ("Invalid number.  ");
               Ada.Text_IO.Put ("Pasta number must be between 1 and ");
               Pasta_ID_IO.Put (Item => Max_Value, Width => 1);
               Ada.Text_IO.New_Line;
         end Validation_Block;
      end loop Validation_Loop;
   end Get_Pasta_ID;

   ----------------------------------------------------------------------------

   procedure Get_Pounds (Amount : out Shipping_Pounds) is

   -- This procedure prompts for and gets a valid weight of a pasta shipment
   -- Handles invalid input.
   --
   -- Preconditions  : None
   --
   -- Postconditions : Amount is passed back

   begin   -- Get_Pounds
      -- Loop gets a valid number of pounds for a truck
      -- Each iteration, one input value is checked
      Validation_Loop :
      loop
         Ada.Text_IO.Put ("Enter the amount (pounds) of pasta in shipment:  ");
         Validation_Block :
         begin
            Ada.Float_Text_IO.Get (Amount);
            exit Validation_Loop;
         exception
            when Ada.IO_Exceptions.Data_Error =>
               Ada.Text_IO.Skip_Line;  -- Skip over bad data
               Ada.Text_IO.Put_Line ("Data must be numeric.");
            when CONSTRAINT_ERROR =>
               Ada.Text_IO.Put_Line ("Invalid amount.");
               Ada.Text_IO.Put ("Enter a number between ");
               Ada.Float_Text_IO.Put (Item => Shipping_Pounds'First,
                                      Fore => 1, Aft  => 1, Exp  => 0);
               Ada.Text_IO.Put (" and ");
               Ada.Float_Text_IO.Put (Item => Shipping_Pounds'Last,
                                      Fore => 1, Aft  => 1, Exp  => 0);
               Ada.Text_IO.New_Line;
         end Validation_Block;
      end loop Validation_Loop;
   end Get_Pounds;

   ----------------------------------------------------------------------------

   procedure Process_Sale (Pasta_File : in out Pasta_IO.File_Type) is

   -- This procedure gets sales information from the user and subtracts
   -- the amount of pasta sold from the inventory.
   --
   -- Preconditions  : None
   --
   -- Postconditions : Pasta_File is updated

      Amount       : Shipping_Pounds;  -- Amount of pasta sold
      Pasta_Number : Pasta_ID_Type;    -- The pasta identification number
      Pasta_Info   : Pasta_Rec;        -- Record with pasta information

   begin   -- Process_Sale
      -- Get a valid Pasta ID number from the user
      -- The maximum legal value of a pasta ID number is the last index
      -- in the direct file of pasta weights
      Get_Pasta_ID (Max_Value => Pasta_IO.Size (Pasta_File),
                    Pasta_ID  => Pasta_Number);
      -- Get a valid amount from the user
      Get_Pounds (Amount);

      -- Update the inventory

      -- Get the total amount in inventory
      Pasta_IO.Read (File => Pasta_File,
                     Item => Pasta_Info,
                     From => Pasta_Number);
      -- Calculate the new total
      Pasta_Info.Amount := Pasta_Info.Amount - Amount;    -- Update total first to catch
      -- Update Num_Sales info                            -- constraint error
      Pasta_Info.Num_Sales := Pasta_Info.Num_Sales + 1;
      -- Update Total_Sales info
      Pasta_Info.Total_Sales := Pasta_Info.Total_Sales + Amount;
      -- Put the new total back in the inventory file
      Pasta_IO.Write (File => Pasta_File,
                      Item => Pasta_Info,
                      To   => Pasta_Number);
   exception
      when CONSTRAINT_ERROR =>   -- Negative total
         Ada.Text_IO.Put_Line ("Not enough Pasta in stock for this sale!");
   end Process_Sale;

   ----------------------------------------------------------------------------

   procedure Process_Shipment (Pasta_File : in out Pasta_IO.File_Type) is

   -- This procedure gets factory production information from the user
   -- and adds it to the inventory
   --
   -- Preconditions  : None
   --
   -- Postconditions : Pasta_File is updated

      Pasta_Number : Pasta_ID_Type;    -- The pasta identification number
      Amount       : Shipping_Pounds;  -- Amount of pasta sold
      Pasta_Info   : Pasta_Rec;        -- Record with pasta information

   begin  -- Process Shipment
      -- Get the necessary data from the user
      -- The maximum legal value of a pasta ID number is the last index
      Get_Pasta_ID (Max_Value => Pasta_IO.Size (Pasta_File),
                    Pasta_ID  => Pasta_Number);
      -- Get a valid amount from the user
      Get_Pounds (Amount);

      -- Update the inventory

      -- Get the total amount in inventory
      Pasta_IO.Read (File => Pasta_File,
                     Item => Pasta_Info,
                     From => Pasta_Number);

      -- Calculate the new total
      Pasta_Info.Amount := Pasta_Info.Amount + Amount;
      -- Put the new total back in the inventory file
      Pasta_IO.Write (File => Pasta_File,
                      Item => Pasta_Info,
                      To   => Pasta_Number);
   exception
      when CONSTRAINT_ERROR =>   -- Inventory exceeded Warehouse_Weight'Last
         Ada.Text_IO.Put_Line ("Not enough room in warehouse for shipment!");
   end Process_Shipment;

   ----------------------------------------------------------------------------

   procedure Process_New (Pasta_File : in out Pasta_IO.File_Type) is

   -- This procedure determines and displays a unique identification
   -- number for a new pasta variety.  It also initializes the inventory
   -- of that variety to zero.
   --
   -- Preconditions  : None
   --
   -- Postconditions : Pasta_File is updated

      New_Pasta_Num : Pasta_ID_Type;  -- The ID number for the new variety
      Pasta_Info    : Pasta_Rec;      -- Record with pasta information
      Last          : Natural;        -- Last good character in description

   begin   -- Process_New
      -- The ID of the new pasta is one greater than
      -- the size of the inventory file
      New_Pasta_Num := Pasta_IO.Size (Pasta_File) + 1;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Please enter a description of the new pasta");
      Ada.Text_IO.Skip_Line;
      Ada.Text_IO.Get_Line (Item => Pasta_Info.Description,
                            Last => Last);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("The number assigned to the new pasta variety is ");
      Pasta_ID_IO.Put (Item  => New_Pasta_Num,
                       Width => 1);

      -- Initialize new pasta
      Pasta_Info.Description (Last + 1 .. Pasta_Info.Description'Last) := (others => ' '); -- pad with blanks
      Pasta_Info.Amount      := 0.0;
      Pasta_Info.Num_Sales   := 0;
      Pasta_Info.Total_Sales := 0.0;

      -- Write the new pasta to file
      Pasta_IO.Write (File => Pasta_File,
                      Item => Pasta_Info,
                      To   => New_Pasta_Num);
   end Process_New;

   ----------------------------------------------------------------------------

   procedure Process_Amount_Query (Pasta_File : in Pasta_IO.File_Type) is

   -- This procedure displays the amount of a particular pasta in stock
   --
   -- Preconditions  : None
   --
   -- Postconditions : Pasta amount is displayed on screen

      Pasta_Number : Pasta_ID_Type;    -- The pasta identification number
      Pasta_Info   : Pasta_Rec;        -- Record with pasta information

   begin   -- Process_Amount_Query
      -- Get the necessary data from the user
      -- The maximum legal value of a pasta ID number is the last index
      Get_Pasta_ID (Max_Value => Pasta_IO.Size (Pasta_File),
                    Pasta_ID  => Pasta_Number);

      -- Get the total amount in inventory
      Pasta_IO.Read (File => Pasta_File,
                     Item => Pasta_Info,
                     From => Pasta_Number);

      -- Display the total
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Pasta ID Number:");
      Ada.Text_IO.Set_Col (20);
      Pasta_ID_IO.Put (Item  => Pasta_Number,
                       Width => 1);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Description:");
      Ada.Text_IO.Set_Col (20);
      Ada.Text_IO.Put (Pasta_Info.Description);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Amount:");
      Ada.Text_IO.Set_Col (20);
      Ada.Float_Text_IO.Put (Item => Pasta_Info.Amount,
                             Fore => 1,
                             Aft  => 1,
                             Exp  => 0);
      Ada.Text_IO.Put (" pounds");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Number of Sales:");
      Ada.Text_IO.Set_Col (20);
      Ada.Integer_Text_IO.Put (Item  => Pasta_Info.Num_Sales,
                               Width => 1);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Total Sales:");
      Ada.Text_IO.Set_Col (20);
      Ada.Float_Text_IO.Put (Item => Pasta_Info.Total_Sales,
                             Fore => 1,
                             Aft  => 1,
                             Exp  => 0);
      Ada.Text_IO.Put (" pounds");

   end Process_Amount_Query;

   ---------------------------------------------------------------

   procedure Process_Total_Query (Pasta_File : in Pasta_IO.File_Type) is

   -- This procedure determines and displays the total amount of
   -- pasta in the warehouse
   --
   -- Preconditions  : None
   --
   -- Postconditions : Table of totals is displayed on screen

      Total_Amount      : Float;       -- The total amount of pasta in stock
      Total_Num_Sales   : Natural;     -- The total number of sales
      Total_Total_Sales : Pounds;      -- The total amount of sales in pounds
      Pasta_Info        : Pasta_Rec;   -- Record with pasta information
      Pasta_Num         : Positive;    -- Index number of pasta

   begin   -- Process_Total_Query
      Total_Amount      := 0.0;   -- Initialize sums
      Total_Num_Sales   := 0;
      Total_Total_Sales := 0.0;
      Pasta_IO.Set_Index (File => Pasta_File,  -- Initialize file index
                          To   => 1);

      -- Display Table Headers
      Ada.Text_IO.Put ("Pasta #  Description");
      Ada.Text_IO.Set_Col (32);
      Ada.Text_IO.Put ("Amount");
      Ada.Text_IO.Set_Col (42);
      Ada.Text_IO.Put ("Number of Sales");
      Ada.Text_IO.Set_Col (62);
      Ada.Text_IO.Put ("Pounds Sold");
      Ada.Text_IO.New_Line (Spacing => 2);

      -- Loop displays each type of pasta and its information and determines totals
      -- Each iteration, displays one kind of pasta and increments sum
      Pasta_Num := 1; -- Initialize Pasta_Num
      Display_Pasta_Loop :
      loop
         exit Display_Pasta_Loop when Pasta_IO.End_Of_File (File => Pasta_File);
         Pasta_IO.Read (File => Pasta_File,
                        Item => Pasta_Info);
         Ada.Integer_Text_IO.Put (Item  => Pasta_Num,
                                  Width => 7);
         Ada.Text_IO.Put ("  ");
         Ada.Text_IO.Put (Pasta_Info.Description);
         Ada.Text_IO.Set_Col (30);
         Ada.Float_Text_IO.Put (Item => Pasta_Info.Amount,
                                Fore => 6,
                                Aft  => 1,
                                Exp  => 0);
         Ada.Text_IO.Set_Col (52);
         Ada.Integer_Text_IO.Put (Item  => Pasta_Info.Num_Sales,
                                  Width => 5);
         Ada.Text_IO.Set_Col (62);
         Ada.Float_Text_IO.Put (Item => Pasta_Info.Total_Sales,
                                Fore => 9,
                                Aft  => 1,
                                Exp  => 0);

         -- Increment Pasta_Num
         Pasta_Num := Pasta_Num + 1;

         -- Increment Totals
         Total_Total_Sales := Total_Total_Sales + Pasta_Info.Total_Sales;
         Total_Num_Sales   := Total_Num_Sales + Pasta_Info.Num_Sales;
         Total_Amount      := Total_Amount + Pasta_Info.Amount;

         Ada.Text_IO.New_Line;
      end loop Display_Pasta_Loop;

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Set_Col (10);
      Ada.Text_IO.Put ("TOTALS");
      Ada.Text_IO.Set_Col (30);
      Ada.Float_Text_IO.Put (Item => Total_Amount,
                             Fore => 6,
                             Aft  => 1,
                             Exp  => 0);
      Ada.Text_IO.Set_Col (52);
      Ada.Integer_Text_IO.Put (Item  => Total_Num_Sales,
                               Width => 5);
      Ada.Text_IO.Set_Col (62);
      Ada.Float_Text_IO.Put (Item => Total_Total_Sales,
                             Fore => 9,
                             Aft  => 1,
                             Exp  => 0);

   end Process_Total_Query;

   ----------------------------------------------------------------------------

   procedure Process_Transaction
         (Which_Transaction : in     Transaction_Type;      -- Which kind
          Inventory_File    : in out Pasta_IO.File_Type) is -- Inventory file

   -- This procedure carries out the appropriate actions
   -- for the given type of transaction.
   --
   -- Preconditions  : None
   --
   -- Postconditions : Proper subprogram is called

   begin   -- Process_Transaction
      case Which_Transaction is
         when Sale =>
            Process_Sale (Inventory_File);
         when Shipment =>
            Process_Shipment (Inventory_File);
         when New_Pasta =>
            Process_New (Inventory_File);
         when Amount =>
            Process_Amount_Query (Inventory_File);
         when Total =>
            Process_Total_Query (Inventory_File);
         when Done =>
            null;
      end case;
   end Process_Transaction;

-------------------------------------------------------------------------------

   Transaction       : Transaction_Type;    -- Type of transaction being processed
   Transaction_Count : Natural;             -- Transaction number
   Pasta_File        : Pasta_IO.File_Type;  -- Pasta inventory totals

begin  -- Program Pasta

   Prepare_File (Pasta_File);

   -- Loop processes all of the user's transactions
   -- Each iteration, one transaction is processed
   Transaction_Count := 1;   -- Initialize loop variable
   Transaction_Loop :
   loop
      Ada.Text_IO.New_Line (Spacing => 2);
      Ada.Text_IO.Put ("----------Transaction #");
      Ada.Integer_Text_IO.Put (Item  => Transaction_Count,
                               Width => 1);
      Ada.Text_IO.Put ("----------");
      Get_Transaction (File        => Pasta_File,
                       Transaction => Transaction);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Processing ");
      Transaction_IO.Put (Transaction);
      Ada.Text_IO.New_Line (Spacing => 2);
      exit Transaction_Loop when Transaction = Done;
      Process_Transaction (Which_Transaction => Transaction,
                           Inventory_File    => Pasta_File);
      Transaction_Count := Transaction_Count + 1; -- Increment loop variable
   end loop Transaction_Loop;
   -- Close the file
   Pasta_IO.Close (Pasta_File);
exception
   when others =>                   -- If any exception is not handled where
      Pasta_IO.Close (Pasta_File);  -- it occurs, close the file and propagate
      raise;                        -- the exception back to the system.
end Pasta;