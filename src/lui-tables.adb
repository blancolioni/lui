package body Lui.Tables is

   -------------
   -- Changed --
   -------------

   function Changed
     (Table : Root_Model_Table)
      return Boolean
   is
   begin
      return Table.First;
   end Changed;

   -------------------
   -- Clear_Changed --
   -------------------

   procedure Clear_Changed
     (Table : in out Root_Model_Table)
   is
   begin
      Table.First := False;
   end Clear_Changed;

   ------------------
   -- Column_Count --
   ------------------

   function Column_Count (Table : Root_Model_Table) return Natural is
   begin
      return Table.Col_Count;
   end Column_Count;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (Item     : in out Root_Model_Table'Class;
                         Name     : in     String;
                         Num_Rows : in     Natural;
                         Num_Cols : in     Natural)
   is
   begin
      Item.Name := new String'(Name);
      Item.Row_Count := Num_Rows;
      Item.Col_Count := Num_Cols;
   end Initialise;

   ----------
   -- Name --
   ----------

   function Name (Table : Root_Model_Table) return String is
   begin
      return Table.Name.all;
   end Name;

   ---------------
   -- No_Tables --
   ---------------

   function No_Tables return Array_Of_Model_Tables is
   begin
      return Result : Array_Of_Model_Tables (1 .. 0) do
         null;
      end return;
   end No_Tables;

   ---------------
   -- Row_Count --
   ---------------

   function Row_Count (Table : Root_Model_Table) return Natural is
   begin
      return Table.Row_Count;
   end Row_Count;

end Lui.Tables;
