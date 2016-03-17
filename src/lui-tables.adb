package body Lui.Tables is

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

   ----------------------
   -- Contents_Changed --
   ----------------------

   function Contents_Changed
     (Table : in out Root_Model_Table)
      return Boolean
   is
      use Ada.Strings.Unbounded;
      Changed : Boolean := False;
   begin
      if Root_Model_Table'Class (Table).Layout_Changed then
         return True;
      end if;

      if Table.Cache = null then
         Table.Cache :=
           new Contents_Cache (1 .. Table.Row_Count, 1 .. Table.Col_Count);
      end if;

      for Row in 1 .. Table.Row_Count loop
         for Col in 1 .. Table.Col_Count loop
            declare
               Text : constant String :=
                        Root_Model_Table'Class (Table).Cell_Text (Row, Col);
            begin
               if not Changed and then Text /= Table.Cache (Row, Col) then
                  Changed := True;
               end if;

               if Changed then
                  Table.Cache (Row, Col) := To_Unbounded_String (Text);
               end if;
            end;
         end loop;
      end loop;

      return Changed;
   end Contents_Changed;

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

   --------------------
   -- Layout_Changed --
   --------------------

   function Layout_Changed
     (Table : Root_Model_Table)
      return Boolean
   is
   begin
      return Table.First;
   end Layout_Changed;

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
