with Ada.Text_IO;

package body Lui.Tables is

   ------------------
   -- Cell_Changed --
   ------------------

   function Cell_Changed
     (Table    : in out Root_Model_Table;
      Row, Col : Positive)
      return Boolean
   is
   begin
      for Change of Table.Changes loop
         if Change.Row = Row and then Change.Column = Col then
            return True;
         end if;
      end loop;
      return False;
   end Cell_Changed;

   -------------------
   -- Clear_Changed --
   -------------------

   procedure Clear_Changed
     (Table : in out Root_Model_Table)
   is
   begin
      Table.First := False;
      Table.Changes.Clear;
   end Clear_Changed;

   ------------------
   -- Column_Count --
   ------------------

   function Column_Count (Table : Root_Model_Table) return Natural is
   begin
      return Table.Table_Col_Count;
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
      Rows    : constant Natural :=
                  Root_Model_Table'Class (Table).Row_Count;
      Cols    : constant Natural :=
                  Root_Model_Table'Class (Table).Column_Count;
   begin
      if Root_Model_Table'Class (Table).Layout_Changed then
         return True;
      end if;

      if Table.Cache = null then
         Table.Cache :=
           new Contents_Cache (1 .. Rows, 1 .. Cols);
      end if;

      for Row in 1 .. Rows loop
         for Col in 1 .. Cols loop
            declare
               Text : constant String :=
                        Root_Model_Table'Class (Table).Cell_Text (Row, Col);
            begin
               if not Changed and then Text /= Table.Cache (Row, Col) then
                  Changed := True;
               end if;

               if Changed then
                  Table.Cache (Row, Col) := To_Unbounded_String (Text);
                  Table.Changes.Append ((Row, Col));
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
      Item.Table_Row_Count := Num_Rows;
      Item.Table_Col_Count := Num_Cols;
   end Initialise;

   --------------------
   -- Layout_Changed --
   --------------------

   function Layout_Changed
     (Table : in out Root_Model_Table)
      return Boolean
   is
      procedure Report_Reason (Reason : String);

      -------------------
      -- Report_Reason --
      -------------------

      procedure Report_Reason (Reason : String) is
      begin
         Ada.Text_IO.Put_Line
           (Table.Name.all
            & ": layout-change: " & Reason);
      end Report_Reason;

   begin
      if Table.First then
         Report_Reason ("first render");
         return True;
      elsif Table.Cache = null then
         return False;
      else
         declare
            Cache_Rows : constant Natural := Table.Cache'Length (1);
            Cache_Cols : constant Natural := Table.Cache'Length (2);
            New_Rows   : constant Natural :=
                           Root_Model_Table'Class (Table).Row_Count;
            New_Cols   : constant Natural :=
                           Root_Model_Table'Class (Table).Column_Count;
         begin
            if Cache_Rows /= New_Rows then
               Report_Reason ("rows changed");
               Table.Cache := null;
               return True;
            elsif Cache_Cols /= New_Cols then
               Report_Reason ("cols changed");
               Table.Cache := null;
               return True;
            else
               return False;
            end if;
         end;
      end if;

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
      return Table.Table_Row_Count;
   end Row_Count;

end Lui.Tables;
