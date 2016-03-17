limited with Lui.Models;
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

package Lui.Tables is

   type Root_Model_Table is abstract new Root_UI_Element with private;

   function Name (Table : Root_Model_Table) return String;
   function Row_Count (Table : Root_Model_Table) return Natural;
   function Column_Count (Table : Root_Model_Table) return Natural;

   function Cell_Text (Table : Root_Model_Table;
                       Row  : Positive;
                       Column : Positive)
                       return String
                       is abstract;

   function Parent_Row (Table : Root_Model_Table;
                        Row   : Positive)
                        return Natural
                        is (0);

   function Heading_Column_Text (Table  : Root_Model_Table;
                                 Column : Positive) return String
                                 is abstract;

   procedure Initialise (Item     : in out Root_Model_Table'Class;
                         Name     : in     String;
                         Num_Rows : in     Natural;
                         Num_Cols : in     Natural);

   procedure Select_Row (Item : Root_Model_Table;
                         Row  : Positive)
   is null;

   function Row_Model
     (Item : Root_Model_Table;
      Row  : Positive)
      return access Lui.Models.Root_Object_Model'Class
   is (null);

   function Contents_Changed
     (Table : in out Root_Model_Table)
      return Boolean;

   function Cell_Changed
     (Table    : in out Root_Model_Table;
      Row, Col : Positive)
      return Boolean;

   function Layout_Changed
     (Table : Root_Model_Table)
      return Boolean;

   procedure Clear_Changed
     (Table : in out Root_Model_Table);

   type Model_Table is access all Root_Model_Table'Class;

   type Array_Of_Model_Tables is
     array (Positive range <>) of Model_Table;

   function No_Tables return Array_Of_Model_Tables;

private

   type Contents_Cache is
     array (Positive range <>, Positive range <>) of
     Ada.Strings.Unbounded.Unbounded_String;

   type Change_Record is
      record
         Row, Column : Positive;
      end record;

   package List_Of_Changes is
     new Ada.Containers.Doubly_Linked_Lists (Change_Record);

   type Root_Model_Table is abstract new Root_UI_Element with
      record
         Name          : access String;
         First         : Boolean := True;
         Row_Count     : Natural;
         Col_Count     : Natural;
         Cache         : access Contents_Cache;
         Changes       : List_Of_Changes.List;
      end record;

end Lui.Tables;
