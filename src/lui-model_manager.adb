with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash;

package body Lui.Model_Manager is

   package Model_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Object_Model);

   package Model_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Model_Lists.List,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=",
        "="             => Model_Lists."=");

   Map : Model_Maps.Map;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
     (From : Conflict.Db.Record_Interface'Class)
      return Object_Model
   is
      Key : constant String := From.Identity;
   begin
      if Map.Contains (Key) then
         return Map.Element (Key).First_Element;
      else
         return null;
      end if;
   end Get_Model;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
     (Main_Object : Conflict.Db.Record_Interface'Class;
      Sub_Object  : Conflict.Db.Record_Interface'Class)
      return Object_Model
   is
      Key : constant String := Main_Object.Identity;
   begin
      if Map.Contains (Key) then
         for Item of Map.Element (Key) loop
            if Item.Match (Sub_Object) then
               return Item;
            end if;
         end loop;
      end if;

      return null;
   end Get_Model;
   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
     (Object : Conflict.Db.Record_Interface'Class;
      Model  : Object_Model)
   is
      Key : constant String := Object.Identity;
      Element : Model_Lists.List;
   begin
      Element.Append (Model);
      Map.Insert (Key, Element);
   end Set_Model;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
     (Main_Object : Conflict.Db.Record_Interface'Class;
      Sub_Object  : Conflict.Db.Record_Interface'Class;
      Model       : Object_Model)
   is
      Key : constant String := Main_Object.Identity;
      Element : Model_Lists.List :=
                  (if Map.Contains (Key)
                   then Map.Element (Key)
                   else Model_Lists.Empty_List);
   begin
      Element.Append (Model);
      if Map.Contains (Key) then
         Map.Replace (Key, Element);
      else
         Map.Insert (Key, Element);
      end if;
   end Set_Model;

end Lui.Model_Manager;
