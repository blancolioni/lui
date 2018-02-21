private with Ada.Containers.Doubly_Linked_Lists;

with Lui.Models;

package Lui.Handles is

   type Root_UI_Handle is abstract tagged private;

   procedure Show_Model
     (UI    : in out Root_UI_Handle;
      Model : Lui.Models.Object_Model)
   is abstract;

   procedure Push_Model
     (Handle : in out Root_UI_Handle'Class;
      Model  : Lui.Models.Object_Model);

   procedure Pop_Model
     (Handle : in out Root_UI_Handle'Class;
      Model  : out Lui.Models.Object_Model);

   type UI_Handle is access all Root_UI_Handle'Class;

   function Current_UI return UI_Handle;

   procedure Set_Current (UI : not null access Root_UI_Handle'Class);

private

   package Model_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Lui.Models.Object_Model, Lui.Models."=");

   type Root_UI_Handle is abstract tagged
      record
         Stack : Model_Lists.List;
      end record;

end Lui.Handles;
