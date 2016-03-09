with Lui.Models;

package Lui.Handles is

   type Root_UI_Handle is abstract tagged private;

   procedure Show_Model
     (UI    : in out Root_UI_Handle;
      Model : Lui.Models.Object_Model)
   is abstract;

   type UI_Handle is access all Root_UI_Handle'Class;

   function Current_UI return UI_Handle;

   procedure Set_Current (UI : not null access Root_UI_Handle'Class);

private

   type Root_UI_Handle is abstract tagged null record;

end Lui.Handles;
