package body Lui.Handles is

   Local_Current_UI : UI_Handle;

   ----------------
   -- Current_UI --
   ----------------

   function Current_UI return UI_Handle is
   begin
      return Local_Current_UI;
   end Current_UI;

   ---------------
   -- Pop_Model --
   ---------------

   procedure Pop_Model
     (Handle : in out Root_UI_Handle'Class;
      Model  : out Lui.Models.Object_Model)
   is
   begin
      if Handle.Stack.Is_Empty then
         raise Constraint_Error with
           "Lui.Handles.Pop_Model: model stack was empty";
      end if;

      Model := Handle.Stack.Last_Element;
      Handle.Stack.Delete_Last;
   end Pop_Model;

   ----------------
   -- Push_Model --
   ----------------

   procedure Push_Model
     (Handle : in out Root_UI_Handle'Class;
      Model  : Lui.Models.Object_Model)
   is
   begin
      Handle.Stack.Append (Model);
   end Push_Model;

   -----------------
   -- Set_Current --
   -----------------

   procedure Set_Current (UI : not null access Root_UI_Handle'Class) is
   begin
      Local_Current_UI := UI_Handle (UI);
   end Set_Current;

end Lui.Handles;
