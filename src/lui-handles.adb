package body Lui.Handles is

   Local_Current_UI : UI_Handle;

   ----------------
   -- Current_UI --
   ----------------

   function Current_UI return UI_Handle is
   begin
      return Local_Current_UI;
   end Current_UI;

   -----------------
   -- Set_Current --
   -----------------

   procedure Set_Current (UI : not null access Root_UI_Handle'Class) is
   begin
      Local_Current_UI := UI_Handle (UI);
   end Set_Current;

end Lui.Handles;
