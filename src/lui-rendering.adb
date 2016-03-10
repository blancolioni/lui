with Ada.Strings.Unbounded;

package body Lui.Rendering is

   Local_Image_Path : Ada.Strings.Unbounded.Unbounded_String;

   ----------------
   -- Image_Path --
   ----------------

   function Image_Path
     (Renderer        : Root_Renderer'Class;
      Image_File_Name : String)
      return String
   is
      pragma Unreferenced (Renderer);
   begin
      return Ada.Strings.Unbounded.To_String (Local_Image_Path)
        & "/" & Image_File_Name;
   end Image_Path;

end Lui.Rendering;
