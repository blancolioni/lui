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

   ---------------
   -- Rectangle --
   ---------------

   procedure Rectangle
     (Renderer : in out Root_Renderer'Class;
      X, Y     : in     Integer;
      W, H     : in     Natural;
      Filled   : in     Boolean)
   is
   begin
      Renderer.Rectangle ((X, Y, W, H), Filled);
   end Rectangle;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Renderer : in out Root_Renderer'Class;
      R, G, B  : Unit_Real)
   is
   begin
      Renderer.Set_Color ((R, G, B, 1.0));
   end Set_Color;

   --------------------
   -- Set_Image_Path --
   --------------------

   procedure Set_Image_Path
     (Path : String)
   is
   begin
      Local_Image_Path :=
        Ada.Strings.Unbounded.To_Unbounded_String (Path);
   end Set_Image_Path;

end Lui.Rendering;
