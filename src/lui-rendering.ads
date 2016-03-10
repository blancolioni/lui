with WL.Bitmap_IO;

with Lui.Colours;

package Lui.Rendering is

   type Buffer_Point_Type is
      record
         X, Y : Integer;
      end record;

   type Buffer_Points is array (Positive range <>) of Buffer_Point_Type;

   type Root_Renderer is interface;

   function Image_Path
     (Renderer        : Root_Renderer'Class;
      Image_File_Name : String)
      return String;

   procedure Set_Origin
     (Renderer : in out Root_Renderer;
      X, Y     : in     Integer)
   is abstract;

   procedure Draw_Circle (Renderer   : in out Root_Renderer;
                          X, Y       : in     Integer;
                          Radius     : in     Positive;
                          Colour     : in     Lui.Colours.Colour_Type;
                          Filled     : in     Boolean;
                          Line_Width : in Natural := 1)
   is abstract;

   procedure Draw_Rectangle
     (Renderer : in out Root_Renderer;
      X, Y     : in     Integer;
      W, H     : in     Natural;
      Colour   : in     Lui.Colours.Colour_Type;
      Filled   : in     Boolean)
   is abstract;

   procedure Draw_Line
     (Renderer : in out Root_Renderer;
      X1, Y1   : in     Integer;
      X2, Y2   : in     Integer;
      Colour   : in     Lui.Colours.Colour_Type)
   is abstract;

   procedure Draw_Polygon
     (Renderer : in out Root_Renderer;
      Vertices : Buffer_Points;
      Colour   : Lui.Colours.Colour_Type;
      Filled   : Boolean)
   is abstract;

   procedure Draw_String (Renderer : in out Root_Renderer;
                          X, Y     : in     Integer;
                          Size     : in     Positive;
                          Colour   : in     Lui.Colours.Colour_Type;
                          Text     : in     String)
   is abstract;

   procedure Draw_Image (Renderer : in out Root_Renderer;
                         X, Y     : in     Integer;
                         W, H     : in     Positive;
                         Resource : in     String)
   is abstract;

   procedure Create_Bitmap_Resource
     (Renderer      : in out Root_Renderer;
      Resource_Name : in     String;
      Bitmap        : in     WL.Bitmap_IO.Bitmap_Type)
   is abstract;

   function Have_Resource
     (Renderer      : Root_Renderer;
      Resource_Name : String)
      return Boolean
      is abstract;

end Lui.Rendering;
