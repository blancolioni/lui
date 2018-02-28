with WL.Bitmap_IO;
with WL.Images;

with Lui.Colors;

package Lui.Rendering is

   procedure Set_Image_Path
     (Path : String);

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

   procedure Push_Viewport
     (Renderer : in out Root_Renderer;
      Viewport : Layout_Rectangle)
   is abstract;

   procedure Pop_Viewport
     (Renderer : in out Root_Renderer)
   is abstract;

   procedure Set_Color
     (Renderer : in out Root_Renderer;
      Color    : Lui.Colors.Color_Type)
   is abstract;

   procedure Set_Color
     (Renderer : in out Root_Renderer'Class;
      R, G, B  : Unit_Real;
      Alpha    : Unit_Real := 1.0);

   procedure Set_Font
     (Renderer : in out Root_Renderer;
      Name     : String;
      Size     : Real;
      Bold     : Boolean := False;
      Italic   : Boolean := False)
   is abstract;

   procedure Set_Line_Width
     (Renderer : in out Root_Renderer;
      Width    : Positive_Real)
   is abstract;

   procedure Circle
     (Renderer   : in out Root_Renderer;
      X, Y       : in     Integer;
      Radius     : in     Positive;
      Filled     : in     Boolean)
   is abstract;

   procedure Ellipse
     (Renderer   : in out Root_Renderer;
      X, Y       : in     Integer;
      R1, R2     : in     Positive;
      Filled     : in     Boolean)
   is abstract;

   procedure Rectangle
     (Renderer : in out Root_Renderer'Class;
      X, Y     : in     Integer;
      W, H     : in     Natural;
      Filled   : in     Boolean);

   procedure Rectangle
     (Renderer : in out Root_Renderer;
      Rec      : Layout_Rectangle;
      Filled   : in     Boolean)
   is abstract;

   procedure Line
     (Renderer   : in out Root_Renderer;
      X1, Y1     : in     Integer;
      X2, Y2     : in     Integer)
   is abstract;

   procedure Polygon
     (Renderer : in out Root_Renderer;
      Vertices : Buffer_Points;
      Filled   : Boolean)
   is abstract;

   procedure Text
     (Renderer : in out Root_Renderer;
      X, Y     : in     Integer;
      Value    : in     String)
   is abstract;

   procedure Image
     (Renderer : in out Root_Renderer;
      Rec      : Layout_Rectangle;
      Resource : String)
   is abstract;

   procedure Create_Bitmap_Resource
     (Renderer      : in out Root_Renderer;
      Resource_Name : in     String;
      Bitmap        : in     WL.Bitmap_IO.Bitmap_Type)
   is abstract;

   procedure Create_Image_Resource
     (Renderer      : in out Root_Renderer;
      Resource_Name : in     String;
      Image         : WL.Images.Image_Type'Class)
   is abstract;

   function Have_Resource
     (Renderer      : Root_Renderer;
      Resource_Name : String)
      return Boolean
      is abstract;

end Lui.Rendering;
