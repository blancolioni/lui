package Lui is

   subtype Real is Long_Float;

   subtype Positive_Real is Real range 0.0 .. Real'Last;
   subtype Unit_Real is Real range 0.0 .. 1.0;

   type Render_Layer is range 1 .. 16;

   type Lui_UI_Feature is (UI_Model, UI_Table, UI_Gadget);

   type Root_UI_Element is interface;

   type Lui_UI_Element is access all Root_UI_Element'Class;

   function Approximate_Image
     (Value : Real)
      return String;

   function Approximate_Image
     (Value : Integer)
      return String;

   type Layout_Rectangle is
      record
         X, Y          : Integer;
         Width, Height : Natural;
      end record;

   function Contains
     (Rectangle : Layout_Rectangle;
      X, Y      : Integer)
      return Boolean
   is (X in Rectangle.X .. Rectangle.X + Rectangle.Width
       and then Y in Rectangle.Y .. Rectangle.Y + Rectangle.Height);

end Lui;
