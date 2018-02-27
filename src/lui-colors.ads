package Lui.Colors is

   type Color_Byte is mod 256;

   type Color_Type is
      record
         Red, Green, Blue : Unit_Real;
         Alpha            : Unit_Real;
      end record;

   function To_Color (Red, Green, Blue : Color_Byte) return Color_Type;

   function Apply_Alpha
     (Color : Color_Type;
      Alpha  : Unit_Real)
      return Color_Type;

   function Brighten
     (Color : Color_Type;
      Factor : Unit_Real)
      return Color_Type;

   Black : constant Color_Type := (0.0, 0.0, 0.0, 1.0);
   White : constant Color_Type := (1.0, 1.0, 1.0, 1.0);

end Lui.Colors;
