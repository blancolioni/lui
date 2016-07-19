package Lui.Colours is

   type Colour_Byte is mod 256;

   type Colour_Type is
      record
         Red, Green, Blue : Unit_Real;
         Alpha            : Unit_Real;
      end record;

   function To_Colour (Red, Green, Blue : Colour_Byte) return Colour_Type;

   function Apply_Alpha
     (Colour : Colour_Type;
      Alpha  : Unit_Real)
      return Colour_Type;

   function Brighten
     (Colour : Colour_Type;
      Factor : Unit_Real)
      return Colour_Type;

   Black : constant Colour_Type := (0.0, 0.0, 0.0, 1.0);
   White : constant Colour_Type := (1.0, 1.0, 1.0, 1.0);

end Lui.Colours;
