package Lui.Colours is

   type Colour_Byte is mod 256;

   type Colour_Type is
      record
         Red, Green, Blue : Unit_Real;
         Alpha            : Unit_Real;
      end record;

   function To_Colour (Red, Green, Blue : Colour_Byte) return Colour_Type;

   Black : constant Colour_Type := (0.0, 0.0, 0.0, 1.0);
   White : constant Colour_Type := (1.0, 1.0, 1.0, 1.0);

end Lui.Colours;
