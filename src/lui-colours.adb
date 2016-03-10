package body Lui.Colours is

   ---------------
   -- To_Colour --
   ---------------

   function To_Colour (Red, Green, Blue : Colour_Byte) return Colour_Type is
   begin
      return (Real (Red) / 255.0, Real (Green) / 255.0, Real (Blue) / 255.0,
              1.0);
   end To_Colour;

end Lui.Colours;
