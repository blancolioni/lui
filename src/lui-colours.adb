package body Lui.Colours is

   -----------------
   -- Apply_Alpha --
   -----------------

   function Apply_Alpha
     (Colour : Colour_Type;
      Alpha  : Unit_Real)
      return Colour_Type
   is
   begin
      return Result : Colour_Type := Colour do
         Result.Alpha := Alpha;
      end return;
   end Apply_Alpha;

   ---------------
   -- To_Colour --
   ---------------

   function To_Colour (Red, Green, Blue : Colour_Byte) return Colour_Type is
   begin
      return (Real (Red) / 255.0, Real (Green) / 255.0, Real (Blue) / 255.0,
              1.0);
   end To_Colour;

end Lui.Colours;
