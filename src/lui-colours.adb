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

   --------------
   -- Brighten --
   --------------

   function Brighten
     (Colour : Colour_Type;
      Factor : Unit_Real)
      return Colour_Type
   is
      function Apply (X : Unit_Real) return Unit_Real
      is (X + (1.0 - X) * Factor);

   begin
      return (Apply (Colour.Red), Apply (Colour.Green), Apply (Colour.Blue),
              Colour.Alpha);
   end Brighten;

   ---------------
   -- To_Colour --
   ---------------

   function To_Colour (Red, Green, Blue : Colour_Byte) return Colour_Type is
   begin
      return (Real (Red) / 255.0, Real (Green) / 255.0, Real (Blue) / 255.0,
              1.0);
   end To_Colour;

end Lui.Colours;
