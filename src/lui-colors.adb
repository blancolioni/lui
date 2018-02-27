package body Lui.Colors is

   -----------------
   -- Apply_Alpha --
   -----------------

   function Apply_Alpha
     (Color : Color_Type;
      Alpha  : Unit_Real)
      return Color_Type
   is
   begin
      return Result : Color_Type := Color do
         Result.Alpha := Alpha;
      end return;
   end Apply_Alpha;

   --------------
   -- Brighten --
   --------------

   function Brighten
     (Color : Color_Type;
      Factor : Unit_Real)
      return Color_Type
   is
      function Apply (X : Unit_Real) return Unit_Real
      is (X + (1.0 - X) * Factor);

   begin
      return (Apply (Color.Red), Apply (Color.Green), Apply (Color.Blue),
              Color.Alpha);
   end Brighten;

   ---------------
   -- To_Color --
   ---------------

   function To_Color (Red, Green, Blue : Color_Byte) return Color_Type is
   begin
      return (Real (Red) / 255.0, Real (Green) / 255.0, Real (Blue) / 255.0,
              1.0);
   end To_Color;

end Lui.Colors;
