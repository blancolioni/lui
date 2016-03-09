with Ada.Strings.Fixed;

package body Lui is

   function Significant_Digits_Image (Value : Positive_Real;
                                      Sig   : Positive)
                                      return String;

   -----------------------
   -- Approximate_Image --
   -----------------------

   function Approximate_Image
     (Value : Natural)
      return String
   is
      Factors    : constant array (1 .. 3) of Natural :=
        (1E9, 1E6, 1E3);
      Extensions : constant String := "GMK";

      function Image (Q : Natural) return String;

      -----------
      -- Image --
      -----------

      function Image (Q : Natural) return String is
         Result : constant String := Natural'Image (Q);
      begin
         return Result (2 .. Result'Last);
      end Image;

   begin
      for I in Factors'Range loop
         if Value > Factors (I) then
            return Image (Value / Factors (I)) &
            (1 => Extensions (I));
         end if;
      end loop;
      return Image (Value);
   end Approximate_Image;

   -----------------------
   -- Approximate_Image --
   -----------------------

   function Approximate_Image (Value : Real) return String is
      Factors    : constant array (1 .. 3) of Real :=
        (1.0E9, 1.0E6, 1.0E3);
      Extensions : constant String := "GMK";
   begin
      if Value < 0.0 then
         return "-" & Approximate_Image (-Value);
      else
         for I in Factors'Range loop
            if Value > Factors (I) then
               return Significant_Digits_Image (Value / Factors (I), 3) &
               (1 => Extensions (I));
            end if;
         end loop;
         return Significant_Digits_Image (Value, 3);
      end if;
   end Approximate_Image;

   ------------------------------
   -- Significant_Digits_Image --
   ------------------------------

   function Significant_Digits_Image (Value : Positive_Real;
                                      Sig   : Positive)
                                     return String
   is
      Result    : String (1 .. Sig);
      Point     : Natural := 0;
      Acc       : Real := Value;
      Boundary  : constant Real := 10.0**Sig;
   begin
      if Value < 1.0 / Boundary then
         return "0.00";
      end if;

      if Value >= Boundary then
         if Value >= 1.0e6 then
            return Ada.Strings.Fixed.Trim (Positive_Real'Image (Value),
                                           Ada.Strings.Left);
         else
            return Ada.Strings.Fixed.Trim (Integer'Image (Integer (Value)),
                                           Ada.Strings.Left);
         end if;
      else
         while abs Acc * 10.0 < Boundary loop
            Acc := Acc * 10.0;
            Point := Point + 1;
         end loop;

         Result :=
           Ada.Strings.Fixed.Trim (Integer'Image (Integer (Acc - 0.5)),
                                   Ada.Strings.Left);
         if Point < Sig then
            if Point = 0 then
               return Result;
            else
               return Result (1 .. Result'Last - Point) & "." &
                 Result (Result'Last - Point + 1 .. Result'Last);
            end if;
         else
            declare
               Zeroes : constant String (1 .. Point - Sig) :=
                 (others => '0');
            begin
               return "0." & Zeroes & Result;
            end;
         end if;
      end if;
   end Significant_Digits_Image;

end Lui;
