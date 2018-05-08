package body Lui.Colors.Config is

   ----------------------
   -- Configure_Color --
   ----------------------

   function Configure_Color
     (Config     : Tropos.Configuration;
      Child_Name : String := "")
      return Color_Type
   is
      Child : constant Tropos.Configuration :=
                (if Child_Name = ""
                 then Config
                 elsif Config.Contains (Child_Name)
                 then Config.Child (Child_Name)
                 elsif Config.Contains ("color")
                 then Config.Child ("color")
                 elsif Config.Contains ("colour")
                 then Config.Child ("colour")
                 else Config);

      R     : constant Float := Child.Get (1);
      G     : constant Float := Child.Get (2);
      B     : constant Float := Child.Get (3);

   begin

      if R <= 1.0 and then G <= 1.0 and then B <= 1.0 then
         return (Unit_Real (R), Unit_Real (G), Unit_Real (B), 1.0);
      else
         return To_Color (Color_Byte (R), Color_Byte (G), Color_Byte (B));
      end if;
   end Configure_Color;

end Lui.Colors.Config;
