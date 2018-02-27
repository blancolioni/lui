with Tropos;

package Lui.Colors.Config is

   function Configure_Color
     (Config     : Tropos.Configuration;
      Child_Name : String := "")
      return Color_Type;

end Lui.Colors.Config;
