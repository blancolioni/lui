with Tropos;

package Lui.Colours.Config is

   function Configure_Colour
     (Config     : Tropos.Configuration;
      Child_Name : String := "")
      return Colour_Type;

end Lui.Colours.Config;
