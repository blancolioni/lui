with Gtk.Widget;

with Lui.Models;

package Lui.Gtk_UI is

   type Lui_Gtk_Interface is interface;

   procedure Append_Feature
     (To      : in out Lui_Gtk_Interface;
      Feature : Lui_UI_Feature;
      Element : not null access Root_UI_Element'Class;
      Top     : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is abstract;

   procedure Select_Feature
     (To      : in out Lui_Gtk_Interface;
      Feature : Lui_UI_Feature;
      Element : not null access Root_UI_Element'Class;
      Top     : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is abstract;

   procedure Clear_Features
     (To      : in out Lui_Gtk_Interface;
      Feature : Lui_UI_Feature)
   is abstract;

   procedure On_Idle
     (Item : in out Lui_Gtk_Interface)
   is null;

   type Lui_Gtk is access all Lui_Gtk_Interface'Class;

   procedure Start
     (Main : not null access Lui_Gtk_Interface'Class;
      Top  : Lui.Models.Object_Model);

   procedure On_Model_Activation (Model : Lui.Models.Object_Model);

   procedure On_Model_Changed (Model : Lui.Models.Object_Model);

end Lui.Gtk_UI;
