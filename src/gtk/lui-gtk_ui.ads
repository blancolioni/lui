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

   procedure Status_Message
     (To      : in out Lui_Gtk_Interface;
      Message : String)
   is abstract;

   type Lui_Gtk is access all Lui_Gtk_Interface'Class;

   procedure Start
     (Main : Lui_Gtk;
      Top  : Lui.Models.Object_Model);

end Lui.Gtk_UI;
