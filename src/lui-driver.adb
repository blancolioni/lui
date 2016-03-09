with Gtk.Main;

with Lui.Gtk_UI;

with Lui.Sample_Model;
with Lui.Sample_UI;

with Lui.Models.Model_3D;
pragma Unreferenced (Lui.Models.Model_3D);

with Lui.Models.Boxes;
pragma Unreferenced (Lui.Models.Boxes);

procedure Lui.Driver is

begin
   Lui.Gtk_UI.Start
     (Lui.Sample_UI.Create_Sample_UI,
      Lui.Sample_Model.Create_Sample_Model);

   Gtk.Main.Main;
end Lui.Driver;
