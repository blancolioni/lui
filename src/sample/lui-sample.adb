package body Lui.Sample is

   -----------
   -- Start --
   -----------

   procedure Start is
      Builder : Gtk.Builder.Gtk_Builder;
      UI_Path : constant String :=
                  Conflict.Paths.Config_Path & "/Lui";
   begin

      Gtk.Main.Init;

      Gtk.Builder.Gtk_New (Builder);

      State := new Root_UI_State;
      State.Set_Current;

      Ada.Text_IO.Put_Line ("Loading: " & UI_Path);

      declare
         use type Glib.Guint;
         Error : aliased Glib.Error.GError;
         Result : constant Glib.Guint :=
                    Builder.Add_From_File
                      (Filename => UI_Path,
                       Error    => Error'Access);
      begin
         if Result = 0 then
            raise Program_Error with
              "Error opening GUI definition: " & UI_Path
              & ": " & Glib.Error.Get_Message (Error);
         end if;
      end;

      Ada.Text_IO.Put_Line ("done");

      declare
         Main_Window : constant Gtk.Window.Gtk_Window :=
                         Gtk.Window.Gtk_Window
                           (Builder.Get_Object ("Conflict_Main_Window"));
      begin
         Main_Window_Callback.Connect
           (Main_Window,
            "destroy",
            Main_Window_Callback.To_Marshaller (Destroy_Handler'Access));
         Main_Window.Maximize;
         Main_Window.Show_All;
      end;

      Info_Box := Gtk.Box.Gtk_Box (Builder.Get_Object ("Info_Vbox"));
      Gadget_Box := Gtk.Box.Gtk_Box (Builder.Get_Object ("Gadget_Box"));

      Main_Status_Bar :=
        Gtk.Status_Bar.Gtk_Status_Bar
          (Builder.Get_Object ("Status_Bar"));
         Current_Model_Context :=
           Main_Status_Bar.Get_Context_Id ("star mouseover");

      State.Model_List.Notebook :=
        Gtk.Notebook.Gtk_Notebook
          (Builder.Get_Object ("Main_Tab"));
      State.Model_List.Notebook.Remove_Page (0);

      State.Model_List.Notebook.On_Switch_Page
        (On_Switch_Page'Access);

      Main_Property_List :=
        Gtk.Tree_View.Gtk_Tree_View
          (Builder.Get_Object ("Property_View"));

      Date_Label :=
        Gtk.Label.Gtk_Label
          (Builder.Get_Object ("Date_Label"));

      Button_Callback.Connect
        (Gtk.Button.Gtk_Button
           (Builder.Get_Object ("Game_Pause")),
         "clicked",
         Button_Callback.To_Marshaller (Change_Speed_Handler'Access),
         0);

      Button_Callback.Connect
        (Gtk.Button.Gtk_Button
           (Builder.Get_Object ("Game_Play_1")),
         "clicked",
         Button_Callback.To_Marshaller (Change_Speed_Handler'Access),
         1);

      Button_Callback.Connect
        (Gtk.Button.Gtk_Button
           (Builder.Get_Object ("Game_Play_2")),
         "clicked",
         Button_Callback.To_Marshaller (Change_Speed_Handler'Access),
         5);

      declare
         Model       : Gtk.Tree_Store.Gtk_Tree_Store;
         Text_Render : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
         Text_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
         Num         : Glib.Gint;
         pragma Unreferenced (Num);
      begin
         Gtk.Tree_Store.Gtk_New
           (Model,
            (0     => Glib.GType_String,
             1     => Glib.GType_String));

         Gtk.Cell_Renderer_Text.Gtk_New (Text_Render);
         Gtk.Tree_View_Column.Gtk_New (Text_Column);
         Num := Main_Property_List.Append_Column (Text_Column);
         Text_Column.Pack_Start (Text_Render, True);
         Text_Column.Set_Sizing
           (Gtk.Tree_View_Column.Tree_View_Column_Autosize);
         Text_Column.Add_Attribute (Text_Render, "text", 0);

         Gtk.Cell_Renderer_Text.Gtk_New (Text_Render);
         Gtk.Tree_View_Column.Gtk_New (Text_Column);
         Num := Main_Property_List.Append_Column (Text_Column);
         Text_Column.Pack_Start (Text_Render, True);
         Text_Column.Set_Sizing
           (Gtk.Tree_View_Column.Tree_View_Column_Autosize);
         Text_Column.Add_Attribute (Text_Render, "text", 1);

         Main_Property_List.Set_Model
           (Gtk.Tree_Model.Gtk_Tree_Model (Model.To_Interface));

      end;

      declare
         Id : constant Gtk.Status_Bar.Message_Id :=
                Main_Status_Bar.Push (Current_Model_Context, "Conflict");
         pragma Unreferenced (Id);
      begin
         null;
      end;

      State.Model_List.Append
        (Lui.Launcher.Initial_Model);

      Timeout_Id := Glib.Main.Timeout_Add
        (Interval => 100,
         Func     => Timeout_Handler'Access);

      Date_Label.Set_Label (Conflict.Dates.Today);

      Gtk.Main.Main;

   end Start;

end Lui.Sample;
