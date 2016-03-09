with Ada.Text_IO;

with Glib.Error;

with Gtk.Box;
with Gtk.Builder;
with Gtk.Button;
with Gtk.Cell_Renderer_Text;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Main;
with Gtk.Notebook;
with Gtk.Scrolled_Window;
with Gtk.Status_Bar;
with Gtk.Tree_Model;
with Gtk.Tree_Selection;
with Gtk.Tree_Store;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;
with Gtk.Widget;
with Gtk.Window;

with Lui.Gadgets;
with Lui.Models;
with Lui.Paths;
with Lui.Tables;

package body Lui.Sample_UI is

   type Root_Sample_UI is
     new Lui.Gtk_UI.Lui_Gtk_Interface with
      record
         Info_Box       : Gtk.Box.Gtk_Box;
         Gadget_Box     : Gtk.Box.Gtk_Box;
         Status_Bar     : Gtk.Status_Bar.Gtk_Status_Bar;
         Status_Context : Gtk.Status_Bar.Context_Id;
         Property_List  : Gtk.Tree_View.Gtk_Tree_View;
         Notebook       : Gtk.Notebook.Gtk_Notebook;
         Models         : Lui.Models.Active_Model_List;
      end record;

   overriding procedure Append_Feature
     (To      : in out Root_Sample_UI;
      Feature : Lui_UI_Feature;
      Element : not null access Root_UI_Element'Class;
      Top     : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   overriding procedure Select_Feature
     (To      : in out Root_Sample_UI;
      Feature : Lui_UI_Feature;
      Element : not null access Root_UI_Element'Class;
      Top     : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is null;

   overriding procedure Clear_Features
     (To      : in out Root_Sample_UI;
      Feature : Lui_UI_Feature)
   is null;

   overriding procedure Status_Message
     (To      : in out Root_Sample_UI;
      Message : String)
   is null;

   type Sample_UI is access all Root_Sample_UI'Class;

   Current_UI : Sample_UI;

   procedure Destroy_Handler
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure On_Switch_Page
     (Self     : access Gtk.Notebook.Gtk_Notebook_Record'Class;
      Page     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Page_Num : Glib.Guint);

   procedure Show_Gadgets
     (Model   : Lui.Models.Object_Model;
      Gadgets : Lui.Gadgets.Array_Of_Gadgets);

   procedure Show_Info_Boxes
     (Info      : Lui.Tables.Array_Of_Model_Tables);

   procedure Select_Model (Model : Lui.Models.Object_Model);

   procedure Show_Properties
     (Model   : Lui.Models.Object_Model);

   package Select_Item_Handler is
     new Gtk.Handlers.User_Callback
       (Gtk.Tree_View.Gtk_Tree_View_Record,
        Lui.Tables.Model_Table);

   procedure Info_Select_Row_Callback
     (Widget : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Table  : Lui.Tables.Model_Table);

   procedure Clear_Box (Box : Gtk.Box.Gtk_Box);

   type Gadget_Button_State is
      record
         Button : Lui.Gadgets.Model_Gadget;
         Model  : Lui.Models.Object_Model;
      end record;

   package Gadget_Button_Callback is
     new Gtk.Handlers.User_Callback (Gtk.Button.Gtk_Button_Record,
                                     Gadget_Button_State);

   procedure Handle_Gadget_Button
     (Button : access Gtk.Button.Gtk_Button_Record'Class;
      State  : Gadget_Button_State);

   --------------------
   -- Append_Feature --
   --------------------

   overriding procedure Append_Feature
     (To      : in out Root_Sample_UI;
      Feature : Lui_UI_Feature;
      Element : not null access Root_UI_Element'Class;
      Top     : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      case Feature is
         when UI_Gadget =>
            null;
         when UI_Model =>
            To.Models.Append (Lui.Models.Object_Model (Element));
            To.Notebook.Append_Page (Top);
         when UI_Table =>
            null;
      end case;
   end Append_Feature;

   ---------------
   -- Clear_Box --
   ---------------

   procedure Clear_Box (Box : Gtk.Box.Gtk_Box) is
      use type Gtk.Widget.Gtk_Widget;
   begin
      while Box.Get_Child (0) /= null loop
         Box.Remove (Box.Get_Child (0));
      end loop;
   end Clear_Box;

   ----------------------
   -- Create_Sample_UI --
   ----------------------

   function Create_Sample_UI
      return Lui.Gtk_UI.Lui_Gtk
   is
      Result  : constant Sample_UI := new Root_Sample_UI;
      Builder : Gtk.Builder.Gtk_Builder;
      UI_Path : constant String := Lui.Paths.Config_File ("sample.ui");
   begin

      Gtk.Main.Init;

      Gtk.Builder.Gtk_New (Builder);

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
         Main_Window.On_Destroy (Destroy_Handler'Access);
         Main_Window.Maximize;
         Main_Window.Show_All;
      end;

      Result.Info_Box :=
        Gtk.Box.Gtk_Box (Builder.Get_Object ("Info_Vbox"));

      Result.Gadget_Box :=
        Gtk.Box.Gtk_Box (Builder.Get_Object ("Gadget_Box"));

      Result.Status_Bar :=
        Gtk.Status_Bar.Gtk_Status_Bar
          (Builder.Get_Object ("Status_Bar"));
      Result.Status_Context :=
        Result.Status_Bar.Get_Context_Id ("star mouseover");

      Result.Notebook :=
        Gtk.Notebook.Gtk_Notebook
          (Builder.Get_Object ("Main_Tab"));
      Result.Notebook.Remove_Page (0);

      Result.Notebook.On_Switch_Page
        (On_Switch_Page'Access);

      Result.Property_List :=
        Gtk.Tree_View.Gtk_Tree_View
          (Builder.Get_Object ("Property_View"));

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
         Num := Result.Property_List.Append_Column (Text_Column);
         Text_Column.Pack_Start (Text_Render, True);
         Text_Column.Set_Sizing
           (Gtk.Tree_View_Column.Tree_View_Column_Autosize);
         Text_Column.Add_Attribute (Text_Render, "text", 0);

         Gtk.Cell_Renderer_Text.Gtk_New (Text_Render);
         Gtk.Tree_View_Column.Gtk_New (Text_Column);
         Num := Result.Property_List.Append_Column (Text_Column);
         Text_Column.Pack_Start (Text_Render, True);
         Text_Column.Set_Sizing
           (Gtk.Tree_View_Column.Tree_View_Column_Autosize);
         Text_Column.Add_Attribute (Text_Render, "text", 1);

         Result.Property_List.Set_Model
           (Gtk.Tree_Model.Gtk_Tree_Model (Model.To_Interface));

      end;

      declare
         Id : constant Gtk.Status_Bar.Message_Id :=
                Result.Status_Bar.Push
                  (Result.Status_Context, "Lui Sample");
         pragma Unreferenced (Id);
      begin
         null;
      end;

      Current_UI := Result;

      return Lui.Gtk_UI.Lui_Gtk (Result);

   end Create_Sample_UI;

   ---------------------
   -- Destroy_Handler --
   ---------------------

   procedure Destroy_Handler
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Widget);
   begin
      Gtk.Main.Main_Quit;
   end Destroy_Handler;

   --------------------------
   -- Handle_Gadget_Button --
   --------------------------

   procedure Handle_Gadget_Button
     (Button : access Gtk.Button.Gtk_Button_Record'Class;
      State  : Gadget_Button_State)
   is
      pragma Unreferenced (Button);
   begin
      Lui.Gadgets.Root_Button_Gadget'Class
        (State.Button.all).On_Click
        (State.Model);
   end Handle_Gadget_Button;

   ------------------------------
   -- Info_Select_Row_Callback --
   ------------------------------

   procedure Info_Select_Row_Callback
     (Widget : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Table  : Lui.Tables.Model_Table)
   is
      use type Lui.Models.Object_Model;
      Tree_Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Obj_Model  : Lui.Models.Object_Model;
      Selection  : constant Gtk.Tree_Selection.Gtk_Tree_Selection :=
                     Widget.Get_Selection;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Row        : Positive;
   begin
      Gtk.Tree_Selection.Get_Selected (Selection, Tree_Model, Iter);
      Row := Positive (Gtk.Tree_Model."-" (Tree_Model).Get_Int (Iter, 0));
      Table.Select_Row (Row);
      Obj_Model := Table.Row_Model (Row);
      if Obj_Model /= null then
         Select_Model (Obj_Model);
      end if;
   end Info_Select_Row_Callback;

   --------------------
   -- On_Switch_Page --
   --------------------

   procedure On_Switch_Page
     (Self     : access Gtk.Notebook.Gtk_Notebook_Record'Class;
      Page     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Page_Num : Glib.Guint)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Page);
      Model : constant Lui.Models.Object_Model :=
                Current_UI.Models.Model (Natural (Page_Num) + 1);
   begin

      Show_Info_Boxes (Model.Tables);
      Show_Gadgets (Model, Model.Gadgets);
      Show_Properties (Model);

   end On_Switch_Page;

   ------------------
   -- Select_Model --
   ------------------

   procedure Select_Model (Model : Lui.Models.Object_Model) is
      use type Lui.Models.Object_Model;
   begin
      for I in 1 .. Current_UI.Models.Count loop
         if Current_UI.Models.Model (I) = Model then
            Current_UI.Notebook.Set_Current_Page
              (Glib.Gint (I - 1));
            return;
         end if;
      end loop;

      Current_UI.Models.Append (Model);
   end Select_Model;

   ------------------
   -- Show_Gadgets --
   ------------------

   procedure Show_Gadgets
     (Model   : Lui.Models.Object_Model;
      Gadgets : Lui.Gadgets.Array_Of_Gadgets)
   is
   begin

      Clear_Box (Current_UI.Gadget_Box);

      for Gadget of Gadgets loop
         if Gadget.all in Lui.Gadgets.Root_Button_Gadget'Class then
            declare
               Button : Gtk.Button.Gtk_Button;
            begin
               Gtk.Button.Gtk_New (Button, Gadget.Name);
               Current_UI.Gadget_Box.Add (Button);
               Gadget_Button_Callback.Connect
                 (Button, Gtk.Button.Signal_Clicked,
                  Gadget_Button_Callback.To_Marshaller
                    (Handle_Gadget_Button'Access),
                  ((Gadget, Model)));
               Button.Show_All;
            end;
         else
            null;
         end if;
      end loop;

   end Show_Gadgets;

   ---------------------
   -- Show_Info_Boxes --
   ---------------------

   procedure Show_Info_Boxes
     (Info      : Lui.Tables.Array_Of_Model_Tables)
   is
      use type Gtk.Widget.Gtk_Widget;
   begin
      Clear_Box (Current_UI.Info_Box);

      for Table of Info loop
         declare
            Tree : Gtk.Tree_View.Gtk_Tree_View;
            Store : Gtk.Tree_Store.Gtk_Tree_Store;
            Types : Glib.GType_Array (0 .. Glib.Guint (Table.Column_Count));
            Label : Gtk.Label.Gtk_Label;
            Max_Levels : constant := 16;
            Parent_Iters : array (1 .. Max_Levels)
              of Gtk.Tree_Model.Gtk_Tree_Iter :=
                (others => Gtk.Tree_Model.Null_Iter);
            Parent_Rows  : array (1 .. Max_Levels) of Natural :=
                                                (others => 0);
            Current_Level : Positive := 1;
         begin

            Gtk.Label.Gtk_New (Label, Table.Name);
            Label.Show_All;

            Current_UI.Info_Box.Pack_Start
              (Label,
               Expand   => False,
               Fill     => True,
               Padding  => 0);

            Types (0) := Glib.GType_Int;
            for I in 1 .. Table.Column_Count loop
               Types (Glib.Guint (I)) := Glib.GType_String;
            end loop;

            Gtk.Tree_Store.Gtk_New (Store, Types);

            for I in 1 .. Table.Row_Count loop
               declare
                  Result : Gtk.Tree_Model.Gtk_Tree_Iter;
                  Parent_Iter : Gtk.Tree_Model.Gtk_Tree_Iter :=
                                  Gtk.Tree_Model.Null_Iter;
                  Parent_Row : constant Natural :=
                                 Table.Parent_Row (I);
               begin
                  Current_Level := 1;
                  if Parent_Row /= 0 then
                     for P_Index in Parent_Rows'Range loop
                        if Parent_Rows (P_Index) = Parent_Row then
                           Parent_Iter := Parent_Iters (P_Index);
                           Current_Level := P_Index + 1;
                           exit;
                        end if;
                     end loop;
                  end if;

                  Parent_Rows (Current_Level) := I;
                  Store.Append (Result, Parent_Iter);
                  Parent_Iters (Current_Level) := Result;

                  Store.Set (Result, 0, Glib.Gint (I));
                  for J in 1 .. Table.Column_Count loop
                     Store.Set (Result, Glib.Gint (J),
                                Table.Cell_Text (I, J));
                  end loop;
               end;
            end loop;

            Gtk.Tree_View.Gtk_New (Tree, Store);

            for I in 1 .. Table.Column_Count loop
               declare
                  Text_Render : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
                  Text_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
                  Num         : Glib.Gint;
                  pragma Unreferenced (Num);
               begin
                  Gtk.Cell_Renderer_Text.Gtk_New (Text_Render);
                  Gtk.Tree_View_Column.Gtk_New (Text_Column);
                  Num := Tree.Append_Column (Text_Column);
                  Text_Column.Pack_Start (Text_Render, True);
                  Text_Column.Set_Sizing
                    (Gtk.Tree_View_Column.Tree_View_Column_Autosize);
                  Text_Column.Set_Title (Table.Heading_Column_Text (I));
                  Text_Column.Add_Attribute (Text_Render, "text",
                                             Glib.Gint (I));
               end;
            end loop;

            Select_Item_Handler.Connect
              (Tree, "row-activated",
               Info_Select_Row_Callback'Access,
               Table);

            declare
               Scroll : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
            begin
               Gtk.Scrolled_Window.Gtk_New
                 (Scroll);
               Scroll.Add (Tree);
               Scroll.Show_All;
               Current_UI.Info_Box.Pack_Start
                 (Scroll,
                  Expand   => True,
                  Fill     => True,
                  Padding  => 0);
            end;
         end;
      end loop;

   end Show_Info_Boxes;

   ---------------------
   -- Show_Properties --
   ---------------------

   procedure Show_Properties
     (Model   : Lui.Models.Object_Model)
   is
      Count : constant Natural :=
                Model.Property_Count;
      Store : constant Gtk.Tree_Store.Gtk_Tree_Store :=
                Gtk.Tree_Store.Gtk_Tree_Store
                  (Gtk.Tree_Model."-" (Current_UI.Property_List.Get_Model));
   begin
      Store.Clear;
      for I in 1 .. Count loop
         declare
            Result : Gtk.Tree_Model.Gtk_Tree_Iter;
            Name   : constant String :=
                       Model.Property_Name (I);
            Value  : constant String :=
                       Model.Property_Value (I);
         begin
            Store.Append (Result, Gtk.Tree_Model.Null_Iter);
            Store.Set (Result, 0, Name);
            Store.Set (Result, 1, Value);
         end;
      end loop;
   end Show_Properties;

end Lui.Sample_UI;
