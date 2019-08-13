with Ada.Characters.Latin_1;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Numerics;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;

with Glib;
with Glib.Main;
with Glib.Object;

with Gdk.Event;
with Gdk.Types.Keysyms;

with Gtk.Box;
with Gtk.Button;
with Gtk.Cell_Renderer_Text;
with Gtk.Drawing_Area;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Scrolled_Window;
with Gtk.Tooltip;
with Gtk.Tree_Model;
with Gtk.Tree_Selection;
with Gtk.Tree_Store;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;

with Cairo;
with Cairo.Image_Surface;
with Cairo.Png;

with WL.Bitmap_IO;
with WL.Images;

with Lui.Colors;
with Lui.Gadgets;
with Lui.Handles;
with Lui.Rendering;
with Lui.Tables;

package body Lui.Gtk_UI is

   type Surface_Render_Layers is
     array (Render_Layer) of Cairo.Cairo_Surface;

   type Model_Layer_Record is
      record
         Model  : Lui.Models.Object_Model;
         Layers : Surface_Render_Layers;
      end record;

   package Model_Layer_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Model_Layer_Record);

   type Model_Object_Record is
     new Glib.Object.GObject_Record with
      record
         Widget  : Gtk.Drawing_Area.Gtk_Drawing_Area;
         Models  : Model_Layer_Lists.List;
         Width   : Glib.Gint;
         Height  : Glib.Gint;
      end record;

   type Model_Object_Access is access all Model_Object_Record'Class;

   procedure Resize
     (Slot : in out Model_Object_Record'Class);

   package Model_Object_Vectors is
     new Ada.Containers.Vectors
       (Positive, Model_Object_Access);

   type Gtk_Active_Model_List is
     new Lui.Models.Active_Model_List with
      record
         Slots : Model_Object_Vectors.Vector;
      end record;

   overriding
   procedure Append (List  : in out Gtk_Active_Model_List;
                     Model : Lui.Models.Object_Model);

   type Cairo_Renderer is
     new Lui.Rendering.Root_Renderer with
      record
         Context       : Cairo.Cairo_Context;
      end record;

   overriding procedure Set_Font
     (Renderer : in out Cairo_Renderer;
      Name     : String;
      Size     : Real;
      Bold     : Boolean := False;
      Italic   : Boolean := False);

   overriding procedure Set_Line_Width
     (Renderer : in out Cairo_Renderer;
      Width    : Positive_Real);

   overriding procedure Push_Viewport
     (Renderer : in out Cairo_Renderer;
      Viewport : Layout_Rectangle);

   overriding procedure Pop_Viewport
     (Renderer : in out Cairo_Renderer);

   overriding procedure Set_Color
     (Renderer : in out Cairo_Renderer;
      Color    : Lui.Colors.Color_Type);

   overriding procedure Circle
     (Renderer   : in out Cairo_Renderer;
      X, Y       : in     Integer;
      Radius     : in     Positive;
      Filled     : in     Boolean);

   overriding procedure Ellipse
     (Renderer   : in out Cairo_Renderer;
      X, Y       : in     Integer;
      R1, R2     : in     Positive;
      Filled     : in     Boolean);

   overriding procedure Line
     (Renderer   : in out Cairo_Renderer;
      X1, Y1     : in     Integer;
      X2, Y2     : in     Integer);

   overriding procedure Polygon
     (Renderer : in out Cairo_Renderer;
      Vertices : Lui.Rendering.Buffer_Points;
      Filled   : Boolean);

   overriding procedure Rectangle
     (Renderer : in out Cairo_Renderer;
      Rec      : Layout_Rectangle;
      Filled   : Boolean);

   overriding procedure Text
     (Renderer : in out Cairo_Renderer;
      X, Y     : Integer;
      Value    : String);

   overriding procedure Image
     (Renderer : in out Cairo_Renderer;
      Rec      : Layout_Rectangle;
      Resource : String);

   overriding procedure Create_Bitmap_Resource
     (Renderer      : in out Cairo_Renderer;
      Resource_Name : in     String;
      Bitmap        : in     WL.Bitmap_IO.Bitmap_Type);

   overriding procedure Create_Image_Resource
     (Renderer      : in out Cairo_Renderer;
      Resource_Name : in     String;
      Image         : WL.Images.Image_Type'Class);

   overriding function Have_Resource
     (Renderer      : Cairo_Renderer;
      Resource_Name : String)
      return Boolean;

   procedure Render_Model_Layers
     (Model   : Lui.Models.Object_Model;
      Layers  : Surface_Render_Layers);

   procedure Render_Model
     (Model   : Lui.Models.Object_Model;
      Surface : Cairo.Cairo_Surface;
      Layer   : Render_Layer);

   package Image_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
        Element_Type    => Cairo.Cairo_Surface,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => Ada.Strings.Unbounded."=",
        "="             => Cairo."=");

   type Model_Table_Info is
      record
         Table : Lui.Tables.Model_Table;
         Store : Gtk.Tree_Store.Gtk_Tree_Store;
         View  : Gtk.Tree_View.Gtk_Tree_View;
      end record;

   package Model_Table_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Model_Table_Info);

   type Root_UI_State is new Lui.Handles.Root_UI_Handle with
      record
         Main               : Lui_Gtk;
         Models             : Gtk_Active_Model_List;
         Tables             : Model_Table_Lists.List;
         Image_Cache        : Image_Maps.Map;
         Dragging           : Boolean := False;
         Last_Drag_X        : Integer := Integer'First;
         Last_Drag_Y        : Integer := Integer'First;
         Active             : Lui.Models.Object_Model := null;
         Timeout_Id         : Glib.Main.G_Source_Id;
      end record;

   overriding procedure Show_Model
     (State : in out Root_UI_State;
      Model : in     Lui.Models.Object_Model);

   overriding procedure On_Model_Added
     (State  : in out Root_UI_State;
      Model  : not null access Lui.Models.Root_Object_Model'Class);

   overriding procedure On_Model_Removed
     (State  : in out Root_UI_State;
      Model  : not null access Lui.Models.Root_Object_Model'Class);

   type UI_State is access all Root_UI_State'Class;

   State     : UI_State;

   function Timeout_Handler return Boolean;

   package Drawing_Area_Model_Callback is
     new Gtk.Handlers.User_Return_Callback
       (Widget_Type => Gtk.Drawing_Area.Gtk_Drawing_Area_Record,
        Return_Type => Boolean,
        User_Type   => Lui.Models.Object_Model);

   package Select_Item_Handler is
     new Gtk.Handlers.User_Callback
       (Gtk.Tree_View.Gtk_Tree_View_Record,
        Lui.Tables.Model_Table);

   function Configure_Model_Handler
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Configure)
      return Boolean;

   function Expose_Model_Handler
     (Self : access Glib.Object.GObject_Record'Class;
      Cr   : Cairo.Cairo_Context)
      return Boolean;

   function Model_Motion_Notify_Handler
     (W : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Model : Lui.Models.Object_Model)
      return Boolean;

   function Model_Mouse_Button_Press_Handler
     (W : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Model : Lui.Models.Object_Model)
      return Boolean;

   function Model_Mouse_Button_Release_Handler
     (W : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Model : Lui.Models.Object_Model)
      return Boolean;

   function Model_Key_Press_Handler
     (W : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Model : Lui.Models.Object_Model)
      return Boolean;

   function Model_Query_Tooltip_Handler
     (Self          : access Gtk.Widget.Gtk_Widget_Record'Class;
      X             : Glib.Gint;
      Y             : Glib.Gint;
      Keyboard_Mode : Boolean;
      Tooltip       : not null access Glib.Object.GObject_Record'Class)
      return Boolean;

   function Model_Zoom_Handler
     (W : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Model : Lui.Models.Object_Model)
      return Boolean;

   procedure Info_Select_Row_Callback
     (Widget : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Table  : Lui.Tables.Model_Table);

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

   procedure Show_Model
     (Context       : Cairo.Cairo_Context;
      Layer         : Render_Layer;
      Model         : Lui.Models.Object_Model);

   procedure Show_Gadget
     (Gadget : Lui.Gadgets.Model_Gadget;
      Model  : Lui.Models.Object_Model);

   procedure Show_Table
     (Table : Lui.Tables.Model_Table);

   procedure Refresh_Table
     (Table : Lui.Tables.Model_Table);

   procedure Load_Table
     (Table       : Lui.Tables.Model_Table;
      Store       : Gtk.Tree_Store.Gtk_Tree_Store);

   procedure Refresh_Table
     (Table       : Lui.Tables.Model_Table;
      Store       : Gtk.Tree_Store.Gtk_Tree_Store);

--     procedure Show_Properties
--       (Model   : Lui.Models.Object_Model);

   procedure Select_Model (Model : Lui.Models.Object_Model);

   ------------
   -- Append --
   ------------

   overriding
   procedure Append (List  : in out Gtk_Active_Model_List;
                     Model : Lui.Models.Object_Model)
   is
      Page : Gtk.Drawing_Area.Gtk_Drawing_Area;
   begin
      Lui.Models.Active_Model_List (List).Append (Model);

      Gtk.Drawing_Area.Gtk_New (Page);
      declare
         use Gdk.Event;
      begin
         Page.Set_Events
           (Exposure_Mask
            or Pointer_Motion_Mask
            or Pointer_Motion_Hint_Mask
            or Button_Motion_Mask
            or Button_Press_Mask
            or Button_Release_Mask
            or Key_Press_Mask
            or Key_Release_Mask
            or Structure_Mask
            or Scroll_Mask);
      end;

      Page.Set_Can_Focus (True);
      Page.Set_Has_Tooltip (True);

      declare
         Slot : constant Model_Object_Access :=
                  new Model_Object_Record'
                    (Glib.Object.GObject_Record with
                     Widget   => Page,
                     Models   => Model_Layer_Lists.Empty_List,
                     Width    => 1,
                     Height   => 1);
      begin
         Slot.Models.Append
           (Model_Layer_Record'
              (Model  => Model,
               Layers => (others => Cairo.Null_Surface)));
         Slot.Initialize;

         Page.On_Draw
           (Expose_Model_Handler'Access, Slot);
         Page.On_Configure_Event
           (Configure_Model_Handler'Access, Slot);
         List.Slots.Append (Slot);
      end;

      Drawing_Area_Model_Callback.Connect
        (Page, "motion-notify-event",
         Drawing_Area_Model_Callback.To_Marshaller
           (Model_Motion_Notify_Handler'Access),
         Model);
      Drawing_Area_Model_Callback.Connect
        (Page, Gtk.Widget.Signal_Button_Press_Event,
         Drawing_Area_Model_Callback.To_Marshaller
           (Model_Mouse_Button_Press_Handler'Access),
        Model);
      Drawing_Area_Model_Callback.Connect
        (Page, Gtk.Widget.Signal_Button_Release_Event,
         Drawing_Area_Model_Callback.To_Marshaller
           (Model_Mouse_Button_Release_Handler'Access),
        Model);

      Drawing_Area_Model_Callback.Connect
        (Page, Gtk.Widget.Signal_Key_Press_Event,
         Drawing_Area_Model_Callback.To_Marshaller
           (Model_Key_Press_Handler'Access),
        Model);

      Drawing_Area_Model_Callback.Connect
        (Page, "scroll-event",
         Drawing_Area_Model_Callback.To_Marshaller
           (Model_Zoom_Handler'Access),
         Model);

--        Drawing_Area_Model_Callback.Connect
--          (Page, Gtk.Widget.Signal_Query_Tooltip,
--           Drawing_Area_Model_Callback.To_Marshaller
--             (Model_Query_Tooltip_Handler'Access),
--           Model);

      Page.On_Query_Tooltip (Model_Query_Tooltip_Handler'Access);

      Page.Show_All;
      Page.Ref;

      State.Main.Append_Feature (UI_Model, Model, Top => Page);

   end Append;

   ------------
   -- Circle --
   ------------

   overriding procedure Circle
     (Renderer   : in out Cairo_Renderer;
      X, Y       : in     Integer;
      Radius     : in     Positive;
      Filled     : in     Boolean)
   is
      use Glib;
   begin
      Cairo.Arc
        (Cr     => Renderer.Context,
         Xc     => Glib.Gdouble (X),
         Yc     => Glib.Gdouble (Y),
         Radius => Glib.Gdouble (Radius),
         Angle1 => 0.0,
         Angle2 => 2.0 * Ada.Numerics.Pi);

      if Filled then
         Cairo.Fill (Renderer.Context);
      else
         Cairo.Stroke (Renderer.Context);
      end if;

   end Circle;

   -----------------------------
   -- Configure_Model_Handler --
   -----------------------------

   function Configure_Model_Handler
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Configure)
      return Boolean
   is
      Slot : Model_Object_Record renames
               Model_Object_Record (Self.all);

   begin
      Slot.Width := Event.Width;
      Slot.Height := Event.Height;
      Lui.Models.Set_Screen_Size
        (Natural (Event.Width), Natural (Event.Height));

      Slot.Resize;

      Slot.Widget.Grab_Focus;

      return True;
   end Configure_Model_Handler;

   ----------------------------
   -- Create_Bitmap_Resource --
   ----------------------------

   overriding procedure Create_Bitmap_Resource
     (Renderer      : in out Cairo_Renderer;
      Resource_Name : in     String;
      Bitmap        : in     WL.Bitmap_IO.Bitmap_Type)
   is
      pragma Unreferenced (Renderer);
      use Glib;
      Bitmap_Width  : constant Natural := WL.Bitmap_IO.Width (Bitmap);
      Bitmap_Height : constant Natural := WL.Bitmap_IO.Height (Bitmap);
      Data          : constant Cairo.Image_Surface.ARGB32_Array_Access :=
                         new Cairo.Image_Surface.ARGB32_Array
                          (0 .. Bitmap_Width * Bitmap_Height - 1);

   begin
      for Y in 0 .. Bitmap_Height - 1 loop
         for X in 0 .. Bitmap_Width - 1 loop
            declare
               subtype Byte is Cairo.Image_Surface.Byte;
               Color : constant WL.Bitmap_IO.Color_Type :=
                          WL.Bitmap_IO.Color
                            (Bitmap, X, Bitmap_Height - Y - 1);
               ARGB32 : constant Cairo.Image_Surface.ARGB32_Data :=
                          (Alpha => Byte (Color.Alpha),
                           Blue  => Byte (Color.B),
                           Green => Byte (Color.G),
                           Red   => Byte (Color.R));
               Index  : constant Natural := X + Y * Bitmap_Width;
            begin
               Data (Index) := ARGB32;
            end;
         end loop;
      end loop;

      declare
         use Ada.Strings.Unbounded;
         Image         : constant Cairo.Cairo_Surface :=
                           Cairo.Image_Surface.Create_For_Data_ARGB32
                             (Data   => Data,
                              Width  => Gint (Bitmap_Width),
                              Height => Gint (Bitmap_Height));
         Resource_Key  : constant Unbounded_String :=
                           To_Unbounded_String (Resource_Name);
      begin
         if State.Image_Cache.Contains (Resource_Key) then
            declare
               Old_Image : constant Cairo.Cairo_Surface :=
                             State.Image_Cache (Resource_Key);
            begin
               Cairo.Surface_Destroy (Old_Image);
               State.Image_Cache.Delete (Resource_Key);
            end;
         end if;

         State.Image_Cache.Insert (Resource_Key, Image);
      end;

   end Create_Bitmap_Resource;

   ---------------------------
   -- Create_Image_Resource --
   ---------------------------

   overriding procedure Create_Image_Resource
     (Renderer      : in out Cairo_Renderer;
      Resource_Name : in     String;
      Image         : WL.Images.Image_Type'Class)
   is
      pragma Unreferenced (Renderer);
      use Glib;
      use type WL.Images.Layer_Count;

   begin
      for Layer in 1 .. Image.Number_Of_Layers loop
         declare
            Width  : constant Natural := Natural (Image.Width (Layer));
            Height : constant Natural := Natural (Image.Height (Layer));
            Data   : constant Cairo.Image_Surface.ARGB32_Array_Access :=
                       new Cairo.Image_Surface.ARGB32_Array
                         (0 .. Width * Height - 1);
            Layer_Name : constant String :=
                           Resource_Name
                           & (if Image.Number_Of_Layers = 1 then ""
                              else Integer'Image (-(Integer (Layer))));
         begin
            for X in 1 .. Image.Width (Layer) loop
               for Y in 1 .. Image.Height (Layer) loop
                  declare
                     subtype Byte is Cairo.Image_Surface.Byte;
                     Color : constant WL.Images.Image_Color :=
                                Image.Color (X, Y);
                     ARGB32 : constant Cairo.Image_Surface.ARGB32_Data :=
                                (Alpha => Byte (Color.Alpha),
                                 Blue  => Byte (Color.Blue),
                                 Green => Byte (Color.Green),
                                 Red   => Byte (Color.Red));
                     Index  : constant Natural :=
                                Natural (X) - 1
                                + (Height - Natural (Y)) * Width;
                  begin
                     Data (Index) := ARGB32;
                  end;
               end loop;
            end loop;

            declare
               use Ada.Strings.Unbounded;
               Image         : constant Cairo.Cairo_Surface :=
                                 Cairo.Image_Surface.Create_For_Data_ARGB32
                                   (Data   => Data,
                                    Width  => Gint (Width),
                                    Height => Gint (Height));
               Resource_Key  : constant Unbounded_String :=
                                 To_Unbounded_String (Layer_Name);
            begin
               if State.Image_Cache.Contains (Resource_Key) then
                  declare
                     Old_Image : constant Cairo.Cairo_Surface :=
                                   State.Image_Cache (Resource_Key);
                  begin
                     Ada.Text_IO.Put_Line
                       ("warning: replacing: " & Layer_Name);
                     Cairo.Surface_Destroy (Old_Image);
                     State.Image_Cache.Delete (Resource_Key);
                  end;
               end if;

               State.Image_Cache.Insert (Resource_Key, Image);
            end;
         end;
      end loop;

   end Create_Image_Resource;

   -------------
   -- Ellipse --
   -------------

   overriding procedure Ellipse
     (Renderer   : in out Cairo_Renderer;
      X, Y       : in     Integer;
      R1, R2     : in     Positive;
      Filled     : in     Boolean)
   is
      use Glib;
   begin
      Cairo.Save (Renderer.Context);
      Cairo.Translate
        (Renderer.Context,
         Glib.Gdouble (X),
         Glib.Gdouble (Y));
      Cairo.Scale
        (Renderer.Context, Glib.Gdouble (R1), Glib.Gdouble (R2));

      Cairo.Arc (Renderer.Context, 0.0, 0.0, 1.0,
                 0.0, 2.0 * Ada.Numerics.Pi);

      if Filled then
         Cairo.Fill (Renderer.Context);
      else
         Cairo.Stroke (Renderer.Context);
      end if;

      Cairo.Restore (Renderer.Context);

   end Ellipse;

   --------------------------
   -- Expose_Model_Handler --
   --------------------------

   function Expose_Model_Handler
     (Self : access Glib.Object.GObject_Record'Class;
      Cr   : Cairo.Cairo_Context)
      return Boolean
   is
      Slot : Model_Object_Record renames
               Model_Object_Record (Self.all);

   begin

      for Model_Layer of Slot.Models loop
         for Layer in
           Render_Layer range 1 .. Model_Layer.Model.Last_Render_Layer
         loop
            Cairo.Set_Source_Surface
              (Cr, Model_Layer.Layers (Layer),
               Glib.Gdouble (Model_Layer.Model.Layout_X),
               Glib.Gdouble (Model_Layer.Model.Layout_Y));
            Cairo.Paint (Cr);
         end loop;
      end loop;
      return True;
   end Expose_Model_Handler;

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

   -------------------
   -- Have_Resource --
   -------------------

   overriding function Have_Resource
     (Renderer      : Cairo_Renderer;
      Resource_Name : String)
      return Boolean
   is
      pragma Unreferenced (Renderer);
   begin
      return State.Image_Cache.Contains
        (Ada.Strings.Unbounded.To_Unbounded_String
           (Resource_Name));
   end Have_Resource;

   -----------
   -- Image --
   -----------

   overriding procedure Image
     (Renderer : in out Cairo_Renderer;
      Rec      : Layout_Rectangle;
      Resource : String)
   is
      use Ada.Strings.Unbounded;
      Base_Image_Key   : constant Unbounded_String :=
                           To_Unbounded_String (Resource);
      Base_Image       : Cairo.Cairo_Surface;
   begin
      if State.Image_Cache.Contains (Base_Image_Key) then
         Base_Image := State.Image_Cache.Element (Base_Image_Key);
      else
         declare
            Path : constant String :=
                     Renderer.Image_Path (Resource & ".png");
         begin
            if not Ada.Directories.Exists (Path) then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "file not found: " & Path);
            end if;
            Base_Image := Cairo.Png.Create_From_Png (Path);
            State.Image_Cache.Insert (Base_Image_Key, Base_Image);
         end;
      end if;

      declare
         use Glib;
         Img_Height   : constant Gint :=
                          Cairo.Image_Surface.Get_Height (Base_Image);
         Img_Width    : constant Gint :=
                          Cairo.Image_Surface.Get_Width (Base_Image);
         Height_Ratio : constant Gdouble :=
                          Gdouble (Rec.Height) / Gdouble (Img_Height);
         Width_Ratio  : constant Gdouble :=
                          Gdouble (Rec.Width) / Gdouble (Img_Width);
      begin

         Cairo.Save (Renderer.Context);
         Cairo.Translate
           (Renderer.Context, Gdouble (Rec.X), Gdouble (Rec.Y));
         Cairo.Scale (Renderer.Context, Width_Ratio, Height_Ratio);
         Cairo.Set_Source_Surface (Renderer.Context, Base_Image, 0.0, 0.0);
         Cairo.Paint (Renderer.Context);
         Cairo.Restore (Renderer.Context);

      end;

   end Image;

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

   ----------
   -- Line --
   ----------

   overriding procedure Line
     (Renderer   : in out Cairo_Renderer;
      X1, Y1     : in     Integer;
      X2, Y2     : in     Integer)
   is
      use Glib;
   begin
      Cairo.Set_Line_Cap (Renderer.Context, Cairo.Cairo_Line_Cap_Round);
      Cairo.Move_To (Renderer.Context, Gdouble (X1), Gdouble (Y1));
      Cairo.Line_To (Renderer.Context, Gdouble (X2), Gdouble (Y2));
      Cairo.Stroke (Renderer.Context);
   end Line;

   ----------------
   -- Load_Table --
   ----------------

   procedure Load_Table
     (Table       : Lui.Tables.Model_Table;
      Store       : Gtk.Tree_Store.Gtk_Tree_Store)
   is
      Max_Levels : constant := 16;
      type Parent_Array is
        array (1 .. Max_Levels) of Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent_Iters : Parent_Array :=
                       (others => Gtk.Tree_Model.Null_Iter);
      Parent_Rows  : array (1 .. Max_Levels) of Natural :=
                       (others => 0);
      Current_Level : Positive := 1;
   begin

      Store.Clear;

      for I in 1 .. Table.Row_Count loop
         declare
            Result      : Gtk.Tree_Model.Gtk_Tree_Iter;
            Parent_Iter : Gtk.Tree_Model.Gtk_Tree_Iter :=
                            Gtk.Tree_Model.Null_Iter;
            Parent_Row  : constant Natural :=
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

   end Load_Table;

   -----------------------------
   -- Model_Key_Press_Handler --
   -----------------------------

   function Model_Key_Press_Handler
     (W : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Model : Lui.Models.Object_Model)
      return Boolean
   is
      pragma Unreferenced (W);
      use type Gdk.Types.Gdk_Key_Type;
      Code : constant Gdk.Types.Gdk_Key_Type :=
               Gdk.Event.Get_Key_Val (Event);
   begin
      if Code in 32 .. 127 then
         Model.On_Key_Press (Character'Val (Code));
      elsif Code = Gdk.Types.Keysyms.GDK_Escape then
         Model.On_Key_Press (Ada.Characters.Latin_1.ESC);
      end if;
      return True;
   end Model_Key_Press_Handler;

   ---------------------------------
   -- Model_Motion_Notify_Handler --
   ---------------------------------

   function Model_Motion_Notify_Handler
     (W : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Model : Lui.Models.Object_Model)
      return Boolean
   is
      pragma Unreferenced (W);
      X : constant Integer := Integer (Event.Motion.X);
      Y : constant Integer := Integer (Event.Motion.Y);
   begin
      if State.Dragging then
         Model.On_Drag
           (X - State.Last_Drag_X, Y - State.Last_Drag_Y);
         State.Last_Drag_X := X;
         State.Last_Drag_Y := Y;
      end if;

      return True;
   end Model_Motion_Notify_Handler;

   --------------------------------------
   -- Model_Mouse_Button_Press_Handler --
   --------------------------------------

   function Model_Mouse_Button_Press_Handler
     (W : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Model : Lui.Models.Object_Model)
      return Boolean
   is
      use Glib;
      X : constant Integer := Integer (Event.Motion.X);
      Y : constant Integer := Integer (Event.Motion.Y);
      M : constant Lui.Models.Object_Model :=
            Model.Model_At (X, Y);
   begin
      if Event.Button.Button = 1 then
         M.Select_XY (X - M.Layout_X, Y - M.Layout_Y);
         W.Queue_Draw;
         return True;
      else
         return False;
      end if;
   end Model_Mouse_Button_Press_Handler;

   ----------------------------------------
   -- Model_Mouse_Button_Release_Handler --
   ----------------------------------------

   function Model_Mouse_Button_Release_Handler
     (W : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Model : Lui.Models.Object_Model)
      return Boolean
   is
      pragma Unreferenced (W);
      pragma Unreferenced (Model);
      use Glib;
   begin
      if Event.Button.Button = 1 then
         State.Dragging := False;
         return True;
      else
         return False;
      end if;
   end Model_Mouse_Button_Release_Handler;

--     function Model_Query_Tooltip_Handler
--       (W     : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
--        Event : Gdk.Event.Gdk_Event;
--        Model : Lui.Models.Object_Model)
--        return Boolean
--     is
--        Tooltip : constant Gtk.Tooltip.Gtk_Tooltip :=
--                    Event.Proximity.
--                    Gdk.Event.Gdk_Event_Proximity (Event

   ---------------------------------
   -- Model_Query_Tooltip_Handler --
   ---------------------------------

   function Model_Query_Tooltip_Handler
     (Self          : access Gtk.Widget.Gtk_Widget_Record'Class;
      X             : Glib.Gint;
      Y             : Glib.Gint;
      Keyboard_Mode : Boolean;
      Tooltip       : not null access Glib.Object.GObject_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Self, Keyboard_Mode);
      Message : constant String :=
                  State.Active.Tooltip (Integer (X), Integer (Y));
   begin
      if Message /= "" then
         Gtk.Tooltip.Gtk_Tooltip (Tooltip).Set_Text (Message);
         return True;
      else
         return False;
      end if;
   end Model_Query_Tooltip_Handler;

   ------------------------
   -- Model_Zoom_Handler --
   ------------------------

   function Model_Zoom_Handler
     (W : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Model : Lui.Models.Object_Model)
      return Boolean
   is
      use type Gdk.Types.Gdk_Modifier_Type;
      pragma Unreferenced (W);
      X : constant Integer := Integer (Event.Scroll.X);
      Y : constant Integer := Integer (Event.Scroll.Y);
      Target : constant Lui.Models.Object_Model :=
                 Model.Model_At (X, Y);
   begin
      case Event.Scroll.Direction is
         when Gdk.Event.Scroll_Up | Gdk.Event.Scroll_Left =>
            Target.Zoom
              (-1, X, Y,
               (Event.Scroll.State and Gdk.Types.Control_Mask) /= 0);
         when Gdk.Event.Scroll_Down | Gdk.Event.Scroll_Right =>
            Target.Zoom
              (1, X, Y,
               (Event.Scroll.State and Gdk.Types.Control_Mask) /= 0);
         when Gdk.Event.Scroll_Smooth =>
            null;
      end case;
      Model.Queue_Render;
      return True;
   end Model_Zoom_Handler;

   -------------------------
   -- On_Model_Activation --
   -------------------------

   procedure On_Model_Activation (Model : Lui.Models.Object_Model) is
   begin
      Select_Model (Model);
   end On_Model_Activation;

   --------------------
   -- On_Model_Added --
   --------------------

   overriding procedure On_Model_Added
     (State  : in out Root_UI_State;
      Model  : not null access Lui.Models.Root_Object_Model'Class)
   is
      use type Lui.Models.Object_Model;
   begin
      for Slot of State.Models.Slots loop
         declare
            Found : Boolean := False;
         begin
            for Model_Layer of Slot.Models loop
               if Model_Layer.Model = Model.Parent_Model then
                  Found := True;
                  exit;
               end if;
            end loop;
            if Found then
               Slot.Models.Append
                 (Model_Layer_Record'
                    (Model  => Lui.Models.Object_Model (Model),
                     Layers => (others => Cairo.Null_Surface)));
               Slot.Resize;
               return;
            end if;
         end;
      end loop;
      raise Constraint_Error with
        Model.Name & ": no parent found";
   end On_Model_Added;

   ----------------------
   -- On_Model_Changed --
   ----------------------

   procedure On_Model_Changed (Model : Lui.Models.Object_Model) is
   begin
      Select_Model (Model);
   end On_Model_Changed;

   ----------------------
   -- On_Model_Removed --
   ----------------------

   overriding procedure On_Model_Removed
     (State  : in out Root_UI_State;
      Model  : not null access Lui.Models.Root_Object_Model'Class)
   is
      use type Lui.Models.Object_Model;
   begin
      for Slot of State.Models.Slots loop
         declare
            use Model_Layer_Lists;
            Position : Cursor := No_Element;
         begin
            for P in Slot.Models.Iterate loop
               if Element (P).Model = Lui.Models.Object_Model (Model) then
                  Position := P;
                  exit;
               end if;
            end loop;
            if Has_Element (Position) then
               Slot.Models.Delete (Position);
               for Model_Layer of Slot.Models loop
                  Model_Layer.Model.Set_Changed;
               end loop;
               return;
            end if;
         end;
      end loop;
      raise Constraint_Error with
        Model.Name & ": not found";
   end On_Model_Removed;

   -------------
   -- Polygon --
   -------------

   overriding procedure Polygon
     (Renderer : in out Cairo_Renderer;
      Vertices : Lui.Rendering.Buffer_Points;
      Filled   : Boolean)
   is
      use Glib;
      First : Boolean := True;
   begin

      for V of Vertices loop
         declare
            X : constant Gdouble := Gdouble (V.X);
            Y : constant Gdouble := Gdouble (V.Y);
         begin
            if First then
               Cairo.Move_To (Renderer.Context, X, Y);
               First := False;
            else
               Cairo.Line_To (Renderer.Context, X, Y);
            end if;
         end;
      end loop;

      Cairo.Line_To
        (Renderer.Context,
         Gdouble (Vertices (Vertices'First).X),
         Gdouble (Vertices (Vertices'First).Y));

      if Filled then
         Cairo.Fill_Preserve (Renderer.Context);
         Cairo.Set_Source_Rgb (Renderer.Context, 0.0, 0.0, 0.0);
         Cairo.Set_Line_Width (Renderer.Context, 1.0);
      end if;

      Cairo.Stroke (Renderer.Context);

   end Polygon;

   ------------------
   -- Pop_Viewport --
   ------------------

   overriding procedure Pop_Viewport
     (Renderer : in out Cairo_Renderer)
   is null;
--        Top     : constant Viewport_Record :=
--                    Renderer.Viewports.Last_Element;
--     begin
--        Renderer.Viewports.Delete_Last;
--
--        Cairo.Destroy (Renderer.Context);
--        Renderer.Context := Top.Context;
--
--        Cairo.Save (Renderer.Context);
--
--        Cairo.Set_Source_Surface
--          (Cr      => Renderer.Context,
--           Surface => Top.Surface,
--           X       => Glib.Gdouble (Top.Layout.X),
--           Y       => Glib.Gdouble (Top.Layout.Y));
--        Cairo.Paint (Renderer.Context);
--        Cairo.Restore (Renderer.Context);
--
--        Cairo.Surface_Destroy (Top.Surface);
--     end Pop_Viewport;

   -------------------
   -- Push_Viewport --
   -------------------

   overriding procedure Push_Viewport
     (Renderer : in out Cairo_Renderer;
      Viewport : Layout_Rectangle)
   is null;
--        Surface : constant Cairo.Cairo_Surface :=
--                    Cairo.Image_Surface.Create
--                      (Format => Cairo.Image_Surface.Cairo_Format_ARGB32,
--                       Width  => Glib.Gint (Viewport.Width),
--                       Height => Glib.Gint (Viewport.Height));
--     begin
--        Renderer.Viewports.Append
--          (Viewport_Record'
--             (Context => Renderer.Context,
--              Surface => Surface,
--              Layout  => Viewport));
--        Renderer.Context := Cairo.Create (Surface);
--     end Push_Viewport;

   ---------------
   -- Rectangle --
   ---------------

   overriding procedure Rectangle
     (Renderer : in out Cairo_Renderer;
      Rec      : Layout_Rectangle;
      Filled   : Boolean)
   is
      use Glib;
   begin
      Cairo.Rectangle
        (Cr     => Renderer.Context,
         X      => Gdouble (Rec.X),
         Y      => Gdouble (Rec.Y),
         Width  => Gdouble (Rec.Width),
         Height => Gdouble (Rec.Height));
      if Filled then
         Cairo.Fill (Renderer.Context);
      else
         Cairo.Set_Line_Width (Renderer.Context, 1.0);
         Cairo.Stroke (Renderer.Context);
      end if;
   end Rectangle;

   -------------------
   -- Refresh_Table --
   -------------------

   procedure Refresh_Table
     (Table       : Lui.Tables.Model_Table;
      Store       : Gtk.Tree_Store.Gtk_Tree_Store)
   is
      Current_Row  : Gtk.Tree_Model.Gtk_Tree_Iter :=
                          Store.Get_Iter_First;
   begin
      for I in 1 .. Table.Row_Count loop
         for J in 1 .. Table.Column_Count loop
            if Table.Cell_Changed (I, J) then
               Store.Set (Current_Row, Glib.Gint (J),
                          Table.Cell_Text (I, J));
            end if;
         end loop;

         Store.Next (Current_Row);
      end loop;

   end Refresh_Table;

   -------------------
   -- Refresh_Table --
   -------------------

   procedure Refresh_Table
     (Table : Lui.Tables.Model_Table)
   is
      use type Lui.Tables.Model_Table;
   begin
      for Info of State.Tables loop
         if Info.Table = Table then
            --  Info.View.Freeze_Child_Notify;
            Refresh_Table (Info.Table, Info.Store);
            --  Info.View.Thaw_Child_Notify;
--              Info.View.Queue_Draw;

         end if;
      end loop;
   end Refresh_Table;

   ------------------
   -- Render_Model --
   ------------------

   procedure Render_Model
     (Model   : Lui.Models.Object_Model;
      Surface : Cairo.Cairo_Surface;
      Layer   : Render_Layer)
   is
      Context : constant Cairo.Cairo_Context :=
                  Cairo.Create (Surface);

   begin
      Show_Model (Context, Layer, Model);
      Cairo.Destroy (Context);
   end Render_Model;

   -------------------------
   -- Render_Model_Layers --
   -------------------------

   procedure Render_Model_Layers
     (Model   : Lui.Models.Object_Model;
      Layers  : Surface_Render_Layers)
   is

      procedure Render_Single_Model
        (M : Lui.Models.Object_Model);

      -------------------------
      -- Render_Single_Model --
      -------------------------

      procedure Render_Single_Model
        (M : Lui.Models.Object_Model)
      is
      begin
         M.Before_Render;

         for I in 1 .. M.Last_Render_Layer loop
            if M.Render_Layer_Changed (I) then
               Render_Model (M, Layers (I), I);
               M.Clear_Render_Layer_Changed (I);
            end if;
         end loop;
         M.After_Render;

      end Render_Single_Model;

   begin

      Render_Single_Model (Model);

      if Model.Properties_Changed then
         for Gadget of Model.Gadgets loop
            Show_Gadget (Gadget, Model);
         end loop;

         Model.Clear_Changed;
      end if;

      declare
         Reload_Tables : Boolean := False;
         Tables        : constant Lui.Tables.Array_Of_Model_Tables :=
                           Model.Tables;
      begin
         for I in 1 .. Tables'Length loop
            if Tables (I).Layout_Changed then
               Reload_Tables := True;
            elsif Tables (I).Contents_Changed then
               Refresh_Table (Tables (I));
            end if;
            Tables (I).Clear_Changed;
         end loop;

         if Reload_Tables then
            State.Main.Clear_Features (UI_Table);
            State.Tables.Clear;
            for Table of Model.Tables loop
               Show_Table (Table);
            end loop;
         end if;
      end;

   end Render_Model_Layers;

   procedure Resize
     (Slot : in out Model_Object_Record'Class)
   is
      procedure Resize
        (Model  : Lui.Models.Object_Model;
         Layers : in out Surface_Render_Layers);

      ------------
      -- Resize --
      ------------

      procedure Resize
        (Model  : Lui.Models.Object_Model;
         Layers : in out Surface_Render_Layers)
      is
         use type Cairo.Cairo_Surface;
      begin

         Model.Resize;

         for Surface of Layers loop

            if Surface /= Cairo.Null_Surface then
               Cairo.Surface_Destroy (Surface);
            end if;

            Surface :=
              Cairo.Image_Surface.Create
                (Format => Cairo.Image_Surface.Cairo_Format_ARGB32,
                 Width  => Glib.Gint (Model.Width),
                 Height => Glib.Gint (Model.Height));
         end loop;

         Render_Model_Layers (Model, Layers);

      end Resize;

   begin
      for Model_Layers of Slot.Models loop
         Resize (Model_Layers.Model, Model_Layers.Layers);
      end loop;
   end Resize;

   ------------------
   -- Select_Model --
   ------------------

   procedure Select_Model (Model : Lui.Models.Object_Model) is
      use type Lui.Models.Object_Model;
      Found : Boolean := False;
      Draw  : Gtk.Drawing_Area.Gtk_Drawing_Area;
   begin
      if Model = State.Active then
         return;
      end if;

      State.Active := Model;

      for I in 1 .. State.Models.Count loop
         if State.Models.Model (I) = Model then
            Draw := State.Models.Slots.Element (I).Widget;
            State.Main.Select_Feature
              (UI_Model, Model, Draw);
            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         State.Models.Append (Model);
         Draw := State.Models.Slots.Last_Element.Widget;
      end if;

      State.Main.Clear_Features (UI_Table);
      State.Main.Clear_Features (UI_Gadget);

      for Table of Model.Tables loop
         Show_Table (Table);
      end loop;

      Model.Activate;

   end Select_Model;

   ----------------
   -- Set_Color --
   ----------------

   overriding procedure Set_Color
     (Renderer : in out Cairo_Renderer;
      Color    : Lui.Colors.Color_Type)
   is
      use Glib;
      R : constant Gdouble := Gdouble (Color.Red);
      G : constant Gdouble := Gdouble (Color.Green);
      B : constant Gdouble := Gdouble (Color.Blue);
      A : constant Gdouble := Gdouble (Color.Alpha);
   begin
      if A < 1.0 then
         Cairo.Set_Source_Rgba (Renderer.Context, R, G, B, A);
      else
         Cairo.Set_Source_Rgba (Renderer.Context, R, G, B, A);
      end if;
   end Set_Color;

   --------------
   -- Set_Font --
   --------------

   overriding procedure Set_Font
     (Renderer : in out Cairo_Renderer;
      Name     : String;
      Size     : Real;
      Bold     : Boolean := False;
      Italic   : Boolean := False)
   is
   begin
      Cairo.Select_Font_Face
        (Cr     => Renderer.Context,
         Family => Name,
         Slant  =>
           (if Italic
            then Cairo.Cairo_Font_Slant_Italic
            else Cairo.Cairo_Font_Slant_Normal),
         Weight =>
           (if Bold
            then Cairo.Cairo_Font_Weight_Bold
            else Cairo.Cairo_Font_Weight_Normal));
      Cairo.Set_Font_Size (Renderer.Context, Glib.Gdouble (Size));
   end Set_Font;

   --------------------
   -- Set_Line_Width --
   --------------------

   overriding procedure Set_Line_Width
     (Renderer : in out Cairo_Renderer;
      Width    : Positive_Real)
   is
   begin
      Cairo.Set_Line_Width (Renderer.Context,
                            Width => Glib.Gdouble (Width));
   end Set_Line_Width;

   ------------------
   -- Show_Gadgets --
   ------------------

   procedure Show_Gadget
     (Gadget : Lui.Gadgets.Model_Gadget;
      Model  : Lui.Models.Object_Model)
   is
   begin
      if Gadget.all in Lui.Gadgets.Root_Button_Gadget'Class then
         declare
            Button : Gtk.Button.Gtk_Button;
         begin
            Gtk.Button.Gtk_New (Button, Gadget.Name);

            Gadget_Button_Callback.Connect
              (Button, Gtk.Button.Signal_Clicked,
               Gadget_Button_Callback.To_Marshaller
                 (Handle_Gadget_Button'Access),
               ((Gadget, Model)));
            Button.Show_All;

            State.Main.Append_Feature (UI_Gadget, Gadget, Button);
         end;
      else
         null;
      end if;
   end Show_Gadget;

   ----------------
   -- Show_Model --
   ----------------

   procedure Show_Model
     (Context       : Cairo.Cairo_Context;
      Layer         : Render_Layer;
      Model         : Lui.Models.Object_Model)
   is
   begin
      Cairo.Save (Context);
      if Layer = Render_Layer'First then
         declare
            Bg : constant Lui.Colors.Color_Type := Model.Background;
         begin
            Cairo.Set_Source_Rgba
              (Cr    => Context,
               Red   => Glib.Gdouble (Bg.Red),
               Green => Glib.Gdouble (Bg.Green),
               Blue  => Glib.Gdouble (Bg.Blue),
               Alpha => Glib.Gdouble (Bg.Alpha));
         end;

         Cairo.Set_Operator (Context, Cairo.Cairo_Operator_Source);
      else
         Cairo.Set_Operator (Context, Cairo.Cairo_Operator_Clear);
      end if;

      Cairo.Paint (Context);
      Cairo.Restore (Context);

      declare
         Renderer : Cairo_Renderer;
      begin
         Renderer.Context := Context;
         Model.Render (Renderer, Layer);
      end;

   end Show_Model;

   ----------------
   -- Show_Model --
   ----------------

   overriding procedure Show_Model
     (State : in out Root_UI_State;
      Model : in     Lui.Models.Object_Model)
   is
      pragma Unreferenced (State);
   begin
      Select_Model (Model);
   end Show_Model;

   ----------------
   -- Show_Table --
   ----------------

   procedure Show_Table
     (Table : Lui.Tables.Model_Table)
   is
      Tree : Gtk.Tree_View.Gtk_Tree_View;
      Store : Gtk.Tree_Store.Gtk_Tree_Store;
      Types : Glib.GType_Array (0 .. Glib.Guint (Table.Column_Count));
      Label : Gtk.Label.Gtk_Label;
      Info_Box                        : Gtk.Box.Gtk_Box;
   begin

      Gtk.Box.Gtk_New
        (Info_Box, Gtk.Enums.Orientation_Vertical, 0);

      Gtk.Label.Gtk_New (Label, Table.Name);
      Label.Show_All;

      Info_Box.Pack_Start (Label,
                           Expand   => False,
                           Fill     => True,
                           Padding  => 0);

      Types (0) := Glib.GType_Int;
      for I in 1 .. Table.Column_Count loop
         Types (Glib.Guint (I)) := Glib.GType_String;
      end loop;

      Gtk.Tree_Store.Gtk_New (Store, Types);

      Load_Table (Table, Store);

      Gtk.Tree_View.Gtk_New (Tree, Store);
      State.Tables.Append ((Table, Store, Tree));

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
        (Tree, Gtk.Tree_View.Signal_Row_Activated,
         Info_Select_Row_Callback'Access,
         Table);

      declare
         Scroll : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      begin
         Gtk.Scrolled_Window.Gtk_New
           (Scroll);
         Scroll.Add (Tree);
         Scroll.Show_All;
         Info_Box.Pack_Start (Scroll,
                              Expand   => True,
                              Fill     => True,
                              Padding  => 0);
         Info_Box.Show_All;
      end;

      State.Main.Append_Feature
        (UI_Table, Table, Info_Box);

   end Show_Table;

   -----------
   -- Start --
   -----------

   procedure Start
     (Main : not null access Lui_Gtk_Interface'Class;
      Top  : Lui.Models.Object_Model)
   is
   begin
      State      := new Root_UI_State;
      Lui.Handles.Set_Current (State);
      --        State.Timeout_Id :=
--          Glib.Main.Timeout_Add
--            (Interval => 100,
--             Func     => Timeout_Handler'Access);
      State.Timeout_Id :=
        Glib.Main.Idle_Add
          (Func     => Timeout_Handler'Access);
      State.Main := Main;
      Top.Activate;
      State.Active := Top;
      State.Models.Append (Top);
   end Start;

   ----------
   -- Text --
   ----------

   overriding procedure Text
     (Renderer : in out Cairo_Renderer;
      X, Y     : Integer;
      Value    : String)
   is
   begin
      Cairo.Move_To
        (Renderer.Context,
         Glib.Gdouble (X),
         Glib.Gdouble (Y));
      Cairo.Show_Text (Renderer.Context, Value);
   end Text;

   ---------------------
   -- Timeout_Handler --
   ---------------------

   function Timeout_Handler return Boolean is
   begin
      State.Main.On_Idle;

      for I in 1 .. State.Models.Count loop
         declare
            Slot    : constant Model_Object_Access :=
                        State.Models.Slots.Element (I);
         begin

            if not Slot.Models.Is_Empty
              and then Slot.Models.First_Element.Model.Is_Active
            then
               for Model_Layer of Slot.Models loop
                  Model_Layer.Model.Update;
                  Render_Model_Layers
                    (Model_Layer.Model, Model_Layer.Layers);
               end loop;
            end if;

            Slot.Widget.Queue_Draw;
         end;
      end loop;
      return True;
   end Timeout_Handler;

end Lui.Gtk_UI;
