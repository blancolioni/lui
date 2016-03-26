with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Numerics;
with Ada.Strings.Unbounded.Hash;

with Glib;
with Glib.Main;
with Glib.Object;

with Gdk.Event;
with Gdk.Types.Keysyms;
with Gdk.Window;

with Gtk.Box;
with Gtk.Button;
with Gtk.Cell_Renderer_Text;
with Gtk.Drawing_Area;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Scrolled_Window;
with Gtk.Tree_Model;
with Gtk.Tree_Selection;
with Gtk.Tree_Store;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;

with Cairo;
with Cairo.Image_Surface;
with Cairo.Png;

with WL.Bitmap_IO;

with Lui.Colours;
with Lui.Gadgets;
with Lui.Handles;
with Lui.Rendering;
with Lui.Tables;

package body Lui.Gtk_UI is

   type Model_Object_Record is
     new Glib.Object.GObject_Record with
      record
         Model   : Lui.Models.Object_Model;
         Widget  : Gtk.Drawing_Area.Gtk_Drawing_Area;
         Surface : Cairo.Cairo_Surface := Cairo.Null_Surface;
         Width   : Glib.Gint;
         Height  : Glib.Gint;
      end record;

   type Model_Object_Access is access all Model_Object_Record'Class;

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
         Context  : Cairo.Cairo_Context;
         Origin_X : Integer := 0;
         Origin_Y : Integer := 0;
      end record;

   overriding
   procedure Set_Origin
     (Renderer : in out Cairo_Renderer;
      X, Y     : in     Integer);

   overriding
   procedure Draw_Circle
     (Renderer   : in out Cairo_Renderer;
      X, Y       : in     Integer;
      Radius     : in     Positive;
      Colour     : in     Lui.Colours.Colour_Type;
      Filled     : in     Boolean;
      Line_Width : in     Natural := 1);

   overriding
   procedure Draw_Line
     (Renderer   : in out Cairo_Renderer;
      X1, Y1     : in     Integer;
      X2, Y2     : in     Integer;
      Colour     : in     Lui.Colours.Colour_Type;
      Line_Width : Natural := 1);

   overriding
   procedure Draw_Polygon
     (Renderer : in out Cairo_Renderer;
      Vertices : Lui.Rendering.Buffer_Points;
      Colour   : Lui.Colours.Colour_Type;
      Filled   : Boolean);

   overriding
   procedure Draw_Rectangle
     (Renderer : in out Cairo_Renderer;
      X, Y     : in     Integer;
      W, H     : in     Natural;
      Colour   : in     Lui.Colours.Colour_Type;
      Filled   : in     Boolean);

   overriding
   procedure Draw_String (Renderer : in out Cairo_Renderer;
                          X, Y     : in     Integer;
                          Size     : in     Positive;
                          Colour   : in     Lui.Colours.Colour_Type;
                          Text     : in     String);

   overriding
   procedure Draw_Image (Renderer : in out Cairo_Renderer;
                         X, Y     : in     Integer;
                         W, H     : in     Positive;
                         Resource : in     String);

   overriding procedure Create_Bitmap_Resource
     (Renderer      : in out Cairo_Renderer;
      Resource_Name : in     String;
      Bitmap        : in     WL.Bitmap_IO.Bitmap_Type);

   overriding function Have_Resource
     (Renderer      : Cairo_Renderer;
      Resource_Name : String)
      return Boolean;

   procedure Set_Colour (Renderer : Cairo_Renderer'Class;
                         Colour   : Lui.Colours.Colour_Type);

   procedure Set_Line_Width (Renderer : Cairo_Renderer'Class;
                             Width    : Natural);

   Renderer : Cairo_Renderer;

   procedure Render_Model
     (Model   : Lui.Models.Object_Model;
      Surface : Cairo.Cairo_Surface;
      Width   : Glib.Gdouble;
      Height  : Glib.Gdouble);

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
         Main         : Lui_Gtk;
         Models       : Gtk_Active_Model_List;
         Tables       : Model_Table_Lists.List;
         Image_Cache  : Image_Maps.Map;
         Dragging     : Boolean := False;
         Last_Drag_X  : Integer := Integer'First;
         Last_Drag_Y  : Integer := Integer'First;
         Active       : Lui.Models.Object_Model := null;
         Timeout_Id : Glib.Main.G_Source_Id;
      end record;

   overriding procedure Show_Model
     (State : in out Root_UI_State;
      Model : in     Lui.Models.Object_Model);

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
      Width, Height : Glib.Gdouble;
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
      Label : Gtk.Label.Gtk_Label;
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

      declare
         Slot : constant Model_Object_Access :=
                  new Model_Object_Record'
                    (Glib.Object.GObject_Record with
                     Model   => Model,
                     Widget  => Page,
                     Surface => Cairo.Null_Surface,
                     Width   => 1,
                     Height  => 1);
      begin
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

      Page.Show_All;

      Gtk.Label.Gtk_New (Label, Model.Name);
      Label.Show;

      State.Main.Append_Feature (UI_Model, Model, Top => Page);

   end Append;

   -----------------------------
   -- Configure_Model_Handler --
   -----------------------------

   function Configure_Model_Handler
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Configure)
      return Boolean
   is
      use type Cairo.Cairo_Surface;
      Slot : Model_Object_Record renames
               Model_Object_Record (Self.all);
   begin
      Slot.Width := Event.Width;
      Slot.Height := Event.Height;
      if Slot.Surface /= Cairo.Null_Surface then
         Cairo.Surface_Destroy (Slot.Surface);
      end if;
      Slot.Surface :=
        Gdk.Window.Create_Similar_Surface
          (Slot.Widget.Get_Window,
           Cairo.Cairo_Content_Color_Alpha,
           Slot.Width, Slot.Height);

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
               Colour : constant WL.Bitmap_IO.Colour_Type :=
                          WL.Bitmap_IO.Colour
                            (Bitmap, X, Bitmap_Height - Y - 1);
               ARGB32 : constant Cairo.Image_Surface.ARGB32_Data :=
                          (Alpha => Byte (Colour.Alpha),
                           Blue  => Byte (Colour.B),
                           Green => Byte (Colour.G),
                           Red   => Byte (Colour.R));
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

   -----------------
   -- Draw_Circle --
   -----------------

   overriding
   procedure Draw_Circle
     (Renderer   : in out Cairo_Renderer;
      X, Y       : in     Integer;
      Radius     : in     Positive;
      Colour     : in     Lui.Colours.Colour_Type;
      Filled     : in     Boolean;
      Line_Width : in     Natural := 1)
   is
      use Glib;
   begin
      Set_Colour (Renderer, Colour);
      Set_Line_Width (Renderer, Line_Width);

      Cairo.Arc
        (Cr     => Renderer.Context,
         Xc     => Glib.Gdouble (X + Renderer.Origin_X),
         Yc     => Glib.Gdouble (Y + Renderer.Origin_Y),
         Radius => Glib.Gdouble (Radius),
         Angle1 => 0.0,
         Angle2 => 2.0 * Ada.Numerics.Pi);

      if Filled then
         Cairo.Fill (Renderer.Context);
      else
         Cairo.Stroke (Renderer.Context);
      end if;

   end Draw_Circle;

   ----------------
   -- Draw_Image --
   ----------------

   overriding
   procedure Draw_Image (Renderer : in out Cairo_Renderer;
                         X, Y     : in     Integer;
                         W, H     : in     Positive;
                         Resource : in     String)
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
            Base_Image := Cairo.Png.Create_From_Png (Path);
            State.Image_Cache.Insert (Base_Image_Key, Base_Image);
         end;
      end if;

      declare
         use Glib;
         Img_Height : constant Gint :=
                        Cairo.Image_Surface.Get_Height (Base_Image);
         Img_Width  : constant Gint :=
                        Cairo.Image_Surface.Get_Width (Base_Image);
         Height_Ratio : constant Gdouble := Gdouble (H) / Gdouble (Img_Height);
         Width_Ratio  : constant Gdouble := Gdouble (W) / Gdouble (Img_Width);
      begin

         Cairo.Save (Renderer.Context);
         Cairo.Translate
           (Renderer.Context,
            Gdouble (X + Renderer.Origin_X),
            Gdouble (Y + Renderer.Origin_Y));
         Cairo.Scale (Renderer.Context, Width_Ratio, Height_Ratio);
         Cairo.Set_Source_Surface (Renderer.Context, Base_Image, 0.0, 0.0);
         Cairo.Paint (Renderer.Context);
         Cairo.Restore (Renderer.Context);

      end;

   end Draw_Image;

   ---------------
   -- Draw_Line --
   ---------------

   overriding
   procedure Draw_Line
     (Renderer   : in out Cairo_Renderer;
      X1, Y1     : in     Integer;
      X2, Y2     : in     Integer;
      Colour     : in     Lui.Colours.Colour_Type;
      Line_Width : Natural := 1)
   is
      use Glib;
   begin
      Set_Colour (Renderer, Colour);
      Cairo.Set_Line_Width (Renderer.Context, Gdouble (Line_Width));
      Cairo.Move_To (Renderer.Context,
                     Gdouble (X1 + Renderer.Origin_X),
                     Gdouble (Y1 + Renderer.Origin_Y));
      Cairo.Line_To (Renderer.Context,
                     Gdouble (X2 + Renderer.Origin_X),
                     Gdouble (Y2 + Renderer.Origin_Y));
      Cairo.Stroke (Renderer.Context);
   end Draw_Line;

   ------------------
   -- Draw_Polygon --
   ------------------

   overriding procedure Draw_Polygon
     (Renderer : in out Cairo_Renderer;
      Vertices : Lui.Rendering.Buffer_Points;
      Colour   : Lui.Colours.Colour_Type;
      Filled   : Boolean)
   is
      use Glib;
      First : Boolean := True;
   begin
      Set_Colour (Renderer, Colour);

      for V of Vertices loop
         declare
            X : constant Gdouble := Gdouble (V.X + Renderer.Origin_X);
            Y : constant Gdouble := Gdouble (V.Y + Renderer.Origin_Y);
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
         Gdouble (Vertices (Vertices'First).X + Renderer.Origin_X),
         Gdouble (Vertices (Vertices'First).Y + Renderer.Origin_Y));

      if Filled then
         Cairo.Fill_Preserve (Renderer.Context);
         Cairo.Set_Source_Rgb (Renderer.Context, 0.0, 0.0, 0.0);
         Cairo.Set_Line_Width (Renderer.Context, 1.0);
      end if;

      Cairo.Stroke (Renderer.Context);

   end Draw_Polygon;

   --------------------
   -- Draw_Rectangle --
   --------------------

   overriding
   procedure Draw_Rectangle
     (Renderer : in out Cairo_Renderer;
      X, Y     : in     Integer;
      W, H     : in     Natural;
      Colour   : in     Lui.Colours.Colour_Type;
      Filled   : in     Boolean)
   is
      use Glib;
   begin
      Set_Colour (Renderer, Colour);
      Cairo.Rectangle
        (Cr     => Renderer.Context,
         X      => Gdouble (X + Renderer.Origin_X),
         Y      => Gdouble (Y + Renderer.Origin_Y),
         Width  => Gdouble (W),
         Height => Gdouble (H));
      if Filled then
         Cairo.Fill (Renderer.Context);
      else
         Cairo.Stroke (Renderer.Context);
      end if;
   end Draw_Rectangle;

   -----------------
   -- Draw_String --
   -----------------

   overriding
   procedure Draw_String (Renderer : in out Cairo_Renderer;
                          X, Y     : in     Integer;
                          Size     : in     Positive;
                          Colour   : in     Lui.Colours.Colour_Type;
                          Text     : in     String)
   is
   begin
      Set_Colour (Renderer, Colour);
      Cairo.Select_Font_Face (Renderer.Context, "Sans",
                              Cairo.Cairo_Font_Slant_Normal,
                              Cairo.Cairo_Font_Weight_Bold);
      Cairo.Move_To
        (Renderer.Context,
         Glib.Gdouble (X + Renderer.Origin_X),
         Glib.Gdouble (Y + Renderer.Origin_Y));
      Cairo.Set_Font_Size (Renderer.Context, Glib.Gdouble (Size));
      Cairo.Show_Text (Renderer.Context, Text);
   end Draw_String;

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
      Cairo.Set_Source_Surface
        (Cr, Slot.Surface, 0.0, 0.0);
      Cairo.Paint (Cr);
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
      use Gdk.Types.Keysyms;
      Updated : Boolean := False;
      DX, DY  : Integer := 0;
   begin
      case Gdk.Event.Get_Key_Val (Event) is
         when GDK_Left =>
            DX := -1;
         when GDK_Right =>
            DX := 1;
         when GDK_Up =>
            DY := -1;
         when GDK_Down =>
            DY := 1;
         when others =>
            null;
      end case;

      if DX /= 0 or else DY /= 0 then
         case Model.Get_Drag_Behaviour is
            when Lui.Models.Rotation =>
               --  passing DY to rotate x and DX to rotate y is not
               --  an error, but a consequence of the fact that it
               --  is a more natural rotation from the user's perspective
               Model.Rotate_X (Real (DY));
               Model.Rotate_Y (Real (DX));
            when Lui.Models.Translation =>
               Model.Move (DX, DY);
         end case;
         Updated := True;
      end if;

      if Updated then
         W.Queue_Draw;
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
      X : constant Integer := Integer (Event.Motion.X);
      Y : constant Integer := Integer (Event.Motion.Y);
   begin
      if State.Dragging then
         case Model.Get_Drag_Behaviour is
            when Lui.Models.Rotation =>
               Model.Rotate_Y ((Real (State.Last_Drag_X) - Real (X))
                               * 360.0 / Real (W.Get_Allocated_Width));
               Model.Rotate_X ((Real (Y) - Real (State.Last_Drag_Y))
                               * 360.0 / Real (W.Get_Allocated_Height));
            when Lui.Models.Translation =>
               Model.Move (X - State.Last_Drag_X, Y - State.Last_Drag_Y);
         end case;
         W.Queue_Draw;
         State.Last_Drag_X := X;
         State.Last_Drag_Y := Y;
      else
         declare
            Message : constant String :=
                        Model.Tooltip (X, Y);
         begin
            State.Main.Status_Message (Message);
         end;
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
      use Lui;
      use Glib;
      X : constant Integer := Integer (Event.Motion.X);
      Y : constant Integer := Integer (Event.Motion.Y);
   begin
      if Event.Button.Button = 1 then
         Model.Select_XY (X, Y);
         declare
            use Lui.Models;
            New_Model : constant Object_Model :=
                          Model.Select_XY
                            (X - Renderer.Origin_X,
                             Y - Renderer.Origin_Y);
         begin
            if New_Model /= null then
               Select_Model (New_Model);
            else
               State.Dragging := True;
               State.Last_Drag_X := X;
               State.Last_Drag_Y := Y;
            end if;
         end;
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
   begin
      case Event.Scroll.Direction is
         when Gdk.Event.Scroll_Up | Gdk.Event.Scroll_Left =>
            Model.Zoom
              (-1, (Event.Scroll.State and Gdk.Types.Control_Mask) /= 0);
         when Gdk.Event.Scroll_Down | Gdk.Event.Scroll_Right =>
            Model.Zoom
              (1, (Event.Scroll.State and Gdk.Types.Control_Mask) /= 0);
         when Gdk.Event.Scroll_Smooth =>
            null;
      end case;
      W.Queue_Draw;
      return True;
   end Model_Zoom_Handler;

   -------------------------
   -- On_Model_Activation --
   -------------------------

   procedure On_Model_Activation (Model : Lui.Models.Object_Model) is
   begin
      Select_Model (Model);
   end On_Model_Activation;

   ----------------------
   -- On_Model_Changed --
   ----------------------

   procedure On_Model_Changed (Model : Lui.Models.Object_Model) is
   begin
      Select_Model (Model);
   end On_Model_Changed;

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
            Info.View.Freeze_Child_Notify;
            Refresh_Table (Info.Table, Info.Store);
            Info.View.Thaw_Child_Notify;
         end if;
      end loop;
   end Refresh_Table;

   ------------------
   -- Render_Model --
   ------------------

   procedure Render_Model
     (Model   : Lui.Models.Object_Model;
      Surface : Cairo.Cairo_Surface;
      Width   : Glib.Gdouble;
      Height  : Glib.Gdouble)
   is
      Context : constant Cairo.Cairo_Context :=
                  Cairo.Create (Surface);

   begin
      Show_Model (Context, Width, Height, Model);
      Cairo.Destroy (Context);
   end Render_Model;

   ------------------
   -- Select_Model --
   ------------------

   procedure Select_Model (Model : Lui.Models.Object_Model) is
      use type Lui.Models.Object_Model;
      Found : Boolean := False;
   begin
      if Model = State.Active then
         return;
      end if;

      State.Active := Model;

      for I in 1 .. State.Models.Count loop
         if State.Models.Model (I) = Model then
            State.Main.Select_Feature
              (UI_Model, Model, State.Models.Slots.Element (I).Widget);
            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         State.Models.Append (Model);
      end if;

      State.Main.Clear_Features (UI_Table);
      State.Main.Clear_Features (UI_Gadget);

      for Table of Model.Tables loop
         Show_Table (Table);
      end loop;

   end Select_Model;

   ----------------
   -- Set_Colour --
   ----------------

   procedure Set_Colour (Renderer : Cairo_Renderer'Class;
                         Colour   : Lui.Colours.Colour_Type)
   is
      use Glib;
      R : constant Gdouble := Gdouble (Colour.Red);
      G : constant Gdouble := Gdouble (Colour.Green);
      B : constant Gdouble := Gdouble (Colour.Blue);
      A : constant Gdouble := Gdouble (Colour.Alpha);
   begin
      Cairo.Set_Source_Rgba (Renderer.Context, R, G, B, A);
   end Set_Colour;

   --------------------
   -- Set_Line_Width --
   --------------------

   procedure Set_Line_Width (Renderer : Cairo_Renderer'Class;
                             Width    : Natural)
   is
   begin
      Cairo.Set_Line_Width (Renderer.Context,
                            Width => Glib.Gdouble (Width));
   end Set_Line_Width;

     ----------------
     -- Set_Origin --
     ----------------

   overriding
   procedure Set_Origin
     (Renderer : in out Cairo_Renderer;
      X, Y     : in     Integer)
   is
   begin
      Renderer.Origin_X := X;
      Renderer.Origin_Y := Y;
   end Set_Origin;

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
      Width, Height : Glib.Gdouble;
      Model         : Lui.Models.Object_Model)
   is
   begin
      Renderer.Context := Context;
      Set_Colour (Renderer, Model.Background);

      Cairo.Rectangle
        (Cr     => Renderer.Context,
         X      => 0.0,
         Y      => 0.0,
         Width  => Width,
         Height => Height);
      Cairo.Fill (Renderer.Context);

      Model.Resize (Natural (Width), Natural (Height));
      Model.Before_Render (Renderer);
      Model.Render (Renderer);
      Model.After_Render (Renderer);

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
--        State.Timeout_Id :=
--          Glib.Main.Timeout_Add
--            (Interval => 100,
--             Func     => Timeout_Handler'Access);
      State.Timeout_Id :=
        Glib.Main.Idle_Add
          (Func     => Timeout_Handler'Access);
      State.Main := Main;
      State.Models.Append (Top);
   end Start;

   ---------------------
   -- Timeout_Handler --
   ---------------------

   function Timeout_Handler return Boolean is
   begin
      State.Main.On_Idle;
      for I in 1 .. State.Models.Count loop
         declare
            Updated : Boolean := False;
            Slot    : constant Model_Object_Access :=
                        State.Models.Slots.Element (I);
         begin
            Slot.Model.Idle_Update (Updated);
            if Updated then
               Render_Model
                 (Slot.Model, Slot.Surface,
                  Glib.Gdouble (Slot.Width),
                  Glib.Gdouble (Slot.Height));
               Slot.Widget.Queue_Draw;
            end if;
         end;
      end loop;
      return True;
   end Timeout_Handler;

end Lui.Gtk_UI;
