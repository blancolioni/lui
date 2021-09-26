with Ada.Strings.Fixed;

with Lui.Elementary_Functions;
with Lui.Handles;

package body Lui.Models is

   type Root_Model_Record is
     new Root_Object_Model with null record;

   overriding procedure Render
     (Model    : in out Root_Model_Record;
      Renderer : in out Lui.Rendering.Root_Renderer'Class;
      Layer    : Render_Layer)
   is null;

   Root_Model : Object_Model;

   --------------
   -- Activate --
   --------------

   procedure Activate (Model : in out Root_Object_Model) is
   begin
      Model.Active := True;
   end Activate;

   -----------------------
   -- Active_Transition --
   -----------------------

   function Active_Transition
     (Model : Root_Object_Model)
      return Boolean
   is
   begin
      return Model.Active_Transition;
   end Active_Transition;

   ----------------------
   -- Add_Inline_Model --
   ----------------------

   procedure Add_Inline_Model
     (To_Model          : not null access Root_Object_Model'Class;
      Anchor            : Model_Anchor;
      Resizeable_Width  : Boolean;
      Resizeable_Height : Boolean;
      W, H              : Positive;
      Model             : not null access Root_Object_Model'Class)
   is
   begin
      To_Model.Inline_Models.Append
        (Inline_Model_Entry'
           (Resizeable_Width  => Resizeable_Width,
            Resizeable_Height => Resizeable_Height,
            Anchor            => Anchor,
            Model             => Model));
      Model.Layout.Width := W;
      Model.Layout.Height := H;
      Model.Parent := Object_Model (To_Model);
      Model.Anchor := Anchor;
      Model.Resizeable_Width := Resizeable_Width;
      Model.Resizeable_Height := Resizeable_Height;

      Lui.Handles.Current_UI.On_Model_Added (Model);

      To_Model.Queue_Render;
   end Add_Inline_Model;

   ----------------------
   -- Add_Offset_Model --
   ----------------------

   procedure Add_Offset_Model
     (To_Model        : not null access Root_Object_Model'Class;
      Model           : not null access Root_Object_Model'Class;
      Left_Offset     : Integer := 0;
      Top_Offset      : Integer := 0;
      Right_Offset    : Integer := 0;
      Bottom_Offset   : Integer := 0)
   is
      Anchor : constant Model_Anchor :=
                 Model_Anchor'
                   (Left          => True,
                    Top           => True,
                    Right         => True,
                    Bottom        => True,
                    Left_Offset   => Left_Offset,
                    Top_Offset    => Top_Offset,
                    Right_Offset  => Right_Offset,
                    Bottom_Offset => Bottom_Offset);
   begin
      To_Model.Add_Inline_Model
        (Anchor            => Anchor,
         Resizeable_Width  => True,
         Resizeable_Height => True,
         W                 => 1,
         H                 => 1,
         Model             => Model);
   end Add_Offset_Model;

   ------------------
   -- Add_Property --
   ------------------

   procedure Add_Property (Item  : in out Root_Object_Model;
                           Name  : in     String;
                           Value : in String)
   is
   begin
      Item.Properties.Append
        (Property_Entry'(new String'(Name), new String'(Value)));
   end Add_Property;

   ------------------
   -- Add_Property --
   ------------------

   procedure Add_Property (Item  : in out Root_Object_Model;
                           Name  : in     String;
                           Value : in     Long_Float;
                           Units : in     String)
   is
   begin
      Item.Add_Property
        (Name,
         Approximate_Image (Real (Value)) & " " & Units);
   end Add_Property;

   ------------------
   -- Add_Property --
   ------------------

   procedure Add_Property (Item  : in out Root_Object_Model;
                           Name  : in     String;
                           Value : in Integer)
   is
      use Ada.Strings, Ada.Strings.Fixed;
   begin
      Item.Add_Property (Name, Trim (Integer'Image (Value), Left));
   end Add_Property;

   ----------------------
   -- Add_Static_Model --
   ----------------------

   procedure Add_Static_Model
     (To_Model      : not null access Root_Object_Model'Class;
      Width         : Positive;
      Height        : Positive;
      Model         : not null access Root_Object_Model'Class;
      Attach_Left   : Boolean := False;
      Attach_Right  : Boolean := False;
      Attach_Top    : Boolean := False;
      Attach_Bottom : Boolean := False)
   is
   begin
      To_Model.Add_Inline_Model
        (Anchor => (Left => Attach_Left, Right => Attach_Right,
                    Top    => Attach_Top, Bottom => Attach_Bottom,
                    others => 0),
         Resizeable_Width  => False,
         Resizeable_Height => False,
         W                 => Width,
         H      => Height,
         Model  => Model);
   end Add_Static_Model;

   ------------------
   -- After_Render --
   ------------------

   procedure After_Render
     (Item     : in out Root_Object_Model)
   is null;

   ------------
   -- Append --
   ------------

   procedure Append
     (List  : in out Active_Model_List;
      Model : Object_Model)
   is
   begin
      List.Models.Append (Model);
   end Append;

   ----------------
   -- Background --
   ----------------

   function Background
     (Item : Root_Object_Model)
      return Lui.Colors.Color_Type
   is
   begin
      return Item.Background;
   end Background;

   -------------------
   -- Before_Render --
   -------------------

   procedure Before_Render
     (Item     : in out Root_Object_Model)
   is
   begin
      Item.Queued_Render := False;
      if Item.Active_Transition then
         declare
            use Ada.Calendar;
            Elapsed_Time : constant Duration :=
                             Ada.Calendar.Clock - Item.Transition_Start;
         begin
            if Elapsed_Time >= Item.Transition_Length then
               Item.Eye_X := Item.Target_X;
               Item.Eye_Y := Item.Target_Y;
               Item.Eye_Z := Item.Target_Z;
               Item.Active_Transition := False;
               Root_Object_Model'Class (Item).After_Transition;
            else
               Item.Progress :=
                 Real (Elapsed_Time) / Real (Item.Transition_Length);
               Item.Eye_X :=
                 Item.Start_X + Item.Progress * (Item.Target_X - Item.Start_X);
               Item.Eye_Y :=
                 Item.Start_Y + Item.Progress * (Item.Target_Y - Item.Start_Y);
               Item.Eye_Z :=
                 Item.Start_Z + Item.Progress * (Item.Target_Z - Item.Start_Z);
            end if;
         end;
      end if;
   end Before_Render;

   ------------
   -- Border --
   ------------

   function Border
     (Item : Root_Object_Model)
      return Lui.Colors.Color_Type
   is
   begin
      return Item.Border;
   end Border;

   -------------------
   -- Clear_Changed --
   -------------------

   procedure Clear_Changed
     (Model : in out Root_Object_Model)
   is
   begin
      Model.First := False;
   end Clear_Changed;

   --------------------------------
   -- Clear_Render_Layer_Changed --
   --------------------------------

   procedure Clear_Render_Layer_Changed
     (Model : in out Root_Object_Model'Class;
      Layer : Render_Layer)
   is
   begin
      Model.Layer_Changed (Layer) := False;
   end Clear_Render_Layer_Changed;

   -----------
   -- Count --
   -----------

   function Count
     (List : Active_Model_List)
      return Natural
   is
   begin
      return List.Models.Last_Index;
   end Count;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate (Model : in out Root_Object_Model) is
   begin
      Model.Active := False;
   end Deactivate;

   -----------------------------
   -- Drag_Rotation_Behaviour --
   -----------------------------

   procedure Drag_Rotation_Behaviour
     (Model     : in out Root_Object_Model'Class;
      Y_Axis    : Boolean := True;
      X_Axis    : Boolean := True;
      Z_Axis    : Boolean := False;
      Reverse_X : Boolean := False;
      Reverse_Y : Boolean := False;
      Reverse_Z : Boolean := False)
   is
   begin
      Model.Drag_Translates := False;
      Model.Enable_Drag_X := Y_Axis;
      Model.Enable_Drag_Y := X_Axis;
      Model.Enable_Drag_Z := Z_Axis;
      Model.Reverse_Drag_X := Reverse_X;
      Model.Reverse_Drag_Y := Reverse_Y;
      Model.Reverse_Drag_Z := Reverse_Z;
   end Drag_Rotation_Behaviour;

   -----------
   -- Eye_X --
   -----------

   function Eye_X (Model : Root_Object_Model'Class) return Real is
   begin
      return Model.Eye_X;
   end Eye_X;

   -----------
   -- Eye_Y --
   -----------

   function Eye_Y (Model : Root_Object_Model'Class) return Real is
   begin
      return Model.Eye_Y;
   end Eye_Y;

   -----------
   -- Eye_Z --
   -----------

   function Eye_Z (Model : Root_Object_Model'Class) return Real is
   begin
      return Model.Eye_Z;
   end Eye_Z;

   -------------
   -- Gadgets --
   -------------

   function Gadgets
     (Model : Root_Object_Model)
      return Lui.Gadgets.Array_Of_Gadgets is
   begin
      if Model.Gadgets = null then
         return Lui.Gadgets.No_Gadgets;
      else
         return Model.Gadgets.all;
      end if;
   end Gadgets;

   ------------------
   -- Get_Location --
   ------------------

   procedure Get_Location
     (Item : Root_Object_Model;
      X, Y : out Integer)
   is
   begin
      X := Item.Layout.X;
      Y := Item.Layout.Y;
   end Get_Location;

   ------------------
   -- Get_Rotation --
   ------------------

   procedure Get_Rotation
     (Model   : Root_Object_Model'Class;
      X, Y, Z : out Real)
   is
   begin
      X := Model.X_Rotation;
      Y := Model.Y_Rotation;
      Z := Model.Z_Rotation;
   end Get_Rotation;

   ----------------------------
   -- Get_Screen_Coordinates --
   ----------------------------

   procedure Get_Screen_Coordinates
     (Model              : Root_Object_Model;
      X, Y, Z            : Real;
      Screen_X, Screen_Y : out Integer;
      Screen_Z           : out Real)
   is
      use Lui.Elementary_Functions;
      Rot_Z : Real := Z;
      Cos_X_Rot : constant Real :=
                    (if Model.Rotated
                     then Cos (Model.X_Rotation, 360.0)
                     else 1.0);
      Cos_Y_Rot : constant Real :=
                    (if Model.Rotated
                     then Cos (Model.Y_Rotation, 360.0)
                     else 1.0);
      Sin_X_Rot : constant Real :=
                    (if Model.Rotated
                     then Sin (Model.X_Rotation, 360.0)
                     else 0.0);
      Sin_Y_Rot : constant Real :=
                    (if Model.Rotated
                     then Sin (Model.Y_Rotation, 360.0)
                     else 0.0);

      X_2D      : constant Real :=
                    (X - Model.Eye_X) * Cos_Y_Rot
                    + Z * Sin_Y_Rot;
      Y_2D      : constant Real :=
                    (Y - Model.Eye_Y) * Cos_X_Rot
                    + Z * Sin_X_Rot;
      Scale : constant Real :=
                Real (Natural'Min (Model.Width, Model.Height));
   begin
      Screen_X := Integer (Scale * X_2D / Model.Eye_Z) + Model.Width / 2;
      Screen_Y := Integer (Scale * Y_2D / Model.Eye_Z) + Model.Height / 2;
      if Model.Rotated then
--           Rot_X := X * Cos (Model.Y_Rotation, 360.0)
--             + Z * Sin (Model.Y_Rotation, 360.0);
         Rot_Z := Z * Cos_Y_Rot - X * Sin_Y_Rot;
         Rot_Z := Y * Sin_X_Rot + Rot_Z * Cos_X_Rot;
      end if;

      Screen_Z := Rot_Z - Model.Eye_Z;
   end Get_Screen_Coordinates;

   ------------
   -- Height --
   ------------

   function Height (Item : Root_Object_Model) return Natural is
   begin
      return Item.Layout.Height;
   end Height;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise
     (Item              : in out Root_Object_Model;
      Name              : in     String;
      Last_Render_Layer : Render_Layer := 1;
      Tables            : Lui.Tables.Array_Of_Model_Tables :=
        Lui.Tables.No_Tables;
      Gadgets           : Lui.Gadgets.Array_Of_Gadgets :=
        Lui.Gadgets.No_Gadgets)
   is
   begin
      if Root_Model = null then
         Root_Model := new Root_Model_Record;
         Root_Model.Layout := (0, 0, 1, 1);
      end if;

      Item.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Item.Last_Render_Layer := Last_Render_Layer;
      Item.Background := Lui.Colors.Black;
      Item.Parent := Root_Model;
      Item.Anchor := (True, True, True, True, 0, 0, 0, 0);

      Item.Properties.Clear;
      Item.Tables :=
        new Lui.Tables.Array_Of_Model_Tables'(Tables);
      Item.Gadgets :=
        new Lui.Gadgets.Array_Of_Gadgets'(Gadgets);
   end Initialise;

   -----------------------
   -- Last_Render_Layer --
   -----------------------

   function Last_Render_Layer
     (Model : Root_Object_Model'Class)
      return Render_Layer
   is
   begin
      return Model.Last_Render_Layer;
   end Last_Render_Layer;

   -----------
   -- Model --
   -----------

   function Model
     (List : Active_Model_List;
      Index : Positive)
      return Object_Model
   is
   begin
      return List.Models (Index);
   end Model;

   --------------
   -- Model_At --
   --------------

   function Model_At
     (Main_Model : not null access Root_Object_Model'Class;
      X, Y       : Integer)
      return Object_Model
   is
   begin
      for Inline of Main_Model.Inline_Models loop
         declare
            M : constant Object_Model := Object_Model (Inline.Model);
         begin
            if Contains (M.Layout, X, Y) then
               return M.Model_At (X - M.Layout.X, Y - M.Layout.Y);
            end if;
         end;
      end loop;

      return Object_Model (Main_Model);
   end Model_At;

   ----------
   -- Move --
   ----------

   procedure Move (Item    : in out Root_Object_Model;
                   DX, DY  : Integer)
   is
   begin
      Item.Layout.X := Item.Layout.X + DX;
      Item.Layout.Y := Item.Layout.Y + DY;
   end Move;

   ----------
   -- Name --
   ----------

   function Name (Item : Root_Object_Model'Class) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Name);
   end Name;

   -------------
   -- On_Drag --
   -------------

   procedure On_Drag
     (Model   : in out Root_Object_Model;
      DX, DY  : Integer)
   is
      Effective_DX : constant Integer :=
                       (if Model.Enable_Drag_X
                        then (if Model.Reverse_Drag_X then -DX else DX)
                        else 0);
      Effective_DY : constant Integer :=
                       (if Model.Enable_Drag_Y
                        then (if Model.Reverse_Drag_Y then -DY else DY)
                        else 0);
   begin
      if Effective_DX /= 0 or else Effective_DY /= 0 then
         if Model.Drag_Translates then
            Model.Move (-Effective_DX, -Effective_DY);
         else
            if Effective_DX /= 0 then
               Model.Rotate_Y (Real (Effective_DX) / 10.0);
            end if;
            if Effective_DY /= 0 then
               Model.Rotate_X (Real (Effective_DY) / 10.0);
            end if;
         end if;
      end if;
   end On_Drag;

   ------------------
   -- Parent_Model --
   ------------------

   function Parent_Model
     (Model : Root_Object_Model'Class)
      return Object_Model
   is
   begin
      return Model.Parent;
   end Parent_Model;

   ---------------
   -- Pop_Model --
   ---------------

   procedure Pop_Model
     (Current_Model : Root_Object_Model'Class)
   is
      pragma Unreferenced (Current_Model);
      Model : Object_Model;
   begin
      Lui.Handles.Current_UI.Pop_Model (Model);
      Lui.Handles.Current_UI.Show_Model (Model);
   end Pop_Model;

   ------------------------
   -- Properties_Changed --
   ------------------------

   function Properties_Changed
     (Model : Root_Object_Model)
      return Boolean
   is
   begin
      return Model.First;
   end Properties_Changed;

   --------------------
   -- Property_Count --
   --------------------

   function Property_Count (Item : Root_Object_Model) return Natural is
   begin
      return Item.Properties.Last_Index;
   end Property_Count;

   -------------------
   -- Property_Name --
   -------------------

   function Property_Name (Item : Root_Object_Model;
                           Index : Positive)
                           return String
   is
   begin
      return Item.Properties (Index).Name.all;
   end Property_Name;

   --------------------
   -- Property_Value --
   --------------------

   function Property_Value (Item : Root_Object_Model;
                            Index : Positive)
                            return String
   is
   begin
      return Item.Properties (Index).Value.all;
   end Property_Value;

   ----------------
   -- Push_Model --
   ----------------

   procedure Push_Model
     (Model     : not null access Root_Object_Model'Class;
      New_Model : not null access Root_Object_Model'Class)
   is
   begin
      Lui.Handles.Current_UI.Push_Model (Object_Model (Model));
      Lui.Handles.Current_UI.Show_Model (Object_Model (New_Model));
   end Push_Model;

   ------------------
   -- Queue_Render --
   ------------------

   procedure Queue_Render (Model : in out Root_Object_Model) is
   begin
      Model.Queued_Render := True;
   end Queue_Render;

   -------------------
   -- Queued_Render --
   -------------------

   function Queued_Render (Model : Root_Object_Model) return Boolean is
   begin
      return Model.Queued_Render;
   end Queued_Render;

   ------------------------------
   -- Remove_All_Inline_Models --
   ------------------------------

   procedure Remove_All_Inline_Models
     (From_Model : in out Root_Object_Model'Class)
   is
   begin
      while not From_Model.Inline_Models.Is_Empty loop
         declare
            M : constant Object_Model :=
                  From_Model.Inline_Models.First_Element.Model;
         begin
            From_Model.Inline_Models.Delete_First;
            M.Parent := null;
            From_Model.On_Model_Removed (M);
         end;
      end loop;

      From_Model.Queue_Render;
   end Remove_All_Inline_Models;

   -------------------------
   -- Remove_Inline_Model --
   -------------------------

   procedure Remove_Inline_Model
     (From_Model : in out Root_Object_Model'Class;
      Model      : not null access Root_Object_Model'Class)
   is
      use Inline_Model_Lists;
      use Ada.Strings.Unbounded;
      Found_Position : Cursor := No_Element;
   begin
      for Position in From_Model.Inline_Models.Iterate loop
         if Element (Position).Model = Model then
            Found_Position := Position;
            exit;
         end if;
      end loop;

      if Has_Element (Found_Position) then
         From_Model.Inline_Models.Delete (Found_Position);
      else
         raise Constraint_Error with
           "could not find model named '"
           & To_String (Model.Name) & "' in model '"
           & To_String (From_Model.Name) & "'";
      end if;

      Model.Parent := null;
      Lui.Handles.Current_UI.On_Model_Removed (Model);

      From_Model.Queue_Render;

      From_Model.On_Model_Removed (Model);

   end Remove_Inline_Model;

   ------------
   -- Resize --
   ------------

   procedure Resize
     (Model : in out Root_Object_Model)
   is
      function Get_Start
        (Parent_Length, Child_Length : Positive;
         Anchor_Start, Anchor_End    : Boolean)
         return Integer
      is (if not (Anchor_Start xor Anchor_End)
          then Parent_Length / 2 - Child_Length / 2
          elsif Anchor_End
          then Parent_Length - Child_Length
          else 0);

   begin

      if Model.Resizeable_Width then
         Model.Layout.X := Model.Anchor.Left_Offset;
         Model.Layout.Width :=
           Integer'Max
             (Model.Parent.Width
              - Model.Anchor.Right_Offset
              - Model.Layout.X,
              1);
      else
         Model.Layout.X :=
           Get_Start (Model.Parent.Width, Model.Layout.Width,
                      Model.Anchor.Left, Model.Anchor.Right);
      end if;

      if Model.Resizeable_Height then
         Model.Layout.Y := Model.Anchor.Top_Offset;
         Model.Layout.Height :=
           Integer'Max
             (Model.Parent.Height
              - Model.Anchor.Bottom_Offset
              - Model.Layout.Y,
              1);
      else
         Model.Layout.Y :=
           Get_Start (Model.Parent.Height, Model.Layout.Height,
                      Model.Anchor.Top, Model.Anchor.Bottom);
      end if;

      Model.Layer_Changed := (others => True);

   end Resize;

   --------------
   -- Rotate_X --
   --------------

   procedure Rotate_X (Item    : in out Root_Object_Model;
                       Degrees : in Real)
   is
   begin
      Item.X_Rotation := Item.X_Rotation + Degrees;
      Item.Rotated := True;
   end Rotate_X;

   --------------
   -- Rotate_Y --
   --------------

   procedure Rotate_Y (Item    : in out Root_Object_Model;
                       Degrees : in Real)
   is
   begin
      Item.Y_Rotation := Item.Y_Rotation + Degrees;
      Item.Rotated := True;
   end Rotate_Y;

   --------------
   -- Rotate_Z --
   --------------

   procedure Rotate_Z (Item    : in out Root_Object_Model;
                       Degrees : in Real)
   is
   begin
      Item.Z_Rotation := Item.Z_Rotation + Degrees;
      Item.Rotated := True;
   end Rotate_Z;

   ------------------------
   -- Scan_Inline_Models --
   ------------------------

   procedure Scan_Inline_Models
     (Parent_Model : Root_Object_Model'Class;
      Process      : not null access
        procedure (Model : Object_Model))
   is
   begin
      for Inline_Model of Parent_Model.Inline_Models loop
         Process (Object_Model (Inline_Model.Model));
      end loop;
   end Scan_Inline_Models;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Item : in out Root_Object_Model'Class;
      Color : Lui.Colors.Color_Type)
   is
   begin
      Item.Background := Color;
   end Set_Background;

   ----------------
   -- Set_Border --
   ----------------

   procedure Set_Border
     (Item : in out Root_Object_Model'Class;
      Color : Lui.Colors.Color_Type)
   is
   begin
      Item.Border := Color;
   end Set_Border;

   -----------------
   -- Set_Changed --
   -----------------

   procedure Set_Changed
     (Model : in out Root_Object_Model'Class)
   is
   begin
      Model.Layer_Changed := (others => True);
   end Set_Changed;

   ----------------------
   -- Set_Eye_Position --
   ----------------------

   procedure Set_Eye_Position
     (Item : in out Root_Object_Model;
      X, Y, Z : Real)
   is
   begin
      Item.Eye_X := X;
      Item.Eye_Y := Y;
      Item.Eye_Z := Z;
   end Set_Eye_Position;

   ------------------
   -- Set_Location --
   ------------------

   procedure Set_Location
     (Item : in out Root_Object_Model;
      X, Y : Integer)
   is
   begin
      Item.Layout.X := X;
      Item.Layout.Y := Y;
   end Set_Location;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Item : in out Root_Object_Model'Class;
                       Name : String)
   is
   begin
      Item.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

   ------------------------------
   -- Set_Render_Layer_Changed --
   ------------------------------

   procedure Set_Render_Layer_Changed
     (Model : in out Root_Object_Model'Class;
      Layer : Render_Layer)
   is
   begin
      Model.Layer_Changed (Layer) := True;
   end Set_Render_Layer_Changed;

   ---------------------
   -- Set_Screen_Size --
   ---------------------

   procedure Set_Screen_Size
     (Width, Height : Natural)
   is
   begin
      Root_Model.Layout.Width := Width;
      Root_Model.Layout.Height := Height;
   end Set_Screen_Size;

   ----------
   -- Show --
   ----------

   procedure Show
     (Model : not null access Root_Object_Model'Class)
   is
   begin
      Lui.Handles.Current_UI.Show_Model (Object_Model (Model));
   end Show;

   ----------------------
   -- Start_Transition --
   ----------------------

   procedure Start_Transition
     (Model                        : in out Root_Object_Model'Class;
      Target_X, Target_Y, Target_Z : Real;
      Length                       : Duration)
   is
   begin
      Model.Start_X := Model.Eye_X;
      Model.Start_Y := Model.Eye_Y;
      Model.Start_Z := Model.Eye_Z;
      Model.Target_X := Target_X;
      Model.Target_Y := Target_Y;
      Model.Target_Z := Target_Z;
      Model.Transition_Length := Length;
      Model.Progress := 0.0;
      Model.Transition_Start := Ada.Calendar.Clock;
      Model.Active_Transition := True;
   end Start_Transition;

   ------------
   -- Tables --
   ------------

   function Tables (Item : Root_Object_Model'Class)
                    return Lui.Tables.Array_Of_Model_Tables
   is
   begin
      if Item.Tables = null then
         return Lui.Tables.No_Tables;
      else
         return Item.Tables.all;
      end if;
   end Tables;

   -------------------
   -- Update_Models --
   -------------------

   procedure Update_Models
     (Root : in out Root_Object_Model'Class)
   is
   begin
      Root.Update;

      for Inline_Model of Root.Inline_Models loop
         Inline_Model.Model.Update_Models;
      end loop;

   end Update_Models;

   ---------------------
   -- Update_Property --
   ---------------------

   procedure Update_Property
     (Item      : in out Root_Object_Model;
      Name      : in     String;
      New_Value : in String)
   is
   begin
      for I in 1 .. Item.Properties.Last_Index loop
         if Item.Properties.Element (I).Name.all = Name then
            Item.Properties (I).Value := new String'(New_Value);
            return;
         end if;
      end loop;

      Item.Add_Property (Name, New_Value);
   end Update_Property;

   -----------
   -- Width --
   -----------

   function Width (Item : Root_Object_Model) return Natural is
   begin
      return Item.Layout.Width;
   end Width;

   ----------------
   -- X_Rotation --
   ----------------

   function X_Rotation (Model : Root_Object_Model'Class) return Real is
   begin
      return Model.X_Rotation;
   end X_Rotation;

   ----------------
   -- Y_Rotation --
   ----------------

   function Y_Rotation (Model : Root_Object_Model'Class) return Real is
   begin
      return Model.Y_Rotation;
   end Y_Rotation;

   ----------------
   -- Z_Rotation --
   ----------------

   function Z_Rotation (Model : Root_Object_Model'Class) return Real is
   begin
      return Model.Z_Rotation;
   end Z_Rotation;

   ----------
   -- Zoom --
   ----------

   procedure Zoom (Item    : in out Root_Object_Model;
                   Z       : in     Integer;
                   X, Y    : in     Integer;
                   Control : in Boolean)
   is
      pragma Unreferenced (X, Y);
      pragma Unreferenced (Control);
   begin
      if Z < 0 then
         Item.Eye_Z := Item.Eye_Z * 0.9;
      else
         Item.Eye_Z := Item.Eye_Z / 0.9;
      end if;
   end Zoom;

end Lui.Models;
