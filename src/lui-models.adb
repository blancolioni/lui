with Ada.Strings.Fixed;

with Lui.Elementary_Functions;

package body Lui.Models is

   ----------------------
   -- Add_Inline_Model --
   ----------------------

   procedure Add_Inline_Model
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
                    Top => Attach_Top, Bottom => Attach_Bottom),
         W      => Width,
         H      => Height,
         Model  => Model);
   end Add_Inline_Model;

   ----------------------
   -- Add_Inline_Model --
   ----------------------

   procedure Add_Inline_Model
     (To_Model : not null access Root_Object_Model'Class;
      Anchor   : Model_Anchor;
      W, H     : Positive;
      Model    : not null access Root_Object_Model'Class)
   is
   begin
      To_Model.Inline_Models.Append
        ((Anchor, W, H, Model));
      Model.Width := W;
      Model.Height := H;
      To_Model.Queue_Render;
   end Add_Inline_Model;

   ------------------
   -- Add_Property --
   ------------------

   procedure Add_Property (Item  : in out Root_Object_Model;
                           Name  : in     String;
                           Value : in String)
   is
   begin
      Item.Properties.Append ((new String'(Name), new String'(Value)));
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

   ------------------
   -- After_Render --
   ------------------

   procedure After_Render
     (Item     : in out Root_Object_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
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
      for Inline_Model of Item.Inline_Models loop
         declare
            Child   : constant access Root_Object_Model'Class :=
                        Inline_Model.Model;
            Anchor  : constant Model_Anchor := Inline_Model.Anchor;
            Child_X : constant Integer :=
                        Get_Start (Item.Width, Child.Width,
                                   Anchor.Left, Anchor.Right);
            Child_Y : constant Integer :=
                        Get_Start (Item.Height, Child.Height,
                                   Anchor.Top, Anchor.Bottom);
            Origin  : constant Lui.Rendering.Buffer_Point_Type :=
                        Renderer.Get_Origin;
         begin
            Child.Set_Location
              (Item.X + Child_X, Item.Y + Child_Y);
            Renderer.Draw_Rectangle
              (Child_X, Child_Y, Child.Width, Child.Height,
               Child.Background, True);
            Renderer.Draw_Rectangle
              (Child_X, Child_Y, Child.Width, Child.Height,
               Child.Border, False);
            Child.Before_Render (Renderer);
            Child.Render (Renderer);
            Child.After_Render (Renderer);
            Renderer.Set_Origin (Origin.X, Origin.Y);
         end;
      end loop;
   end After_Render;

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
      return Lui.Colours.Colour_Type
   is
   begin
      return Item.Background;
   end Background;

   -------------------
   -- Before_Render --
   -------------------

   procedure Before_Render
     (Item     : in out Root_Object_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
   begin
      Item.Queued_Render := False;
      Renderer.Set_Origin (Item.X, Item.Y);
   end Before_Render;

   ------------
   -- Border --
   ------------

   function Border
     (Item : Root_Object_Model)
      return Lui.Colours.Colour_Type
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
      X := Item.X;
      Y := Item.Y;
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
      X_2D  : constant Real :=
        (if Model.Rotated
         then X * Cos (Model.Y_Rotation, 360.0)
         + Z * Sin (Model.Y_Rotation, 360.0)
         else X);
      Y_2D  : constant Real :=
        (if Model.Rotated
         then Y * Cos (Model.X_Rotation, 360.0)
         + Z * Sin (Model.X_Rotation, 360.0)
         else Y);
      Scale : constant Real :=
                Real (Natural'Min (Model.Width, Model.Height));
   begin
      Screen_X := Integer (Scale * X_2D / Model.Eye_Z) + Model.Width / 2;
      Screen_Y := Integer (Scale * Y_2D / Model.Eye_Z) + Model.Height / 2;
      Screen_Z :=
        (if Model.Rotated
         then Z * Cos (Model.X_Rotation, 360.0) * Cos (Model.Y_Rotation)
         else Z);
   end Get_Screen_Coordinates;

   ------------
   -- Height --
   ------------

   function Height (Item : Root_Object_Model) return Natural is
   begin
      return Item.Height;
   end Height;

   -----------------
   -- Idle_Update --
   -----------------

   procedure Idle_Update
     (Model   : in out Root_Object_Model'Class;
      Updated : out Boolean)
   is
   begin
      Updated := Model.Handle_Update;
      Updated := Updated or else Model.Queued_Render;

      for Inline_Model of Model.Inline_Models loop
         declare
            Model_Updated : constant Boolean :=
                              Inline_Model.Model.Handle_Update;
         begin
            Updated := Updated or else Model_Updated;
         end;
      end loop;

   end Idle_Update;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise
     (Item              : in out Root_Object_Model;
      Name              : in     String;
      Last_Render_Layer : Lui.Rendering.Render_Layer := 1;
      Tables            : Lui.Tables.Array_Of_Model_Tables :=
        Lui.Tables.No_Tables;
      Gadgets           : Lui.Gadgets.Array_Of_Gadgets :=
        Lui.Gadgets.No_Gadgets)
   is
   begin
      Item.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Item.Last_Render_Layer := Last_Render_Layer;
      Item.Background := Lui.Colours.Black;
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
      return Lui.Rendering.Render_Layer
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

   ----------
   -- Move --
   ----------

   procedure Move (Item    : in out Root_Object_Model;
                   DX, DY  : Integer)
   is
   begin
      Item.X := Item.X + DX;
      Item.Y := Item.Y + DY;
   end Move;

   ----------
   -- Name --
   ----------

   function Name (Item : Root_Object_Model'Class) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Name);
   end Name;

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

      From_Model.Queue_Render;

      From_Model.On_Model_Removed (Model);

   end Remove_Inline_Model;

   ------------
   -- Resize --
   ------------

   procedure Resize
     (Item          : in out Root_Object_Model;
      Width, Height : Natural)
   is
   begin
      Item.Width := Width;
      Item.Height := Height;
   end Resize;

   --------------
   -- Rotate_X --
   --------------

   procedure Rotate_X (Item    : in out Root_Object_Model;
                       Degrees : in Real)
   is
   begin
      Item.X_Rotation := Item.X_Rotation + Degrees;
   end Rotate_X;

   --------------
   -- Rotate_Y --
   --------------

   procedure Rotate_Y (Item    : in out Root_Object_Model;
                       Degrees : in Real)
   is
   begin
      Item.Y_Rotation := Item.Y_Rotation + Degrees;
   end Rotate_Y;

   --------------
   -- Rotate_Z --
   --------------

   procedure Rotate_Z (Item    : in out Root_Object_Model;
                       Degrees : in Real)
   is
   begin
      Item.Z_Rotation := Item.Z_Rotation + Degrees;
   end Rotate_Z;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Item : in out Root_Object_Model'Class;
      Colour : Lui.Colours.Colour_Type)
   is
   begin
      Item.Background := Colour;
   end Set_Background;

   ----------------
   -- Set_Border --
   ----------------

   procedure Set_Border
     (Item : in out Root_Object_Model'Class;
      Colour : Lui.Colours.Colour_Type)
   is
   begin
      Item.Border := Colour;
   end Set_Border;

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
      Item.X := X;
      Item.Y := Y;
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
      return Item.Width;
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
                   Control : in Boolean)
   is
      pragma Unreferenced (Control);
   begin
      if Z < 0 then
         Item.Eye_Z := Item.Eye_Z * 0.9;
      else
         Item.Eye_Z := Item.Eye_Z / 0.9;
      end if;
   end Zoom;

end Lui.Models;
