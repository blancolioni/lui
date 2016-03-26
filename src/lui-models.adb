with Ada.Strings.Fixed;

with Lui.Elementary_Functions;

package body Lui.Models is

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
   begin
      null;
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
      Renderer.Set_Origin (Item.X, Item.Y);
   end Before_Render;

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
     (Model   : in out Root_Object_Model;
      Updated : out Boolean)
   is
      pragma Unreferenced (Model);
   begin
      Updated := False;
   end Idle_Update;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise
     (Item    : in out Root_Object_Model;
      Name    : in     String;
      Tables  : Lui.Tables.Array_Of_Model_Tables := Lui.Tables.No_Tables;
      Gadgets : Lui.Gadgets.Array_Of_Gadgets := Lui.Gadgets.No_Gadgets)
   is
   begin
      Item.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Item.Background := Lui.Colours.Black;
      Item.Properties.Clear;
      Item.Tables :=
        new Lui.Tables.Array_Of_Model_Tables'(Tables);
      Item.Gadgets :=
        new Lui.Gadgets.Array_Of_Gadgets'(Gadgets);
   end Initialise;

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
