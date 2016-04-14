private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

with Lui.Colours;
with Lui.Gadgets;
with Lui.Rendering;
with Lui.Tables;

package Lui.Models is

   type Root_Object_Model is abstract new Root_UI_Element with private;

   type Object_Model is access all Root_Object_Model'Class;

   function Row_Model (Item : Lui.Tables.Root_Model_Table;
                       Row  : Positive)
                       return Object_Model
                       is (null);

   function Name (Item : Root_Object_Model'Class) return String;

   procedure Initialise (Item    : in out Root_Object_Model;
                         Name    : in     String;
                         Tables  : Lui.Tables.Array_Of_Model_Tables :=
                           Lui.Tables.No_Tables;
                         Gadgets : Lui.Gadgets.Array_Of_Gadgets :=
                           Lui.Gadgets.No_Gadgets);

   procedure Set_Name (Item : in out Root_Object_Model'Class;
                       Name : String);

   procedure Set_Eye_Position
     (Item : in out Root_Object_Model;
      X, Y, Z : Real);

   procedure Move (Item    : in out Root_Object_Model;
                   DX, DY  : Integer);

   procedure Get_Location
     (Item : Root_Object_Model;
      X, Y : out Integer);

   procedure Set_Location
     (Item : in out Root_Object_Model;
      X, Y : Integer);

   procedure Get_Screen_Coordinates
     (Model              : Root_Object_Model;
      X, Y, Z            : Real;
      Screen_X, Screen_Y : out Integer;
      Screen_Z           : out Real);

   procedure Get_Rotation
     (Model   : Root_Object_Model'Class;
      X, Y, Z : out Real);

   function X_Rotation (Model : Root_Object_Model'Class) return Real;
   function Y_Rotation (Model : Root_Object_Model'Class) return Real;
   function Z_Rotation (Model : Root_Object_Model'Class) return Real;

   function Eye_X (Model : Root_Object_Model'Class) return Real;
   function Eye_Y (Model : Root_Object_Model'Class) return Real;
   function Eye_Z (Model : Root_Object_Model'Class) return Real;

   procedure Resize (Item          : in out Root_Object_Model;
                     Width, Height : Natural);

   procedure Zoom (Item    : in out Root_Object_Model;
                   Z       : in     Integer;
                   Control : in   Boolean);

   procedure Rotate_X (Item    : in out Root_Object_Model;
                       Degrees : in Real);

   procedure Rotate_Y (Item    : in out Root_Object_Model;
                       Degrees : in Real);

   procedure Rotate_Z (Item    : in out Root_Object_Model;
                       Degrees : in Real);

   function Tooltip (Item : Root_Object_Model;
                     X, Y : Natural)
                     return String
                     is ("");

   function Long_Tooltip (Item : Root_Object_Model;
                          X, Y : Natural)
                          return String
                          is ("");

   function Select_XY (Item : Root_Object_Model;
                       X, Y : Natural)
                       return Object_Model
   is (null);

   procedure Select_XY (Item : in out Root_Object_Model;
                        X, Y : Natural)
   is null;

   type Model_Anchor is
      record
         Left, Top, Right, Bottom : Boolean := False;
      end record;

   procedure Add_Inline_Model
     (To_Model : not null access Root_Object_Model'Class;
      Anchor   : Model_Anchor;
      W, H     : Positive;
      Model    : not null access Root_Object_Model'Class);

   procedure Add_Inline_Model
     (To_Model      : not null access Root_Object_Model'Class;
      Width         : Positive;
      Height        : Positive;
      Model         : not null access Root_Object_Model'Class;
      Attach_Left   : Boolean := False;
      Attach_Right  : Boolean := False;
      Attach_Top    : Boolean := False;
      Attach_Bottom : Boolean := False);

   procedure Remove_Inline_Model
     (From_Model : in out Root_Object_Model'Class;
      Model      : not null access Root_Object_Model'Class);

   procedure Remove_All_Inline_Models
     (From_Model : in out Root_Object_Model'Class);

   function Parent_Model
     (Model : Root_Object_Model'Class)
      return Object_Model;

   procedure On_Model_Removed
     (Model : in out Root_Object_Model;
      Child : not null access Root_Object_Model'Class)
   is null;

   type Drag_Behaviour is (Rotation, Translation);

   function Get_Drag_Behaviour
     (Model : Root_Object_Model)
      return Drag_Behaviour
   is (Rotation);

   function Gadgets
     (Model : Root_Object_Model)
      return Lui.Gadgets.Array_Of_Gadgets;

   function Properties_Changed
     (Model : Root_Object_Model)
      return Boolean;

   procedure Clear_Changed
     (Model : in out Root_Object_Model);

   function Handle_Update
     (Model : in out Root_Object_Model)
      return Boolean
   is (False);
   --  Return True if model was changed by this update

   procedure Idle_Update
     (Model   : in out Root_Object_Model'Class;
      Updated : out Boolean);

   function Property_Count (Item : Root_Object_Model) return Natural;
   function Property_Name (Item : Root_Object_Model;
                           Index : Positive)
                           return String;
   function Property_Value (Item : Root_Object_Model;
                            Index : Positive)
                            return String;

   procedure Add_Property (Item  : in out Root_Object_Model;
                           Name  : in     String;
                           Value : in String);

   procedure Add_Property (Item  : in out Root_Object_Model;
                           Name  : in     String;
                           Value : in     Long_Float;
                           Units : in     String);

   procedure Add_Property (Item  : in out Root_Object_Model;
                           Name  : in     String;
                           Value : in Integer);

   procedure Update_Property
     (Item      : in out Root_Object_Model;
      Name      : in     String;
      New_Value : in String);

   procedure Set_Background
     (Item : in out Root_Object_Model'Class;
      Colour : Lui.Colours.Colour_Type);

   function Background
     (Item : Root_Object_Model)
      return Lui.Colours.Colour_Type;

   procedure Set_Border
     (Item : in out Root_Object_Model'Class;
      Colour : Lui.Colours.Colour_Type);

   function Border
     (Item : Root_Object_Model)
      return Lui.Colours.Colour_Type;

   function Width (Item : Root_Object_Model) return Natural;
   function Height (Item : Root_Object_Model) return Natural;

   procedure Before_Render
     (Item     : in out Root_Object_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   procedure After_Render
     (Item     : in out Root_Object_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   procedure Render
     (Model    : in out Root_Object_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is abstract;

   procedure Queue_Render (Model : in out Root_Object_Model);
   function Queued_Render (Model : Root_Object_Model) return Boolean;

   function Tables (Item : Root_Object_Model'Class)
                    return Lui.Tables.Array_Of_Model_Tables;

   type Active_Model_List is tagged private;

   function Count (List : Active_Model_List)
                   return Natural;

   function Model (List : Active_Model_List;
                   Index : Positive)
                   return Object_Model;

   procedure Append (List  : in out Active_Model_List;
                     Model : Object_Model);

private

   type Property_Entry is
      record
         Name      : access String;
         Value     : access String;
      end record;

   package Property_Vectors is
     new Ada.Containers.Vectors
       (Positive, Property_Entry);

   type Inline_Model_Entry is
      record
         Anchor : Model_Anchor;
         W, H   : Positive;
         Model  : access Root_Object_Model'Class;
      end record;

   package Inline_Model_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Inline_Model_Entry);

   type Root_Object_Model is abstract new Root_UI_Element with
      record
         Name          : Ada.Strings.Unbounded.Unbounded_String;
         Parent        : Object_Model;
         First         : Boolean := True;
         Properties    : Property_Vectors.Vector;
         X, Y          : Integer := 0;
         Width, Height : Natural;
         Eye_X, Eye_Y  : Real := 0.0;
         Eye_Z         : Real := 1.0;
         Rotated       : Boolean := False;
         X_Rotation    : Real := 0.0;
         Y_Rotation    : Real := 0.0;
         Z_Rotation    : Real := 0.0;
         Background    : Lui.Colours.Colour_Type := Lui.Colours.Black;
         Border        : Lui.Colours.Colour_Type := Lui.Colours.Black;
         Inline_Models : Inline_Model_Lists.List;
         Tables        : access Lui.Tables.Array_Of_Model_Tables;
         Gadgets       : access Lui.Gadgets.Array_Of_Gadgets;
         Queued_Render : Boolean := True;
      end record;

   package Object_Model_Vectors is
     new Ada.Containers.Vectors (Positive, Object_Model);

   type Active_Model_List is tagged
      record
         Models : Object_Model_Vectors.Vector;
      end record;

end Lui.Models;
