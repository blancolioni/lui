private with Ada.Calendar;
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

   procedure Initialise
     (Item              : in out Root_Object_Model;
      Name              : in     String;
      Last_Render_Layer : Lui.Rendering.Render_Layer := 1;
      Tables            : Lui.Tables.Array_Of_Model_Tables :=
        Lui.Tables.No_Tables;
      Gadgets           : Lui.Gadgets.Array_Of_Gadgets :=
        Lui.Gadgets.No_Gadgets);

   function Is_Active (Model : Root_Object_Model) return Boolean;
   procedure Activate (Model : in out Root_Object_Model);
   procedure Deactivate (Model : in out Root_Object_Model);

   function Last_Render_Layer
     (Model : Root_Object_Model'Class)
      return Lui.Rendering.Render_Layer;

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
                   X, Y    : in     Integer;
                   Control : in     Boolean);

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

   function Select_XY (Item : in out Root_Object_Model;
                       X, Y : Natural)
                       return Object_Model
   is (null);

   procedure Select_XY (Item : not null access Root_Object_Model;
                        X, Y : Natural)
   is null;

   procedure On_Key_Press
     (Item : in out Root_Object_Model;
      Key  : Character)
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

   procedure Scan_Inline_Models
     (Parent_Model : Root_Object_Model'Class;
      Process      : not null access
        procedure (Model : Object_Model));

   function Parent_Model
     (Model : Root_Object_Model'Class)
      return Object_Model;

   function Model_At
     (Main_Model : not null access Root_Object_Model'Class;
      X, Y       : Integer)
      return Object_Model;

   procedure On_Model_Removed
     (Model : in out Root_Object_Model;
      Child : not null access Root_Object_Model'Class)
   is null;

   procedure On_Drag
     (Model   : in out Root_Object_Model;
      DX, DY  : Integer);

   procedure Drag_Rotation_Behaviour
     (Model     : in out Root_Object_Model'Class;
      Y_Axis    : Boolean := True;
      X_Axis    : Boolean := True;
      Z_Axis    : Boolean := False;
      Reverse_X : Boolean := False;
      Reverse_Y : Boolean := False;
      Reverse_Z : Boolean := False);

   function Gadgets
     (Model : Root_Object_Model)
      return Lui.Gadgets.Array_Of_Gadgets;

   function Properties_Changed
     (Model : Root_Object_Model)
      return Boolean;

   procedure Clear_Changed
     (Model : in out Root_Object_Model);

   procedure Show
     (Model : not null access Root_Object_Model'Class);

   procedure Push_Model
     (Model     : not null access Root_Object_Model'Class;
      New_Model : not null access Root_Object_Model'Class);

   procedure Pop_Model
     (Current_Model : Root_Object_Model'Class);

   function Active_Transition
     (Model : Root_Object_Model)
      return Boolean;

   procedure After_Transition
     (Model : in out Root_Object_Model)
   is null;

   function Handle_Update
     (Model : in out Root_Object_Model)
      return Boolean
   is (Model.Active_Transition);
   --  Return True if model was changed by this update

   procedure Idle_Update
     (Model   : in out Root_Object_Model'Class;
      Updated : out Boolean);

   function Render_Layer_Changed
     (Model : in out Root_Object_Model'Class;
      Layer : Lui.Rendering.Render_Layer)
      return Boolean
   is (True);

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

   procedure Start_Transition
     (Model                        : in out Root_Object_Model'Class;
      Target_X, Target_Y, Target_Z : Real;
      Length                       : Duration);

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
         Name              : Ada.Strings.Unbounded.Unbounded_String;
         Parent            : Object_Model;
         Last_Render_Layer : Lui.Rendering.Render_Layer;
         First             : Boolean := True;
         Active            : Boolean := False;
         Properties        : Property_Vectors.Vector;
         X, Y              : Integer := 0;
         Width, Height     : Natural := 0;
         Eye_X, Eye_Y      : Real := 0.0;
         Eye_Z             : Real := 1.0;
         Rotated           : Boolean := False;
         X_Rotation        : Real := 0.0;
         Y_Rotation        : Real := 0.0;
         Z_Rotation        : Real := 0.0;
         Background        : Lui.Colours.Colour_Type := Lui.Colours.Black;
         Border            : Lui.Colours.Colour_Type := Lui.Colours.Black;
         Inline_Models     : Inline_Model_Lists.List;
         Tables            : access Lui.Tables.Array_Of_Model_Tables;
         Gadgets           : access Lui.Gadgets.Array_Of_Gadgets;
         Internal_Changed  : Boolean := False;
         Queued_Render     : Boolean := True;
         Drag_Translates   : Boolean := True;
         Enable_Drag_X     : Boolean := True;
         Enable_Drag_Y     : Boolean := True;
         Enable_Drag_Z     : Boolean := False;
         Reverse_Drag_X    : Boolean := True;
         Reverse_Drag_Y    : Boolean := True;
         Reverse_Drag_Z    : Boolean := False;
         Active_Transition : Boolean := False;
         Transition_Length : Duration;
         Transition_Start  : Ada.Calendar.Time;
         Start_X           : Real;
         Start_Y           : Real;
         Start_Z           : Real;
         Target_X          : Real;
         Target_Y          : Real;
         Target_Z          : Real;
         Progress          : Unit_Real;
      end record;

   function Is_Active (Model : Root_Object_Model) return Boolean
   is (Model.Active);

   package Object_Model_Vectors is
     new Ada.Containers.Vectors (Positive, Object_Model);

   type Active_Model_List is tagged
      record
         Models : Object_Model_Vectors.Vector;
      end record;

end Lui.Models;
