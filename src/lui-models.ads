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

   procedure Move (Item  : in out Root_Object_Model;
                   X, Y  : Integer);

   procedure Get_Location
     (Item : Root_Object_Model;
      X, Y : out Integer);

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

   function Gadgets
     (Model : Root_Object_Model)
      return Lui.Gadgets.Array_Of_Gadgets;

   function Properties_Changed
     (Model : Root_Object_Model)
      return Boolean;

   procedure Clear_Changed
     (Model : in out Root_Object_Model);

   procedure Idle_Update
     (Model   : in out Root_Object_Model;
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

   procedure Set_Background
     (Item : in out Root_Object_Model'Class;
      Colour : Lui.Colours.Colour_Type);

   function Background (Item : Root_Object_Model)
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

   type Root_Object_Model is abstract new Root_UI_Element with
      record
         Name          : Ada.Strings.Unbounded.Unbounded_String;
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
         Tables        : access Lui.Tables.Array_Of_Model_Tables;
         Gadgets       : access Lui.Gadgets.Array_Of_Gadgets;
      end record;

   package Object_Model_Vectors is
     new Ada.Containers.Vectors (Positive, Object_Model);

   type Active_Model_List is tagged
      record
         Models : Object_Model_Vectors.Vector;
      end record;

end Lui.Models;
