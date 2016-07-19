private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

with Lui.Rendering;

package Lui.Models.Charts is

   type Chart_Type is (Line, Bar);

   type Chart_Series is private;

   type Chart_Model is
     new Root_Object_Model with private;

   overriding procedure Initialise
     (Chart             : in out Chart_Model;
      Name              : in     String;
      Last_Render_Layer : Lui.Rendering.Render_Layer := 1;
      Tables            : Lui.Tables.Array_Of_Model_Tables :=
        Lui.Tables.No_Tables;
      Gadgets           : Lui.Gadgets.Array_Of_Gadgets :=
        Lui.Gadgets.No_Gadgets);

   procedure Append_Value
     (Model  : in out Chart_Model'Class;
      Series : Chart_Series;
      Value  : Real);

   function New_Series
     (Model             : in out Chart_Model;
      Name              : String;
      Series_Chart_Type : Chart_Type;
      Y2_Axis           : Boolean             := False)
      return Chart_Series;

   procedure Set_Title
     (Model : in out Chart_Model;
      Title : String);

   overriding function Tooltip
     (Item : Chart_Model;
      X, Y : Natural)
      return String;

   overriding function Long_Tooltip
     (Chart : Chart_Model;
      X, Y  : Natural)
      return String;

   overriding procedure Select_XY
     (Item : in out Chart_Model;
      X, Y : Natural);

   overriding procedure Render
     (Item     : in out Chart_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   overriding function Background
     (Item : Chart_Model)
      return Lui.Colours.Colour_Type
   is (Lui.Colours.White);

private

   type Chart_Point is
      record
         X      : Real;
         Value  : Real;
         Exists : Boolean;
      end record;

   package Point_Vectors is
     new Ada.Containers.Vectors (Positive, Chart_Point);

   type Series_Info is
      record
         Name              : Ada.Strings.Unbounded.Unbounded_String;
         Series_Chart_Type : Chart_Type;
         Y2_Axis           : Boolean;
         Points            : Point_Vectors.Vector;
      end record;

   type Chart_Series is new Positive;

   package Series_Vectors is
     new Ada.Containers.Vectors (Chart_Series, Series_Info);

   type Chart_Model is
     new Root_Object_Model with
      record
         Title  : Ada.Strings.Unbounded.Unbounded_String;
         Series : Series_Vectors.Vector;
      end record;

end Lui.Models.Charts;
