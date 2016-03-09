private with Ada.Containers.Doubly_Linked_Lists;

with Lui.Rendering;                    use Lui.Rendering;

package Lui.Models.Containers is

   type Root_Container_Model is abstract new Root_Object_Model with private;

   overriding function Tooltip
     (Item : Root_Container_Model;
      X, Y : Natural)
      return String;

   overriding function Long_Tooltip
     (Item : Root_Container_Model;
      X, Y : Natural)
      return String;

   overriding procedure Select_XY
     (Item : in out Root_Container_Model;
      X, Y : Natural);

   overriding procedure Render
     (Item     : in out Root_Container_Model;
      Renderer : in out Root_Renderer'Class);

   procedure Add
     (Container : in out Root_Container_Model;
      Item      : not null access Root_Object_Model'Class);

   function Count
     (Container : Root_Container_Model)
      return Natural;

   procedure Iterate
     (Container : Root_Container_Model'Class;
      Process   : not null access
        procedure (Item : Object_Model));

   procedure Set_Child_Border_Style
     (Model  : in out Root_Container_Model'Class;
      Colour : Colour_Type;
      Width  : Natural);

   function Child_Border_Width
     (Model : Root_Container_Model'Class)
      return Natural;

   function Child_Border_Colour
     (Model : Root_Container_Model'Class)
      return Colour_Type;

private

   package Object_Model_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Object_Model);

   type Root_Container_Model is abstract new Root_Object_Model with
      record
         Children            : Object_Model_Lists.List;
         Child_Border_Colour : Colour_Type := Black;
         Child_Border_Width  : Natural := 0;
      end record;

   function Child_Model
     (Container : Root_Container_Model'Class;
      X, Y      : Integer)
      return Object_Model;

end Lui.Models.Containers;
