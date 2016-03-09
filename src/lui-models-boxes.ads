with Lui.Models.Containers;

package Lui.Models.Boxes is

   type Box_Direction is (Horizontal, Vertical);

   type Root_Box_Model is
     new Lui.Models.Containers.Root_Container_Model with private;

   procedure Initialise
     (Model     : in out Root_Box_Model;
      Direction : Box_Direction);

   overriding procedure Resize
     (Model         : in out Root_Box_Model;
      Width, Height : Natural);

   type Box_Model is access all Root_Box_Model'Class;

   function Create (Direction : Box_Direction) return Box_Model;

private

   type Root_Box_Model is
     new Lui.Models.Containers.Root_Container_Model with
      record
         Direction : Box_Direction;
      end record;

end Lui.Models.Boxes;
