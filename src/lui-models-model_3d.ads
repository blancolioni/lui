with Ada.Numerics.Long_Real_Arrays;

private with Ada.Containers.Doubly_Linked_Lists;

with Lui.Colours;                      use Lui.Colours;
--  with Lui.Rendering;                    use Lui.Rendering;

package Lui.Models.Model_3D is

   package Matrices renames Ada.Numerics.Long_Real_Arrays;

   subtype Vector_3 is Matrices.Real_Vector (1 .. 3);

   subtype Vector_4 is Matrices.Real_Vector (1 .. 4);

   subtype Matrix_4 is Matrices.Real_Matrix (1 .. 4, 1 .. 4);

   procedure Rotate
     (Matrix  : in out Matrix_4;
      A, B, C : Real);

   procedure Translate
     (Matrix  : in out Matrix_4;
      X, Y, Z : Real);

   type Root_3D_Model is
     abstract new Root_Object_Model with private;

   procedure Create_Scene (Item : in out Root_3D_Model) is abstract;

   type Render_Mode is (Normal, Object_Ids);

   procedure Set_Render_Mode
     (Model : in out Root_3D_Model'Class;
      Mode  : Render_Mode);

   overriding procedure Render
     (Model    : in out Root_3D_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class);

   procedure Clear_Matrix
     (Model : in out Root_3D_Model'Class);

   procedure Push_Matrix
     (Model : in out Root_3D_Model'Class);

   procedure Push_Matrix
     (Model : in out Root_3D_Model'Class;
      Matrix : in Matrix_4);

   procedure Pop_Matrix
     (Model : in out Root_3D_Model'Class);

   procedure Rotate
     (Model   : in out Root_3D_Model'Class;
      A, B, C : Real);

   procedure Translate
     (Model   : in out Root_3D_Model'Class;
      X, Y, Z : in     Real                 := 0.0);

   procedure Multiply
     (Model   : in out Root_3D_Model'Class;
      Matrix  : Matrix_4);

   procedure Begin_Object
     (Model : in out Root_3D_Model'Class;
      Id    : Positive);

   procedure End_Object
     (Model : in out Root_3D_Model'Class);

   procedure Begin_Surface
     (Model  : in out Root_3D_Model'Class;
      Colour : Lui.Colours.Colour_Type);

   procedure Vertex
     (Model : in out Root_3D_Model'Class;
      X, Y, Z : Real);

   procedure Vertex
     (Model : in out Root_3D_Model'Class;
      V     : Vector_3);

   procedure End_Surface
     (Model  : in out Root_3D_Model'Class);

   procedure Sphere
     (Model      : in out Root_3D_Model'Class;
      Colour     : in     Colour_Type;
      RX, RY, RZ : in     Real;
      Detail     : in     Positive);

   procedure Icosohedral_Sphere
     (Model      : in out Root_3D_Model'Class;
      Colour     : in     Colour_Type;
      RX, RY, RZ : in     Real;
      Detail     : in     Positive);

   procedure Cylinder
     (Model      : in out Root_3D_Model'Class;
      Colour     : in     Colour_Type;
      X, Y, Z    : in     Real;
      RX, RY     : in     Real;
      DZ         : in     Real;
      Detail     : in     Positive);
   --  Draw a cylinder with the base centred at (X, Y, Z), pointing along
   --  the Z axis, with base radius RX, RY.

   procedure Cone
     (Model      : in out Root_3D_Model'Class;
      Colour     : in     Colour_Type;
      X, Y, Z    : in     Real;
      RX, RY     : in     Real;
      DZ         : in     Real;
      Detail     : in     Positive);

   --  Draw a cone with the base centred at (X, Y, Z), pointing along
   --  the Z axis, with base radius RX, RY.

   procedure Conical_Frustum
     (Model      : in out Root_3D_Model'Class;
      Colour     : in     Colour_Type;
      X, Y, Z    : in     Real;
      RX1, RY1   : in     Real;
      RX2, RY2   : in     Real;
      DZ         : in     Real;
      Detail     : in     Positive);

   --  Draw a cone with the base centred at (X, Y, Z), pointing along
   --  the Z axis, with base radius RX1, RY1 and top radius RX2, RY2.
   --  if RX1 = RX2 and RY1 = RY2, this is equivalent to calling Cylindar
   --  if RX2 = RY2 = 0, this is equivalent to calling Cone

private

   package Vertex_Vectors is
     new Ada.Containers.Vectors
       (Positive, Vector_3, Matrices."=");

   type Surface is
      record
         Colour : Colour_Type;
         Vs     : Vertex_Vectors.Vector;
      end record;

   package Surface_Vectors is
     new Ada.Containers.Vectors (Positive, Surface);

   package Matrix_Stacks is
     new Ada.Containers.Doubly_Linked_Lists
       (Matrix_4, Matrices."=");

   type Root_3D_Model is abstract new Root_Object_Model with
      record
         Eye_Vector          : Vector_3;
         Current_Surface     : Surface;
         Current_Matrix      : Matrix_4;
         Matrices            : Matrix_Stacks.List;
         Surfaces            : Surface_Vectors.Vector;
         Current_Object_Id   : Natural := 0;
         Object_Id_Colour    : Lui.Colours.Colour_Type;
         Current_Render_Mode : Render_Mode := Normal;
      end record;

end Lui.Models.Model_3D;
