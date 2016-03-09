with Lui.Elementary_Functions;

package body Lui.Models.Model_3D is

   Wireframe : constant Boolean := False;

   type Z_Buffered_Surface is
      record
         Z      : Real;
         Pts    : Buffer_Points (1 .. 50);
         Count  : Natural;
         Colour : Colour_Type;
         Filled : Boolean;
      end record;

   package Z_Buffer_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Z_Buffered_Surface);

   Z_Buffer : Z_Buffer_Lists.List;

   procedure Clear_Z_Buffer;
   procedure Add_To_Z_Buffer
     (Z      : Real;
      Pts    : Buffer_Points;
      Colour : Colour_Type;
      Filled : Boolean);

   procedure Draw_Z_Buffer
     (Renderer : in out Root_Renderer'Class);

   ---------------------
   -- Add_To_Z_Buffer --
   ---------------------

   procedure Add_To_Z_Buffer
     (Z      : Real;
      Pts    : Buffer_Points;
      Colour : Colour_Type;
      Filled : Boolean)
   is
      use Z_Buffer_Lists;
      Position : Cursor := Z_Buffer.First;
      S : Z_Buffered_Surface;
   begin
      S.Z := Z;
      S.Pts (Pts'Range) := Pts;
      S.Count := Pts'Length;
      S.Colour := Colour;
      S.Filled := Filled;

      while Has_Element (Position)
        and then Element (Position).Z < Z
      loop
         Next (Position);
      end loop;
      if Has_Element (Position) then
         Z_Buffer.Insert (Position, S);
      else
         Z_Buffer.Append (S);
      end if;
   end Add_To_Z_Buffer;

   -------------------
   -- Begin_Surface --
   -------------------

   procedure Begin_Surface
     (Model  : in out Root_3D_Model'Class;
      Colour : Colour_Type)
   is
   begin
      Model.Current_Surface.Vs.Clear;
      Model.Current_Surface.Colour := Colour;
   end Begin_Surface;

   ------------------
   -- Clear_Matrix --
   ------------------

   procedure Clear_Matrix
     (Model : in out Root_3D_Model'Class)
   is
   begin
      Model.Current_Matrix := Matrices.Unit_Matrix (4);
   end Clear_Matrix;

   --------------------
   -- Clear_Z_Buffer --
   --------------------

   procedure Clear_Z_Buffer is
   begin
      Z_Buffer.Clear;
   end Clear_Z_Buffer;

   ----------
   -- Cone --
   ----------

   procedure Cone
     (Model      : in out Root_3D_Model'Class;
      Colour     : in     Colour_Type;
      X, Y, Z    : in     Real;
      RX, RY     : in     Real;
      DZ         : in     Real;
      Detail     : in     Positive)
   is
   begin
      Model.Conical_Frustum
        (Colour => Colour,
         X      => X,
         Y      => Y,
         Z      => Z,
         RX1    => RX,
         RY1    => RY,
         RX2    => 0.0,
         RY2    => 0.0,
         DZ     => DZ,
         Detail => Detail);
   end Cone;

   ---------------------
   -- Conical_Frustum --
   ---------------------

   procedure Conical_Frustum
     (Model      : in out Root_3D_Model'Class;
      Colour     : in     Colour_Type;
      X, Y, Z    : in     Real;
      RX1, RY1   : in     Real;
      RX2, RY2   : in     Real;
      DZ         : in     Real;
      Detail     : in     Positive)
   is
      use Matrices;
      Circles  : array (0 .. Detail, 0 .. 2 * Detail - 1) of Vector_3;
      Heights  : array (0 .. Detail) of Real;
   begin
      for I in Heights'Range loop
         Heights (I) := -DZ + 2.0 * Real (I) * DZ / Real (Detail);
      end loop;
      for I in Circles'Range (2) loop
         declare
            use Lui.Elementary_Functions;
            Longitude : constant Real :=
                          Real (I) * 180.0 / Real (Detail);
         begin
            for H in Circles'Range (1) loop
               declare
                  Factor : constant Real := Real (H) / Real (Detail);
                  X      : constant Real := (RX2 - RX1) * Factor + RX1;
                  Y      : constant Real := (RY2 - RY1) * Factor + RY1;
               begin
                  Circles (H, I) :=
                    (1 => X * Cos (Longitude, 360.0),
                     2 => Y * Sin (Longitude, 360.0),
                     3 => Heights (H));
               end;
            end loop;
         end;
      end loop;

      for H in 0 .. Detail - 1 loop
         for Longitude_Index in Circles'Range (2) loop
            declare
               Long_1 : constant Natural :=
                          (if Longitude_Index < 2 * Detail - 1
                           then Longitude_Index + 1
                           else 0);
               V1     : constant Vector_3 :=
                          Circles (H, Longitude_Index);
               V2 : constant Vector_3 :=
                          Circles (H, Long_1);
               V3     : constant Vector_3 :=
                          Circles (H + 1, Long_1);
               V4     : constant Vector_3 :=
                          Circles (H + 1, Longitude_Index);
            begin
               Model.Begin_Surface (Colour);
               Model.Vertex (V1 (1) + X, V1 (2) + Y, Z + V1 (3));
               Model.Vertex (V2 (1) + X, V2 (2) + Y, Z + V2 (3));
               Model.Vertex (V3 (1) + X, V3 (2) + Y, Z + V3 (3));
               Model.Vertex (V4 (1) + X, V4 (2) + Y, Z + V4 (3));
               Model.End_Surface;
            end;
         end loop;
      end loop;

      if RX1 > 0.0 and then RY1 > 0.0 then
         Model.Begin_Surface (Colour);
         for I in reverse Circles'Range (2) loop
            Model.Vertex (Circles (0, I));
         end loop;
         Model.End_Surface;
      end if;

      if RX2 > 0.0 and then RY2 > 0.0 then
         Model.Begin_Surface (Colour);
         for I in Circles'Range (2) loop
            Model.Vertex (Circles (Detail, I));
         end loop;
         Model.End_Surface;
      end if;

   end Conical_Frustum;

   --------------
   -- Cylinder --
   --------------

   procedure Cylinder
     (Model      : in out Root_3D_Model'Class;
      Colour     : in     Colour_Type;
      X, Y, Z    : in     Real;
      RX, RY     : in     Real;
      DZ         : in     Real;
      Detail     : in     Positive)
   is
   begin
      Model.Conical_Frustum
        (Colour => Colour,
         X      => X,
         Y      => Y,
         Z      => Z,
         RX1    => RX,
         RY1    => RY,
         RX2    => RX,
         RY2    => RY,
         DZ     => DZ,
         Detail => Detail);
   end Cylinder;

   -------------------
   -- Draw_Z_Buffer --
   -------------------

   procedure Draw_Z_Buffer
     (Renderer : in out Root_Renderer'Class)
   is
   begin
      for S of Z_Buffer loop
         Renderer.Draw_Polygon
           (Vertices => S.Pts (1 .. S.Count),
            Colour   => S.Colour,
            Filled   => S.Filled);
      end loop;
   end Draw_Z_Buffer;

   -----------------
   -- End_Surface --
   -----------------

   procedure End_Surface
     (Model  : in out Root_3D_Model'Class)
   is
   begin
      Model.Surfaces.Append (Model.Current_Surface);
   end End_Surface;

   ------------------------
   -- Icosohedral_Sphere --
   ------------------------

   procedure Icosohedral_Sphere
     (Model      : in out Root_3D_Model'Class;
      Colour     : in     Colour_Type;
      RX, RY, RZ : in     Real;
      Detail     : in     Positive)
   is
      use Matrices;
      X    : constant := 0.525731112119133606;
      Z    : constant := 0.850650808352039932;

      Vertex_Data : constant array (1 .. 12) of Vector_3 :=
                      ((-X, 0.0, Z), (X, 0.0, Z), (-X, 0.0, -Z), (X, 0.0, -Z),
                       (0.0, Z, X), (0.0, Z, -X), (0.0, -Z, X), (0.0, -Z, -X),
                       (Z, X, 0.0), (-Z, X, 0.0), (Z, -X, 0.0), (-Z, -X, 0.0));
      Triangles : constant array (1 .. 20, 1 .. 3) of Positive :=
                ((2, 5, 1), (5, 10, 1), (5, 6, 10), (9, 6, 5), (2, 9, 5),
                 (2, 11, 9), (11, 4, 9), (9, 4, 6), (4, 3, 6), (4, 8, 3),
                 (4, 11, 8), (11, 7, 8), (7, 12, 8), (7, 1, 12), (7, 2, 1),
                 (11, 2, 7), (12, 1, 10), (3, 12, 10), (6, 3, 10), (12, 3, 8));

      procedure Subdivide
        (V1, V2, V3 : Vector_3;
         Depth      : Natural);

      ---------------
      -- Subdivide --
      ---------------

      procedure Subdivide
        (V1, V2, V3 : Vector_3;
         Depth      : Natural)
      is
      begin
         if Depth = 0 then
            Model.Begin_Surface (Colour);
            Model.Vertex (V1 (1) * RX, V1 (2) * RY, V1 (3) * RZ);
            Model.Vertex (V2 (1) * RX, V2 (2) * RY, V2 (3) * RZ);
            Model.Vertex (V3 (1) * RX, V3 (2) * RY, V3 (3) * RZ);
            Model.End_Surface;
         else
            declare
               V12 : Vector_3 :=  (V1 + V2) / 2.0;
               V23 : Vector_3 :=  (V2 + V3) / 2.0;
               V31 : Vector_3 :=  (V3 + V1) / 2.0;
            begin
               V12 := V12 / abs V12;
               V23 := V23 / abs V23;
               V31 := V31 / abs V31;
               Subdivide (V1, V12, V31, Depth - 1);
               Subdivide (V2, V23, V12, Depth - 1);
               Subdivide (V3, V31, V23, Depth - 1);
               Subdivide (V12, V23, V31, Depth - 1);
            end;
         end if;
      end Subdivide;

   begin
      for I in Triangles'Range (1) loop
         Subdivide (Vertex_Data (Triangles (I, 1)),
                    Vertex_Data (Triangles (I, 2)),
                    Vertex_Data (Triangles (I, 3)),
                    Detail);
      end loop;
   end Icosohedral_Sphere;

   --------------
   -- Multiply --
   --------------

   procedure Multiply
     (Model   : in out Root_3D_Model'Class;
      Matrix  : Matrix_4)
   is
      use Matrices;
   begin
      Model.Current_Matrix := Model.Current_Matrix * Matrix;
   end Multiply;

   ----------------
   -- Pop_Matrix --
   ----------------

   procedure Pop_Matrix
     (Model : in out Root_3D_Model'Class)
   is
   begin
      Model.Current_Matrix := Model.Matrices.Last_Element;
      Model.Matrices.Delete_Last;
   end Pop_Matrix;

   -----------------
   -- Push_Matrix --
   -----------------

   procedure Push_Matrix
     (Model : in out Root_3D_Model'Class)
   is
   begin
      Model.Matrices.Append (Model.Current_Matrix);
   end Push_Matrix;

   -----------------
   -- Push_Matrix --
   -----------------

   procedure Push_Matrix
     (Model : in out Root_3D_Model'Class;
      Matrix : in Matrix_4)
   is
   begin
      Model.Matrices.Append (Matrix);
   end Push_Matrix;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Model    : in out Root_3D_Model;
      Renderer : in out Root_Renderer'Class)
   is
      View_Transform : Matrix_4 := Matrices.Unit_Matrix (4);
   begin

      Model.Surfaces.Clear;
      Model.Clear_Matrix;

      Rotate
        (View_Transform,
         Model.X_Rotation, Model.Y_Rotation, Model.Z_Rotation);

      Root_3D_Model'Class (Model).Create_Scene;

      Clear_Z_Buffer;

      for Surface of Model.Surfaces loop
         declare
            use Matrices;
            Vs : array (1 .. Surface.Vs.Last_Index) of Vector_4;
            Pts : Buffer_Points (Vs'Range);
            Z_Coord : Real := 0.0;
         begin

            for I in Vs'Range loop
               Vs (I) (1 .. 3) := Surface.Vs (I);
               Vs (I) (4) := 1.0;
               Vs (I) := View_Transform * Vs (I);
               Z_Coord := Z_Coord + Vs (I) (3);

               declare
                  Z : constant Real :=
                        100.0 / (Vs (I) (3) - 50.0) / Model.Eye_Z;
               begin
                  Pts (I).X :=
                    Integer (Vs (I) (1) * 16.0 * Z) + Model.Width / 2;
                  Pts (I).Y :=
                    Integer (Vs (I) (2) * 16.0 * Z) + Model.Height / 2;
               end;
            end loop;

            Z_Coord := Z_Coord / Real (Vs'Length);

            if Vs'Length >= 3 then
               declare
                  A : constant Vector_4 := Vs (2) - Vs (1);
                  B : constant Vector_4 := Vs (3) - Vs (2);
                  Z : constant Real :=
                        A (1) * B (2) - A (2) * B (1);
               begin
                  if Z >= 0.0 then
                     Add_To_Z_Buffer
                       (Z_Coord, Pts, Surface.Colour, not Wireframe);
--                       for I in Xs'Range loop
--                          declare
--                             J  : constant Positive :=
--                                    (if I = Xs'Last then 1 else I + 1);
--                          begin
--                             Renderer.Draw_Line
--                               (Xs (I), Ys (I), Xs (J), Ys (J),
--                                Colour => Surface.Colour);
--                          end;
--                       end loop;
                  end if;
               end;
            end if;
         end;
      end loop;

      Draw_Z_Buffer (Renderer);

   end Render;

   ------------
   -- Rotate --
   ------------

   procedure Rotate
     (Matrix  : in out Matrix_4;
      A, B, C : Real)
   is
      use Matrices;
      use Elementary_Functions;
      Cos_A : constant Real := Cos (A, 360.0);
      Sin_A : constant Real := Sin (A, 360.0);
      Cos_B : constant Real := Cos (B, 360.0);
      Sin_B : constant Real := Sin (B, 360.0);
      Cos_C : constant Real := Cos (C, 360.0);
      Sin_C : constant Real := Sin (C, 360.0);

      MX : constant Matrix_4 := ((1.0, 0.0, 0.0, 0.0),
                                 (0.0, Cos_A, -Sin_A, 0.0),
                                 (0.0, Sin_A, Cos_A, 0.0),
                                 (0.0, 0.0, 0.0, 1.0));
      MY    : constant Matrix_4 := ((Cos_B, 0.0, Sin_B, 0.0),
                                    (0.0, 1.0, 0.0, 0.0),
                                    (-Sin_B, 0.0, Cos_B, 0.0),
                                    (0.0, 0.0, 0.0, 1.0));
      MZ    : constant Matrix_4 := ((Cos_C, -Sin_C, 0.0, 0.0),
                                    (Sin_C, Cos_C, 0.0, 0.0),
                                    (0.0, 0.0, 1.0, 0.0),
                                    (0.0, 0.0, 0.0, 1.0));
   begin
      Matrix := MX * MY * MZ * Matrix;
   end Rotate;

   ------------
   -- Rotate --
   ------------

   procedure Rotate
     (Model   : in out Root_3D_Model'Class;
      A, B, C : Real)
   is
   begin
      Rotate (Model.Current_Matrix, A, B, C);
   end Rotate;

   ------------
   -- Sphere --
   ------------

   procedure Sphere
     (Model      : in out Root_3D_Model'Class;
      Colour     : in     Colour_Type;
      RX, RY, RZ : in     Real;
      Detail     : in     Positive)
   is
      use Matrices;
      Mesh : array (0 .. Detail - 1, 0 .. 2 * Detail - 1) of Vector_3;
   begin
      for Latitude_Index in 0 .. Detail - 1 loop
         declare
            Latitude : constant Real :=
                         Real (Latitude_Index) * 90.0 / Real (Detail);
         begin
            for Longitude_Index in 0 .. 2 * Detail - 1 loop
               declare
                  use Lui.Elementary_Functions;
                  Longitude : constant Real :=
                                Real (Longitude_Index) * 180.0 / Real (Detail);
               begin
                  Mesh (Latitude_Index, Longitude_Index) :=
                    (1 => RX * Cos (Latitude, 360.0) * Cos (Longitude, 360.0),
                     2 => RY * Cos (Latitude, 360.0) * Sin (Longitude, 360.0),
                     3 => RZ * Sin (Latitude, 360.0));
               end;
            end loop;
         end;
      end loop;

      for Latitude_Index in 0 .. Detail - 1 loop
         for Longitude_Index in 0 .. 2 * Detail - 1 loop
            declare
               Long_1 : constant Natural :=
                          (if Longitude_Index < 2 * Detail - 1
                           then Longitude_Index + 1
                           else 0);
               V1 : constant Vector_3 :=
                      Mesh (Latitude_Index, Longitude_Index);
               V2 : constant Vector_3 :=
                          Mesh (Latitude_Index, Long_1);
               V3     : constant Vector_3 :=
                          (if Latitude_Index < Detail - 1
                           then Mesh (Latitude_Index + 1, Long_1)
                           else (0.0, 0.0, RZ));
               V4     : constant Vector_3 :=
                          (if Latitude_Index < Detail - 1
                           then Mesh (Latitude_Index + 1, Longitude_Index)
                           else (0.0, 0.0, RZ));
            begin
               Model.Begin_Surface (Colour);
               Model.Vertex (V1 (1), V1 (2), V1 (3));
               Model.Vertex (V2 (1), V2 (2), V2 (3));
               Model.Vertex (V3 (1), V3 (2), V3 (3));
               if Latitude_Index < Detail - 1 then
                  Model.Vertex (V4 (1), V4 (2), V4 (3));
               end if;
               Model.End_Surface;
               Model.Begin_Surface (Colour);
               if Latitude_Index < Detail - 1 then
                  Model.Vertex (V4 (1), V4 (2), -V4 (3));
               end if;
               Model.Vertex (V3 (1), V3 (2), -V3 (3));
               Model.Vertex (V2 (1), V2 (2), -V2 (3));
               Model.Vertex (V1 (1), V1 (2), -V1 (3));
               Model.End_Surface;
            end;
         end loop;
      end loop;

   end Sphere;

   ---------------
   -- Translate --
   ---------------

   procedure Translate
     (Matrix  : in out Matrix_4;
      X, Y, Z : Real)
   is
      use Matrices;
      M : constant Matrix_4 :=
            ((1.0, 0.0, 0.0, X),
             (0.0, 1.0, 0.0, Y),
             (0.0, 0.0, 1.0, Z),
             (0.0, 0.0, 0.0, 1.0));
   begin
      Matrix := Matrix * M;
   end Translate;

   ---------------
   -- Translate --
   ---------------

   procedure Translate
     (Model   : in out Root_3D_Model'Class;
      X, Y, Z : in     Real                 := 0.0)
   is
   begin
      Translate (Model.Current_Matrix, X, Y, Z);
   end Translate;

   ------------
   -- Vertex --
   ------------

   procedure Vertex
     (Model : in out Root_3D_Model'Class;
      X, Y, Z : Real)
   is
      use Matrices;
      V : constant Vector_4 :=
            Model.Current_Matrix * (X, Y, Z, 1.0);
   begin
      Model.Current_Surface.Vs.Append (V (1 .. 3));
   end Vertex;

   ------------
   -- Vertex --
   ------------

   procedure Vertex
     (Model : in out Root_3D_Model'Class;
      V     : Vector_3)
   is
   begin
      Model.Vertex (V (1), V (2), V (3));
   end Vertex;

end Lui.Models.Model_3D;
