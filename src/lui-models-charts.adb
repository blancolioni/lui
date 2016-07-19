with Lui.Elementary_Functions;

package body Lui.Models.Charts is

   Colour_Scheme : constant array (Natural range <>) of Natural :=
                     (0 => 16#4D4D4D#,
                      1 => 16#5DA5DA#,
                      2 => 16#FAA43A#,
                      3 => 16#60BD68#,
                      4 => 16#F17CB0#,
                      5 => 16#B2912F#,
                      6 => 16#B276B2#,
                      7 => 16#DECF3F#,
                      8 => 16#F15854#);

   function Get_Colour
     (Series : Chart_Series)
      return Lui.Colours.Colour_Type;

   procedure Calculate_Boundary
     (Model                 : in out Chart_Model'Class;
      Have_Y2               : out Boolean;
      Min_X, Min_Y1, Min_Y2 : out Real;
      Max_X, Max_Y1, Max_Y2 : out Real);

   function Major_Tick
     (Min, Max : Real)
      return Real;

   function Round_Max
     (Max : Real)
      return Real;

   ------------------
   -- Append_Value --
   ------------------

   procedure Append_Value
     (Model  : in out Chart_Model'Class;
      Series : Chart_Series;
      Value  : Real)
   is
      Info : Series_Info renames Model.Series (Series);
   begin
      Info.Points.Append ((Real (Info.Points.Last_Index + 1), Value, True));
   end Append_Value;

   ------------------------
   -- Calculate_Boundary --
   ------------------------

   procedure Calculate_Boundary
     (Model                 : in out Chart_Model'Class;
      Have_Y2               : out Boolean;
      Min_X, Min_Y1, Min_Y2 : out Real;
      Max_X, Max_Y1, Max_Y2 : out Real)
   is
   begin
      Min_X := Real'Last;
      Min_Y1 := Real'Last;
      Min_Y2 := Real'Last;
      Max_X := Real'First;
      Max_Y1 := Real'First;
      Max_Y2 := Real'First;

      Have_Y2 := False;

      for Series of Model.Series loop
         declare
            Y2 : constant Boolean := Series.Y2_Axis;
         begin
            Have_Y2 := Have_Y2 or Y2;
            for Point of Series.Points loop
               if Point.X < Min_X then
                  Min_X := Point.X;
               end if;
               if Point.X > Max_X then
                  Max_X := Point.X;
               end if;
               if Y2 then
                  if Point.Value < Min_Y2 then
                     Min_Y2 := Point.Value;
                  end if;
                  if Point.Value > Max_Y2 then
                     Max_Y2 := Point.Value;
                  end if;
               else
                  if Point.Value < Min_Y1 then
                     Min_Y1 := Point.Value;
                  end if;
                  if Point.Value > Max_Y1 then
                     Max_Y1 := Point.Value;
                  end if;
               end if;
            end loop;
         end;
      end loop;

      Min_Y1 := Real'Min (Min_Y1, 0.0);

      Max_Y1 := Round_Max (Max_Y1);

      if Have_Y2 then
         Min_Y2 := Real'Min (Min_Y2, 0.0);
         Max_Y2 := Round_Max (Max_Y2);
      else
         Min_Y2 := 0.0;
         Max_Y2 := 100.0;
      end if;

   end Calculate_Boundary;

   ----------------
   -- Get_Colour --
   ----------------

   function Get_Colour
     (Series : Chart_Series)
      return Lui.Colours.Colour_Type
   is
      use Lui.Colours;
      Value : Natural :=
                Colour_Scheme (Natural (Series) mod Colour_Scheme'Length);
      R, G, B : Colour_Byte;
   begin
      B := Colour_Byte (Value mod 256);
      Value := Value / 256;
      G := Colour_Byte (Value mod 256);
      Value := Value / 256;
      R := Colour_Byte (Value mod 256);
      Value := Value / 256;
      return To_Colour (R, G, B);
   end Get_Colour;

   ----------------
   -- Initialise --
   ----------------

   overriding procedure Initialise
     (Chart             : in out Chart_Model;
      Name              : in     String;
      Last_Render_Layer : Lui.Rendering.Render_Layer := 1;
      Tables            : Lui.Tables.Array_Of_Model_Tables :=
        Lui.Tables.No_Tables;
      Gadgets           : Lui.Gadgets.Array_Of_Gadgets :=
        Lui.Gadgets.No_Gadgets)
   is
   begin
      Root_Object_Model (Chart).Initialise
        (Name, Last_Render_Layer, Tables, Gadgets);
      Chart.Background := Lui.Colours.White;
   end Initialise;

   ------------------
   -- Long_Tooltip --
   ------------------

   overriding function Long_Tooltip
     (Chart : Chart_Model;
      X, Y  : Natural)
      return String
   is
      pragma Unreferenced (Chart);
      pragma Unreferenced (X);
      pragma Unreferenced (Y);
   begin
      return "";
   end Long_Tooltip;

   ----------------
   -- Major_Tick --
   ----------------

   function Major_Tick
     (Min, Max : Real)
      return Real
   is
      pragma Unreferenced (Min);
   begin
      return Max / 10.0;
   end Major_Tick;

   ----------------
   -- New_Series --
   ----------------

   function New_Series
     (Model             : in out Chart_Model;
      Name              : String;
      Series_Chart_Type : Chart_Type;
      Y2_Axis           : Boolean             := False)
      return Chart_Series
   is
   begin
      Model.Series.Append
        ((Ada.Strings.Unbounded.To_Unbounded_String (Name),
         Series_Chart_Type, Y2_Axis,
         Point_Vectors.Empty_Vector));
      return Model.Series.Last_Index;
   end New_Series;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Item     : in out Chart_Model;
      Renderer : in out Lui.Rendering.Root_Renderer'Class)
   is
      Min_X, Min_Y1, Min_Y2, Max_X, Max_Y1, Max_Y2 : Real;

      Draw_Area_Width  : constant Integer := Item.Width - 100;
      Draw_Area_Height : constant Integer := Item.Height - 80;
      Draw_Area_Min_X  : constant := 60;
      Draw_Area_Min_Y  : constant := 20;

      function To_Screen_X (X : Real) return Integer
      is (Draw_Area_Min_X
          + (if Max_X = Min_X then Draw_Area_Width / 2
             else Integer ((X - Min_X) * Real (Draw_Area_Width)
                           / (Max_X - Min_X))));

      function To_Screen_Y1 (Y : Real) return Integer
      is (Draw_Area_Min_Y +
          (if Max_Y1 = Min_Y1 then Draw_Area_Height / 2
           else Draw_Area_Height -
             Integer ((Y - Min_Y1) * Real (Draw_Area_Height)
                      / (Max_Y1 - Min_Y1))));

      function To_Screen_Y2 (Y : Real) return Integer
      is (Draw_Area_Min_Y +
          (if Max_Y2 = Min_Y2 then Draw_Area_Height / 2
           else Draw_Area_Height -
             Integer ((Y - Min_Y2) * Real (Draw_Area_Height)
                      / (Max_Y2 - Min_Y2))));

      Have_Y2 : Boolean;

   begin

      if Draw_Area_Width <= 50 or else Draw_Area_Height <= 50 then
         return;
      end if;

      Calculate_Boundary (Item, Have_Y2,
                          Min_X, Min_Y1, Min_Y2,
                          Max_X, Max_Y1, Max_Y2);

      declare
         X1 : constant Integer := To_Screen_X (0.0);
         Y1 : constant Integer := To_Screen_Y1 (0.0);
         X2 : constant Integer := To_Screen_X (Max_X) + 10;
         Y2 : constant Integer := To_Screen_Y1 (0.0);
      begin
         Renderer.Draw_Line (X1, Y1, X2, Y2, Lui.Colours.Black);
      end;

      declare
         Tick   : constant Real := 1.0;
         Tick_X : Real := Tick;
      begin
         while Tick_X <= Max_X loop
            Renderer.Draw_Line
              (To_Screen_X (Tick_X), To_Screen_Y1 (0.0),
               To_Screen_X (Tick_X), To_Screen_Y1 (0.0) + 5,
               Lui.Colours.Black);
            Tick_X := Tick_X + Tick;
         end loop;
      end;

      Renderer.Draw_Line
        (X1     => To_Screen_X (0.0),
         Y1     => To_Screen_Y1 (Min_Y1),
         X2     => To_Screen_X (0.0),
         Y2     => To_Screen_Y1 (Max_Y1),
         Colour => Lui.Colours.Black);

      declare
         Tick   : constant Real := Major_Tick (Min_Y1, Max_Y1);
         Tick_Y : Real := Min_Y1;
      begin
         while Tick_Y <= Max_Y1 loop
            Renderer.Draw_Line
              (To_Screen_X (0.0) - 5, To_Screen_Y1 (Tick_Y),
               To_Screen_X (0.0), To_Screen_Y1 (Tick_Y),
               Lui.Colours.Black);
            Renderer.Draw_String
              (X      => 10,
               Y      => To_Screen_Y1 (Tick_Y),
               Size   => 10,
               Colour => Lui.Colours.Black,
               Text   => Approximate_Image (Tick_Y));
            Tick_Y := Tick_Y + Tick;
         end loop;
      end;

      if Have_Y2 then

         declare
            X : constant Integer := To_Screen_X (Max_X) + 10;
            Tick   : constant Real := Major_Tick (Min_Y2, Max_Y2);
            Tick_Y : Real := Min_Y2;
         begin

            Renderer.Draw_Line
              (X1     => X,
               Y1     => To_Screen_Y2 (Min_Y2),
               X2     => X,
               Y2     => To_Screen_Y2 (Max_Y2),
               Colour => Lui.Colours.Black);

            while Tick_Y <= Max_Y2 loop
               Renderer.Draw_Line
                 (X, To_Screen_Y2 (Tick_Y),
                  X + 5, To_Screen_Y2 (Tick_Y),
                  Lui.Colours.Black);
               Renderer.Draw_String
                 (X      => X + 10,
                  Y      => To_Screen_Y2 (Tick_Y) + 5,
                  Size   => 10,
                  Colour => Lui.Colours.Black,
                  Text   => Approximate_Image (Tick_Y));
               Tick_Y := Tick_Y + Tick;
            end loop;
         end;
      end if;

      for I in 1 .. Item.Series.Last_Index loop
         declare
            Series : Series_Info renames Item.Series (I);
            Colour : constant Lui.Colours.Colour_Type := Get_Colour (I);
         begin
            case Series.Series_Chart_Type is
               when Line =>
                  declare
                     First  : Boolean := True;
                     X1, Y1 : Real := 0.0;
                     X2, Y2 : Real := 0.0;
                  begin
                     for Point of Series.Points loop
                        X1 := X2;
                        Y1 := Y2;
                        X2 := Point.X;
                        Y2 := Point.Value;
                        if First then
                           First := False;
                        elsif Series.Y2_Axis then
                           Renderer.Draw_Line
                             (X1     => To_Screen_X (X1),
                              Y1     => To_Screen_Y2 (Y1),
                              X2     => To_Screen_X (X2),
                              Y2     => To_Screen_Y2 (Y2),
                              Colour => Colour);
                        else
                           Renderer.Draw_Line
                             (X1     => To_Screen_X (X1),
                              Y1     => To_Screen_Y1 (Y1),
                              X2     => To_Screen_X (X2),
                              Y2     => To_Screen_Y1 (Y2),
                              Colour => Colour);
                        end if;
                     end loop;
                  end;
               when Bar =>
                  declare
                     Count : constant Positive :=
                               Positive (Series.Points.Length);
                     Real_Bar_Width : constant Real :=
                                        (Max_X - Min_X) / Real (Count) / 2.0;
                     Screen_Bar_Width : constant Positive :=
                                          Natural'Max
                                            (1,
                                             To_Screen_X (Real_Bar_Width)
                                             - To_Screen_X (0.0));
                  begin
                     for Point of Series.Points loop
                        declare
                           X : constant Natural := To_Screen_X (Point.X);
                           Y1 : constant Natural :=
                                  (if Series.Y2_Axis
                                   then To_Screen_Y2 (0.0)
                                   else To_Screen_Y1 (0.0));
                           Y2 : constant Natural :=
                                  (if Series.Y2_Axis
                                   then To_Screen_Y2 (Point.Value)
                                   else To_Screen_Y1 (Point.Value));
                        begin
                           Renderer.Draw_Rectangle
                             (X      => X - Screen_Bar_Width / 2,
                              Y      => Integer'Min (Y1, Y2),
                              W      => Screen_Bar_Width,
                              H      => abs (Y2 - Y1),
                              Colour => Colour,
                              Filled => True);
                        end;
                     end loop;
                  end;
            end case;
         end;
      end loop;

      declare
         use Ada.Strings.Unbounded;
         Title : constant String :=
                   (if Item.Title /= Null_Unbounded_String
                    then To_String (Item.Title)
                    else To_String (Item.Name));
      begin
         Renderer.Draw_String
           (10, 10, 10, Lui.Colours.Black, Title);
      end;

   end Render;

   ---------------
   -- Round_Max --
   ---------------

   function Round_Max
     (Max : Real)
      return Real
   is
      use Lui.Elementary_Functions;
      Log_Max : constant Integer :=
                  Integer (Real'Truncation (Log (Max, 10.0)));
      Base    : constant Real := 10.0 ** Log_Max;
      Result  : Real := Base;
   begin
      if Max <= Base * 2.0 then
         Result := Base * 2.0;
      elsif Max <= Base * 5.0 then
         Result := Base * 5.0;
      else
         Result := Base * 10.0;
      end if;
      return Result;
   end Round_Max;

   ---------------
   -- Select_XY --
   ---------------

   overriding procedure Select_XY
     (Item : in out Chart_Model;
      X, Y : Natural)
   is
      pragma Unreferenced (Item);
      pragma Unreferenced (X);
      pragma Unreferenced (Y);
   begin
      null;
   end Select_XY;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Model : in out Chart_Model;
      Title : String)
   is
   begin
      Model.Title :=
        Ada.Strings.Unbounded.To_Unbounded_String (Title);
   end Set_Title;

   -------------
   -- Tooltip --
   -------------

   overriding function Tooltip
     (Item : Chart_Model;
      X, Y : Natural)
      return String
   is
      pragma Unreferenced (Item);
      pragma Unreferenced (X);
      pragma Unreferenced (Y);
   begin
      return "";
   end Tooltip;

end Lui.Models.Charts;
