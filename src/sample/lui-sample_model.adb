with WL.Random;

with Lui.Gadgets;
with Lui.Models.Charts;
with Lui.Tables;

package body Lui.Sample_Model is

   type Sample_Table_Data is
     array (Positive range <>) of Real;

   type Sample_Table is new Lui.Tables.Root_Model_Table with
      record
         Data : access Sample_Table_Data;
      end record;

   overriding function Cell_Text
     (Table  : Sample_Table;
      Row    : Positive;
      Column : Positive)
      return String;

   overriding function Heading_Column_Text
     (Table  : Sample_Table;
      Column : Positive) return String;

   function Create_Inline_Model
     return Lui.Models.Object_Model;

   type Sample_Model is new Lui.Models.Charts.Chart_Model with
      record
         Show_Mini_Chart : Boolean := False;
         Mini_Chart      : Lui.Models.Object_Model;
      end record;

   overriding procedure Select_XY
     (Model : in out Sample_Model;
      X, Y  : Natural);

   ---------------
   -- Cell_Text --
   ---------------

   overriding function Cell_Text
     (Table  : Sample_Table;
      Row    : Positive;
      Column : Positive)
      return String
   is
   begin
      case Column is
         when 1 =>
            return Row'Img;
         when 2 =>
            return Approximate_Image (Table.Data (Row));
         when others =>
            raise Constraint_Error;
      end case;
   end Cell_Text;

   -------------------------
   -- Create_Inline_Model --
   -------------------------

   function Create_Inline_Model
      return Lui.Models.Object_Model
   is
      Result : Lui.Models.Charts.Chart_Model;
      Series : Lui.Models.Charts.Chart_Series;
      Data   : Sample_Table_Data (1 .. 20);
      Table  : Sample_Table;
      Min    : Real := Real'Last;
      Max    : Real := Real'First;
   begin
      for I in Data'Range loop
         Data (I) := Real (I * 10 + WL.Random.Random_Number (1, 5 * I));
         Min := Real'Min (Min, Data (I));
         Max := Real'Max (Max, Data (I));
      end loop;

      Table.Initialise ("Data Table",
                        Num_Rows => Data'Length,
                        Num_Cols => 2);
      Table.Data := new Sample_Table_Data'(Data);

      Result.Initialise ("Inline chart",
                         (1 => new Sample_Table'(Table)),
                         Lui.Gadgets.No_Gadgets);
      Result.Set_Title ("Inline Chart");
      Series := Result.New_Series ("series 1", Lui.Models.Charts.Bar);
      for I in Data'Range loop
         Result.Append_Value (Series, Data (I));
      end loop;

      Result.Add_Property ("Value count", Approximate_Image (Data'Length));
      Result.Add_Property ("Minimum", Approximate_Image (Min));
      Result.Add_Property ("Maximum", Approximate_Image (Max));

      return R : constant Lui.Models.Object_Model :=
        new Lui.Models.Charts.Chart_Model'(Result)
      do
         null;
      end return;

   end Create_Inline_Model;

   -------------------------
   -- Create_Sample_Model --
   -------------------------

   function Create_Sample_Model
      return Lui.Models.Object_Model
   is
      Result : Sample_Model;
      Series : Lui.Models.Charts.Chart_Series;
      Data   : Sample_Table_Data (1 .. 20);
      Table  : Sample_Table;
      Min    : Real := Real'Last;
      Max    : Real := Real'First;
   begin
      for I in Data'Range loop
         Data (I) := Real (I * 10 + WL.Random.Random_Number (1, 5 * I));
         Min := Real'Min (Min, Data (I));
         Max := Real'Max (Max, Data (I));
      end loop;

      Table.Initialise ("Data Table",
                        Num_Rows => Data'Length,
                        Num_Cols => 2);
      Table.Data := new Sample_Table_Data'(Data);

      Result.Initialise ("Sample chart",
                         (1 => new Sample_Table'(Table)),
                         Lui.Gadgets.No_Gadgets);
      Result.Set_Title ("Sample Chart");
      Series := Result.New_Series ("series 1", Lui.Models.Charts.Bar);
      for I in Data'Range loop
         Result.Append_Value (Series, Data (I));
      end loop;

      Result.Add_Property ("Value count", Approximate_Image (Data'Length));
      Result.Add_Property ("Minimum", Approximate_Image (Min));
      Result.Add_Property ("Maximum", Approximate_Image (Max));

      Result.Mini_Chart := Create_Inline_Model;

      return R : constant Lui.Models.Object_Model :=
        new Sample_Model'(Result)
      do
         null;
      end return;

   end Create_Sample_Model;

   -------------------------
   -- Heading_Column_Text --
   -------------------------

   overriding function Heading_Column_Text
     (Table  : Sample_Table;
      Column : Positive)
      return String
   is
      pragma Unreferenced (Table);
   begin
      case Column is
         when 1 =>
            return "Index";
         when 2 =>
            return "Value";
         when others =>
            raise Constraint_Error;
      end case;
   end Heading_Column_Text;

   ---------------
   -- Select_XY --
   ---------------

   overriding procedure Select_XY
     (Model : in out Sample_Model;
      X, Y  : Natural)
   is
      pragma Unreferenced (X, Y);
   begin
      Model.Show_Mini_Chart := not Model.Show_Mini_Chart;
      if Model.Show_Mini_Chart then
         Model.Add_Inline_Model
           (Width         => 400,
            Height        => 200,
            Model         => Model.Mini_Chart,
            Attach_Right  => True,
            Attach_Bottom => True);
      else
         Model.Remove_Inline_Model (Model.Mini_Chart);
      end if;
   end Select_XY;

end Lui.Sample_Model;
