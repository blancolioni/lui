package body Lui.Models.Boxes is

   Next_Box_Index : Positive := 1;

   ------------
   -- Create --
   ------------

   function Create (Direction : Box_Direction) return Box_Model is
      Result : Root_Box_Model;
   begin
      Result.Initialise (Direction);
      Result.Set_Name (Box_Direction'Image (Direction)
                       & Integer'Image (-Next_Box_Index));
      Next_Box_Index := Next_Box_Index + 1;
      return new Root_Box_Model'(Result);
   end Create;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise
     (Model     : in out Root_Box_Model;
      Direction : Box_Direction)
   is
   begin
      Model.Direction := Direction;
   end Initialise;

   ------------
   -- Resize --
   ------------

   overriding procedure Resize
     (Model         : in out Root_Box_Model;
      Width, Height : Natural)
   is
      X, Y : Integer;
   begin
      Root_Object_Model (Model).Resize (Width, Height);

      Model.Get_Location (X, Y);

      if Model.Count = 0 then
         return;
      end if;

      declare
         Count    : constant Positive := Model.Count;
         New_Size : constant Natural :=
                      (case Model.Direction is
                          when Horizontal => Width,
                          when Vertical   => Height);
         Index    : Natural := 0;
         Offset   : Natural := 0;

         procedure Update_Size (Item : Object_Model);

         -----------------
         -- Update_Size --
         -----------------

         procedure Update_Size (Item : Object_Model) is
            New_Offset : constant Natural :=
                           (Index + 1) * New_Size / Count;
            Child_X     : constant Integer :=
                            (case Model.Direction is
                                when Vertical => X,
                                when Horizontal => X + Offset);
            Child_Y     : constant Integer :=
                            (case Model.Direction is
                                when Vertical => Y + Offset,
                                when Horizontal => Y);
            Child_Width : constant Integer :=
                            (case Model.Direction is
                                when Vertical =>
                                   Width,
                                when Horizontal =>
                                   New_Offset - Offset);
            Child_Height : constant Integer :=
                             (case Model.Direction is
                                 when Vertical =>
                                    New_Offset - Offset,
                                 when Horizontal =>
                                    Height);
         begin
            Index := Index + 1;

            if Child_Width > Model.Child_Border_Width * 2
              and then Child_Height > Model.Child_Border_Width * 2
            then
               Item.Move (Child_X + Model.Child_Border_Width,
                          Child_Y + Model.Child_Border_Width);
               Item.Resize (Child_Width - Model.Child_Border_Width * 2,
                            Child_Height - Model.Child_Border_Width * 2);
            end if;
            Offset := New_Offset;
         end Update_Size;

      begin
         Model.Iterate (Update_Size'Access);
      end;

   end Resize;

end Lui.Models.Boxes;
