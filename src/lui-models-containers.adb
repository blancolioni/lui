package body Lui.Models.Containers is

   ---------
   -- Add --
   ---------

   procedure Add
     (Container : in out Root_Container_Model;
      Item      : not null access Root_Object_Model'Class)
   is
   begin
      Container.Children.Append (Object_Model (Item));
   end Add;

   -------------------------
   -- Child_Border_Colour --
   -------------------------

   function Child_Border_Colour
     (Model : Root_Container_Model'Class)
      return Lui.Colours.Colour_Type
   is
   begin
      return Model.Child_Border_Colour;
   end Child_Border_Colour;

   ------------------------
   -- Child_Border_Width --
   ------------------------

   function Child_Border_Width
     (Model : Root_Container_Model'Class)
      return Natural
   is
   begin
      return Model.Child_Border_Width;
   end Child_Border_Width;

   -----------------
   -- Child_Model --
   -----------------

   function Child_Model
     (Container : Root_Container_Model'Class;
      X, Y      : Integer)
      return Object_Model
   is
   begin
      for Child of Container.Children loop
         if X in Child.X .. Child.X + Child.Width - 1
           and then Y in Child.Y .. Child.Y + Child.Height - 1
         then
            return Child;
         end if;
      end loop;
      return null;
   end Child_Model;

   -----------
   -- Count --
   -----------

   function Count
     (Container : Root_Container_Model)
      return Natural
   is
   begin
      return Natural (Container.Children.Length);
   end Count;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Root_Container_Model'Class;
      Process   : not null access
        procedure (Item : Object_Model))
   is
   begin
      for Child of Container.Children loop
         Process (Child);
      end loop;
   end Iterate;

   ------------------
   -- Long_Tooltip --
   ------------------

   overriding function Long_Tooltip
     (Item : Root_Container_Model;
      X, Y : Natural)
      return String
   is
      Child : constant Object_Model := Item.Child_Model (X, Y);
   begin
      if Child /= null then
         return Child.Long_Tooltip (X - Child.X, Y - Child.Y);
      else
         return "";
      end if;
   end Long_Tooltip;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Item     : in out Root_Container_Model;
      Renderer : in out Root_Renderer'Class)
   is
      Border : constant Natural := Item.Child_Border_Width;
   begin
      for Child of Item.Children loop
         if Border > 0 then
            Renderer.Draw_Rectangle
              (X      => Child.X - Border,
               Y      => Child.Y - Border,
               W      => Child.Width + 2 * Border,
               H      => Child.Height + 2 * Border,
               Colour => Item.Child_Border_Colour,
               Filled => False);
         end if;
         Child.Before_Render (Renderer);
         Child.Render (Renderer);
         Child.After_Render (Renderer);
      end loop;
   end Render;

   ---------------
   -- Select_XY --
   ---------------

   overriding procedure Select_XY
     (Item : in out Root_Container_Model;
      X, Y : Natural)
   is
      Child : constant Object_Model := Item.Child_Model (X, Y);
   begin
      if Child /= null then
         Child.Select_XY (X - Child.X, Y - Child.Y);
      end if;
   end Select_XY;

   ----------------------------
   -- Set_Child_Border_Style --
   ----------------------------

   procedure Set_Child_Border_Style
     (Model  : in out Root_Container_Model'Class;
      Colour : Lui.Colours.Colour_Type;
      Width  : Natural)
   is
   begin
      Model.Child_Border_Colour := Colour;
      Model.Child_Border_Width := Width;
   end Set_Child_Border_Style;

   -------------
   -- Tooltip --
   -------------

   overriding function Tooltip
     (Item : Root_Container_Model;
      X, Y : Natural)
      return String
   is
      Child : constant Object_Model := Item.Child_Model (X, Y);
   begin
      if Child /= null then
         return Child.Tooltip (X - Child.X, Y - Child.Y);
      else
         return "";
      end if;
   end Tooltip;

end Lui.Models.Containers;
