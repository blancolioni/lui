package body Lui.Gadgets is

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise
     (Gadget : in out Root_Model_Gadget;
      Name   : in     String)
   is
   begin
      Gadget.Name := new String'(Name);
   end Initialise;

   ----------
   -- Name --
   ----------

   function Name (Gadget : Root_Model_Gadget'Class) return String is
   begin
      return Gadget.Name.all;
   end Name;

   ----------------
   -- No_Gadgets --
   ----------------

   function No_Gadgets return Array_Of_Gadgets is
   begin
      return Result : Array_Of_Gadgets (1 .. 0) do
         null;
      end return;
   end No_Gadgets;

end Lui.Gadgets;
