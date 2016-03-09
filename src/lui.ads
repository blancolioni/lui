package Lui is

   subtype Real is Long_Float;

   subtype Positive_Real is Real range 0.0 .. Real'Last;
   subtype Unit_Real is Real range 0.0 .. 1.0;

   type Lui_UI_Feature is (UI_Model, UI_Table, UI_Gadget);

   type Root_UI_Element is interface;

   type Lui_UI_Element is access all Root_UI_Element'Class;

   function Approximate_Image
     (Value : Real)
      return String;

   function Approximate_Image
     (Value : Natural)
      return String;

end Lui;
