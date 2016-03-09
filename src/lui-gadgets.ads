limited with Lui.Models;

package Lui.Gadgets is

   type Root_Model_Gadget is abstract new Root_UI_Element with private;

   function Name (Gadget : Root_Model_Gadget'Class) return String;

   procedure Initialise
     (Gadget : in out Root_Model_Gadget;
      Name   : in     String);

   type Model_Gadget is access all Root_Model_Gadget'Class;

   type Array_Of_Gadgets is array (Positive range <>) of Model_Gadget;

   function No_Gadgets return Array_Of_Gadgets;

   function Model
     (Gadget : Root_Model_Gadget)
      return access Lui.Models.Root_Object_Model'Class
   is (null);

   type Root_Button_Gadget is
     abstract new Root_Model_Gadget with private;

   procedure On_Click
     (Button : Root_Button_Gadget;
      Model  : not null access Lui.Models.Root_Object_Model'Class)
   is abstract;

private

   type Root_Model_Gadget is abstract new Root_UI_Element with
      record
         Name : access String;
      end record;

   type Root_Button_Gadget is
     abstract new Root_Model_Gadget with null record;

end Lui.Gadgets;
