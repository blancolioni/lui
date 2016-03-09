with Conflict.Db;

package Lui.Model_Manager is

   function Get_Model
     (From : Conflict.Db.Record_Interface'Class)
      return Object_Model;

   function Get_Model
     (Main_Object : Conflict.Db.Record_Interface'Class;
      Sub_Object  : Conflict.Db.Record_Interface'Class)
      return Object_Model;

   procedure Set_Model
     (Object : Conflict.Db.Record_Interface'Class;
      Model  : Object_Model)
   with
     Pre => Get_Model (Object) = null,
     Post => Get_Model (Object) = Model;

   procedure Set_Model
     (Main_Object : Conflict.Db.Record_Interface'Class;
      Sub_Object  : Conflict.Db.Record_Interface'Class;
      Model       : Object_Model)
   with
     Pre => Get_Model (Main_Object, Sub_Object) = null,
     Post => Get_Model (Main_Object, Sub_Object) = Model;

end Lui.Model_Manager;
