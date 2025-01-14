with Ada.Containers.Vectors;

package Systems is

   type System_Type is abstract tagged null record;
   type System_Access is access all System_Type'Class;

   package System_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => System_Access);

   type System_Manager is tagged record
      Systems: System_Vectors.Vector;
   end record;
   
   procedure Initialize (Manager: in out System_Manager);
   procedure Register_System (Manager: in out System_Manager; System: System_Access);
   procedure Update_System (Manager: in out System_Manager; Delta_Time: Float);

end Systems;