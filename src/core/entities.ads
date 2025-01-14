with Ada.Containers.Vectors;

package Entities is

   type Entity_Id is new Natural;

   package Entity_Id_Vectors is new Ada.Containers.Vectors
    (Index_Type   => Natural,
     Element_Type => Entity_Id);

   type Entity_Manager is tagged record
      Next_Id           : Entity_Id:= 0;
      Active_Entities   : Entity_Id_Vectors.Vector;
   end record;

   type Entity_Id_Vector_Access is access all Entity_Id_Vectors.Vector;

   procedure Initialize (Manager: in out Entity_Manager);
   function Create_Entity (Manager: in out Entity_Manager) return Entity_Id;
   procedure Destroy_Entity (Manager: in out Entity_Manager; Entity: Entity_Id);
   function Is_Entity_Active (Manager: Entity_Manager; Entity: Entity_Id) return Boolean;

end Entities;