with Ada.Containers;
with Entity_Vectors; 

package body Entities is

   procedure Initialize (Manager : in out Entity_Manager) is
   begin
      Manager.Next_Id := 0;
      Entity_Vectors.Entity_Id_Vectors.Initialize (Manager.Active_Entities);
   end Initialize;

   function Create_Entity (Manager : in out Entity_Manager) return Entity_Id is
   begin
      Manager.Next_Id := Manager.Next_Id + 1;
      Entity_Vectors.Entity_Id_Vectors.Append (Manager.Active_Entities, Manager.Next_Id);
      return Manager.Next_Id;
   end Create_Entity;

   procedure Destroy_Entity (Manager : in out Entity_Manager; Entity : Entity_Id) is
      Cursor : Entity_Vectors.Entity_Id_Vectors.Cursor;
   begin
      Cursor := Entity_Vectors.Entity_Id_Vectors.Find (Manager.Active_Entities, Entity);
      if Cursor /= Entity_Vectors.Entity_Id_Vectors.No_Element then
         Entity_Vectors.Entity_Id_Vectors.Delete (Manager.Active_Entities, Cursor);
      end if;
   end Destroy_Entity;

   function Is_Entity_Active (Manager : Entity_Manager; Entity : Entity_Id) return Boolean is
   begin
      return Entity_Vectors.Entity_Id_Vectors.Contains (Manager.Active_Entities, Entity);
   end Is_Entity_Active;

end Entities;
