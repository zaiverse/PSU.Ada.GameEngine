package body Components is

   procedure Initialize (Manager : in out Component_Manager) is
   begin
      Entity_Component_Map.Clear (Manager.Components);
   end Initialize;

   procedure Add_Component (Manager : in out Component_Manager; Entity : Entities.Entity_Id; Component : Component_Access) is
   begin
      if not Entity_Component_Map.Contains (Manager.Components, Entity) then
         Entity_Component_Map.Insert (Manager.Components, Entity, Component);
      else
         -- Handle the case where the component already exists, if necessary
         -- Nothing for now
      end if;
   end Add_Component;

   procedure Add_Component (Manager : in out Component_Manager; Entity : Entities.Entity_Id; Component : Component_Access) is
   begin
      if not Entity_Component_Map.Contains (Manager.Components, Entity) then
         Entity_Component_Map.Insert (Manager.Components, Entity, Component);
      else
         -- Handle the case where the component already exists, if necessary
         -- Nothing for now
      end if;
   end Add_Component;

   function Get_Component (Manager : Component_Manager; Entity : Entities.Entity_Id) return Component_Access is
   begin
      return Entity_Component_Map.Element (Manager.Components, Entity);
   exception
      when Ada.Containers.Key_Error =>
         return null;
   end Get_Component;

   function Hash_Entity_Id (Key : Entities.Entity_Id) return Ada.Containers.Hash_Type is
   begin
      -- Example for string-based Entity_Id
      return Ada.Strings.Hash (Key);
      -- For other types, implement an appropriate hash function
   end Hash_Entity_Id;

   function Equal_Entity_Id (Left, Right : Entities.Entity_Id) return Boolean is
   begin
      -- Example for string-based Entity_Id
      return Left = Right;
      -- For other types, implement appropriate equality comparison
   end Equal_Entity_Id;

end Components;
