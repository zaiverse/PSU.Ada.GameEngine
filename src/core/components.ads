with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Text_IO; use Ada.Text_IO;
with Entities;

package Components is

   type Component_Type is abstract tagged null record;
   type Component_Access is access all Component_Type'Class;

   -- Hash function for Entity_Id
   function Hash_Entity_Id (Key : Entities.Entity_Id) return Ada.Containers.Hash_Type;

   -- Equality function for Entity_Id
   function Equal_Entity_Id (Left, Right : Entities.Entity_Id) return Boolean;

   package Entity_Component_Map is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => Entities.Entity_Id,
      Element_Type    => Component_Access,
      Hash            => Hash_Entity_Id,
      Equivalent_Keys => Equal_Entity_Id);

   type Component_Manager is tagged record
      Components : Entity_Component_Map.Map;
   end record;

   procedure Initialize (Manager: in out Component_Manager);
   procedure Add_Component (Manager: in out Component_Manager; Entity: Entities.Entity_Id; Component: Component_Access);
   procedure Remove_Component (Manager: in out Component_Manager; Entity: Entities.Entity_Id);
   function Get_Component (Manager: Component_Manager; Entity: Entities.Entity_Id) return Component_Access;

end Components;