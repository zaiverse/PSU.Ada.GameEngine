with ecs.component; use ecs.component;
with Ada.Tags; use Ada.Tags;
with Ada.Containers; use Ada.Containers;
package body ecs.entity is
   function Get_Component (E : Entity_T'Class; Tag : Ada.Tags.Tag) return Component_Access is
   begin
      for C of E.Components loop
         if C /= null and then C'Tag = Tag then
            return C; 
         end if;
      end loop;
      return null; -- Return null if no matching component is found
   end Get_Component;

   procedure Add_Component (E : in out Entity_T'Class; Component : Component_Access) is
   begin
      E.Components.Append(Component);
   end Add_Component;

   procedure Free_Components (E : in out Entity_T'Class) is
   begin
      for C of E.Components loop
         Free_Component(C);
      end loop;
      E.Components.Clear;
   end Free_Components;
end ecs.entity;