
package body ECS.System.Render is 
   -- Draw the entity to the screen
   procedure Execute (Self       : in out Render_T;
                      Dt         : Duration;
                      Manager    : access Entity_Manager_T'Class ) is
   begin
      for Entity of Manager.all.Entities loop
         declare
            Trans       : Component_Access   :=    Entity.all.Get_Component(Transform_T'Tag);
            Shape       : Component_Access   :=    Entity.all.Get_Component(Shape_T'Tag);
         begin
            if Trans = null or else Shape = null then
               Put_Line("Entity with missing components");
               return;
            end if;
            declare
               T renames Transform_T(Trans.all);
               S renames Shape_T(Shape.all);
            begin
               Draw_Regular_Polygon(Self.Buffer.all, S.Sides, S.Radius, T.Position.X,T.Position.Y, S.C, Self.Width, Self.Height);
            end;
         end;
      end loop; 
   end Execute;
end ECS.System.Render;