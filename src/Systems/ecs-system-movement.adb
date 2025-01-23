

package body ECS.System.Movement is
   procedure Execute ( Self      : in out Mover_T;
                       Dt        : Duration; 
                       Manager   : access Entity_Manager_T'Class ) is
   begin
      for Entity of Manager.all.Entities loop
         declare
            Trans       : Component_Access   :=    Entity.all.Get_Component (Transform_T'Tag);
            Rigidbodies : Component_Access   :=    Entity.all.Get_Component (Rigidbody_T'Tag);
            AABB        : Component_Access   :=    Entity.all.Get_Component (AABB_T'Tag);
            
            begin
               if Trans = null then
                  Put_Line ("No Transform");
                  return;
               end if;
               if Rigidbodies = null then
                  Put_Line ("No Rigidbodies");
                  return;
               end if;
               if AABB = null then 
                  Put_Line ("No AABB");
                  return;
               end if;

            declare
               T renames Transform_T (Trans.all);
               R renames Rigidbody_T (Rigidbodies.all);
               B renames AABB_T(AABB.all);
               Velocity_Scaled : ECS.Vec2.Vec2 := New_Vec2(X_In => T.Velocity.X, Y_In => T.Velocity.Y);
               begin
                  Scale(Velocity_Scaled, Float(Dt));
                  -- Update the entity position while maintaining the position within screen bounds
                  T.Position.X := T.Position.X + Velocity_Scaled.X;
                  if T.Position.X >= Float(Self.Width) then
                     T.Position.X := T.Position.X - Float(Self.Width);
                  elsif T.Position.X < 0.0 then 
                     T.Position.X := T.Position.X + Float(Self.Width);
                  end if;

                  T.Position.Y := T.Position.Y + Velocity_Scaled.Y;
                  if T.Position.Y >= Float(Self.Height) then
                     T.Position.Y := T.Position.Y - Float(Self.Height);
                  elsif T.Position.Y < 0.0 then
                     T.Position.Y := T.Position.Y + Float(Self.Height);
                  end if;

                  -- Sync bounding box
                  B.Left := B.Left + Velocity_Scaled.X;
                  B.Right := B.Right + Velocity_Scaled.X;
                  B.Top := B.Top + Velocity_Scaled.Y;
                  B.Bottom := B.Bottom + Velocity_Scaled.Y;
               end;
            end;
      end loop;   
   end Execute;
end ECS.System.Movement;