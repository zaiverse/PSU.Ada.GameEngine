with Ada.Text_IO; use Ada.Text_IO;

package body ECS.System.Animation is

   procedure Execute(Self : in out Animation_T;
                     Dt   : Duration;
                     Manager : access Entity_Manager_T'Class) is
   begin
      for Entity of Manager.all.Entities loop
         declare
            Animation_Comp : constant Component_Access := Entity.all.Get_Component(Animation_Component_T'Tag);
         begin
            if Animation_Comp /= null then
               declare
                  As : Animation_Component_T renames Animation_Component_T(Animation_Comp.all);
                  A   : Single_Animation_Access := As.Animations(As.Current);
               begin
                  -- Debugging output
                  --  Put_Line("Animating entity ID: " & Entity.all.Id'Image &
                  --           " with state: " & Entity_State'Image(As.Current) &
                  --           " Frame: " & Integer'Image(A.CurFrame) &
                  --           " Time: " & Duration'Image(A.Total_Time));

                  -- Ensure A is not null
                  if A = null then
                     null;
                     --  Put_Line("Error: Animation for state " & Entity_State'Image(As.Current) & " is not initialized for entity ID: " & Entity.all.Id'Image);
                  else
                     -- Animation logic
                     if A.Total_Time >= A.Time then
                        A.CurX := A.CurX + A.OffsetX;
                        A.CurY := A.CurY + A.OffsetY;
                        A.Total_Time := 0.0;
                        A.CurFrame := A.CurFrame + 1;
                        if A.CurFrame >= A.TotFrame then
                           A.CurFrame := 0;
                           A.CurX := A.InitialX;
                           A.CurY := A.InitialY;
                        end if;
                     else
                        A.Total_Time := A.Total_Time + Dt;
                     end if;
                  end if;
               end;
            else
               null;
               --  Put_Line("Entity ID: " & Entity.all.Id'Image & " missing Animation component.");
            end if;
         end;
      end loop;
   end Execute;

end ECS.System.Animation;
