with Ada.Text_IO;             use Ada.Text_IO;
package body ECS.System.Animation is 
   procedure Execute ( Self : in out Animation_T;
                       Dt   : Duration;
                       Manager    : access Entity_Manager_T'Class ) is
   begin
   for Entity of Manager.all.Entities loop
      declare
         Animation   : Component_Access   :=    Entity.all.Get_Component(Animation_Component_T'Tag);
      begin
         if Animation /= null then
            declare
               As renames Animation_Component_T(Animation.all);
               A : Single_Animation_Access := As.Animations(As.Current);

            begin
               if A.Total_Time >=  A.Time then
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
            end;
         end if;
      end;
   end loop;
   end Execute;
end ECS.System.Animation;