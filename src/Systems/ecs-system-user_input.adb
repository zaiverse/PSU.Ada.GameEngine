
package body ECS.System.User_Input is 

   procedure Register_Input_Callback(Key : Integer; Callback : Input_Callback) is
   begin
      Key_Callbacks(Key) := Callback;
   end Register_Input_Callback;

   overriding procedure Execute ( Self      : in out User_Input_T;
                       Dt        : Duration;
                       Manager   : access Entity_Manager_T'Class) is
   Trans    : Component_Access := Self.Player_Entity.all.Get_Component(Transform_T'Tag);
   T renames Transform_T(Trans.all);
   Event : Event_T := Get_Next_Event(Self.Handler.all);
   begin
      case Event.EventType is
         when ECS.Event.KeyDown =>
            if Key_Callbacks(Integer(Event.Data.KeyCode)) /= null then
               Key_Callbacks(Integer(Event.Data.KeyCode)).all(Manager, Dt, True);
            end if;
   
         when ECS.Event.KeyUp =>
            if Key_Callbacks(Integer(Event.Data.KeyCode)) /= null then
               Key_Callbacks(Integer(Event.Data.KeyCode)).all(Manager, Dt, False);
            end if;
         --Todo: Mouse callbacks
         when ECS.Event.L_MouseDown =>  
            null;
         when ECS.Event.L_MouseUp =>
            null;
         when ECS.Event.MouseMove =>
            null;
         when others =>
            null;
      end case;
   end Execute;
end ECS.System.User_Input;