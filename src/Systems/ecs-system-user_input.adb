
package body ECS.System.User_Input is 
   overriding procedure Execute ( Self      : in out User_Input_T;
                       Dt        : Duration;
                       Manager   : access Entity_Manager_T'Class) is
   Trans    : Component_Access := Self.Player_Entity.all.Get_Component(Transform_T'Tag);
   T renames Transform_T(Trans.all);
   Event : Event_T := Get_Next_Event(Self.Handler.all);
   MousePos : ECS.Vec2.Vec2 := New_Vec2(0.0,0.0);
   Direction : ECS.Vec2.Vec2;
   begin
      case Event.EventType is
         when ECS.Event.KeyDown =>
            case Event.Data.KeyCode is
               when 87 => -- W 
                  T.Velocity.Y := -200.0;
               when 83 => -- S
                  T.Velocity.Y := 200.0;
               when 65 => -- A
                  T.Velocity.X := -200.0;
               when 68 => -- D 
                  T.Velocity.X := 200.0;
               when 16#20# => -- Spacebar
                  null;
               when others =>
                  null;
            end case;
         when ECS.Event.KeyUp =>
            T.Velocity.X := 0.0;
            T.Velocity.Y := 0.0;
         -- I commented out all the click to move code because it is interfering with testing and 
         -- we need to discuss do we even want to support click to move or should it simply be optional
         -- in the context of a side scrolling 2D platformer we don't need it all but we might want to allow the option
         -- regardless.
         when ECS.Event.L_MouseDown =>  
            --  MousePos := (Float(Event.Data.MouseX), Float(Event.Data.MouseY));
            --  Direction := MousePos - T.Position;
            --  Self.MouseDown := True;
            --  Self.MouseUp   := False;
            --  ECS.Vec2.Normalize(Direction);
            --  ECS.Vec2.Scale(Direction,100.0);
            --  T.Velocity.X := Direction.X;
            --  T.Velocity.Y := Direction.Y;
            null;
         when ECS.Event.L_MouseUp =>
            --  Self.MouseUp := True;
            --  Self.MouseDown := False;
            --  T.Velocity.X := 0.0;
            --  T.Velocity.Y := 0.0;
            null;
         when ECS.Event.MouseMove =>
            --  if Self.MouseDown then
            --     MousePos := (Float(Event.Data.MouseX), Float(Event.Data.MouseY));
            --     Direction := MousePos - T.Position;
            --     ECS.Vec2.Normalize(Direction);
            --     ECS.Vec2.Scale(Direction, 100.0);
            --     T.Velocity.X := Direction.X;
            --     T.Velocity.Y := Direction.Y;
            --  else
            --     T.Velocity.X := 0.0;
            --     T.Velocity.Y := 0.0;
            --  end if;
            null;
         when others =>
            null;
      end case;
   end Execute;
end ECS.System.User_Input;