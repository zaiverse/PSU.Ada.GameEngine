with Ada.Text_IO; use Ada.Text_IO;
with ECS.Entity;
with ECS.Entity_Manager; use ECS.Entity_Manager;
with ECS.Component; use ECS.Component;
with ECS.Config_Loader; use ECS.Config_Loader;
with Window; use Window;
with Win32; use Win32;
with Graphics.Renderer; use Graphics.Renderer;
with Ada.Real_Time; use Ada.Real_Time;
with Graphics.Color; use Graphics.Color;
with ECS.Event_Manager; use ECS.Event_Manager;
with ecs.System.Render; use ecs.System.Render;
with ECS.System.Movement; use ECS.System.Movement;
with ECS.System.Collision; use ECS.System.Collision;
with ECS.System.Animation; use ECS.System.Animation;
with Graphics.Texture_Loader; use Graphics.Texture_Loader;
with ECS.System.User_Input; use ECS.System.User_Input;
with ECS.Event; use ECS.Event;

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Text_IO; use Ada.Text_IO;

procedure ECS_Config_Window_System_Test is
   Manager : aliased Entity_Manager_T;
   Window_Width : Integer;
   Window_Height : Integer;
   Window_Color : Graphics.Color.Color;
   User_Input_System : User_Input_T;

   procedure Print_Entity_Info(Entity : ECS.Entity.Entity_Access) is
      Transform : constant Component_Access := Entity.Get_Component(Transform_T'Tag);
      Quad      : constant Component_Access := Entity.Get_Component(Quad_T'Tag);
      Texture   : constant Component_Access := Entity.Get_Component(Texture_T'Tag);
      Animation : constant Component_Access := Entity.Get_Component(Animation_Component_T'Tag);
   begin
      Put_Line("Entity ID: " & Entity.Id);
      if Transform /= null then
         Put_Line("  Transform: Position(" &
                   Float'Image(Transform_T(Transform.all).Position.X) & ", " &
                   Float'Image(Transform_T(Transform.all).Position.Y) & "), Velocity(" &
                   Float'Image(Transform_T(Transform.all).Velocity.X) & ", " &
                   Float'Image(Transform_T(Transform.all).Velocity.Y) & "), Rotation(" &
                   Float'Image(Transform_T(Transform.all).Rotation) & ")");
      else
         Put_Line("  Transform: Not Initialized");
      end if;

      if Quad /= null then
         Put_Line("  Quad: Width(" & Float'Image(Quad_T(Quad.all).Width) & "), Height(" &
                   Float'Image(Quad_T(Quad.all).Height) & "), Color(" &
                   Graphics.Color.Color_Int'Image(Quad_T(Quad.all).C.R) & ", " &
                   Graphics.Color.Color_Int'Image(Quad_T(Quad.all).C.G) & ", " &
                   Graphics.Color.Color_Int'Image(Quad_T(Quad.all).C.B) & ", " &
                   Graphics.Color.Color_Int'Image(Quad_T(Quad.all).C.A) & ")");
      else
         Put_Line("  Quad: Not Initialized");
      end if;

      if Texture /= null then
         Put_Line("  Texture: Width(" & Integer'Image(Texture_T(Texture.all).Width) & "), Height(" &
                   Integer'Image(Texture_T(Texture.all).Height) & ")");
      else
         Put_Line("  Texture: Not Initialized");
      end if;

      if Animation /= null then
         Put_Line("  Animation: Current State(" & Entity_State'Image(Animation_Component_T(Animation.all).Current) & ")");
      else
         Put_Line("  Animation: Not Initialized");
      end if;
   end Print_Entity_Info;

begin
   -- Load configuration from INI file and initialize systems
   Load_Config(Manager, "..\src\tests\Config Tests\entities.ini", Window_Width, Window_Height, Window_Color, User_Input_System);

   -- Update the manager to process any pending entities
   Manager.Update;

   -- Print initialized entities and components
   Put_Line("Initialized Entities and Components:");
   for Entity of Manager.Entities loop
      Print_Entity_Info(Entity);
   end loop;

   -- Initialize and start the game engine systems
   Initialize_Systems(Manager, Window_Width, Window_Height, Window_Color, User_Input_System);
end ECS_Config_Window_System_Test;
