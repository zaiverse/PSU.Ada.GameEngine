with ECS.Component; use ECS.Component;
with ECS.Entity; use ECS.Entity;
with Ada.Tags; use Ada.Tags;
with ECS.Entity_Manager; use ECS.Entity_Manager;
with ECS.Event; use ECS.Event;
with ECS.Event_Manager; use ECS.Event_Manager;
with Renderer; use Renderer;
with Ada.Text_IO; use Ada.Text_IO;
with ECS.Vec2; use ECS.Vec2;
with Win32; use Win32;

package ECS.System is

   type System_T is interface;

   procedure Execute ( Self : in out System_T;
                       Dt   : Duration; 
                       Manager    : access Entity_Manager_T'Class ) is abstract;

   type System_Access is access all System_T'Class;

end ECS.System; 