with ECS.Component; use ECS.Component;
with ECS.Entity; use ECS.Entity;
with Ada.Tags; use Ada.Tags;
with ECS.Entity_Manager; use ECS.Entity_Manager;
with ECS.Event; use ECS.Event;
with ECS.Event_Manager; use ECS.Event_Manager;
with Win32; use Win32;
package ECS.System is

   type System_T is interface;

   procedure Execute ( Self : in out System_T;
                       Dt   : Duration; 
                       Manager    : access Entity_Manager_T'Class ) is abstract;

   type System_Access is access all System_T'Class;

   type Mover_T is new System_T with null record;

   procedure Execute ( Self : in out Mover_T;
                       Dt   : Duration; 
                       Manager    : access Entity_Manager_T'Class );

   type Collision_T is new System_T with null record;

   procedure Execute ( Self : in out Collision_T;
                       Dt   : Duration;
                       Manager    : access Entity_Manager_T'Class );

   type Render_T is new System_T with record
      Width : Natural;
      Buffer : Byte_Array_Access;
   end record;

   procedure Execute (Self : in out Render_T;
                      Dt   : Duration;
                      Manager : access Entity_Manager_T'Class );
   
   type User_Input_T is new System_T with record
      Player_Entity : Entity_Access;
      Handler       : Platform_Event_Handler_Access;
      MouseDown     : Boolean := False;
      MouseUp       : Boolean := True;
   end record;

   procedure Execute (Self : in out User_Input_T;
                      Dt   : Duration;
                      Manager : access Entity_Manager_T'Class );


end ECS.System;