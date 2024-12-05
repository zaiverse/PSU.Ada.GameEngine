with ECS.Component; use ECS.Component;
with ECS.Entity; use ECS.Entity;
with Ada.Tags; use Ada.Tags;
with ECS.Entity_Manager; use ECS.Entity_Manager;
with Win32; use Win32;
package ECS.System is

   type System_T is interface;

   procedure Execute ( Self : System_T;
                       Dt   : Duration; 
                       Manager    : access Entity_Manager_T'Class ) is abstract;

   type System_Access is access all System_T'Class;

   type Mover_T is new System_T with null record;

   procedure Execute ( Self : Mover_T;
                       Dt   : Duration; 
                       Manager    : access Entity_Manager_T'Class );

   type Collision_T is new System_T with null record;

   procedure Execute ( Self : Collision_T;
                       Dt   : Duration;
                       Manager    : access Entity_Manager_T'Class );

   type Render_T is new System_T with record
      Width : Natural;
      Buffer : Byte_Array_Access;
   end record;

   procedure Execute (Self : Render_T;
                      Dt   : Duration;
                      Manager : access Entity_Manager_T'Class );

end ECS.System;