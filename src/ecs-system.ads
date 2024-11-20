with ecs.component; use ecs.component;
with ecs.entity; use ecs.entity;
with Ada.Tags; use Ada.Tags;

package ecs.system is

    type System_T is interface;

    procedure Execute (Self : System_T;
                       Dt   : Duration; 
                       E    : access Entity_T'Class;  
                       ES   : Entities_T := Entities_T'(1 .. 0 => null)) is abstract;

    type System_Access is access all System_T'Class;

    type Mover_T is new System_T with null record;
    procedure Execute (Self : Mover_T;
                       Dt   : Duration; 
                       E    : access Entity_T'Class;  
                       ES   : Entities_T := Entities_T'(1 .. 0 => null));

    type Collision_T is new System_T with null record;
    procedure Execute (Self : Collision_T;
                       Dt   : Duration;
                       E    : access Entity_T'Class;
                       ES   : Entities_T := Entities_T'(1 .. 0 => null));

end ecs.system;