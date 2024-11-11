with ecs.component; use ecs.component;
with Ada.Tags; use Ada.Tags;

package ecs.entity is 

    subtype Id_T is String (1 .. 5);

    type Entity_T (Count : Positive) is tagged record
        Id         : Id_T;
        Components : Components_T (0 .. Count);
    end record;

    type Entity_Access is access all Entity_T'Class;
    type Entities_T    is array (Natural range <>) of Entity_Access;

    function Get_Components (E: Entity_T'Class; Tag: Ada.Tags.Tag) return Components_T;

end ecs.entity;