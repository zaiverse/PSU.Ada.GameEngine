with Ada.Tags; use Ada.Tags;
with Ada.Numerics; use Ada.Numerics;

package ecs is
    subtype Id_T is String (1 .. 5);

    type Component_T is tagged null record;
    type Component_Access is access all Component_T'Class;
    type Components_T is array (Natural range <>) of Component_Access;

    type Entity_T (Count : Positive) is tagged record
        Id         : Id_T;
        Components : Components_T (0 .. Count);
    end record;

    type Entity_Access is access all Entity_T'Class;
    type Entities_T    is array (Natural range <>) of Entity_Access;

    function Get_Components (E: Entity_T'Class; Tag: Ada.Tags.Tag) return Components_T;

    type System_T is interface;
    procedure Execute (Self : System_T;
                       Dt   : Duration; 
                       E    : in out Entity_T'Class;  
                       ES   : Entities_T := Entities_T'(1 .. 0 => null)) is abstract;
    type System_Access is access all System_T'Class;

    type Transform_T is new Component_T with record
        X, Y : Float;
        Rotation : Float;
    end record;

    type Rigidbody_T is new Component_T with record
        Mass : Float;
    end record;

    type Mover_T is new System_T with null record;
    procedure Execute (Self : Mover_T;
                       Dt   : Duration; 
                       E    : in out Entity_T'Class;  
                       ES   : Entities_T := Entities_T'(1 .. 0 => null));

end ecs;