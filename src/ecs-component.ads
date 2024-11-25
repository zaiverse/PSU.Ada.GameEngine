with Ada.Tags; use Ada.Tags;

package ECS.Component is 

    type Component_T is tagged null record;
    type Component_Access is access all Component_T'Class;
    type Components_T is array (Natural range <>) of Component_Access;

    type Transform_T is new Component_T with record
        X, Y : Float;
        VX,VY : Float;
        Rotation : Float;
    end record;

    type Rigidbody_T is new Component_T with record
        Mass : Float;
    end record;
    -- Axis aligned bounding box for collision detection between objects
    type AABB_T is new Component_T with record
        Left : Float;
        Bottom : Float;
        Right : Float;
        Top : Float;
    end record;

    type Collision_Params_T is new Component_T with record
      Collision_Enabled : Boolean;
      Destroy_On_Collision : Boolean;
      -- Change in velocity if Destroy_On_Collision is false
      Delta_VX : Float; 
      Delta_VY : Float; 
    end record;

    
end ECS.Component;