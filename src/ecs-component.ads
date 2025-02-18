with Ada.Tags; use Ada.Tags;
with ECS.Vec2;
with Graphics.Color; use Graphics.Color;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Graphics.Renderer; use Graphics.Renderer;

package ECS.Component is 

   type Component_T is tagged null record;
   type Component_Access is access all Component_T'Class;
   type Components_T is array (Natural range <>) of Component_Access;

   type Transform_T is new Component_T with record
      Position : ECS.Vec2.Vec2;
      Velocity : ECS.Vec2.Vec2;
      Rotation : Float;
   end record;

   type Rigidbody_T is new Component_T with record
      Mass : Float;
   end record;
    -- Axis aligned bounding box for collision detection between objects
   type AABB_T is new Component_T with record
      Left   : Float;
      Bottom : Float;
      Right  : Float;
      Top    : Float;
   end record;

   type Collision_Params_T is new Component_T with record
      Collision_Enabled    : Boolean;
      Collision_Occurred   : Boolean := False;
      Destroy_On_Collision : Boolean := False;
      Wall_Collision       : Boolean := False;
   end record;

   type Circle_T is new Component_T with record
      Sides    : Positive;
      Radius   : Positive;
      C        : Color;
   end record;
   
   type Quad_T is new Component_T with record
      Width    : Float;
      Height   : Float;
      C        : Color;
   end record;

   type Text_T is new Component_T with record
      Text    : Unbounded_String;
      C       : Color;
   end record;

   type Texture_T is new Component_T with record
      Width    : Integer;
      Height   : Integer;
      Data : Storage_Array_Access;
   end record;

end ECS.Component;