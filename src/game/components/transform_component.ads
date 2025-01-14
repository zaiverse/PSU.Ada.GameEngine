with Components;
with Math; -- Assuming Math provides Vector2D and relevant operations

package Transform_Component is

   -- Transform Component Type
   type Transform_Component_T is new Components.Component_Type with record
      Position : Math.Vector2D := (X => 0.0, Y => 0.0); -- Entity position
      Rotation : Float := 0.0;                         -- Rotation in radians
      Scale    : Float := 1.0;                         -- Uniform scale factor
   end record;

   -- Access Type for Transform Components
   type Transform_Component_Access is access all Transform_Component_T;

end Transform_Component;
