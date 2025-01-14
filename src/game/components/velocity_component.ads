with Components;

package Velocity_Component is

   type Velocity_Component is new Components.Component_Type with record
      VX: Float := 0.0;
      VY: FLoat := 0.0;
   end record;
   
   type Velocity_Component_Access is access all Velocity_Component;

end Velocity_Component;