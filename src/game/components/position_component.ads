with Components;

package Position_Component is

   type Position_Component is new Components.Component_Type with record
      X: Float := 0.0;
      Y: Float := 0.0;
   end record;

   type Position_Component_Access is access all Position_Component;

end Position_Component;